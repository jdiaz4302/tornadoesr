


# Packages
library(dplyr)
library(ggplot2)


# Get all the files from the directory
files <- list.files('Complete_Workflow/')


# Get the indices of the metrics csv files
metrics_indices <- grep('metrics.csv', files)


# Subset files to only those indices
files <- files[metrics_indices]


# Get the full file names
file.names <- paste0('Complete_Workflow/', files)


# Load in all the metrics csv files
for (i in 1:length(files)) {
  
  if (i == 1) {
    
    # Make the data.frame on the first iteration
    metrics_df <- read.csv(file.names[i])
    
  } else {
    
    # Make temporary data.frames on other iterations
    temp_df <- read.csv(file.names[i])
    # Append those temporary data.frames to the true data.frame
    metrics_df <- rbind(metrics_df, temp_df)
    
  }
  
}


# Remove the temporary data.frame
rm(temp_df)


# Fix the index column
metrics_df$X <- 1:nrow(metrics_df)


# Removing very poor models
metrics_df <- dplyr::filter(metrics_df,
                            R.squared > 0.01)


# Get the lowest MSE from each notebook / model type
best_of_metrics <- data.table::data.table(metrics_df)
best_of_metrics <- best_of_metrics[, list(min_MSE = min(Mean.Squared.Error)),
                   by = notebook_id]


# Match the lowest MSEs to the data.frame MSE values
best_of_metrics <- best_of_metrics[match(metrics_df$Mean.Squared.Error, best_of_metrics$min_MSE)][, 2] %>%
  data.frame()
best_of_metrics <- cbind(metrics_df, best_of_metrics) %>%
  na.omit()
best_of_metrics <- dplyr::select(best_of_metrics, -min_MSE)




# Get all the files from the directory
files <- list.files('Complete_Workflow/')


# Get the indices of the metrics csv files
metrics_indices <- grep('cv_perf.csv', files)


# Subset files to only those indices
files <- files[metrics_indices]


# Get the full file names
file.names <- paste0('Complete_Workflow/', files)


# Load in all the prediction csv files
for (i in 1:length(files)) {
  
  if (i == 1) {
    
    # Make the data.frame on the first iteration
    predictions_df <- read.csv(file.names[i])
    
  } else {
    
    # Make temporary data.frames on other iterations
    temp_df <- read.csv(file.names[i])
    # Append those temporary data.frames to the true data.frame
    predictions_df <- rbind(predictions_df, temp_df)
    
  }
  
}


# Remove the temporary data.frame
rm(temp_df)


# Match the lowest MSE for each notebook to the cross-validation predictions
# Non-best models for each notebook will recieve NA values
predictions_df$cv_MSE <- best_of_metrics$Mean.Squared.Error[match(do.call(paste,
                                                                          dplyr::select(predictions_df,
                                                                                        c(notebook_number,
                                                                                          model_number))),
                                                                  do.call(paste,
                                                                          dplyr::select(best_of_metrics,
                                                                                        c(notebook_id,
                                                                                          model_id))))]


# Remove the non-best models by omitting NA's
predictions_df <- na.omit(predictions_df)


# Import the unprocessed data
not_processed_df <- read.csv("data/raw/tor_data_with_interact_effects.csv")


# Get the values needed to undo the mean normalization
log_mean <- mean(log(not_processed_df$DAMAGE_PROPERTY + 1))
log_sd <- sd(log(not_processed_df$DAMAGE_PROPERTY + 1))


# Undo the mean normalization
predictions_df$predicted_values <- predictions_df$predicted_values*log_sd + log_mean
predictions_df$true_values <- predictions_df$true_values*log_sd + log_mean


# Plot the predicted versus observed values for the best model from each notebook
ggplot(predictions_df,
       aes(x = true_values,
           y = predicted_values)) +
  geom_point(pch = 21,
             size = 0.9) +
  facet_wrap(~notebook_number) +
  scale_x_continuous(limits = c(-2, 22)) +
  scale_y_continuous(limits = c(-2, 22)) +
  geom_abline(intercept = 0,
              slope = 1) +
  theme_bw() +
  labs(x = 'Observed Value',
       y = 'Predicted Value',
       title = 'Cross-Validation Set Performance',
       subtitle = 'On Log-Transformed US dollars + 1') +
  theme(aspect.ratio = 4/5,
        plot.title = element_text(hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 15))


# Filter out the non-holistic models
param_plot_df <- dplyr::filter(metrics_df,
                               notebook_id >= 17)


# Keep the notebooks that test different parameter numbers
# Other than dropout percent and l2 penalty
param_plot_df <- dplyr::filter(param_plot_df,
                               notebook_id %in% c(17, 18, 19, 22, 23, 24))


# Label the models as neural network or multivariate regression
param_plot_df$model_type <- ifelse(param_plot_df$Number.of.Parameters > 100,
                                   'Neural Network', 'Multivariate Regression')


# Plot MSE by number of parameters for each notebook
ggplot(param_plot_df,
       aes(x = Number.of.Parameters,
           y = Mean.Squared.Error,
           shape = model_type)) +
  facet_wrap(~notebook_id,
             scale = 'free') +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  labs(x = 'Number of Parameters',
       y = 'Mean Squared Error',
       title = 'How does Complexity affect Performance?') +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 13)) 


# Plot far-end MSE by number of parameters for each notebook
ggplot(param_plot_df,
       aes(x = Number.of.Parameters,
           y = Mean.Squared.Error.Over.1M,
           shape = model_type)) +
  facet_wrap(~notebook_id,
             scale = 'free') +
  labs(x = 'Number of Parameters',
       y = 'Mean Squared Error',
       title = 'How does Complexity affect Performance on Extreme Events?',
       subtitle = 'Extreme Events = Tornadoes that caused > $1,000,000') +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 13))


# Get the absolute lowest MSE
best_MSE <- min(best_of_metrics$Mean.Squared.Error)


# Filter out MSE that aren't the lowest
best_model <- dplyr::filter(best_of_metrics,
                            Mean.Squared.Error == best_MSE)


# Make notebook_id a character so we can easily get the file path
best_notebook <- as.character(best_model$notebook_id)


# Get the file path
best_model_filename <- paste0('Complete_Workflow/',
                              best_notebook,
                              '_test_perf.csv')


# Read the .csv file for that notebook
best_models_test_perf <- read.csv(best_model_filename)


# Filter to the best model
best_models_test_perf <- dplyr::filter(best_models_test_perf,
                                       model_number == best_model$model_id)


# Compute the test set MSE
best_models_test_perf$Squared.Error <- best_models_test_perf$predicted_values -
  best_models_test_perf$true_values
best_models_test_perf$Squared.Error <- best_models_test_perf$Squared.Error^2
test_MSE <- sum(best_models_test_perf$Squared.Error) / length(best_models_test_perf$Squared.Error)


# Compute the test set R-squared
best_models_test_perf$true_mean <- mean(best_models_test_perf$true_values)

best_models_test_perf$tot_sum_square <- best_models_test_perf$true_mean -
  best_models_test_perf$true_values
best_models_test_perf$tot_sum_square <- best_models_test_perf$tot_sum_square^2
tot_sum_square <- sum(best_models_test_perf$tot_sum_square)


best_models_test_perf$resid_sum_square <- best_models_test_perf$predicted_values -
  best_models_test_perf$true_values
best_models_test_perf$resid_sum_square <- best_models_test_perf$resid_sum_square^2
resid_sum_square <- sum(best_models_test_perf$resid_sum_square)

R_squared <- 1 - (resid_sum_square / tot_sum_square)


# Undo the mean normalization
best_models_test_perf$true_values <- best_models_test_perf$true_values*log_sd + log_mean
best_models_test_perf$predicted_values <- best_models_test_perf$predicted_values*log_sd + log_mean


# Plot the predicted versus true for the best model's test set performance
ggplot(best_models_test_perf,
       aes(x = true_values,
           y = predicted_values)) +
  geom_point(size = 2,
             alpha = .5) +
  scale_x_continuous(limits = c(1.0952, 23)) +
  scale_y_continuous(limits = c(1.0952, 23)) +
  geom_abline(intercept = 0,
              slope = 1) +
  theme_bw() +
  labs(x = 'Observed Values',
       y = 'Predicted Values',
       title = 'Test Set Performance',
       subtitle = 'On Log-Transformed US dollars + 1') +
  theme(aspect.ratio = 8/9,
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(hjust = 0.5, size = 13)) +
  annotate('text', x = 3, y = 23,
           label = paste0('R^2: ', as.character(R_squared)),
           parse = TRUE) +
  annotate('text', x = 3.5, y = 22,
           label = paste0('MSE: ', as.character.Date(test_MSE)),
           parse = TRUE)


