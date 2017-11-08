


# Packages
library(dplyr)
library(gridExtra)
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
not_processed_df <- read.csv("data/raw/tor_data_with_derived.csv")


# Get the values needed to undo the mean normalization
log_mean <- mean(log(not_processed_df$DAMAGE_PROPERTY + 1))
log_sd <- sd(log(not_processed_df$DAMAGE_PROPERTY + 1))


# Undo the mean normalization and get them in log10 scale
predictions_df$predicted_values <- ((predictions_df$predicted_values * log_sd) + log_mean) %>%
  exp() %>%
  log10()

predictions_df$true_values <- ((predictions_df$true_values * log_sd) + log_mean) %>%
  exp() %>%
  log10()


# Get better labels for the facetting
predictions_df$label <- ifelse(predictions_df$notebook_number == 11,
                                'Before. with Zeros',
                                ifelse(predictions_df$notebook_number == 12,
                                       'Storm Character. with Zeros',
                                       ifelse(predictions_df$notebook_number == 13,
                                              'Combined with Zeros',
                                              ifelse(predictions_df$notebook_number == 14,
                                                     'Before. without Zeros',
                                                     ifelse(predictions_df$notebook_number == 15,
                                                            'Storm Character. without Zeros',
                                                            ifelse(predictions_df$notebook_number == 16,
                                                                   'Combined without Zeros',
                                                                   ifelse(predictions_df$notebook_number == 17,
                                                                          'Deep Models',
                                                                          ifelse(predictions_df$notebook_number == 18,
                                                                                 'Wide Models',
                                                                                 ifelse(predictions_df$notebook_number == 19,
                                                                                        'Wide Once Reg.',
                                                                                        ifelse(predictions_df$notebook_number == 20,
                                                                                               'Wide Twice Reg.',
                                                                                               ifelse(predictions_df$notebook_number == 21,
                                                                                                      'Deep ELU',
                                                                                                      ifelse(predictions_df$notebook_number == 22,
                                                                                                             'Wide ELU',
                                                                                                             ifelse(predictions_df$notebook_number == 23,
                                                                                                                    'Descending ELU',
                                                                                                                    'Error'))))))))))))) %>%
  as.factor()


# Get the levels in a more appealing order
predictions_df$label <- factor(predictions_df$label,
                               levels(predictions_df$label)[c(1, 8, 3, 2,
                                                              9, 4, 6, 11,
                                                              12, 13, 7, 5,
                                                              10)])


# Plot the predicted versus observed values for the best model from each notebook
ggplot(predictions_df,
       aes(x = true_values,
           y = predicted_values)) +
  geom_point(pch = 21,
             size = 0.9) +
  facet_wrap(~label, ncol = 4) +
  scale_x_continuous(lim = c(-1, 10),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_y_continuous(lim = c(-1, 10),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  geom_abline(intercept = 0,
              slope = 1) +
  theme_bw() +
  labs(x = 'Observed Value',
       y = 'Predicted Value',
       title = 'Cross-Validation Set Performance',
       subtitle = 'Units = Magnitude of US dollars') +
  theme(aspect.ratio = 4/5,
        plot.title = element_text(hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 15),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))


# Filter out the non-holistic models
param_plot_df <- dplyr::filter(metrics_df,
                               notebook_id >= 16)


# Keep the notebooks that test different parameter numbers
# Other than dropout percent and l2 penalty
param_plot_df <- dplyr::filter(param_plot_df,
                               notebook_id %in% c(16, 17, 18, 21, 22, 23))


# Label the models as neural network or multivariate regression
param_plot_df$model_type <- ifelse(param_plot_df$Number.of.Parameters > 100,
                                   'Neural Network', 'Multivariate Regression')


# Get better labels
param_plot_df$label <- ifelse(param_plot_df$notebook_id == 16,
                              'Combined without Zeros',
                              ifelse(param_plot_df$notebook_id == 17,
                                     'Deep Models',
                                     ifelse(param_plot_df$notebook_id == 18,
                                            'Wide Models',
                                            ifelse(param_plot_df$notebook_id == 21,
                                                   'Deep ELU',
                                                   ifelse(param_plot_df$notebook_id == 22,
                                                          'Wide ELU',
                                                          ifelse(param_plot_df$notebook_id == 23,
                                                                 'Descending ELU',
                                                                 'Error')))))) %>%
  as.factor()


# Get proper ordering of labels
param_plot_df$label <- factor(param_plot_df$label,
                              levels(param_plot_df$label)[c(1, 3, 6,
                                                            4, 2, 5)])


# Plot MSE by number of parameters for each notebook
ggplot(param_plot_df,
       aes(x = Number.of.Parameters,
           y = Mean.Squared.Error,
           shape = model_type)) +
  facet_wrap(~label,
             scale = 'free',
             ncol = 3) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  labs(x = 'Number of Parameters',
       y = 'Mean Squared Error',
       title = 'How does Complexity affect Performance?',
       shape = 'Model Type') +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 13))


# Plot far-end MSE by number of parameters for each notebook
ggplot(param_plot_df,
       aes(x = Number.of.Parameters,
           y = Mean.Squared.Error.Over.1M,
           shape = model_type)) +
  facet_wrap(~label,
             scale = 'free') +
  labs(x = 'Number of Parameters',
       y = 'Mean Squared Error',
       title = 'How does Complexity affect Performance on Extreme Events?',
       subtitle = 'Extreme Events = Tornadoes that caused > $1,000,000',
       shape = 'Model Type') +
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


# Undo the mean normalization and get in log 10 scale
best_models_test_perf$true_values <- ((best_models_test_perf$true_values * log_sd) + log_mean) %>%
  exp() %>%
  log10()

best_models_test_perf$predicted_values <- ((best_models_test_perf$predicted_values*log_sd) + log_mean) %>%
  exp() %>%
  log10()


# Plot the predicted versus true for the best model's test set performance
ggplot(best_models_test_perf,
       aes(x = true_values,
           y = predicted_values)) +
  geom_abline(intercept = 0,
              slope = 1) +
  geom_point(size = 3,
             pch = 21) +
  scale_x_continuous(lim = c(0, 10),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_y_continuous(lim = c(0, 10),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_bw() +
  labs(x = 'Observed Values',
       y = 'Predicted Values',
       title = 'Test Set Performance',
       subtitle = 'Units = Magnitude of US dollars') +
  theme(aspect.ratio = 8/9,
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(hjust = 0.5, size = 13)) +
  annotate('text', x = 0.4, y = 10,
           label = paste0('R^2: ', as.character(R_squared)),
           parse = TRUE) +
  annotate('text', x = 0.4, y = 9.25,
           label = paste0('MSE: ', as.character.Date(test_MSE)),
           parse = TRUE)


# Make a table
# Renaming some columns
colnames(best_of_metrics) <- c("X",
                               "Number of Parameters",
                               "R-squared",
                               "Mean Squared Error",
                               "Mean Squared Error Over 1M",
                               "notebook_id",
                               "model_id")


# Give better labels
best_of_metrics$Label <- ifelse(best_of_metrics$notebook_id == 11,
                                'Before. with Zeros',
                                ifelse(best_of_metrics$notebook_id == 12,
                                       'Storm Character. with Zeros',
                                       ifelse(best_of_metrics$notebook_id == 13,
                                              'Combined with Zeros',
                                              ifelse(best_of_metrics$notebook_id == 14,
                                                     'Before. without Zeros',
                                                     ifelse(best_of_metrics$notebook_id == 15,
                                                            'Storm Character. without Zeros',
                                                            ifelse(best_of_metrics$notebook_id == 16,
                                                                   'Combined without Zeros',
                                                                   ifelse(best_of_metrics$notebook_id == 17,
                                                                          'Deep Models',
                                                                          ifelse(best_of_metrics$notebook_id == 18,
                                                                                 'Wide Models',
                                                                                 ifelse(best_of_metrics$notebook_id == 19,
                                                                                        'Wide Once Reg.',
                                                                                        ifelse(best_of_metrics$notebook_id == 20,
                                                                                               'Wide Twice Reg.',
                                                                                               ifelse(best_of_metrics$notebook_id == 21,
                                                                                                      'Deep ELU',
                                                                                                      ifelse(best_of_metrics$notebook_id == 22,
                                                                                                             'Wide ELU',
                                                                                                             ifelse(best_of_metrics$notebook_id == 23,
                                                                                                                    'Descending ELU',
                                                                                                                    'Error'))))))))))))) %>%
  as.factor()


# Remove irrelevant information
best_of_metrics <- dplyr::select(best_of_metrics,
                                 -c(X,
                                    notebook_id,
                                    model_id))


# Rounding the numbers
best_of_metrics$`R-squared` <- signif(best_of_metrics$`R-squared`,
                                      digits = 3)

best_of_metrics$`Mean Squared Error` <- signif(best_of_metrics$`Mean Squared Error`,
                                               digits = 3)

best_of_metrics$`Mean Squared Error Over 1M` <- signif(best_of_metrics$`Mean Squared Error Over 1M`,
                                                       digits = 3)


# Better order of variables
best_of_metrics <- dplyr::select(best_of_metrics,
                                 c(Label,
                                   `Mean Squared Error`,
                                   `R-squared`,
                                   `Mean Squared Error Over 1M`,
                                   `Number of Parameters`))


# Order the table
best_of_metrics <- best_of_metrics[order(-best_of_metrics$`Mean Squared Error`),]


# Plot it
dev.off()

grid.table(best_of_metrics,
           rows = NULL,
           theme = ttheme_minimal())


