


# Import data
tor_df <- read.csv("data/raw/Tor_data_with_mob_home.csv")


# Functions that need defining
# Define a simple mean normalization function
mean_normalize <- function(to_normalize){
  
  # descr:  simple mean normalization... (x - mean(x))/sd(x)
  # arg:    thing to normalize
  # return: that thing normalized
  
  numerator <- to_normalize - mean(to_normalize)
  
  normalized <- numerator / sd(to_normalize)
  
  return(normalized)
  
}


# Define a mean normalization following a log-transformation
mean_norm_log_xform <- function(to_process) {
  
  # descr:  log transform (base e) then mean normalize
  # arg:    thing to process
  # return: that thing processed
  
  log_xformed <- log(to_process + 1,
                     base = exp(1))
  
  mean_norm_log_xformed_variable <- mean_normalize(log_xformed)
  
  return(mean_norm_log_xformed_variable)
  
}


# Define a mean normalization following a log-transformation
# following a multiplication
mean_norm_log_xform_prop <- function(to_process) {
  
  # descr:  multiple by 10000, then log transform (base e), then mean normalize
  #         this is for proportions, the 10000 multiplications makes the log
  #         transformation more effective
  # arg:    thing to process
  # return: that thing processed
  
  to_process_10000 <- to_process * 10000
  
  log_xformed <- log(to_process_10000 + 1,
                     base = exp(1))
  
  mean_norm_log_xformed_variable <- mean_normalize(log_xformed)
  
  return(mean_norm_log_xformed_variable)
  
}


# Let's have time start at 7 AM, expected sunrise
tor_df$after_7 <- tor_df$BEGIN_TIME > 420

tor_after_7 <- tor_df[tor_df$after_7 == TRUE, ]
tor_before_7 <- tor_df[tor_df$after_7 == FALSE, ]

tor_after_7$TIME <- tor_after_7$BEGIN_TIME - 420
tor_before_7$TIME <- tor_before_7$BEGIN_TIME + 1020


##################################################
##################################################
# LOOK AT THE MONTH GIF AND DO SOMETHING SIMILAR #
# AS THE ABOVE TO MEASURE MONTH INTEGER BY #######
##################################################


