


# Import the tornado data with LC proportions
tor_LC_df <- read.csv("data/raw/Tor_data_with_LC.csv")


# The distribution as presented
hist(tor_LC_df$DAMAGE_PROPERTY,
     breaks = 100)


# Defining a simple mean normalization function
mean_normalize <- function(to_normalize){
  
  # descr:  simple mean normalization... (x - mean(x))/sd(x)
  # arg:    thing to normalize
  # return: that thing normalized
  
  numerator <- to_normalize - mean(to_normalize)
  
  normalized <- numerator / sd(to_normalize)
  
  return(normalized)
  
}


# The distribution mean-normalized
hist(mean_normalize(tor_LC_df$DAMAGE_PROPERTY),
     breaks = 100)
# This is largely the same thing - really


# The distribution mean-normalized and log-transformed
hist(mean_normalize(log(tor_LC_df$DAMAGE_PROPERTY + 1, exp(1))),
     breaks = 100)
# This is real progress away from the data distribution as presented


