


# Import the tornado data with LC proportions
tor_LC_df <- read.csv("data/raw/Tor_data_with_LC.csv")


# Defining a simple mean normalization function
mean_normalize <- function(to_normalize) {
  
  # descr:  simple mean normalization... (x - mean(x))/sd(x)
  # arg:    thing to normalize
  # return: that thing normalized
  
  numerator <- to_normalize - mean(to_normalize)
  
  normalized <- numerator / sd(to_normalize)
  
  return(normalized)
  
}


# Defining a mean normalization following a log-transformation
mean_norm_log_xform <- function(to_process) {
  
  # descr:  log transform (base e) then mean normalize
  # arg:    thing to process
  # return: that thing processed
  
  log_xformed <- log(to_process + 1,
                     base = exp(1))
  
  mean_norm_log_xformed_variable <- mean_normalize(log_xformed)
  
  return(mean_norm_log_xformed_variable)
  
}


### Lets start with duration ###
# The distribution as is; not good
hist(tor_LC_df$DURATION_SECONDS,
     breaks = 100)


# The distribution normalized; not good
hist(mean_normalize(tor_LC_df$DURATION_SECONDS),
     breaks = 100)


# The distribution normalized, log-transformed
# NOT BAD
hist(mean_norm_log_xform(tor_LC_df$DURATION_SECONDS),
     breaks = 100)


### Now latitude ###
# The distribution as is; not bad
hist(tor_LC_df$BEGIN_LAT,
     breaks = 100)


# The distribution normalized; not bad
# GOING WITH THIS
hist(mean_normalize(tor_LC_df$BEGIN_LAT),
     breaks = 100)


# The distribution normalized, log-transformed
# MAY BE WORSE
# but I think I prefer consistancy
hist(mean_norm_log_xform(tor_LC_df$BEGIN_LAT),
     breaks = 100)


### Now longitude ###
# The distribution as is; not bad
hist(tor_LC_df$BEGIN_LON,
     breaks = 100)


# The distribution normalized; not bad
# GOING WITH THIS
hist(mean_normalize(tor_LC_df$BEGIN_LON),
     breaks = 100)


# The distribution normalized, log-transformed
# CANT DO BECAUSE OF NEGATIVE VALUES
hist(mean_norm_log_xform(tor_LC_df$BEGIN_LON),
     breaks = 100)


### Now length ###
# The distribution as is; not good
hist(tor_LC_df$TOR_LENGTH,
     breaks = 100)


# The distribution normalized; not good
hist(mean_normalize(tor_LC_df$TOR_LENGTH),
     breaks = 100)


# The distribution normalized, log-transformed
# I believe this is better
# GOING WITH THIS
hist(mean_norm_log_xform(tor_LC_df$TOR_LENGTH),
     breaks = 100)


### Now width ###
# The distribution as is; not good
hist(tor_LC_df$TOR_WIDTH,
     breaks = 100)


# The distribution normalized; not good
hist(mean_normalize(tor_LC_df$TOR_WIDTH),
     breaks = 100)


# The distribution normalized, log-transformed
# Much better
# GOING WITH THIS
hist(mean_norm_log_xform(tor_LC_df$TOR_WIDTH),
     breaks = 100)


### Now date/time ###
# The distribution as is; neutral
hist(tor_LC_df$BEGIN_DATE_TIME,
     breaks = 100)


# The distribution normalized; neutral
# GOING TO GO WITH THIS
hist(mean_normalize(tor_LC_df$BEGIN_DATE_TIME),
     breaks = 100)


# The distribution normalized, log-transformed
# didn't really change
hist(mean_norm_log_xform(tor_LC_df$BEGIN_DATE_TIME),
     breaks = 100)


### Now Open water ###
# The distribution as is; not good
hist(tor_LC_df$OPEN_WATER_PROP,
     breaks = 100)


# The distribution normalized, not good
hist(mean_normalize(tor_LC_df$OPEN_WATER_PROP),
     breaks = 100)


# The distribution normalized, log-transformed
# Times a thousand to make the log-transformation useful
hist(mean_norm_log_xform(10000 * tor_LC_df$OPEN_WATER_PROP),
     breaks = 100)


### Now developed open space ###
# The distribution as is; neutral
hist(tor_LC_df$DEV_OPEN_PROP,
     breaks = 100)


# The distribution normalized; neutral
hist(mean_normalize(tor_LC_df$DEV_OPEN_PROP),
     breaks = 100)


# The distribution normalized, log-transformed
# This is really good
hist(mean_norm_log_xform(10000 * tor_LC_df$DEV_OPEN_PROP),
     breaks = 100)
# This times 10,000 method seems to work really well for the proportions


# Try it for the other developed land covers
hist(mean_norm_log_xform(10000 * tor_LC_df$DEV_LOW_PROP),
     breaks = 100)

hist(mean_norm_log_xform(10000 * tor_LC_df$DEV_MED_PROP),
     breaks = 100)

hist(mean_norm_log_xform(10000 * tor_LC_df$DEV_HIGH_PROP),
     breaks = 100)


# Try it for decid forest, barren land, and crops
hist(mean_norm_log_xform(10000 * tor_LC_df$DECID_FOREST_PROP),
     breaks = 100)

hist(mean_norm_log_xform(10000 * tor_LC_df$BARREN_LAND_PROP),
     breaks = 100)

hist(mean_norm_log_xform(10000 * tor_LC_df$CULT_CROPS_PROP),
     breaks = 100)
# This is looking like a viable method of decompressing the variables
# While homogenizing the range and variance
# So I'm going to keep it