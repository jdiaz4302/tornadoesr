


# Packages
library(readr)
library(reshape2)
library(ggplot2)


# Import the data
tor_LC_df <- read.csv("data/raw/Tor_data_with_interact_effects.csv")


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


# Turns out that ICE_SNOW_PROP only has one entry that isn't
# a value of 0, so remove it
tor_LC_df <- dplyr::select(tor_LC_df,
                           -ICE_SNOW_PROP)


# Process the Storm Events variables
tor_LC_df$DURATION_SECONDS <- mean_norm_log_xform(tor_LC_df$DURATION_SECONDS)

tor_LC_df$BEGIN_LAT <- mean_normalize(tor_LC_df$BEGIN_LAT)

tor_LC_df$BEGIN_LON <- mean_normalize(tor_LC_df$BEGIN_LON)

tor_LC_df$TOR_LENGTH <- mean_norm_log_xform(tor_LC_df$TOR_LENGTH)

tor_LC_df$TOR_WIDTH <- mean_norm_log_xform(tor_LC_df$TOR_WIDTH)


# Process the outcome variable
tor_LC_df$DAMAGE_PROPERTY <- mean_norm_log_xform(tor_LC_df$DAMAGE_PROPERTY)


# Process the land cover values
tor_LC_df$OPEN_WATER_PROP <- mean_norm_log_xform_prop(tor_LC_df$OPEN_WATER_PROP)

tor_LC_df$DEV_OPEN_PROP <- mean_norm_log_xform_prop(tor_LC_df$DEV_OPEN_PROP)

tor_LC_df$DEV_LOW_PROP <- mean_norm_log_xform_prop(tor_LC_df$DEV_LOW_PROP)

tor_LC_df$DEV_MED_PROP <- mean_norm_log_xform_prop(tor_LC_df$DEV_MED_PROP)

tor_LC_df$DEV_HIGH_PROP <- mean_norm_log_xform_prop(tor_LC_df$DEV_HIGH_PROP)

tor_LC_df$DECID_FOREST_PROP <- mean_norm_log_xform_prop(tor_LC_df$DECID_FOREST_PROP)

tor_LC_df$EVERGR_FOREST_PROP <- mean_norm_log_xform_prop(tor_LC_df$EVERGR_FOREST_PROP)

tor_LC_df$MIXED_FOREST_PROP <- mean_norm_log_xform_prop(tor_LC_df$MIXED_FOREST_PROP)

tor_LC_df$SHRUB_SCRUB_PROP <- mean_norm_log_xform_prop(tor_LC_df$SHRUB_SCRUB_PROP)

tor_LC_df$GRASS_LAND_PROP <- mean_norm_log_xform_prop(tor_LC_df$GRASS_LAND_PROP)

tor_LC_df$PASTURE_HAY_PROP <- mean_norm_log_xform_prop(tor_LC_df$PASTURE_HAY_PROP)

tor_LC_df$CULT_CROPS_PROP <- mean_norm_log_xform_prop(tor_LC_df$CULT_CROPS_PROP)

tor_LC_df$WOOD_WETLAND_PROP <- mean_norm_log_xform_prop(tor_LC_df$WOOD_WETLAND_PROP)

tor_LC_df$HERB_WETLAND_PROP <- mean_norm_log_xform_prop(tor_LC_df$HERB_WETLAND_PROP)

tor_LC_df$BARREN_LAND_PROP <- mean_norm_log_xform_prop(tor_LC_df$BARREN_LAND_PROP)


# Process income data
tor_LC_df$INCOME <- mean_norm_log_xform(tor_LC_df$INCOME)


# Process the interaction effects
tor_LC_df$TOR_AREA <- mean_norm_log_xform_prop(tor_LC_df$TOR_AREA)

tor_LC_df$TOT_DEV_INT <- mean_norm_log_xform_prop(tor_LC_df$TOT_DEV_INT)

tor_LC_df$TOT_WOOD_AREA <- mean_norm_log_xform_prop(tor_LC_df$TOT_WOOD_AREA)

tor_LC_df$WOOD_DEV_INT <- mean_norm_log_xform_prop(tor_LC_df$WOOD_DEV_INT)

tor_LC_df$EXP_INC_AREA <- mean_norm_log_xform(tor_LC_df$EXP_INC_AREA)


# Storm events variables that took more work to get
tor_LC_df$DAY_OF_YEAR <- mean_norm_log_xform(tor_LC_df$DAY_OF_YEAR)

tor_LC_df$MONTH <- mean_norm_log_xform(tor_LC_df$MONTH)

tor_LC_df$BEGIN_TIME <- mean_normalize(tor_LC_df$BEGIN_TIME)

tor_LC_df$STATE_RANK <- mean_norm_log_xform_prop(tor_LC_df$STATE_RANK)

tor_LC_df$YEAR <- mean_normalize(tor_LC_df$YEAR)


# Take a look at all the distributions
tor_hist_data <- melt(tor_LC_df)

tor_hist_data <- dplyr::select(tor_hist_data,
                               -c(CZ_NAME,
                                  STATE,
                                  BEGIN_DATE_TIME,
                                  state_abbrev))

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "EVENT_ID")

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "BEGIN_HOUR")

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "BEGIN_MINUTE")


# Plot them
ggplot(tor_hist_data,
       aes(x = value)) +
  geom_histogram(bins = 100,
                 fill = "dark red") +
  facet_wrap(~variable) +
  theme_bw() +
  scale_x_continuous(limits = c(-6,
                                6))


# Get rid of the variables not being analyzed
tor_LC_df <- dplyr::select(tor_LC_df,
                           -c(STATE,
                              EVENT_ID,
                              BEGIN_DATE_TIME,
                              CZ_NAME,
                              state_abbrev,
                              BEGIN_HOUR,
                              BEGIN_MINUTE))


# Save it
# write_csv(tor_LC_df, "data/raw/Tor_data_with_interact_effects_processed.csv")


