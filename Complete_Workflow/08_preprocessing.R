


# Packages
library(ggplot2)
library(reshape2)
library(readr)


# Import data
tor_df <- read.csv("data/raw/tor_data_with_interact_effects.csv")


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
# Get "Which start after 7?"
tor_df$after_7 <- tor_df$BEGIN_TIME > 420

# Make into separate df's
tor_after_7 <- tor_df[tor_df$after_7 == TRUE, ]
tor_before_7 <- tor_df[tor_df$after_7 == FALSE, ]

# Make the proper adjustments
tor_after_7$TIME <- tor_after_7$BEGIN_TIME - 420
tor_before_7$TIME <- tor_before_7$BEGIN_TIME + 1020

# Recombine them
tor_df <- rbind(tor_after_7, tor_before_7)


# Process the Storm Events variables
tor_df$DURATION_SECONDS <- mean_norm_log_xform(tor_df$DURATION_SECONDS)

tor_df$BEGIN_LAT <- mean_normalize(tor_df$BEGIN_LAT)

tor_df$BEGIN_LON <- mean_normalize(tor_df$BEGIN_LON)

tor_df$TOR_LENGTH <- mean_norm_log_xform(tor_df$TOR_LENGTH)

tor_df$TOR_WIDTH <- mean_norm_log_xform(tor_df$TOR_WIDTH)


# Process the outcome variable
tor_df$DAMAGE_PROPERTY <- mean_norm_log_xform(tor_df$DAMAGE_PROPERTY)


# Process the land cover values
tor_df$OPEN_WATER_PROP <- mean_norm_log_xform_prop(tor_df$OPEN_WATER_PROP)

tor_df$DEV_OPEN_PROP <- mean_norm_log_xform_prop(tor_df$DEV_OPEN_PROP)

tor_df$DEV_LOW_PROP <- mean_norm_log_xform_prop(tor_df$DEV_LOW_PROP)

tor_df$DEV_MED_PROP <- mean_norm_log_xform_prop(tor_df$DEV_MED_PROP)

tor_df$DEV_HIGH_PROP <- mean_norm_log_xform_prop(tor_df$DEV_HIGH_PROP)

tor_df$DECID_FOREST_PROP <- mean_norm_log_xform_prop(tor_df$DECID_FOREST_PROP)

tor_df$EVERGR_FOREST_PROP <- mean_norm_log_xform_prop(tor_df$EVERGR_FOREST_PROP)

tor_df$MIXED_FOREST_PROP <- mean_norm_log_xform_prop(tor_df$MIXED_FOREST_PROP)

tor_df$SHRUB_SCRUB_PROP <- mean_norm_log_xform_prop(tor_df$SHRUB_SCRUB_PROP)

tor_df$GRASS_LAND_PROP <- mean_norm_log_xform_prop(tor_df$GRASS_LAND_PROP)

tor_df$PASTURE_HAY_PROP <- mean_norm_log_xform_prop(tor_df$PASTURE_HAY_PROP)

tor_df$CULT_CROPS_PROP <- mean_norm_log_xform_prop(tor_df$CULT_CROPS_PROP)

tor_df$WOOD_WETLAND_PROP <- mean_norm_log_xform_prop(tor_df$WOOD_WETLAND_PROP)

tor_df$HERB_WETLAND_PROP <- mean_norm_log_xform_prop(tor_df$HERB_WETLAND_PROP)

tor_df$BARREN_LAND_PROP <- mean_norm_log_xform_prop(tor_df$BARREN_LAND_PROP)


# Process income data
tor_df$INCOME <- mean_norm_log_xform(tor_df$INCOME)


# Process the interaction effects
tor_df$TOR_AREA <- mean_norm_log_xform_prop(tor_df$TOR_AREA)

tor_df$TOT_DEV_INT <- mean_norm_log_xform_prop(tor_df$TOT_DEV_INT)

tor_df$TOT_WOOD_AREA <- mean_norm_log_xform_prop(tor_df$TOT_WOOD_AREA)

tor_df$WOOD_DEV_INT <- mean_norm_log_xform_prop(tor_df$WOOD_DEV_INT)

tor_df$EXP_INC_AREA <- mean_norm_log_xform(tor_df$EXP_INC_AREA)


# Storm events variables that took more work to get
tor_df$DAY_OF_YEAR <- mean_norm_log_xform(tor_df$DAY_OF_YEAR)

tor_df$MONTH <- mean_normalize(tor_df$MONTH)

tor_df$TIME <- mean_normalize(tor_df$TIME)

tor_df$STATE_RANK <- mean_norm_log_xform_prop(tor_df$STATE_RANK)

tor_df$YEAR <- mean_normalize(tor_df$YEAR)


# Process ACS data
tor_df$MOB_HOME_DENS <- mean_norm_log_xform_prop(tor_df$MOB_HOME_DENS)

tor_df$POP_DENS <- mean_norm_log_xform(tor_df$POP_DENS)


# Take a look at all the distributions
tor_hist_data <- melt(tor_df)

tor_hist_data <- dplyr::select(tor_hist_data,
                               -c(CZ_NAME,
                                  STATE,
                                  BEGIN_DATE_TIME,
                                  state_abbrev,
                                  after_7))

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "EVENT_ID")

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "BEGIN_HOUR")

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "BEGIN_MINUTE")

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "BEGIN_TIME")


# Plot them
ggplot(tor_hist_data,
       aes(x = value)) +
  geom_histogram(bins = 100,
                 fill = "dark red") +
  facet_wrap(~variable) +
  theme_bw() +
  scale_x_continuous(limits = c(-7, 7))


# Get rid of the variables not being analyzed
tor_df <- dplyr::select(tor_df,
                           -c(STATE,
                              EVENT_ID,
                              BEGIN_DATE_TIME,
                              CZ_NAME,
                              state_abbrev,
                              BEGIN_TIME,
                              BEGIN_HOUR,
                              BEGIN_MINUTE,
                              after_7))


# Save it
# write_csv(tor_df, "data/raw/tor_data_processed.csv")


