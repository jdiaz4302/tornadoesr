


# Packages
library(ggplot2)
library(reshape2)
library(readr)


# Import data
tor_df <- read.csv("data/raw/Tor_data_with_mob_home.csv")


# Get rid of NA's
nrow(tor_df)
# 19688
nrow(na.omit(tor_df))
# 19666 - insignificant loss
tor_df <- na.omit(tor_df)


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


# Let's have month start in October because it gives the shape
# An easy to figure out parabola
tor_df$after_oct <- tor_df$MONTH >= 10

# Make into separate df's
tor_after_oct <- tor_df[tor_df$after_oct == TRUE, ]
tor_before_oct <- tor_df[tor_df$after_oct == FALSE, ]

# Make the proper adjustments
tor_after_oct$MONTH_MEAN <- tor_after_oct$MONTH - 10
tor_before_oct$MONTH_MEAN <- tor_before_oct$MONTH + 2

# Recombine them
tor_df <- rbind(tor_after_oct, tor_before_oct)


# Keep name consistant with my copy-paste from earlier script
tor_LC_df <- tor_df


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

tor_LC_df$MONTH_MEAN <- mean_normalize(tor_LC_df$MONTH_MEAN)

tor_LC_df$TIME <- mean_normalize(tor_LC_df$TIME)

tor_LC_df$STATE_RANK <- mean_norm_log_xform_prop(tor_LC_df$STATE_RANK)

tor_LC_df$YEAR <- mean_normalize(tor_LC_df$YEAR)


# Process mobile home data
tor_LC_df$MOB_HOM_COUNT <- mean_norm_log_xform(tor_LC_df$MOB_HOM_COUNT)


# Take a look at all the distributions
tor_hist_data <- melt(tor_LC_df)

tor_hist_data <- dplyr::select(tor_hist_data,
                               -c(CZ_NAME,
                                  STATE,
                                  BEGIN_DATE_TIME,
                                  state_abbrev,
                                  after_7,
                                  after_oct))

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "EVENT_ID")

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "BEGIN_HOUR")

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "BEGIN_MINUTE")

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "MONTH")

tor_hist_data <- dplyr::filter(tor_hist_data,
                               variable != "BEGIN_TIME")


# Plot them
ggplot(tor_hist_data,
       aes(x = value)) +
  geom_histogram(bins = 100,
                 fill = "dark red") +
  facet_wrap(~variable) +
  theme_bw() +
  scale_x_continuous(limits = c(-6, 6))


# Get rid of the variables not being analyzed
tor_LC_df <- dplyr::select(tor_LC_df,
                           -c(STATE,
                              EVENT_ID,
                              BEGIN_DATE_TIME,
                              CZ_NAME,
                              state_abbrev,
                              MONTH,
                              BEGIN_TIME,
                              BEGIN_HOUR,
                              BEGIN_MINUTE,
                              after_7,
                              after_oct))


# Save it
# write_csv(tor_LC_df, "data/raw/Tor_data_with_mob_home_processed.csv")


