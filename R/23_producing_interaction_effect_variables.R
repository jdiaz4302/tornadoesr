


# Packages
library(readr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(chron)


# Import tornado events with LC proportions
tor_LC_df <- read.csv("data/raw/Tor_data_with_LC.csv")


# Have to merge income data here
county_income_data <- read.csv("data/raw/GeoFRED_Per_Capita_Personal_Income_by_County_Dollars.csv")

# Change the state names to abbreviations
tor_LC_df$state_abbrev <- state.abb[match(tor_LC_df$STATE,
                                          toupper(state.name))]

# Add income to the other data
tor_LC_df_names <- dplyr::select(tor_LC_df,
                                 c(CZ_NAME, state_abbrev))

tor_LC_df$INCOME <- county_income_data$X.2[match(do.call(paste, tor_LC_df_names),
                                                 toupper(sub("\\s+\\w+,", "",
                                                             county_income_data$X)))]
# End of income merge


# 1. Produce the average linear speed of the tornado
tor_LC_df$AVG_LIN_SPEED <- tor_LC_df$TOR_LENGTH / tor_LC_df$DURATION_SECONDS


# 2. Produce tornado area
tor_LC_df$TOR_AREA <- tor_LC_df$TOR_LENGTH * tor_LC_df$TOR_WIDTH


# 3. Produce the average rate of area covered by the tornado
tor_LC_df$AVG_AREA_COV <- tor_LC_df$TOR_AREA / tor_LC_df$DURATION_SECONDS


# 4. Produce total developed intensity
tor_LC_df$TOT_DEV_INT <- tor_LC_df$DEV_OPEN_PROP * 0.10 +
  tor_LC_df$DEV_LOW_PROP * 0.35 +
  tor_LC_df$DEV_MED_PROP * 0.65 +
  tor_LC_df$DEV_HIGH_PROP * 0.90


# 5. Produce total wooded prop
tor_LC_df$TOT_WOOD_AREA <- tor_LC_df$WOOD_WETLAND_PROP +
  tor_LC_df$DECID_FOREST_PROP +
  tor_LC_df$EVERGR_FOREST_PROP +
  tor_LC_df$MIXED_FOREST_PROP


# 6. Produce total wood-dev interaction
tor_LC_df$WOOD_DEV_INT <- tor_LC_df$TOT_DEV_INT * tor_LC_df$TOT_WOOD_AREA


# 7. Produce expected income of the area
tor_LC_df$EXP_INC_AREA <- tor_LC_df$INCOME * tor_LC_df$TOR_AREA


# 8. Produce the rate that the expected income of the area was hit
tor_LC_df$EXP_INC_RATE <- tor_LC_df$EXP_INC_AREA / tor_LC_df$DURATION_SECONDS


# Get the unix time in usable format
tor_LC_df$BEGIN_DATE_TIME <- as.POSIXct(tor_LC_df$BEGIN_DATE_TIME, origin="1970-01-01")


# 9. Produce day of year
tor_LC_df$DAY_OF_YEAR <- as.numeric(strftime(tor_LC_df$BEGIN_DATE_TIME, format = "%j"))


# 10. Produce month
tor_LC_df$MONTH <- as.numeric(substr(tor_LC_df$BEGIN_DATE_TIME, 6, 7))


# 11. Produce time of day in minutes
# Get only time
tor_LC_df$BEGIN_TIME <- substring(tor_LC_df$BEGIN_DATE_TIME, 12)

# Get hours and minutes separately
tor_LC_df$BEGIN_HOUR <- substr(tor_LC_df$BEGIN_TIME, 0, 2)

tor_LC_df$BEGIN_MINUTE <- substr(tor_LC_df$BEGIN_TIME, 4, 5)

# Use those to get minute of day
tor_LC_df$BEGIN_TIME <- (as.numeric(tor_LC_df$BEGIN_HOUR) * 60) + 
  as.numeric(tor_LC_df$BEGIN_MINUTE)


# 12. Process STATE - need an informative way to numerify this
# Get cumulative damage for each state
# Get the sum
DamPerState <- aggregate(tor_LC_df$DAMAGE_PROPERTY,
                         by = list(Category = tor_LC_df$STATE),
                         FUN = sum)

# Order them
cum_dam_order <- sort(DamPerState$x, decreasing = TRUE)

# Make the ordered damage a dataframe
cum_dam_rank <- as.data.frame(cum_dam_order)

# Create a rank dataframe
rank <- as.data.frame(c(1:49))

# Assign rank
cum_dam_rank <- cbind(rank, cum_dam_rank)

# Make them have same column name
DamPerState$cum_dam_order <- DamPerState$x

# Get rank matched with state name
dam_per_state_rank <- merge(x = DamPerState,
                            y = cum_dam_rank,
                            by = "cum_dam_order")

# Name state and rank correctly
dam_per_state_rank$STATE <- dam_per_state_rank$Category

dam_per_state_rank$STATE_RANK <- dam_per_state_rank$`c(1:49)`

# Get only state name and rank
dam_per_state_rank <- dplyr::select(dam_per_state_rank,
                                    c(STATE, STATE_RANK))

# Merge this back to the original dataframe
tor_LC_df <- merge(x = tor_LC_df,
                   dam_per_state_rank,
                   by = "STATE")


