


# Packages
library(readr)


# Import tornado events with LC proportions
tor_df <- read.csv("data/raw/tor_data_with_ACS.csv")


# 1. Produce tornado area
tor_df$TOR_AREA <- tor_df$TOR_LENGTH * tor_df$TOR_WIDTH


# 2. Produce total developed intensity
tor_df$TOT_DEV_INT <- tor_df$DEV_OPEN_PROP * 0.10 +
  tor_df$DEV_LOW_PROP * 0.35 +
  tor_df$DEV_MED_PROP * 0.65 +
  tor_df$DEV_HIGH_PROP * 0.90


# 3. Produce total wooded prop
tor_df$TOT_WOOD_AREA <- tor_df$WOOD_WETLAND_PROP +
  tor_df$DECID_FOREST_PROP +
  tor_df$EVERGR_FOREST_PROP +
  tor_df$MIXED_FOREST_PROP


# 4. Produce total wood-dev interaction
tor_df$WOOD_DEV_INT <- tor_df$TOT_DEV_INT * tor_df$TOT_WOOD_AREA


# 5. Produce an approximation of wealth contained in the the tornado area
tor_df$EXP_INC_AREA <- tor_df$INCOME * tor_df$TOR_AREA


# Get the unix time in usable format
tor_df$BEGIN_DATE_TIME <- as.POSIXct(tor_df$BEGIN_DATE_TIME, origin="1970-01-01")


# 6. Produce day of year
tor_df$DAY_OF_YEAR <- as.numeric(strftime(tor_df$BEGIN_DATE_TIME, format = "%j"))


# 7. Produce month
tor_df$MONTH <- as.numeric(substr(tor_df$BEGIN_DATE_TIME, 6, 7))


# 8. Produce time of day in minutes
# Get only time
tor_df$BEGIN_TIME <- substring(tor_df$BEGIN_DATE_TIME, 12)


# Get hours and minutes separately
tor_df$BEGIN_HOUR <- substr(tor_df$BEGIN_TIME, 0, 2)

tor_df$BEGIN_MINUTE <- substr(tor_df$BEGIN_TIME, 4, 5)


# Use those to get minute of day
tor_df$BEGIN_TIME <- (as.numeric(tor_df$BEGIN_HOUR) * 60) + 
  as.numeric(tor_df$BEGIN_MINUTE)


# 9. Process STATE - need an informative way to numerify this
# Get cumulative damage for each state
# Get the sum
DamPerState <- aggregate(tor_df$DAMAGE_PROPERTY,
                         by = list(Category = tor_df$STATE),
                         FUN = sum)

# Order them
cum_dam_order <- sort(DamPerState$x, decreasing = TRUE)

# Make the ordered damage a dataframe
cum_dam_rank <- as.data.frame(cum_dam_order)

# Create a rank dataframe
rank <- as.data.frame(c(1:nrow(cum_dam_rank)))

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

dam_per_state_rank$STATE_RANK <- dam_per_state_rank$`c(1:nrow(cum_dam_rank))`

# Get only state name and rank
dam_per_state_rank <- dplyr::select(dam_per_state_rank,
                                    c(STATE, STATE_RANK))

# Merge this back to the original dataframe
tor_df <- merge(x = tor_df,
                   dam_per_state_rank,
                   by = "STATE")


# Just to be safe
tor_df <- na.omit(tor_df)


# Save it
# write_csv(tor_df, "data/raw/tor_data_with_interact_effects.csv")


