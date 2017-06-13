


# Import all the .csv's
train_set <- read.csv("data/raw/train_with_econ_with_zeros.csv")

train_set_no_0 <- read.csv("data/raw/train_with_econ_wout_zeros.csv")

cv_set <- read.csv("data/raw/cv_with_econ_with_zeros.csv")

cv_set_no_0 <- read.csv("data/raw/cv_with_econ_wout_zeros.csv")

test_set <- read.csv("data/raw/test_with_econ_with_zeros.csv")

test_set_no_0 <- read.csv("data/raw/test_with_econ_wout_zeros.csv")


# Combine the ones w/ zeros to get total
intermed_df <- rbind(train_set,
                     cv_set)

total_df <- rbind(intermed_df,
                  test_set)


# 1. Produce the average linear speed of the tornado
total_df$AVG_LIN_SPEED <- total_df$TOR_LENGTH / total_df$DURATION_SECONDS


# 2. Produce tornado area
total_df$TOR_AREA <- total_df$TOR_LENGTH * total_df$TOR_WIDTH


# 3. Produce the average rate of area covered by the tornado
total_df$AVG_AREA_COV <- total_df$TOR_AREA / total_df$DURATION_SECONDS


# 4. Produce total developed intensity
total_df$TOT_DEV_INT <- total_df$DEV_OPEN_PROP * 0.10 +
  total_df$DEV_LOW_PROP * 0.35 +
  total_df$DEV_MED_PROP * 0.65 +
  total_df$DEV_HIGH_PROP * 0.90


# 5. Produce total wooded prop
total_df$TOT_WOOD_AREA <- total_df$WOOD_WETLAND_PROP +
  total_df$DECID_FOREST_PROP +
  total_df$EVERGR_FOREST_PROP +
  total_df$MIXED_FOREST_PROP


# 6. Produce total wood-dev interaction
total_df$WOOD_DEV_INT <- total_df$TOT_DEV_INT * total_df$TOT_WOOD_AREA


# 7. Produce expected income of the area
total_df$EXP_INC_AREA <- total_df$INCOME * total_df$TOR_AREA


# 8. Produce the rate that the expected income of the area was hit
total_df$EXP_INC_RATE <- total_df$EXP_INC_AREA / total_df$DURATION_SECONDS


