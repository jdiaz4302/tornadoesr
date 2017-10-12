




###
###
# NEW SPLINE WORK
###
###





# Packages
library(splines)
library(dplyr)


# Import the data
tor_df <- read.csv('data/raw/tor_data_with_ACS.csv')


# Get time as its own variable and in minutes from midnight
tor_df$BEGIN_TIME <- as.POSIXct(tor_df$BEGIN_DATE_TIME, origin = '1970-01-01') %>%
  substring(12)
tor_df$BEGIN_TIME <- substr(tor_df$BEGIN_TIME, 1, 2) %>%
  as.numeric() %>%
  `*`(60) %>%
  `+`(as.numeric(substr(tor_df$BEGIN_TIME, 4, 5)))


# Get Julian Day as its own variable
tor_df$JULIAN_DAY <- as.POSIXct(tor_df$BEGIN_DATE_TIME, origin = '1970-01-01') %>%
  strftime(format = '%j') %>%
  as.numeric()


# Get date as its own variable and in days since 1970-01-01
tor_df$BEGIN_DATE <- (tor_df$BEGIN_DATE_TIME / 60) %>%
  `/`(60) %>%
  `/`(24) %>%
  floor()


# Get a sequence of ordered days since 1970-01-01
date_seq <- seq(from = min(tor_df$BEGIN_DATE),
                to = max(tor_df$BEGIN_DATE),
                by = 1)
# Get the splines
# MAYBE MAKE '4' A GENERALIZED CALCULATION
coarse_date_splines <- bs(date_seq, 4, intercept = TRUE)

yearly_date_splines <- bs(date_seq, 20, intercept = TRUE)






time_seq <- seq(from = min(tor_df$BEGIN_TIME),
                to = max(tor_df$BEGIN_TIME),
                by = 1)
coarse_time_splines <- bs(time_seq, 4, intercept = TRUE)
hourly_time_splines <- bs(time_seq, 24, intercept = TRUE)




julian_seq <- seq(from = 1,
                  to = 365,
                  by = 1)
monthly_splines <- bs(julian_seq, 12, intercept = TRUE)
plot(julian_seq, monthly_splines[, 1], type = 'l',
     xlab = 'Day of the Year',
     ylab = 'Spline Height')
for (i in 2:ncol(monthly_splines)) {
  lines(julian_seq, monthly_splines[, i], col = i)
}




# all date splines
for (i in 1:ncol(coarse_date_splines)) {
  if (i == 1) {
    plot(date_seq, coarse_date_splines[, i], type = 'l',
         xlab = 'Days Since 1970-01-01',
         ylab = 'Spline Height')
  } else {
    lines(date_seq, coarse_date_splines[, i], col = i)
  }
}
for (i in 1:ncol(yearly_date_splines)) {
  lines(date_seq, yearly_date_splines[, i], col = i)
}




# all time splines
for (i in 1:ncol(coarse_time_splines)) {
  if (i == 1) {
    plot(time_seq, coarse_time_splines[, i], type = 'l',
         xlab = 'Minutes since Midnight',
         ylab = 'Spline Height')
  } else {
    lines(time_seq, coarse_time_splines[, i], col = i)
  }
}
for (i in 1:ncol(hourly_time_splines)) {
  lines(time_seq, hourly_time_splines[, i], col = i)
}











bs_to_format_df <- function(col_prefix, bs_result) {
  
  bs_df <- as.data.frame(bs_result)
  
  col_prefix_vector <- rep(col_prefix, length(colnames(bs_df)))
  
  colnames(bs_df) <- paste0(col_prefix_vector,
                            colnames(bs_df))
  
  return(bs_df)
  
}



coarse_date_df <- bs_to_format_df('CD_SPLINE_', coarse_date_splines)
yearly_df <- bs_to_format_df('FD_SPLINE_', yearly_date_splines)
date_df <- cbind(date_seq,
                 coarse_date_df,
                 yearly_df)
colnames(date_df)[1] <- 'BEGIN_DATE'
monthly_df <- bs_to_format_df('MONTH_SPLINE_', monthly_splines)
monthly_df <- cbind(julian_seq,
                    monthly_df)
colnames(monthly_df)[1] <- 'JULIAN_DAY'
coarse_time_df <- bs_to_format_df('CT_SPLINE_', coarse_time_splines)
hourly_df <- bs_to_format_df('FT_SPLINE_', hourly_time_splines)
time_df <- cbind(time_seq,
                 coarse_time_df,
                 hourly_df)
colnames(time_df)[1] <- 'BEGIN_TIME'



tor_df <- merge(tor_df, monthly_df,
                by = 'JULIAN_DAY')
tor_df <- merge(tor_df, date_df,
                by = 'BEGIN_DATE')
tor_df <- merge(tor_df, time_df,
                by = 'BEGIN_TIME')

tor_df <- dplyr::select(tor_df,
                        -c(BEGIN_TIME,
                           BEGIN_DATE,
                           JULIAN_DAY,
                           EVENT_ID))




###
###
# ROUGHLY COPIED FROM Complete_Workflow/08...
###
###




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
                by = "STATE") %>%
  na.omit()





###
###
# ROUGHLY COPIED FROM Complete_Workflow/09...
###
###





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


# Process ACS data
tor_df$MOB_HOME_DENS <- mean_norm_log_xform_prop(tor_df$MOB_HOME_DENS)

tor_df$POP_DENS <- mean_norm_log_xform(tor_df$POP_DENS)


# Storm events variables that took more work to get
tor_df$STATE_RANK <- mean_norm_log_xform_prop(tor_df$STATE_RANK)


tor_df <- dplyr::select(tor_df,
                        -c(STATE,
                           BEGIN_DATE_TIME,
                           CZ_NAME,
                           state_abbrev,
                           YEAR))


###
###
# ROUGHLY COPIED FROM Complete_Workflow/10_
###
###





# Shuffle it, for safety
tor_df <- tor_df[sample(nrow(tor_df)), ]

# Reset index
rownames(tor_df) <- NULL


# Set split percentages
fractionTraining <- 0.60
fractionValidation <- 0.20
fractionTest <- 0.20


# Getting the sample sizes, +1's are to grab remainders
sampleSizeTraining <- floor(fractionTraining * nrow(tor_df))

sampleSizeValidation <- floor(fractionValidation * nrow(tor_df)) + 1

sampleSizeTest <- floor(fractionTest * nrow(tor_df)) + 1


# Get indices for training set
indicesTraining <- sort(sample(seq_len(nrow(tor_df)),
                               size = sampleSizeTraining))


# Remove the training indices from possible indices
indicesNotTraining <- setdiff(seq_len(nrow(tor_df)),
                              indicesTraining)


# Get indices for validation
indicesValidation <- sort(sample(indicesNotTraining,
                                 size = sampleSizeValidation))


# Give test set what indices remain
indicesTest <- setdiff(indicesNotTraining,
                       indicesValidation)

# Subset the data
train_set <- tor_df[indicesTraining, ]

cv_set <- tor_df[indicesValidation, ]


# 0 is the lowest DAMAGE_PROPERTY value
# Its also by-far the most abundant value
# So, the minimum value of the processed data.frames
# Will be a true property damage value of 0
zero_damage <- min(train_set$DAMAGE_PROPERTY)


train_set <- dplyr::filter(train_set,
                           DAMAGE_PROPERTY > zero_damage)
cv_set <- dplyr::filter(cv_set,
                        DAMAGE_PROPERTY > zero_damage)

readr::write_csv(train_set, 'data/raw/spline_train.csv')
readr::write_csv(cv_set, 'data/raw/spline_cv.csv')


