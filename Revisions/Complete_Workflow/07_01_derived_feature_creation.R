


# Packages
library(readr)
library(splines)
library(dplyr)


# Import tornado events with LC and ACS
tor_df <- read.csv("data/raw/tor_data_with_ACS.csv")
# Omit rows with missing values
tor_df <- na.omit(tor_df)


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
tor_df$EXP_INC_AREA <- tor_df$MEDIAN_HOUSE_INC * tor_df$TOR_AREA


# 6. Temporal Variables
# Make R realize that BEGIN_DATE_TIME is epoch time
tor_df$BEGIN_DATE_TIME <- as.POSIXct(tor_df$BEGIN_DATE_TIME, origin="1970-01-01")


# 6.1 Get time-of-day-based basis splines
# Exclude date information
tor_df$BEGIN_TIME <- tor_df$BEGIN_DATE_TIME %>%
  substring(12)

# Get minutes since midnight
tor_df$BEGIN_TIME <- tor_df$BEGIN_TIME %>%
  substr(1, 2) %>%
  as.numeric() %>%
  `*`(60) %>%
  `+`(as.numeric(substr(tor_df$BEGIN_TIME, 4, 5)))

# Get a sequence (evenly-spaced) for the range of
# possible minute-since-midnight Values
BEGIN_TIME <- seq(from = 0,
                  to = 1440,
                  by = 1)

# Get 8 b-splines for the sequence
time_splines <- bs(BEGIN_TIME, 8, intercept = TRUE)

# Defining a function to convert the bs() results into dataframes
# With usable columns
bs_to_format_df <- function(col_prefix, bs_result, ref_df) {
  
  # bs_result = the output of bs()
  # col_prefix = useful identifier for bs() columns
  # Outputs a data.frame
  
  
  # Convert the bs_result to a data.frame
  bs_df <- as.data.frame(bs_result)
  
  # Get the prefix as a data.frame-length vector
  col_prefix_vector <- rep(col_prefix, length(colnames(bs_df)))
  
  # Add the prefix to the column names, so they're not just integers
  colnames(bs_df) <- paste0(col_prefix_vector,
                            colnames(bs_df))
  
  # Add the reference sequence as the first column
  bs_df <- cbind(ref_df, bs_df)
  
  # Name the first column the same as the ref_df object name
  colnames(bs_df)[1] <- deparse(substitute(ref_df))
  
  # Return the new, modified data.frame
  return(bs_df)
  
}

# Make a data.frame with reference and b-spline values
time_ref_df <- bs_to_format_df('TIME_SPLINE_',
                               time_splines,
                               BEGIN_TIME)

# Connect these b-spline values to observed time values
tor_df <- merge(tor_df, time_ref_df,
                by = 'BEGIN_TIME')


# 6.2 Get day-of-year-based basis splines
# Get Julian day (day of the year)
tor_df$JULIAN_DAY <- tor_df$BEGIN_DATE_TIME %>%
  strftime(format = '%j') %>%
  as.numeric()

# Get a sequence (evenly-spaced) for the range of
# possible values
JULIAN_DAY <- seq(from = 1,
                  to = 366,
                  by = 1)

# Get 12 b-splines for the sequence
julian_splines <- bs(JULIAN_DAY, 12, intercept = TRUE)

# Make a data.frame with reference and b-spline values
julian_ref_df <- bs_to_format_df('JULIAN_SPLINE_',
                                 julian_splines,
                                 JULIAN_DAY)

# Connect these b-spline values to observed date values
tor_df <- merge(tor_df, julian_ref_df,
                by = 'JULIAN_DAY')


# Just to be safe
tor_df <- na.omit(tor_df)


# Save it
# write_csv(tor_df, "data/raw/tor_data_with_derived.csv")
# Save the reference-spline data frames as well
# write_csv(time_ref_df, 'data/raw/time_splines_ref.csv')
# write_csv(julian_ref_df, 'data/raw/julian_splines_ref.csv')


