


# Calling the data
source("R/merge_data.R")


# Packages
library(dplyr)


# Filter to only tornadoes
tor_df <- filter(tor_df,
                 EVENT_TYPE == "Tornado")


# Subsetting the dataset by date to ignore procedural changes
tor_df <- filter(tor_df,
                 YEAR >= 1997)


# Limiting it to continental US - getting rid of messed up lat/long values
tor_df <- filter(tor_df,
                 BEGIN_LAT <= 50 & BEGIN_LAT >= 23)

tor_df <- filter(tor_df,
                 BEGIN_LON <= -66 & BEGIN_LON >= -125)


# Monetary values
# Fixing monetary values - crops
tor_df$DAMAGE_CROPS <- gsub("M",
                            "000000",
                            tor_df$DAMAGE_CROPS)

tor_df$DAMAGE_CROPS <- gsub("K",
                            "000",
                            tor_df$DAMAGE_CROPS)

tor_df$DAMAGE_CROPS <- gsub("\\.",
                            "",
                            tor_df$DAMAGE_CROPS)

tor_df$DAMAGE_CROPS <- as.numeric(tor_df$DAMAGE_CROPS)


# Fixing monetary values - property
tor_df$DAMAGE_PROPERTY <- gsub("M",
                               "000000",
                               tor_df$DAMAGE_PROPERTY)

tor_df$DAMAGE_PROPERTY <- gsub("K",
                               "000",
                               tor_df$DAMAGE_PROPERTY)

tor_df$DAMAGE_PROPERTY <- gsub("\\.",
                               "",
                               tor_df$DAMAGE_PROPERTY)

tor_df$DAMAGE_PROPERTY <- as.numeric(tor_df$DAMAGE_PROPERTY)


# Formatting date
tor_df$BEGIN_DATE_TIME <- as.POSIXct(tor_df$BEGIN_DATE_TIME,
                                     format = "%d-%B-%y %H:%M:%S")


# Fix years, such as 2050 should be 1950
tor_df$BEGIN_DATE_TIME <- as.POSIXct(ifelse(tor_df$BEGIN_DATE_TIME >= "2049-12-31 23:59:59",
                                            format(tor_df$BEGIN_DATE_TIME,
                                                   "19%y-%m-%d %H:%M:%S"),
                                            format(tor_df$BEGIN_DATE_TIME)))


# Getting tornado duration
# Getting the time of day that each tornado started at
tor_df$BEGIN_TIME <- substring(tor_df$BEGIN_DATE_TIME,
                               12)

tor_df$BEGIN_TIME <- as.POSIXct(tor_df$BEGIN_TIME,
                                format = "%H:%M:%S")

# Getting the time of day that each tornado ended at
tor_df$END_TIME <- substring(tor_df$END_DATE_TIME,
                             11)

tor_df$END_TIME <- as.POSIXct(tor_df$END_TIME,
                              format = "%H:%M:%S")


# Duration in seconds of each tornado
tor_df$DURATION_SECONDS <- as.numeric(tor_df$END_TIME - tor_df$BEGIN_TIME)


# Getting rid of obviously wrong durations
tor_df <- filter(tor_df,
                 DURATION_SECONDS >= 0 & DURATION_SECONDS <= 13500)


# Get date as a number for numpy cooperation
tor_df$BEGIN_DATE_TIME <- as.numeric(tor_df$BEGIN_DATE_TIME)


# Keeping only variables of interest
tor_df <- dplyr::select(tor_df, c(INJURIES_DIRECT,
                                  DEATHS_DIRECT,
                                  DAMAGE_PROPERTY,
                                  DAMAGE_CROPS,
                                  DURATION_SECONDS,
                                  BEGIN_LAT,
                                  BEGIN_LON,
                                  TOR_LENGTH,
                                  TOR_WIDTH,
                                  BEGIN_DATE_TIME,
                                  EVENT_ID))


# PyTorch really doesn't like NA values
tor_df <- na.omit(tor_df)


