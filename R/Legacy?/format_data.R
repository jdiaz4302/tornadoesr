


# Import the data
source("R/merge_data.R")


# Only tornadoes
tor_df <- filter(tor_df,
                 EVENT_TYPE == "Tornado")


# Converting the money from, for example, 2M to 2000000 for crops
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


# Converting the money from, for example, 2M to 2000000 for property
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


# Filtering the data to appromixately the continental United States ------------
#to get rid of errors in data entry


tor_df <- filter(tor_df, BEGIN_LAT <= 50 & BEGIN_LAT >= 23)

tor_df <- filter(tor_df, BEGIN_LON <= -66 & BEGIN_LON >= -125)


# Make month names as numeric 1-12: --------------------------------------------


tor_df$MONTH_NAME <- match(tor_df$MONTH_NAME, month.name)

tor_df$MONTH_NAME <- as.numeric(tor_df$MONTH_NAME)

tor_df <- plyr::rename(tor_df, c("MONTH_NAME" = "MONTH"))


# Getting the time of day that each tornado started at -------------------------


tor_df$BEGIN_TIME <- substring(tor_df$BEGIN_DATE_TIME, 11)

tor_df$BEGIN_TIME <- as.POSIXct(tor_df$BEGIN_TIME, format = "%H:%M:%S")


# Getting the time of day that each tornado ended at ---------------------------


tor_df$END_TIME <- substring(tor_df$END_DATE_TIME, 11)

tor_df$END_TIME <- as.POSIXct(tor_df$END_TIME, format = "%H:%M:%S")


# Getting the month and day of each tornado's start ----------------------------


tor_df$BEGIN_MONTH_DAY <- substring(tor_df$BEGIN_DATE_TIME, 0, 6)

tor_df$BEGIN_MONTH_DAY <- as.POSIXct(tor_df$BEGIN_MONTH_DAY, format = "%d-%B")


# Getting the month and day of each tornado's end ------------------------------


tor_df$END_MONTH_DAY <- substring(tor_df$END_DATE_TIME, 0, 6)

tor_df$END_MONTH_DAY <- as.POSIXct(tor_df$END_MONTH_DAY, format = "%d-%B")


# Time and day of year for tornado's start -------------------------------------


tor_df$BEGIN_TIME_OF_YEAR <- as.POSIXct(tor_df$BEGIN_DATE_TIME, 
                                       format = "%d-%B-%y %H:%M:%S")

tor_df$BEGIN_TIME_OF_YEAR <- substring(tor_df$BEGIN_TIME_OF_YEAR, 6)

tor_df$BEGIN_TIME_OF_YEAR <- as.POSIXct(tor_df$BEGIN_TIME_OF_YEAR, 
                                       format = "%m-%d %H:%M:%S")


# Time and day of year for tornado's end ---------------------------------------


tor_df$END_TIME_OF_YEAR <- as.POSIXct(tor_df$END_DATE_TIME, 
                                     format = "%d-%B-%y %H:%M:%S")

tor_df$END_TIME_OF_YEAR <- substring(tor_df$END_TIME_OF_YEAR, 6)

tor_df$END_TIME_OF_YEAR <- as.POSIXct(tor_df$END_TIME_OF_YEAR, 
                                     format = "%m-%d %H:%M:%S")


# Duration in seconds of tornado -----------------------------------------------


tor_df$DURATION_SECONDS <- as.numeric(tor_df$END_TIME_OF_YEAR -
                                      tor_df$BEGIN_TIME_OF_YEAR)


# Get rid of obviously wrong durations -----------------------------------------


tor_df <- filter(tor_df, DURATION_SECONDS >= 0 &
                         DURATION_SECONDS <= 13500)


# Only keep columns of relevance and good quality ------------------------------


tor_df <- select(tor_df, c(INJURIES_DIRECT, DEATHS_DIRECT,
                           DAMAGE_PROPERTY, DAMAGE_CROPS,
                           DURATION_SECONDS, BEGIN_TIME_OF_YEAR,
                           BEGIN_MONTH_DAY, BEGIN_LAT,
                           BEGIN_LON, TOR_LENGTH, TOR_WIDTH,
                           YEAR, MONTH, BEGIN_TIME,
                           BEGIN_DATE_TIME))


