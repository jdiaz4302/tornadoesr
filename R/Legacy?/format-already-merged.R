# Packages ---------------------------------------------------------------------
library(plyr)


# Loading the data -------------------------------------------------------------
data1 <- read.csv("~/earth-analytics/internship/tornadoesr/merged.csv")


# At 1,407,503 entries, need to subtract 66 (the headers) ----------------------
data1 <- subset(data1, BEGIN_DAY != "BEGIN_DAY")
# Now its at 1,407,437, success!


# Only tornados ----------------------------------------------------------------
data1 <- subset(data1, EVENT_TYPE == "Tornado")


# Get rid of unneeded columns --------------------------------------------------
data1 <- data1[c(9, 11, 12, 18, 20:27, 32:38, 45:48)]


# Everything loaded as factors, so going to have to convert appropriately ------
# when needed; here are the conversions to numeric:
data1$INJURIES_DIRECT <- as.numeric(levels(data1$INJURIES_DIRECT)
                                    [data1$INJURIES_DIRECT])

data1$INJURIES_INDIRECT <- as.numeric(levels(data1$INJURIES_INDIRECT)
                                      [data1$INJURIES_INDIRECT])

data1$DEATHS_DIRECT <- as.numeric(levels(data1$DEATHS_DIRECT)
                                  [data1$DEATHS_DIRECT])

data1$DEATHS_INDIRECT <- as.numeric(levels(data1$DEATHS_INDIRECT)
                                    [data1$DEATHS_INDIRECT])

data1$BEGIN_LAT <- as.numeric(levels(data1$BEGIN_LAT)
                              [data1$BEGIN_LAT])

data1$BEGIN_LON <- as.numeric(levels(data1$BEGIN_LON)
                              [data1$BEGIN_LON])

data1$END_LAT <- as.numeric(levels(data1$END_LAT)
                            [data1$END_LAT])

data1$END_LON <- as.numeric(levels(data1$END_LON)
                            [data1$END_LON])

data1$TOR_WIDTH <- as.numeric(levels(data1$TOR_WIDTH)
                              [data1$TOR_WIDTH])

data1$TOR_LENGTH <- as.numeric(levels(data1$TOR_LENGTH)
                               [data1$TOR_LENGTH])


# Making sure all numbers are considered numbers by R --------------------------
str(data1)


# Subsetting the data to only the continental united states to get rid of ------
# errors in data entry because this csv says there's an -800 Longitude
data1 <- subset(data1, BEGIN_LAT <= 50 & BEGIN_LAT >= 23)
data1 <- subset(data1, BEGIN_LON <= -66 & BEGIN_LON >= -125)


# Converting the money from, for example, 2M to 2000000 for property: ----------
data1$DAMAGE_PROPERTY <- gsub("M", "000000", data1$DAMAGE_PROPERTY)

data1$DAMAGE_PROPERTY <- gsub("K", "000", data1$DAMAGE_PROPERTY)

data1$DAMAGE_PROPERTY <- gsub("\\.", "", data1$DAMAGE_PROPERTY)

data1$DAMAGE_PROPERTY <- as.numeric(data1$DAMAGE_PROPERTY)


# Make sure it worked ----------------------------------------------------------
class(data1$DAMAGE_PROPERTY)
# Numeric - success!


# Converting the money from, for example, 2M to 2000000 for crops: -------------
data1$DAMAGE_CROPS <- gsub("M", "000000", data1$DAMAGE_CROPS)

data1$DAMAGE_CROPS <- gsub("K", "000", data1$DAMAGE_CROPS)

data1$DAMAGE_CROPS <- gsub("\\.", "", data1$DAMAGE_CROPS)

data1$DAMAGE_CROPS <- as.numeric(data1$DAMAGE_CROPS)

class(data1$DAMAGE_CROPS)


# Make month names as numeric 1-12: --------------------------------------------
data1$MONTH_NAME <- match(data1$MONTH_NAME, month.name)

data1$MONTH_NAME <- as.numeric(data1$MONTH_NAME)

data1 <- rename(data1, c("MONTH_NAME" = "MONTH"))

class(data1$MONTH)


# Getting the time of day that each tornado started at -------------------------
data1$BEGIN_TIME <- substring(data1$BEGIN_DATE_TIME, 11)

data1$BEGIN_TIME <- as.POSIXct(data1$BEGIN_TIME, format = "%H:%M:%S")


# Getting the time of day that each tornado ended at ---------------------------
data1$END_TIME <- substring(data1$END_DATE_TIME, 11)

data1$END_TIME <- as.POSIXct(data1$END_TIME, format = "%H:%M:%S")


# Duration of each tornado in seconds ------------------------------------------
data1$DURATION_SECONDS <- data1$END_TIME - data1$BEGIN_TIME


# Getting the month and day of each tornado's start ----------------------------
data1$BEGIN_MONTH_DAY <- substring(data1$BEGIN_DATE_TIME, 0, 6)

data1$BEGIN_MONTH_DAY <- as.POSIXct(data1$BEGIN_MONTH_DAY, format = "%d-%B")


# Getting the month and day of each tornado's end ------------------------------
data1$END_MONTH_DAY <- substring(data1$END_DATE_TIME, 0, 6)

data1$END_MONTH_DAY <- as.POSIXct(data1$END_MONTH_DAY, format = "%d-%B")


# How many days a tornado lasted -----------------------------------------------
data1$DURATION_DAYS <- data1$END_MONTH_DAY - data1$BEGIN_MONTH_DAY


# Time and day of year for tornado's start -------------------------------------
data1$BEGIN_TIME_OF_YEAR <- as.POSIXct(data1$BEGIN_DATE_TIME, 
                                 format = "%d-%B-%y %H:%M:%S")

data1$BEGIN_TIME_OF_YEAR <- substring(data1$BEGIN_TIME_OF_YEAR, 6)

data1$BEGIN_TIME_OF_YEAR <- as.POSIXct(data1$BEGIN_TIME_OF_YEAR, 
                                       format = "%m-%d %H:%M:%S")


# Time and day of year for tornado's end ---------------------------------------
data1$END_TIME_OF_YEAR <- as.POSIXct(data1$END_DATE_TIME, 
                                format = "%d-%B-%y %H:%M:%S")

data1$END_TIME_OF_YEAR <- substring(data1$END_TIME_OF_YEAR, 6)

data1$END_TIME_OF_YEAR <- as.POSIXct(data1$END_TIME_OF_YEAR, 
                                     format = "%m-%d %H:%M:%S")

