


# Packages
library(reshape2)


# Calling the data
source("R/merge_data.R")


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

##
###
#####
###############
############################### TESTING #####################
###############
#####
###
##

# Remove letters from property damage values, store as temp
tor_df$DAMAGE_PROPERTY_temp <- tor_df$DAMAGE_PROPERTY

tor_df$DAMAGE_PROPERTY_temp <- gsub("M",
                                    "",
                                    tor_df$DAMAGE_PROPERTY_temp)

tor_df$DAMAGE_PROPERTY_temp <- gsub("K",
                                    "",
                                    tor_df$DAMAGE_PROPERTY_temp)

tor_df$DAMAGE_PROPERTY_temp <- gsub("B",
                                    "",
                                    tor_df$DAMAGE_PROPERTY_temp)

tor_df$DAMAGE_PROPERTY_temp <- as.numeric(tor_df$DAMAGE_PROPERTY_temp)


# Remove letters from crop damage values
tor_df$DAMAGE_CROPS_temp <- tor_df$DAMAGE_CROPS

tor_df$DAMAGE_CROPS_temp <- gsub("M",
                                 "",
                                 tor_df$DAMAGE_CROPS_temp)

tor_df$DAMAGE_CROPS_temp <- gsub("K",
                                 "",
                                 tor_df$DAMAGE_CROPS_temp)

tor_df$DAMAGE_CROPS_temp <- gsub("B",
                                 "",
                                 tor_df$DAMAGE_CROPS_temp)

tor_df$DAMAGE_CROPS_temp <- as.numeric(tor_df$DAMAGE_CROPS_temp)


# try again
tor_df$DAMAGE_PROPERTY_temp <- sprintf("%10.2f",
                                       tor_df$DAMAGE_PROPERTY_temp)







tor_df$testing <- gsub("K",
                       "0",
                       gsub("\\.",
                            "",
                            tor_df$DAMAGE_PROPERTY_temp))


tor_df$testing <- gsub("M",
                       "0000",
                       gsub("\\.",
                            "",
                            tor_df$testing))


tor_df$testing <- gsub("B",
                       "0000000",
                       gsub("\\.",
                            "",
                            tor_df$testing))

data1 <- select(tor_df,
                c(DAMAGE_PROPERTY,
                  testing))
























# Function to determine how many decimal places
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$',
                       '',
                       as.character(x)),
                   ".",
                   fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }


# Getting decimal places of each property damage value
DAMAGE_PROPERTY_dec_places <- lapply(tor_df$DAMAGE_PROPERTY_temp,
                                     decimalplaces)

DAMAGE_PROPERTY_dec_places <- melt(DAMAGE_PROPERTY_dec_places)

DAMAGE_PROPERTY_dec_places <- select(DAMAGE_PROPERTY_dec_places,
                                     value)

tor_df <- cbind(tor_df,
                DAMAGE_PROPERTY_dec_places)

tor_df$DAMAGE_PROPERTY_dec_places <- tor_df$value

tor_df <- select(tor_df, -value)


# This is some elegant stuff right here
# Fixing thousands
tor_df$testing <- ifelse(tor_df$DAMAGE_PROPERTY_dec_places == 0,
                         tor_df$TeStInG <- gsub("K",
                                                "000",
                                                gsub("\\.",
                                                     "",
                                                     tor_df$DAMAGE_PROPERTY)),
                         ifelse(tor_df$DAMAGE_PROPERTY_dec_places == 1,
                                tor_df$TeStInG <- gsub("K",
                                                       "00",
                                                       gsub("\\.",
                                                            "",
                                                            tor_df$DAMAGE_PROPERTY)),
                                ifelse(tor_df$DAMAGE_PROPERTY_dec_places == 2,
                                       tor_df$TeStInG <- gsub("K",
                                                              "0",
                                                              gsub("\\.",
                                                                   "",
                                                                   tor_df$DAMAGE_PROPERTY)),
                                       NA)))


# Fixing millions
tor_df$testing <- ifelse(tor_df$DAMAGE_PROPERTY_dec_places == 0,
                         tor_df$TeStInG <- gsub("M",
                                                "000000",
                                                gsub("\\.",
                                                     "",
                                                     tor_df$testing)),
                         ifelse(tor_df$DAMAGE_PROPERTY_dec_places == 1,
                                tor_df$TeStInG <- gsub("M",
                                                       "00000",
                                                       gsub("\\.",
                                                            "",
                                                            tor_df$testing)),
                                ifelse(tor_df$DAMAGE_PROPERTY_dec_places == 2,
                                       tor_df$TeStInG <- gsub("M",
                                                              "0000",
                                                              gsub("\\.",
                                                                   "",
                                                                   tor_df$testing)),
                                       NA)))


# Fixing billions
tor_df$testing <- ifelse(tor_df$DAMAGE_PROPERTY_dec_places == 0,
                         tor_df$TeStInG <- gsub("B",
                                                "000000000",
                                                gsub("\\.",
                                                     "",
                                                     tor_df$testing)),
                         ifelse(tor_df$DAMAGE_PROPERTY_dec_places == 1,
                                tor_df$TeStInG <- gsub("B",
                                                       "00000000",
                                                       gsub("\\.",
                                                            "",
                                                            tor_df$testing)),
                                ifelse(tor_df$DAMAGE_PROPERTY_dec_places == 2,
                                       tor_df$TeStInG <- gsub("B",
                                                              "0000000",
                                                              gsub("\\.",
                                                                   "",
                                                                   tor_df$testing)),
                                       NA)))


# Seeing how they line up
data1 <- select(tor_df,
                c(DAMAGE_PROPERTY,
                  testing))


