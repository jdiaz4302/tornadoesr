


# Calling the data
source("R/2_merge_data.R")


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
                                  MONTH_NAME,
                                  EVENT_ID,
                                  YEAR,                  # For why_care_plot
                                  CZ_NAME,               # For income data
                                  STATE))                # For income data   


# PyTorch really doesn't like NA values
tor_df <- na.omit(tor_df)


# BIG TASK - cleaning monetary values
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


# Remove letters from crop damage values, store as temp
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


# Give all entries a homogenous format, for easier cleaning
# For property
tor_df$DAMAGE_PROPERTY_temp <- sprintf("%10.2f",
                                       tor_df$DAMAGE_PROPERTY_temp)

# For crops
tor_df$DAMAGE_CROPS_temp <- sprintf("%10.2f",
                                    tor_df$DAMAGE_CROPS_temp)


# Creates columns to acknowledge the existance of the abbreviations
# For property
tor_df$K_EXIST_PROPERTY <- grepl("K",
                                 tor_df$DAMAGE_PROPERTY)

tor_df$M_EXIST_PROPERTY <- grepl("M",
                                 tor_df$DAMAGE_PROPERTY)

tor_df$B_EXIST_PROPERTY <- grepl("B",
                                 tor_df$DAMAGE_PROPERTY)

# For crops
tor_df$K_EXIST_CROPS <- grepl("K",
                              tor_df$DAMAGE_CROPS)

tor_df$M_EXIST_CROPS <- grepl("M",
                              tor_df$DAMAGE_CROPS)

tor_df$B_EXIST_CROPS <- grepl("B",
                              tor_df$DAMAGE_CROPS)


# Go ahead and drop the decimals
# For property
tor_df$DAMAGE_PROPERTY_temp <- gsub("\\.",
                                    "",
                                    tor_df$DAMAGE_PROPERTY_temp)

# For crops
tor_df$DAMAGE_CROPS_temp <- gsub("\\.",
                                    "",
                                    tor_df$DAMAGE_CROPS_temp)


# Get the correct numeric values, substituting abbreviations for appropriate values
# For property
tor_df$DAMAGE_PROPERTY_FIXED <- ifelse(tor_df$K_EXIST_PROPERTY == TRUE,
                                       tor_df$DAMAGE_PROPERTY_FIXED <- paste0(tor_df$DAMAGE_PROPERTY_temp,
                                                                              "0"),
                                       ifelse(tor_df$M_EXIST_PROPERTY == TRUE,
                                              tor_df$DAMAGE_PROPERTY_FIXED <- paste0(tor_df$DAMAGE_PROPERTY_temp,
                                                                                     "0000"),
                                              ifelse(tor_df$B_EXIST_PROPERTY == TRUE,
                                                     tor_df$DAMAGE_PROPERTY_FIXED <- paste0(tor_df$DAMAGE_PROPERTY_temp,
                                                                                            "0000000"),
                                                     tor_df$DAMAGE_PROPERTY_FIXED <- substr(tor_df$DAMAGE_PROPERTY_temp,
                                                                                            1,
                                                                                            nchar(tor_df$DAMAGE_PROPERTY_temp) - 2)))) %>%
  as.numeric()

# For crops
tor_df$DAMAGE_CROPS_FIXED <- ifelse(tor_df$K_EXIST_CROPS == TRUE,
                                    tor_df$DAMAGE_CROPS_FIXED <- paste0(tor_df$DAMAGE_CROPS_temp,
                                                                        "0"),
                                    ifelse(tor_df$M_EXIST_CROPS == TRUE,
                                           tor_df$DAMAGE_CROPS_FIXED <- paste0(tor_df$DAMAGE_CROPS_temp,
                                                                               "0000"),
                                           ifelse(tor_df$B_EXIST_CROPS == TRUE,
                                                  tor_df$DAMAGE_CROPS_FIXED <- paste0(tor_df$DAMAGE_CROPS_temp,
                                                                                      "0000000"),
                                                  tor_df$DAMAGE_CROPS_FIXED <- substr(tor_df$DAMAGE_CROPS_temp,
                                                                                      1,
                                                                                      nchar(tor_df$DAMAGE_CROPS_temp) - 2)))) %>%
  as.numeric()


# Some inspection
# check_df <- select(tor_df,
#                    c(DAMAGE_PROPERTY,
#                      DAMAGE_PROPERTY_FIXED,
#                      DAMAGE_CROPS,
#                      DAMAGE_CROPS_FIXED))
#
# View(check_df)
# Looks all good!


### Finishing up ###

# Swapping out messy for clean
# For property
tor_df$DAMAGE_PROPERTY <- tor_df$DAMAGE_PROPERTY_FIXED

# For crops
tor_df$DAMAGE_CROPS <- tor_df$DAMAGE_CROPS_FIXED


# Keep only the variables of interest
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
                                  MONTH_NAME,
                                  EVENT_ID,
                                  YEAR,                  # For why_care_plot
                                  CZ_NAME,               # For income data
                                  STATE))                # For income data

# The nonsequential indices are undesirable (to me)
rownames(tor_df) <- NULL


