


# Calling the data
source("R/2_merge_data.R")
# Only relevent for the leaflet plotting
# library(readr)


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


# Keeping only variables of interest
tor_df <- dplyr::select(tor_df, c(DAMAGE_PROPERTY,
                                  BEGIN_LAT,
                                  BEGIN_LON,
                                  BEGIN_DATE_TIME,
                                  MONTH_NAME,
                                  YEAR,                 
                                  STATE))


# Get rid of incomplete entries
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


# Give all entries a homogenous format, for easier cleaning
tor_df$DAMAGE_PROPERTY_temp <- sprintf("%10.2f",
                                       tor_df$DAMAGE_PROPERTY_temp)


# Creates columns to acknowledge the existance of the abbreviations
tor_df$K_EXIST_PROPERTY <- grepl("K",
                                 tor_df$DAMAGE_PROPERTY)

tor_df$M_EXIST_PROPERTY <- grepl("M",
                                 tor_df$DAMAGE_PROPERTY)

tor_df$B_EXIST_PROPERTY <- grepl("B",
                                 tor_df$DAMAGE_PROPERTY)


# Go ahead and drop the decimals
tor_df$DAMAGE_PROPERTY_temp <- gsub("\\.",
                                    "",
                                    tor_df$DAMAGE_PROPERTY_temp)


# Get the correct numeric values, substituting abbreviations for appropriate values
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


# Swapping out messy data for clean data
tor_df$DAMAGE_PROPERTY <- tor_df$DAMAGE_PROPERTY_FIXED


# Keeping only variables of interest
tor_df <- dplyr::select(tor_df, c(DAMAGE_PROPERTY,
                                  BEGIN_LAT,
                                  BEGIN_LON,
                                  BEGIN_DATE_TIME,
                                  MONTH_NAME,
                                  YEAR,                 
                                  STATE))


# Get rid of NAs "introduced by coercion"
tor_df <- na.omit(tor_df)


# Save for leaflet/markdown problems
# write_csv(tor_df, "data/raw/leaflet_viz_data.csv")


