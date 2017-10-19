


# Packages
library(readr)
library(dplyr)
library(reshape2)


# Import all the data
tor_LC_df <- read.csv("data/raw/tor_data_with_LC_only.csv")

county_income_data <- read.csv("data/raw/GeoFRED_Estimate_of_Median_Household_Income_by_County_Dollars.csv")


# Fix (most of) the column names
colnames(county_income_data) <- county_income_data[1, ]


# Get rid of the unneeded columns, which are messing up the later melt
location <- county_income_data[, 2]

county_income_data_reduced <- cbind(location,
                                    county_income_data[, 4:22])


# Get rid of the extra header row, which is acting as a data row
county_income_data_reduced <- dplyr::filter(county_income_data_reduced,
                                            location != "Region Name")


# 2016 and 2017 are not provided, so let's assume that they're the same as 2015
county_income_data_reduced$'2016' <- county_income_data_reduced$`2015`

county_income_data_reduced$'2017' <- county_income_data_reduced$`2015`


# Reshape the data.frame so that year is a variable
county_income_reshaped <- melt(county_income_data_reduced, id = "location")


# Change the state names to abbreviations
tor_LC_df$state_abbrev <- state.abb[match(tor_LC_df$STATE,
                                          toupper(state.name))]


# Get the county, state, and year of each tornado event
tor_county_state_year <- dplyr::select(tor_LC_df,
                                       c(CZ_NAME, state_abbrev, YEAR))


# Get income by location and year
county_income_reshaped$to_match_by <- toupper(sub("\\s+\\w+,", "",
                                                  paste(county_income_reshaped$location,
                                                        county_income_reshaped$variable)))


# Get location and year of each tornado event as one 'column'
tor_county_state_year_together <- do.call(paste, tor_county_state_year)


# Match income with tornado data
tor_LC_df$INCOME <- county_income_reshaped$value[match(tor_county_state_year_together,
                                                       county_income_reshaped$to_match_by)]


# Remove NA'd values, less than 1% of the match resulted in a NA
tor_LC_df <- na.omit(tor_LC_df)


# Save it
# write_csv(tor_LC_df, "data/raw/tor_data_with_income.csv")


