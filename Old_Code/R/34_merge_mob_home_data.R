


# Packages
library(readr)
library(dplyr)


# Optional setup
# source("R/33_fetch_mob_home_data.R")


# Get mobile home density, not just county
# Import census land area by county data
land_area <- read.csv("data/raw/LND01.csv") %>%
  select(c(ï..Areaname, LND010200D))

# Remove country and state summed-values
land_area$filter_var <- grepl(",", land_area$ï..Areaname)

land_area <- filter(land_area,
                    filter_var != FALSE)

# Make easier to handle name
land_area$LOC <- land_area$ï..Areaname

# Give easy to work with format
land_area$LOC <- sprintf("%100s", land_area$LOC)

# Get county name
land_area$county <- substr(land_area$LOC, 1, 96)

land_area$county <- gsub("\\s+",                 # replaces duplicate spacing with single
                         " ", land_area$county)

land_area$county <- substring(land_area$county, 2)

# Get state of county - as abbrev
land_area$state_abb <- substr(land_area$LOC, 99, 100)

# Get state of county - as name
land_area$state_name <- state.name[match(land_area$state_abb, 
                                         state.abb)]

# Get the county and state
land_area_count_stat <- select(land_area, c(county,
                                            state_name))

# Match mobile home count df with land area df
land_area$MOB_HOM_COUNT <- mob_home_df$estimate[match(toupper(do.call(paste, land_area_count_stat)),
                                                      toupper(sub("\\s+\\w+,", "",
                                                                  mob_home_df$NAME)))]

# Notably, this left out St. Louis city, Missouri - big tornado town, rest are mostly Alaska
land_area$MOB_HOM_COUNT[land_area$county == "St. Louis city"] <-
  mob_home_df[mob_home_df$NAME == "St. Louis city, Missouri", ]$estimate

# Get mobile home density
land_area$MOB_HOM_DENS <- land_area$MOB_HOM_COUNT / land_area$LND010200D


# Import the so-far model data
# Which contains LC, income, and interaction effects
tor_df <- read.csv("data/raw/Tor_data_with_interact_effects.csv")


# Get the county names, which can be matched with the mobile home data
tor_df_county_names <- dplyr::select(tor_df,
                                     c(CZ_NAME,
                                       state_abbrev))

# Format land_area$LOC
land_area$LOC <- gsub("\\s+", " ", land_area$LOC) %>%
  substring(first = 2)

# Add mobile home count per county into the original dataset
tor_df$MOB_HOM_DENS <- land_area$MOB_HOM_DENS[match(do.call(paste,
                                                            tor_df_county_names),
                                                    toupper(sub(",", "", land_area$LOC)))]

tor_df <- na.omit(tor_df)


# Save it
# write_csv(tor_df, "data/raw/Tor_data_with_mob_home.csv")


