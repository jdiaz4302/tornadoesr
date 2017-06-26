


# Packages
library(readr)


# Optional setup
# source("R/33_fetch_mob_home_data.R")


# Import the so-far model data
# Which contains LC, income, and interaction effects
tor_df <- read.csv("data/raw/Tor_data_with_interact_effects.csv")


# Get the names, which can be matched with the census data
tor_df_county_names <- dplyr::select(tor_df,
                                     c(CZ_NAME,
                                       STATE))


# Add mobile home count per county into the original dataset
tor_df$MOB_HOM_COUNT <- mob_home_df$estimate[match(do.call(paste,
                                                           tor_df_county_names),
                                                   toupper(sub("\\s+\\w+,", "",
                                                               mob_home_df$NAME)))]


# Save it
# write_csv(tor_df, "data/raw/Tor_data_with_mob_home.csv")


