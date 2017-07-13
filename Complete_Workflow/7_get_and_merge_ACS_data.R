


# Packages
library(tidycensus)


# Set census API
census_api_key("insert your census key")


# Finding variable of interest
# acs_variables <- load_variables(2009, "acs", cache = TRUE)


# Writing a function, because this will be repeated
get_ACS_mob_hom_and_pop <- function(end_year) {
  
  # description: get mobile home count and population of each county from ACS
  # argument: end-year of the ACS
  # return: a dataframe containing mobile home count and population of each county
  
  acs_df <- get_acs(geography = "county", endyear = end_year,
                    variables = c("B25024_010E",   # Total mobile homes
                                  "B01003_001E"))  # Total population
  
  acs_df$YEAR <- rep(end_year, nrow(acs_df))
  
  return(acs_df)
  
}


# Get ACS data for all available years
acs_2009 <- get_ACS_mob_hom_and_pop(2009)

acs_2010 <- get_ACS_mob_hom_and_pop(2010)

acs_2011 <- get_ACS_mob_hom_and_pop(2011)

acs_2012 <- get_ACS_mob_hom_and_pop(2012)

acs_2013 <- get_ACS_mob_hom_and_pop(2013)

acs_2014 <- get_ACS_mob_hom_and_pop(2014)

acs_2015 <- get_ACS_mob_hom_and_pop(2015)
