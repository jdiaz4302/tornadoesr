


# Packages
library(tidycensus)


# Set census API
census_api_key("ENTER CENSUS API KEY")


# Finding variable of interest
acs_variables <- load_variables(2015, "acs5", cache = TRUE)

View(acs_variables)
# Search "mobile home" in RStudio


# Get the mobile home data
mob_home_df <- get_acs(geography = "county", variables = "B25024_010E")
# This is a count of mobile homes not used for business


