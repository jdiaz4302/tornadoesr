


library(tidycensus)


census_api_key("ENTER API KEY HERE")


acs5_2009 <- load_variables(year = 2017, dataset = "acs5", cache = FALSE)


# Point estimate year home built
median_year_struct_built <- get_acs(geography = "county", year = 2009, variables = c("B25035_001"))


# Number of homes builts
num_homes_built <- get_acs(geography = "county", year = 2009, variables = c("B25034_001"))
# We can get these in bins but the bins move with different endyears


# Total people
total_people <- get_acs(geography = "county", year = 2009, variables = c("B02001_001"))
# Total white people
total_white <- get_acs(geography = "county", year = 2009, variables = c("B02001_002"))
# Percent white people - PSUEDOCODE
percent_white <- total_white / total_people


# Total people
total_people <- get_acs(geography = "county", year = 2009, variables = c("B01001_001"))
# Total male
total_male <- get_acs(geography = "county", year = 2009, variables = c("B01001_002"))
# Percent male - PSUEDOCODE
percent_male <- total_male / total_people


# Educational attainment for people over 25
total_people <- get_acs(geography = "county", year = 2009, variables = c("B15003_001"))
# Total high school
total_high_school <- get_acs(geography = "county", year = 2009, variables = c("B15003_017"))
# Total GED
total_GED <- get_acs(geography = "county", year = 2009, variables = c("B15003_018"))
# Total associates
total_assoc <- get_acs(geography = "county", year = 2009, variables = c("B15003_021"))
# Total bach
total_bach <- get_acs(geography = "county", year = 2009, variables = c("B15003_022"))
# Total masters
total_master <- get_acs(geography = "county", year = 2009, variables = c("B15003_023"))
# Total prof
total_prof <- get_acs(geography = "county", year = 2009, variables = c("B15003_024"))
# Total phd
total_phd <- get_acs(geography = "county", year = 2009, variables = c("B15003_025"))


# Total under 18
total_kids <- get_acs(geography = "county", year = 2009, variables = c("B09001_001"))


# Total over 65
# Males
males_total_65_66 <- get_acs(geography = "county", year = 2009, variables = c("B01001_020"))
males_total_67_69 <- get_acs(geography = "county", year = 2009, variables = c("B01001_021"))
males_total_70_74 <- get_acs(geography = "county", year = 2009, variables = c("B01001_022"))
males_total_75_79 <- get_acs(geography = "county", year = 2009, variables = c("B01001_023"))
males_total_80_84 <- get_acs(geography = "county", year = 2009, variables = c("B01001_024"))
males_total_85_up <- get_acs(geography = "county", year = 2009, variables = c("B01001_025"))
# Females
females_total_65_66 <- get_acs(geography = "county", year = 2009, variables = c("B01001_044"))
females_total_67_69 <- get_acs(geography = "county", year = 2009, variables = c("B01001_045"))
females_total_70_74 <- get_acs(geography = "county", year = 2009, variables = c("B01001_046"))
females_total_75_79 <- get_acs(geography = "county", year = 2009, variables = c("B01001_047"))
females_total_80_84 <- get_acs(geography = "county", year = 2009, variables = c("B01001_048"))
females_total_85_up <- get_acs(geography = "county", year = 2009, variables = c("B01001_049"))


# Lower quartile house value
lowerq_house_value <- get_acs(geography = "county", year = 2009, variables = c("B25076_001"))
# Median house value
median_house_value <- get_acs(geography = "county", year = 2009, variables = c("B25077_001"))
# Upper quartile house value
upperq_house_value <- get_acs(geography = "county", year = 2009, variables = c("B25078_001"))


# Poverty
total_poverty <- get_acs(geography = "county", year = 2009, variables = c("B17001_001"))


# Gini index for income inequality
gini_index <- get_acs(geography = "county", year = 2009, variables = c("B19083_001"))


# Employement
num_employed_over_16 <- get_acs(geography = "county", year = 2009, variables = c("B23001_001"))


# Commute
commute_binned_by_mins <- get_acs(geography = "county", year = 2009, variables = c("B08303_001"))


# Departure time of commute
commute_binned_by_depart <- get_acs(geography = "county", year = 2009, variables = c("B08011_001"))



