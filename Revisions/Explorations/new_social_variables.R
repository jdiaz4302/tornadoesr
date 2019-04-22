


library(tidycensus)


census_api_key("REDACTED")


acs5_2010 <- load_variables(year = 2010, dataset = "acs5", cache = FALSE)
acs5_2017 <- load_variables(year = 2017, dataset = "acs5", cache = FALSE)


# Point estimate year home built - SECURE
median_year_struct_built <- get_acs(geography = "county", year = 2009, variables = c("B25035_001"))


# Total number of homes builts - SECURE
num_homes_built <- get_acs(geography = "county", year = 2009, variables = c("B25034_001"))
# We can get these in bins but the bins move with different endyears


# Total people - SECURE
total_people <- get_acs(geography = "county", year = 2009, variables = c("B02001_001"))
# Total white people - SECURE
total_white <- get_acs(geography = "county", year = 2009, variables = c("B02001_002"))
# Percent white people - SECURE
percent_white <- total_white$estimate / total_people$estimate


# Total male - SECURE
total_male <- get_acs(geography = "county", year = 2009, variables = c("B01001_002"))
# Percent male - SECURE
percent_male <- total_male$estimate / total_people$estimate


# Total under 18 - SECURE
total_kids <- get_acs(geography = "county", year = 2009, variables = c("B09001_001"))$estimate
perct_kids <- total_kids / total_people$estimate
total_adults <- (total_people$estimate - total_kids)


# Total high school - SECURE
perct_high_school <- (get_acs(geography = "county",
                              year = 2009,
                              variables = c("B15002_011"))$estimate +
                        get_acs(geography = "county",
                                year = 2009,
                                variables = c("B15002_028"))$estimate) /
  total_adults
# Total associates - SECURE
perct_assoc <- (get_acs(geography = "county",
                        year = 2009,
                        variables = c("B15002_014"))$estimate +
                  get_acs(geography = "county",
                          year = 2009,
                          variables = c("B15002_031"))$estimate) /
  total_adults
# Total bach - SECURE
perct_bach <- (get_acs(geography = "county",
                       year = 2009,
                       variables = c("B15002_015"))$estimate +
                 get_acs(geography = "county",
                         year = 2009,
                         variables = c("B15002_032"))$estimate) /
  total_adults
# Total grad - SECURE
perct_gradte <- (get_acs(geography = "county",
                         year = 2009,
                         variables = c("B15002_016"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B15002_017"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B15002_018"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B15002_033"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B15002_034"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B15002_035"))$estimate) /
  total_adults


# Total over 65 - SECURE
perct_senior <- (get_acs(geography = "county",
                         year = 2009,
                         variables = c("B01001_020"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_021"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_022"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_023"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_024"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_025"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_044"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_045"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_046"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_047"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_048"))$estimate +
                   get_acs(geography = "county",
                           year = 2009,
                           variables = c("B01001_049"))$estimate) /
  total_people$estimate


# Lower quartile house value - SECURE
lowerq_house_value <- get_acs(geography = "county", year = 2009, variables = c("B25076_001"))
# Median house value - SECURE
median_house_value <- get_acs(geography = "county", year = 2009, variables = c("B25077_001"))
# Upper quartile house value - SECURE
upperq_house_value <- get_acs(geography = "county", year = 2009, variables = c("B25078_001"))


# Poverty - SECURE
perct_poverty <- get_acs(geography = "county", year = 2009, variables = c("B17001_001"))$estimate /
  total_people$estimate


# Gini index for income inequality - SECURE
gini_index <- get_acs(geography = "county", year = 2009, variables = c("B19083_001"))


# Employement - DEF NOT SECURE
num_employed_over_16 <- get_acs(geography = "county", year = 2009, variables = c("B23001_001"))


# Commute - DECIDE WHAT TO DO WITH BINS
commute_binned_by_mins <- get_acs(geography = "county", year = 2009, variables = c("B08303_001"))


# Departure time of commute - DECIDE WHAT TO DO WITH BINS
commute_binned_by_depart <- get_acs(geography = "county", year = 2009, variables = c("B08011_001"))



