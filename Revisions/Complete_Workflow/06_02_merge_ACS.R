


# Packages
library(tidycensus)
library(dplyr)
library(readr)


# Import the tornado data - with income and NLCD
tor_df <- read.csv("data/raw/tor_data_with_NLCD.csv")


# Get state abbreviations from the names
tor_df$state_abbrev <- state.abb[match(tor_df$STATE,
                                       toupper(state.name))]


# Import Census-provided land area per county data - gonna get densities
# link: https://www.census.gov/support/USACdataDownloads.html#LND
# Converted from xls to csv via Excel because R proved annoying
land_area <- read.csv("data/raw/LND01.csv") %>%
  select(c(ï..Areaname, LND010200D))


################################################
########## PROCESSSING LAND AREA DATA ##########
################################################
# I'm only interested in county entries, which are always contain a comma
land_area$filter_var <- grepl(",", land_area$ï..Areaname)


# Only keep things with commas (counties) and then get rid of that 
# newly created variable
land_area <- filter(land_area,
                    filter_var != FALSE) %>%
  select(-filter_var)


# Make the location variable easier to type
colnames(land_area)[colnames(land_area) == "ï..Areaname"] <- "LOC"


# Give LOC an easy-to-deal-with format
land_area$LOC <- sprintf("%100s", land_area$LOC)


# Get county name
land_area$county <- substr(land_area$LOC, 1, 96)

# Replaces the duplicate spacing (empty space) with a
# Single space
land_area$county <- gsub("\\s+",
                         " ", land_area$county)

# Removes the first character, which is always a single space
land_area$county <- substring(land_area$county, 2)


# Get state of county - as abbrev
land_area$state_abb <- substr(land_area$LOC, 99, 100)


# Get state of county - as name
land_area$state_name <- state.name[match(land_area$state_abb, 
                                         state.abb)]

# Get years
year_1997 <- rep(1997, nrow(land_area))
year_1998 <- rep(1998, nrow(land_area))
year_1999 <- rep(1999, nrow(land_area))
year_2000 <- rep(2000, nrow(land_area))
year_2001 <- rep(2001, nrow(land_area))
year_2002 <- rep(2002, nrow(land_area))
year_2003 <- rep(2003, nrow(land_area))
year_2004 <- rep(2004, nrow(land_area))
year_2005 <- rep(2005, nrow(land_area))
year_2006 <- rep(2006, nrow(land_area))
year_2007 <- rep(2007, nrow(land_area))
year_2008 <- rep(2008, nrow(land_area))
year_2009 <- rep(2009, nrow(land_area))
year_2010 <- rep(2010, nrow(land_area))
year_2011 <- rep(2011, nrow(land_area))
year_2012 <- rep(2012, nrow(land_area))
year_2013 <- rep(2013, nrow(land_area))
year_2014 <- rep(2014, nrow(land_area))
year_2015 <- rep(2015, nrow(land_area))
year_2016 <- rep(2016, nrow(land_area))
year_2017 <- rep(2017, nrow(land_area))


# Get county/year combinations
land_area <- do.call(rbind, replicate(21, as.matrix(land_area), simplify = FALSE)) %>%
  as.data.frame()

land_area$YEAR <- c(year_1997,
                    year_1998,
                    year_1999,
                    year_2000,
                    year_2001,
                    year_2002,
                    year_2003,
                    year_2004,
                    year_2005,
                    year_2006,
                    year_2007,
                    year_2008,
                    year_2009,
                    year_2010,
                    year_2011,
                    year_2012,
                    year_2013,
                    year_2014,
                    year_2015,
                    year_2016,
                    year_2017)


# Get the county and state
land_area_county_state_year <- select(land_area,
                                      c(county,
                                        state_name,
                                        YEAR))
################################################
###### END OF PROCESSSING LAND AREA DATA #######
################################################


# Set census API
census_api_key("REDACTED")


# Writing a function, because this will be repeated
get_ACS_for_tor <- function(end_year) {
  
  # description: get mobile home count and population of each county from ACS
  # argument: end-year of the ACS
  # return: a dataframe containing mobile home count, total population, and
  #     median household income of each county
  
  acs_df <- get_acs(geography = "county", year = end_year,
                    variables = c("B25024_010",   # Total mobile homes
                                  "B01003_001",   # Total population
                                  "B19013_001",   # Median household income
                                  "B25035_001",   # Median year structures built
                                  "B25034_001",   # Number of homes
                                  "B02001_001",   # Number of people
                                  "B02001_002",   # Number of white peope
                                  "B01001_002",   # Number of males
                                  "B09001_001",   # Number under 18 yrs old
                                  "B15002_011",   # Males with high school equiv. edu
                                  "B15002_028",   # Females with high school equiv. edu
                                  "B15002_014",   # Males with associates
                                  "B15002_031",   # Females with associates
                                  "B15002_015",   # Males with bachelors
                                  "B15002_032",   # Females with bachelors
                                  "B15002_016",   # Males with masters
                                  "B15002_017",   # Males with professional deg
                                  "B15002_018",   # Males with doctorates
                                  "B15002_033",   # Females with masters
                                  "B15002_034",   # Females with professional deg
                                  "B15002_035",   # Females with doctorates
                                  "B01001_020",   # Males 65-66
                                  "B01001_021",   # Males 67-69
                                  "B01001_022",   # Males 70-74
                                  "B01001_023",   # Males 75-79
                                  "B01001_024",   # Males 80-84
                                  "B01001_025",   # Males 85+
                                  "B01001_044",   # Females 65-66
                                  "B01001_045",   # Females 67-69
                                  "B01001_046",   # Females 70-74
                                  "B01001_047",   # Females 75-79
                                  "B01001_048",   # Females 80-84
                                  "B01001_049",   # Females 85+
                                  "B25076_001",   # Lower quartile home values
                                  "B25077_001",   # Median home values
                                  "B25078_001",   # Upper quartile home values
                                  "B17001_001",   # People w/ poverty status in the past 12 months
                                  "B19083_001",   # Gini index of income inequality
                                  "B12006_006",   # Unemployed males in labor force (nev. married)
                                  "B12006_007",   # Males not in the labor force (nev. married)
                                  "B12006_011",   # Unemployed females in labor force (nev. marr.),
                                  "B12006_012",   # Females not in the labor force (nev. married)
                                  "B12006_017",   # Unemployed males in labor force (married)
                                  "B12006_018",   # Males not in the labor force (married)
                                  "B12006_022",   # Unemployed females in labor force (married)
                                  "B12006_023",   # Females not in the labor force (married)
                                  "B12006_028",   # Unemployed males in labor force (separated)
                                  "B12006_029",   # Males not in the labor force (separated)
                                  "B12006_033",   # Unemployed females in labor force ( separated)
                                  "B12006_034",   # Females not in the labor force (separated)
                                  "B12006_039",   # Unemployed males in labor force (widowed)
                                  "B12006_040",   # Males not in the labor force (widowed)
                                  "B12006_044",   # Unemployed females in labor force (widowed)
                                  "B12006_045",   # Females not in the labor force (widowed)
                                  "B12006_050",   # Unemployed males in labor force (divorced)
                                  "B12006_051",   # Males not in labor force (divorced)
                                  "B12006_055",   # Unemployed females in labor force (divorced)
                                  "B12006_056",   # Females not in labor force (divorced)
                                  "B08303_008",   # 30-34min travel time to work
                                  "B08303_009",   # 35-39min travel time to work
                                  "B08303_010",   # 40-44min travel time to work
                                  "B08303_011",   # 45-59min travel time to work
                                  "B08303_012",   # 60-89min travel time to work
                                  "B08303_013",   # 90min+ travel time to work
                                  "B08011_002",   # depart for work between midnight & 5am
                                  "B08011_003",   # depart for work between 5 and 5:30 am
                                  "B08011_004",   # depart for work between 5:30 and 6 am
                                  "B08011_015"   # depart for work between 4pm and midnight
                                  ))  
  
  acs_df$YEAR <- rep(end_year, nrow(acs_df))
  
  return(acs_df)
  
}


# Get ACS data for all available years
# Assigning unavailable years to their closest neighbor
acs_2009 <- get_ACS_for_tor(2009)

acs_1997 <- acs_2009
acs_1997$YEAR <- rep(1997, nrow(acs_1997))

acs_1998 <- acs_2009
acs_1998$YEAR <- rep(1998, nrow(acs_1998))

acs_1999 <- acs_2009
acs_1999$YEAR <- rep(1999, nrow(acs_1999))

acs_2000 <- acs_2009
acs_2000$YEAR <- rep(2000, nrow(acs_2000))

acs_2001 <- acs_2009
acs_2001$YEAR <- rep(2001, nrow(acs_2001))

acs_2002 <- acs_2009
acs_2002$YEAR <- rep(2002, nrow(acs_2002))

acs_2003 <- acs_2009
acs_2003$YEAR <- rep(2003, nrow(acs_2003))

acs_2004 <- acs_2009
acs_2004$YEAR <- rep(2004, nrow(acs_2004))

acs_2005 <- acs_2009
acs_2005$YEAR <- rep(2005, nrow(acs_2005))

acs_2006 <- acs_2009
acs_2006$YEAR <- rep(2006, nrow(acs_2006))

acs_2007 <- acs_2009
acs_2007$YEAR <- rep(2007, nrow(acs_2007))

acs_2008 <- acs_2009
acs_2008$YEAR <- rep(2008, nrow(acs_2008))

acs_2010 <- get_ACS_for_tor(2010)

acs_2011 <- get_ACS_for_tor(2011)

acs_2012 <- get_ACS_for_tor(2012)

acs_2013 <- get_ACS_for_tor(2013)

acs_2014 <- get_ACS_for_tor(2014)

acs_2015 <- get_ACS_for_tor(2015)

acs_2016 <- get_ACS_for_tor(2016)

acs_2017 <- get_ACS_for_tor(2017)

acs_2018 <- acs_2017
acs_2018$YEAR <- rep(2018, nrow(acs_2018))


# Put them all together
acs_df <- rbind(acs_1997,
                acs_1998,
                acs_1999,
                acs_2000,
                acs_2001,
                acs_2002,
                acs_2003,
                acs_2004,
                acs_2005,
                acs_2006,
                acs_2007,
                acs_2008,
                acs_2009,
                acs_2010,
                acs_2011,
                acs_2012,
                acs_2013,
                acs_2014,
                acs_2015,
                acs_2016,
                acs_2017,
                acs_2018) %>%
  dplyr::select(-c(GEOID, moe))


# Get only population counts
population_data <- filter(acs_df,
                          variable == "B01003_001")


# Get only mobile home counts
mob_home_data <- filter(acs_df,
                        variable == "B25024_010")


# Get only median household income
income_data <- filter(acs_df,
                      variable == "B19013_001")


# Match mobile home count to the land area data.frame
land_area$MOB_HOM_COUNT <- mob_home_data$estimate[match(toupper(do.call(paste, land_area_county_state_year)),
                                                        paste(toupper(sub("\\s+\\w+,", "",
                                                                          mob_home_data$NAME)),
                                                              mob_home_data$YEAR))]


# Match population count to the land area data.frame
land_area$POP_COUNT <- population_data$estimate[match(toupper(do.call(paste, land_area_county_state_year)),
                                                      paste(toupper(sub("\\s+\\w+,", "",
                                                                  population_data$NAME)),
                                                            population_data$YEAR))]

# Match median household income to the land area data.frame
land_area$MED_INC <- income_data$estimate[match(toupper(do.call(paste, land_area_county_state_year)),
                                                paste(toupper(sub("\\s+\\w+,", "",
                                                                  population_data$NAME)),
                                                      population_data$YEAR))]


# Notably, this left out St. Louis city, Missouri - big tornado town, rest are mostly Alaska
# Manually fixing the St. Louis City
#land_area$MOB_HOM_COUNT[land_area$county == "St. Louis city"] <-
#  mob_home_data[mob_home_data$NAME == "St. Louis city, Missouri", ]$estimate


#land_area$POP_COUNT[land_area$county == "St. Louis city"] <-
#  population_data[population_data$NAME == "St. Louis city, Missouri", ]$estimate


# Make land area numeric
land_area$LND010200D <- as.numeric(land_area$LND010200D)


# Get mobile home density
land_area$MOB_HOM_DENS <- land_area$MOB_HOM_COUNT / land_area$LND010200D


# Get population density
land_area$POP_DENS <- land_area$POP_COUNT / land_area$LND010200D


# Get the county, state, and year for matching
tor_county_state_year <- dplyr::select(tor_df,
                                       c(CZ_NAME,
                                         state_abbrev,
                                         YEAR))

# Format land_area$LOC
land_area$LOC <- gsub("\\s+", " ", land_area$LOC) %>%
  substring(first = 2)


# Match ACS data to tornado data
tor_df$MOB_HOME_DENS <- land_area$MOB_HOM_DENS[match(toupper(do.call(paste, tor_county_state_year)),
                                                     paste(toupper(sub(",", "",
                                                                       land_area$LOC)),
                                                           land_area$YEAR))]

tor_df$POP_DENS <- land_area$POP_DENS[match(toupper(do.call(paste, tor_county_state_year)),
                                            paste(toupper(sub(",", "",
                                                              land_area$LOC)),
                                                  land_area$YEAR))]

tor_df$INCOME <- land_area$MED_INC[match(toupper(do.call(paste, tor_county_state_year)),
                                         paste(toupper(sub(",", "",
                                                           land_area$LOC)),
                                               land_area$YEAR))]


# Get rid of any now-incomplete rows
tor_df <- na.omit(tor_df)


# Save it
# write_csv(tor_df, "data/raw/tor_data_with_ACS.csv")


