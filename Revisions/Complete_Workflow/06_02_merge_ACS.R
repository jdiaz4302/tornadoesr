


# Packages
install.packages('tidycensus')
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
  select(c(Areaname, LND010200D))


################################################
########## PROCESSSING LAND AREA DATA ##########
################################################
# I'm only interested in county entries, which are always contain a comma
land_area$filter_var <- grepl(",", land_area$Areaname)


# Only keep things with commas (counties) and then get rid of that 
# newly created variable
land_area <- filter(land_area,
                    filter_var != FALSE) %>%
  select(-filter_var)


# Make the location variable easier to type
colnames(land_area)[colnames(land_area) == "Areaname"] <- "LOC"


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
year_2018 <- rep(2018, nrow(land_area))


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
  
  acs_df <- get_acs(geography = "county", year = end_year, geometry = TRUE, 
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
acs_2010 <- get_ACS_for_tor(2010)
acs_2010_wide <- dplyr::select(data.frame(acs_2010),
                               c('NAME', 'variable', 'estimate'))
acs_2010_wide <- reshape(data = acs_2010_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2010_wide$YEAR <- rep(2010, nrow(acs_2010_wide))


acs_1997 <- acs_2010
acs_1997$YEAR <- rep(1997, nrow(acs_1997))
acs_1997_wide <- dplyr::select(data.frame(acs_1997),
                               c('NAME', 'variable', 'estimate'))
acs_1997_wide <- reshape(data = acs_1997_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_1997_wide$YEAR <- rep(1997, nrow(acs_1997_wide))

acs_1998 <- acs_2010
acs_1998$YEAR <- rep(1998, nrow(acs_1998))
acs_1998_wide <- dplyr::select(data.frame(acs_1998),
                               c('NAME', 'variable', 'estimate'))
acs_1998_wide <- reshape(data = acs_1998_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_1998_wide$YEAR <- rep(1998, nrow(acs_1998_wide))

acs_1999 <- acs_2010
acs_1999$YEAR <- rep(1999, nrow(acs_1999))
acs_1999_wide <- dplyr::select(data.frame(acs_1999),
                               c('NAME', 'variable', 'estimate'))
acs_1999_wide <- reshape(data = acs_1999_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_1999_wide$YEAR <- rep(1999, nrow(acs_1999_wide))

acs_2000 <- acs_2010
acs_2000$YEAR <- rep(2000, nrow(acs_2000))
acs_2000_wide <- dplyr::select(data.frame(acs_2000),
                               c('NAME', 'variable', 'estimate'))
acs_2000_wide <- reshape(data = acs_2000_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2000_wide$YEAR <- rep(2000, nrow(acs_2000_wide))

acs_2001 <- acs_2010
acs_2001$YEAR <- rep(2001, nrow(acs_2001))
acs_2001_wide <- dplyr::select(data.frame(acs_2001),
                               c('NAME', 'variable', 'estimate'))
acs_2001_wide <- reshape(data = acs_2001_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2001_wide$YEAR <- rep(2001, nrow(acs_2001_wide))

acs_2002 <- acs_2010
acs_2002$YEAR <- rep(2002, nrow(acs_2002))
acs_2002_wide <- dplyr::select(data.frame(acs_2002),
                               c('NAME', 'variable', 'estimate'))
acs_2002_wide <- reshape(data = acs_2002_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2002_wide$YEAR <- rep(2002, nrow(acs_2002_wide))

acs_2003 <- acs_2010
acs_2003$YEAR <- rep(2003, nrow(acs_2003))
acs_2003_wide <- dplyr::select(data.frame(acs_2003),
                               c('NAME', 'variable', 'estimate'))
acs_2003_wide <- reshape(data = acs_2003_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2003_wide$YEAR <- rep(2003, nrow(acs_2003_wide))

acs_2004 <- acs_2010
acs_2004$YEAR <- rep(2004, nrow(acs_2004))
acs_2004_wide <- dplyr::select(data.frame(acs_2004),
                               c('NAME', 'variable', 'estimate'))
acs_2004_wide <- reshape(data = acs_2004_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2004_wide$YEAR <- rep(2004, nrow(acs_2004_wide))

acs_2005 <- acs_2010
acs_2005$YEAR <- rep(2005, nrow(acs_2005))
acs_2005_wide <- dplyr::select(data.frame(acs_2005),
                               c('NAME', 'variable', 'estimate'))
acs_2005_wide <- reshape(data = acs_2005_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2005_wide$YEAR <- rep(2005, nrow(acs_2005_wide))

acs_2006 <- acs_2010
acs_2006$YEAR <- rep(2006, nrow(acs_2006))
acs_2006_wide <- dplyr::select(data.frame(acs_2006),
                               c('NAME', 'variable', 'estimate'))
acs_2006_wide <- reshape(data = acs_2006_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2006_wide$YEAR <- rep(2006, nrow(acs_2006_wide))

acs_2007 <- acs_2010
acs_2007$YEAR <- rep(2007, nrow(acs_2007))
acs_2007_wide <- dplyr::select(data.frame(acs_2007),
                               c('NAME', 'variable', 'estimate'))
acs_2007_wide <- reshape(data = acs_2007_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2007_wide$YEAR <- rep(2007, nrow(acs_2007_wide))

acs_2008 <- acs_2010
acs_2008$YEAR <- rep(2008, nrow(acs_2008))
acs_2008_wide <- dplyr::select(data.frame(acs_2008),
                               c('NAME', 'variable', 'estimate'))
acs_2008_wide <- reshape(data = acs_2008_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2008_wide$YEAR <- rep(2008, nrow(acs_2008_wide))

acs_2009 <- acs_2010
acs_2009$YEAR <- rep(2009, nrow(acs_2009))
acs_2009_wide <- dplyr::select(data.frame(acs_2009),
                               c('NAME', 'variable', 'estimate'))
acs_2009_wide <- reshape(data = acs_2009_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2009_wide$YEAR <- rep(2009, nrow(acs_2009_wide))

acs_2011 <- get_ACS_for_tor(2011)
acs_2011_wide <- dplyr::select(data.frame(acs_2011),
                               c('NAME', 'variable', 'estimate'))
acs_2011_wide <- reshape(data = acs_2011_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2011_wide$YEAR <- rep(2011, nrow(acs_2011_wide))

acs_2012 <- get_ACS_for_tor(2012)
acs_2012_wide <- dplyr::select(data.frame(acs_2012),
                               c('NAME', 'variable', 'estimate'))
acs_2012_wide <- reshape(data = acs_2012_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2012_wide$YEAR <- rep(2012, nrow(acs_2012_wide))

acs_2013 <- get_ACS_for_tor(2013)
acs_2013_wide <- dplyr::select(data.frame(acs_2013),
                               c('NAME', 'variable', 'estimate'))
acs_2013_wide <- reshape(data = acs_2013_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2013_wide$YEAR <- rep(2013, nrow(acs_2013_wide))

acs_2014 <- get_ACS_for_tor(2014)
acs_2014_wide <- dplyr::select(data.frame(acs_2014),
                               c('NAME', 'variable', 'estimate'))
acs_2014_wide <- reshape(data = acs_2014_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2014_wide$YEAR <- rep(2014, nrow(acs_2014_wide))

acs_2015 <- get_ACS_for_tor(2015)
acs_2015_wide <- dplyr::select(data.frame(acs_2015),
                               c('NAME', 'variable', 'estimate'))
acs_2015_wide <- reshape(data = acs_2015_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2015_wide$YEAR <- rep(2015, nrow(acs_2015_wide))

acs_2016 <- get_ACS_for_tor(2016)
acs_2016_wide <- dplyr::select(data.frame(acs_2016),
                               c('NAME', 'variable', 'estimate'))
acs_2016_wide <- reshape(data = acs_2016_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2016_wide$YEAR <- rep(2016, nrow(acs_2016_wide))

acs_2017 <- get_ACS_for_tor(2017)
acs_2017_wide <- dplyr::select(data.frame(acs_2017),
                               c('NAME', 'variable', 'estimate'))
acs_2017_wide <- reshape(data = acs_2017_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2017_wide$YEAR <- rep(2017, nrow(acs_2017_wide))

acs_2018 <- acs_2017
acs_2018$YEAR <- rep(2018, nrow(acs_2018))
acs_2018_wide <- dplyr::select(data.frame(acs_2018),
                               c('NAME', 'variable', 'estimate'))
acs_2018_wide <- reshape(data = acs_2018_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_2018_wide$YEAR <- rep(2018, nrow(acs_2018_wide))


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
acs_df_wide <- rbind(acs_1997_wide,
                     acs_1998_wide,
                     acs_1999_wide,
                     acs_2000_wide,
                     acs_2001_wide,
                     acs_2002_wide,
                     acs_2003_wide,
                     acs_2004_wide,
                     acs_2005_wide,
                     acs_2006_wide,
                     acs_2007_wide,
                     acs_2008_wide,
                     acs_2009_wide,
                     acs_2010_wide,
                     acs_2011_wide,
                     acs_2012_wide,
                     acs_2013_wide,
                     acs_2014_wide,
                     acs_2015_wide,
                     acs_2016_wide,
                     acs_2017_wide,
                     acs_2018_wide)


# Clearing up RAM
rm(list = c('acs_1997',
            'acs_1998',
            'acs_1999',
            'acs_2000',
            'acs_2001',
            'acs_2002',
            'acs_2003',
            'acs_2004',
            'acs_2005',
            'acs_2006',
            'acs_2007',
            'acs_2008',
            'acs_2009',
            'acs_2010',
            'acs_2011',
            'acs_2012',
            'acs_2013',
            'acs_2014',
            'acs_2015',
            'acs_2016',
            'acs_2017',
            'acs_2018'))
gc()


# Performing weighted extractions now
library(raster)
library(sf)


# Import the NLCD raster for its CRS so we don't have to recode much of this process
NLCD <- raster::raster(paste0('data/raw/nlcd_2001_landcover_2011_edition_2014_10_10/',
                              'nlcd_2001_landcover_2011_edition_2014_10_10.img'))


# Getting the coordinates for spatial data formalization
tor_coord <- dplyr::select(tor_df,
                           c(BEGIN_LON,
                             BEGIN_LAT))


# Spatial data formalization
tor_Spatial <- SpatialPointsDataFrame(tor_coord,
                                      tor_df,
                                      proj4string = CRS("+init=epsg:4326"))


# Giving it the same CRS as NLCD
tor_Spatial <- spTransform(tor_Spatial,
                           crs(NLCD))


# Getting the transformed coordinates for the template's manual georeferencing
coords <- tor_Spatial@coords
coords <- coords[1:(length(coords) / 2), ]

years <- tor_Spatial@data$YEAR


# Storing the standard deviation of tornado path length
stand_dev_of_tor_length <- sd(tor_df$TOR_LENGTH) / 10 * 1609.34


# Creating a Gaussian template with NLCD CRS and tornado path length sd
Gaussian_template <- focalWeight(NLCD,
                                 d = stand_dev_of_tor_length,
                                 type = 'Gauss')


# Defining function for the weighted extraction
weighted_extract <- function(coords, year, ACS_data) {
  ACS_data <- dplyr::filter(ACS_data,
                            YEAR == year)
  ACS_data <- dplyr::filter(ACS_data,
                            variable == 'B01001_002') # Random one to remove duplicate .shp's
  
  # Creating grid of lats and lon
  coords_1 <- seq(as.numeric(coords[1]) - (dim(Gaussian_template)[1] - 1) / 2 * 30,
                  as.numeric(coords[1]) + (dim(Gaussian_template)[1] - 1) / 2 * 30,
                  by = 30)
  coords_2 <- seq(as.numeric(coords[2]) - (dim(Gaussian_template)[2] - 1) / 2 * 30,
                  as.numeric(coords[2]) + (dim(Gaussian_template)[2] - 1) / 2 * 30,
                  by = 30)
  
  # Making them into data.frame columns
  lon <- as.vector(t(replicate(dim(Gaussian_template)[2], coords_1)))
  lat <- as.vector(replicate(dim(Gaussian_template)[1], coords_2))
  coords_df <- data.frame(lon, lat)
  
  # Formalizing the spatial object
  Gaussian_template_for_tor <- SpatialPointsDataFrame(coords_df,
                                                      coords_df,
                                                      proj4string = crs(NLCD))
  # Converting to the census CRS
  Gaussian_template_for_tor <- spTransform(Gaussian_template_for_tor,
                                           crs(ACS_data))
  
  # Extract the all social value indices within the Gaussian template extent
  Gaussian_template_area_extraction <- over(Gaussian_template_for_tor,
                                            as_Spatial(ACS_data$geometry))
  
  # Get the data from those indices
  Gaussian_template_data <- ACS_data[Gaussian_template_area_extraction, ]
  
  # From that data, match the long data (georef.) to the wide data (not-georef., compacted vars)
  all_vars_indices <- match(paste(Gaussian_template_data$NAME, year),
                            paste(acs_df_wide$NAME, acs_df_wide$YEAR))
  return(all_vars_indices)
}


# Performing the weighted extraction for all ACS variables
new_features <- matrix(ncol = 21, nrow = nrow(tor_df))
for (i in 1:nrow(tor_df)) {
  a <- weighted_extract(coords[i, ], years[i], acs_df)
  
  # Number of mobile homes
  mobile_homes_list <- acs_df_wide$estimate.B25024_010[a]
  mobile_homes_weighted <- stats::weighted.mean(mobile_homes_list,
                                                as.vector(Gaussian_template),
                                                na.rm = TRUE) # For the coastal cases and
                                                              # partially missing data
  
  # Total population count
  population_list <- acs_df_wide$estimate.B01003_001[a]
  population_weighted <- stats::weighted.mean(population_list,
                                              as.vector(Gaussian_template),
                                              na.rm = TRUE)
  new_features[i, 2] <- population_weighted
  
  # Median household income
  med_income_list <- acs_df_wide$estimate.B19013_001[a]
  med_income_weighted <- stats::weighted.mean(med_income_list,
                                              as.vector(Gaussian_template),
                                              na.rm = TRUE)
  new_features[i, 3] <- med_income_weighted
  
  # Median year home built
  med_home_age_list <- acs_df_wide$estimate.B25035_001[a]
  med_home_age_weighted <- stats::weighted.mean(med_home_age_list,
                                                as.vector(Gaussian_template),
                                                na.rm = TRUE)
  new_features[i, 4] <- med_home_age_weighted
  
  # Number homes
  num_homes_list <- acs_df_wide$estimate.B25034_001[a]
  num_homes_weighted <- stats::weighted.mean(num_homes_list,
                                             as.vector(Gaussian_template),
                                             na.rm = TRUE)
  new_features[i, 5] <- num_homes_weighted
  percent_mobile_homes_weighted <- mobile_homes_weighted / num_homes_weighted
  new_features[i, 1] <- percent_mobile_homes_weighted
  
  # Number white
  num_white_list <- acs_df_wide$estimate.B02001_002[a]
  num_white_weighted <- stats::weighted.mean(num_white_list,
                                             as.vector(Gaussian_template),
                                             na.rm = TRUE)
  percent_white_weighted <- num_white_weighted / population_weighted
  new_features[i, 6] <- percent_white_weighted
  
  # Number males
  num_males_list <- acs_df_wide$estimate.B01001_002[a]
  num_males_weighted <- stats::weighted.mean(num_males_list,
                                             as.vector(Gaussian_template),
                                             na.rm = TRUE)
  percent_males_weighted <- num_males_weighted / population_weighted
  new_features[i, 7] <- percent_males_weighted
  
  # Number children
  num_children_list <- acs_df_wide$estimate.B09001_001[a]
  num_children_weighted <- stats::weighted.mean(num_children_list,
                                                as.vector(Gaussian_template),
                                                na.rm = TRUE)
  percent_kids_weighted <- num_children_weighted / population_weighted
  new_features[i, 8] <- percent_kids_weighted
  num_adults <- population_weighted - num_children_weighted
  
  # Number with high school level edu
  num_high_school_list <- (acs_df_wide$estimate.B15002_011[a] +
                             acs_df_wide$estimate.B15002_028[a])
  num_high_school_weighted <- stats::weighted.mean(num_high_school_list,
                                                   as.vector(Gaussian_template),
                                                   na.rm = TRUE)
  percent_high_school_weighted <- num_high_school_weighted / num_adults
  new_features[i, 9] <- percent_high_school_weighted
  
  # Number with associates level edu
  num_assoc_list <- (acs_df_wide$estimate.B15002_014[a] +
                       acs_df_wide$estimate.B15002_031[a])
  num_assoc_weighted <- stats::weighted.mean(num_assoc_list,
                                             as.vector(Gaussian_template),
                                             na.rm = TRUE)
  percent_assoc_weighted <- num_assoc_weighted / num_adults
  new_features[i, 10] <- percent_assoc_weighted
  
  # Number with bachelors level edu
  num_bach_list <- (acs_df_wide$estimate.B15002_015[a] +
                      acs_df_wide$estimate.B15002_032[a])
  num_bach_weighted <- stats::weighted.mean(num_bach_list,
                                            as.vector(Gaussian_template),
                                            na.rm = TRUE)
  percent_bach_weighted <- num_bach_weighted / num_adults
  new_features[i, 11] <- percent_bach_weighted
  
  # Number with graduate level edu
  num_grad_list <- (acs_df_wide$estimate.B15002_016[a] +
                      acs_df_wide$estimate.B15002_017[a] +
                      acs_df_wide$estimate.B15002_018[a] +
                      acs_df_wide$estimate.B15002_033[a] +
                      acs_df_wide$estimate.B15002_034[a] +
                      acs_df_wide$estimate.B15002_035[a])
  num_grad_weighted <- stats::weighted.mean(num_grad_list,
                                            as.vector(Gaussian_template),
                                            na.rm = TRUE)
  percent_grad_weighted <- num_grad_weighted / num_adults
  new_features[i, 12] <- percent_grad_weighted
  
  # Number senior citizens
  num_senr_list <- (acs_df_wide$estimate.B01001_020[a] +
                      acs_df_wide$estimate.B01001_021[a] +
                      acs_df_wide$estimate.B01001_022[a] +
                      acs_df_wide$estimate.B01001_023[a] +
                      acs_df_wide$estimate.B01001_024[a] +
                      acs_df_wide$estimate.B01001_025[a] +
                      acs_df_wide$estimate.B01001_044[a] +
                      acs_df_wide$estimate.B01001_045[a] +
                      acs_df_wide$estimate.B01001_046[a] +
                      acs_df_wide$estimate.B01001_047[a] +
                      acs_df_wide$estimate.B01001_048[a] +
                      acs_df_wide$estimate.B01001_049[a])
  num_senr_weighted <- stats::weighted.mean(num_senr_list,
                                            as.vector(Gaussian_template),
                                            na.rm = TRUE)
  percent_senr_weighted <- num_senr_weighted / population_weighted
  new_features[i, 13] <- percent_senr_weighted
  
  # Lower quartile home value
  lowerq_home_list <- acs_df_wide$estimate.B25076_001[a]
  lowerq_home_weighted <- stats::weighted.mean(lowerq_home_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  new_features[i, 14] <- lowerq_home_weighted
  
  # Median home value
  median_home_list <- acs_df_wide$estimate.B25077_001[a]
  median_home_weighted <- stats::weighted.mean(median_home_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  new_features[i, 15] <- median_home_weighted
  
  # Upper quartile home value
  upperq_home_list <- acs_df_wide$estimate.B25078_001[a]
  upperq_home_weighted <- stats::weighted.mean(upperq_home_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  new_features[i, 16] <- upperq_home_weighted
  
  # Num in poverty in last 12 months
  num_poverty_list <- acs_df_wide$estimate.B17001_001[a]
  num_poverty_weighted <- stats::weighted.mean(num_poverty_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  percent_poverty_weighted <- num_poverty_weighted / population_weighted
  new_features[i, 17] <- percent_poverty_weighted
  
  # Gini
  gini_index_list <- acs_df_wide$estimate.B19083_001[a]
  gini_index_weighted <- stats::weighted.mean(gini_index_list,
                                              as.vector(Gaussian_template),
                                              na.rm = TRUE)
  new_features[i, 18] <- gini_index_weighted
  
  # Number not working
  not_working_list <- (acs_df_wide$estimate.B12006_006[a] +
                         acs_df_wide$estimate.B12006_007[a] +
                         acs_df_wide$estimate.B12006_011[a] +
                         acs_df_wide$estimate.B12006_012[a] +
                         acs_df_wide$estimate.B12006_017[a] +
                         acs_df_wide$estimate.B12006_018[a] +
                         acs_df_wide$estimate.B12006_022[a] +
                         acs_df_wide$estimate.B12006_023[a] +
                         acs_df_wide$estimate.B12006_028[a] +
                         acs_df_wide$estimate.B12006_029[a] +
                         acs_df_wide$estimate.B12006_033[a] +
                         acs_df_wide$estimate.B12006_034[a] +
                         acs_df_wide$estimate.B12006_039[a] +
                         acs_df_wide$estimate.B12006_040[a] +
                         acs_df_wide$estimate.B12006_044[a] +
                         acs_df_wide$estimate.B12006_045[a] +
                         acs_df_wide$estimate.B12006_050[a] +
                         acs_df_wide$estimate.B12006_051[a] +
                         acs_df_wide$estimate.B12006_055[a] +
                         acs_df_wide$estimate.B12006_056[a])
  not_working_weighted <- stats::weighted.mean(not_working_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  percent_not_working_weighted <- not_working_weighted / num_adults
  new_features[i, 19] <- percent_not_working_weighted
  
  # Number with commute 30mins or greater
  commuter_30_list <- (acs_df_wide$estimate.B08303_008[a] +
                         acs_df_wide$estimate.B08303_009[a] +
                         acs_df_wide$estimate.B08303_010[a] +
                         acs_df_wide$estimate.B08303_011[a] +
                         acs_df_wide$estimate.B08303_012[a] +
                         acs_df_wide$estimate.B08303_013[a])
  commuter_30_weighted <- stats::weighted.mean(commuter_30_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  percent_commuter_30_weighted <- commuter_30_weighted / num_adults
  new_features[i, 20] <- percent_commuter_30_weighted
  
  # Number with odd-hour commute
  odd_commuter_list <- acs_df_wide$estimate.B08011_002[a]
  odd_commuter_weighted <- stats::weighted.mean(odd_commuter_list,
                                                as.vector(Gaussian_template),
                                                na.rm = TRUE)
  percent_odd_commuter_weighted <- odd_commuter_weighted / num_adults
  new_features[i, 21] <- percent_odd_commuter_weighted
}


# Hard-coding the column names - triple check'd but fragile to change
new_features_colnames <- c('PERC_MOB_HOMES', 'POP_DENS', 'MEDIAN_HOUSE_INC',
                           'MED_HOME_AGE', 'NUM_HOMES', 'PERC_WHITE',
                           'PERC_MALE', 'PERC_KIDS', 'PERC_HIGH_SCHOOL',
                           'PERC_ASSOC', 'PERC_BACH', 'PERC_GRAD',
                           'PERC_SENIOR', 'LOWERQ_HOME_VAL', 'MED_HOME_VAL',
                           'UPPERQ_HOME_VAL', 'PERC_POVERTY', 'GINI_INDEX',
                           'PERC_NOT_WORKING', 'PERC_COMM_30', 'PERC_COMM_EARLY')
# Changing the matrix to a data.frame and renaming the columns appropriately
ACS_vars <- data.frame(new_features)
colnames(ACS_vars) <- new_features_colnames


# Merging tor data and ACS data
tor_df_with_ACS <- cbind(tor_df, ACS_vars)


# Save it
# write_csv(tor_df_with_ACS, "data/raw/tor_data_with_ACS.csv")


