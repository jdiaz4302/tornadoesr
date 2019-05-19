


# Packages
library(sp)
library(raster)
library(dplyr)
library(readr)
library(maps)
library(maptools)
library(reshape2)
#install.packages('tidycensus')
library(tidycensus)
library(ggplot2)
#install.packages('questionr')
library(questionr)
library(sf)


# Get all possible longitude values
LON <- seq(from = -125,
           to = -66,
           by = 10)  ################################ Change this


# Iteratively add each possible latitude value in an
# Adjacent column to the longitude values
for (i in seq(from = 23, to = 50, by = 10)) {        ############################ Change this
  
  if (i == 23) {
    
    # Make the data.frame on the first iteration
    grid_df <- as.data.frame(LON)
    grid_df$LAT <- rep(i, length(grid_df))
    
  } else {
    
    # On every other iteration make a temporary data.frame
    temp_df <- as.data.frame(LON)
    temp_df$LAT <- rep(i, length(temp_df))
    
    # Then bind it to the original
    grid_df <- rbind(grid_df, temp_df)
    
  }
  
}


# Remove the temporary data.frame
rm(temp_df)


# Give each lat/lon combo an ID
grid_df$ID <- seq(1, nrow(grid_df))


# Get those points as a Spatial object
grid_points <- SpatialPointsDataFrame(coords = grid_df[, 1:2],
                                      data = as.data.frame(grid_df$ID),
                                      proj4string = CRS('+init=epsg:4326'))


# Read the census-provided United States shapefile
# Convert its crs
# And crop it to only the continental US
us_shape <- shapefile('data/raw/cb_2015_us_nation_5m.shp') %>%
  spTransform(CRSobj = crs(grid_points)) %>%
  crop(extent(-125, -66, 23, 50))


# Only points within the continental US
grid_points <- grid_points[us_shape, ]


# Import the NLCD raster
NLCD_2011 <- raster("data/raw/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img")


# Convert the grid's crs to that of the NLCD
grid_NLCD_crs <- spTransform(grid_points,
                             crs(NLCD_2011))


# Get the extraction buffer
source("Revisions/Complete_Workflow/03_clean_StormEvents_files.R")
stand_dev_of_tor_length <- sd(tor_df$TOR_LENGTH) * 1609.34


# We need the coordinates for the weighted extractions and Guassian template georef.
# Keeping the EVENT_IDs too
coords_grid <- grid_NLCD_crs@coords
coords_grid <- coords_grid[1:(length(coords_grid) / 2), ]
IDs_grid <- grid_NLCD_crs@data$`grid_df$ID`


# Creating a Gaussian template with NLCD CRS and tornado path length sd
Gaussian_template <- focalWeight(NLCD_2011,
                                 d = stand_dev_of_tor_length,
                                 type = 'Gauss')


# Defining function for the weighted extraction
weighted_extract <- function(coords) {
  # Creating grid of lats and lon
  coords_1 <- seq(coords[1] - dim(Gaussian_template)[1] / 2 * 30,
                  coords[1] + dim(Gaussian_template)[1] / 2 * 30,
                  by = 30)
  coords_2 <- seq(coords[2] - dim(Gaussian_template)[2] / 2 * 30,
                  coords[2] + dim(Gaussian_template)[2] / 2 * 30,
                  by = 30)
  
  # Making them into data.frame columns
  lon <- as.vector(t(replicate(dim(Gaussian_template)[2], coords_1)))
  lat <- as.vector(replicate(dim(Gaussian_template)[1], coords_2))
  coords_df <- data.frame(lon, lat)
  
  # Formalizing the spatial object
  Gaussian_template_for_tor <- SpatialPointsDataFrame(coords_df,
                                                      coords_df,
                                                      proj4string = crs(grid_points))
  
  # Extract the all LC values within the Gaussian template extent
  Gaussian_template_area_extraction <- raster::extract(NLCD_2011,
                                                       extent(Gaussian_template_for_tor))
  return(Gaussian_template_area_extraction)
}


# Sitting up the data.frames for weighted extraction percentages
NLCD_2011_factor_levels <- levels(NLCD_2011)
NLCD_2011_factor_levels_filtered <- NLCD_2011_factor_levels[[1]][NLCD_2011_factor_levels[[1]]$COUNT != 0, ]$ID
LC_grid_df <- matrix(ncol = length(NLCD_2011_factor_levels_filtered) + 1,
                                     nrow = 0) %>%
  data.frame()
colnames(LC_grid_df) <- c(paste0('percent.', NLCD_2011_factor_levels_filtered),
                                          'EVENT_ID')


# Performing the weighted extractions and filling in data.frame rows
for (i in 1:nrow(coords_grid)) {
  weighted_extractions <- wtd.table(weighted_extract(coords_grid[i, ]), weights = Gaussian_template)
  curr_df <- data.frame(weighted_extractions)
  curr_df <- tidyr::spread(curr_df, key = 'Var1', value = 'Freq', sep = "")
  new_colnames <- c()
  for (j in strsplit(colnames(curr_df), 'Var1')[]) {
    new_colnames <- c(new_colnames, paste0('percent.', (j[2])))
  }                             
  colnames(curr_df) <- new_colnames
  LC_grid_df <- plyr::rbind.fill(LC_grid_df, curr_df)
  LC_grid_df[i, 'EVENT_ID'] <- IDs_grid[i]
}


# All NA's should be 0's because it's 'missing value'
# For a land cover classification; ie, none of that classification
LC_grid_df[is.na(LC_grid_df)] <- 0


# Classification value of 0 is not valid, so git rid of that as well
LC_grid_df <- dplyr::select(LC_grid_df,
                            -percent.0)
LC_grid_df <- dplyr::select(LC_grid_df,
                            -percent.12)


# Merge the LC proportions to their lat/lon combos
grid_with_LC <- merge(x = grid_df,
                      y = LC_grid_df,
                      by.x = 'ID',
                      by.y = 'EVENT_ID')


# Give the LC values a name
colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.11"] <- "OPEN_WATER_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.21"] <- "DEV_OPEN_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.22"] <- "DEV_LOW_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.23"] <- "DEV_MED_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.24"] <- "DEV_HIGH_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.41"] <- "DECID_FOREST_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.42"] <- "EVERGR_FOREST_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.43"] <- "MIXED_FOREST_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.52"] <- "SHRUB_SCRUB_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.71"] <- "GRASS_LAND_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.81"] <- "PASTURE_HAY_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.82"] <- "CULT_CROPS_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.90"] <- "WOOD_WETLAND_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.95"] <- "HERB_WETLAND_PROP"

colnames(grid_with_LC)[colnames(grid_with_LC) == "percent.31"] <- "BARREN_LAND_PROP"


# Import the 2015 census-provided US shapefile for counties
# Get it in the right CRS
# And crop it to only the continental US
us_counties <- shapefile('data/raw/cb_2015_us_county_5m.shp') %>%
  spTransform(CRSobj = crs(grid_points)) %>%
  crop(extent(-125, -66, 23, 50))


# Remove grid points not within the continental US
# And make county names capitalized
grid_with_LC$county_name <- over(grid_points, us_counties)$NAME %>%
  toupper()


# Define a function to name the state that a coordinate belongs to
# Provided via stackoverflow
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


# Grid the state of the grid points
# And capitalize it
grid_with_LC$state <- latlong2state(grid_with_LC[2:3]) %>%
  toupper()


# Assign the year 2019 to all rows
grid_with_LC$YEAR <- rep(2019, nrow(grid_with_LC))


# Change the state names to abbreviations
grid_with_LC$state_abbrev <- state.abb[match(grid_with_LC$state,
                                             toupper(state.name))]


# Get the county, state, and year of each grid point
grid_county_state_year <- dplyr::select(grid_with_LC,
                                        c(county_name, state_abbrev, YEAR))


# Import the census-provided county area data.frame
land_area <- read.csv("data/raw/LND01.csv") %>%
  select(c(Areaname, LND010200D))


################################################
########## PROCESSSING LAND AREA DATA ##########
################################################
# I'm only interested in county entries, which always contain a comma
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
year_2019 <- rep(2019, nrow(land_area))


# Give land_area the year 2019
land_area$YEAR <- year_2019


# Get the county and state
land_area_county_state_year <- select(land_area,
                                      c(county,
                                        state_name,
                                        YEAR))
################################################
###### END OF PROCESSSING LAND AREA DATA #######
################################################


# Set census API
census_api_key("redacted")


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


# Get ACS data
acs_2017 <- get_ACS_for_tor(2017)
acs_2017_wide <- dplyr::select(data.frame(acs_2017),
                               c('NAME', 'variable', 'estimate'))
acs_2017_wide <- reshape(data = acs_2017_wide, idvar = 'NAME',
                         timevar = 'variable', direction = 'wide')
acs_df <- acs_2017

acs_df$YEAR <- rep(2019, nrow(acs_df))
rm(acs_2017)

#acs_df <- dplyr::select(acs_df,
#                        -c(GEOID, moe))

# Defining function for the weighted extraction
weighted_extract <- function(coords, year, ACS_data) {
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
                                                      proj4string = crs(NLCD_2011))
  # Converting to the census CRS
  Gaussian_template_for_tor <- spTransform(Gaussian_template_for_tor,
                                           crs(ACS_data))
  
  # Extract the all social value indices within the Gaussian template extent
  Gaussian_template_area_extraction <- over(Gaussian_template_for_tor,
                                            as_Spatial(ACS_data$geometry))
  
  # Get the data from those indices
  Gaussian_template_data <- ACS_data[Gaussian_template_area_extraction, ]
  
  # From that data, match the long data (georef.) to the wide data (not-georef., compacted vars)
  all_vars_indices <- match(paste(Gaussian_template_data$NAME, 2019),
                            paste(acs_2017_wide$NAME, 2019))
  return(all_vars_indices)
}


# Performing the weighted extraction for all ACS variables
new_features <- matrix(ncol = 21, nrow = nrow(tor_df))
for (i in 1:nrow(grid_points@data)) {
  print(i)
  print(Sys.time())
  a <- weighted_extract(coords_grid[i, ], 2019, acs_df)
  
  # Number of mobile homes
  mobile_homes_list <- acs_2017_wide$estimate.B25024_010[a]
  mobile_homes_weighted <- stats::weighted.mean(mobile_homes_list,
                                                as.vector(Gaussian_template),
                                                na.rm = TRUE) # For the coastal cases and
  # partially missing data
  
  # Total population count
  population_list <- acs_2017_wide$estimate.B01003_001[a]
  population_weighted <- stats::weighted.mean(population_list,
                                              as.vector(Gaussian_template),
                                              na.rm = TRUE)
  new_features[i, 2] <- population_weighted
  
  # Median household income
  med_income_list <- acs_2017_wide$estimate.B19013_001[a]
  med_income_weighted <- stats::weighted.mean(med_income_list,
                                              as.vector(Gaussian_template),
                                              na.rm = TRUE)
  new_features[i, 3] <- med_income_weighted
  
  # Median year home built
  med_home_age_list <- acs_2017_wide$estimate.B25035_001[a]
  med_home_age_weighted <- stats::weighted.mean(med_home_age_list,
                                                as.vector(Gaussian_template),
                                                na.rm = TRUE)
  new_features[i, 4] <- med_home_age_weighted
  
  # Number homes
  num_homes_list <- acs_2017_wide$estimate.B25034_001[a]
  num_homes_weighted <- stats::weighted.mean(num_homes_list,
                                             as.vector(Gaussian_template),
                                             na.rm = TRUE)
  new_features[i, 5] <- num_homes_weighted
  percent_mobile_homes_weighted <- mobile_homes_weighted / num_homes_weighted
  new_features[i, 1] <- percent_mobile_homes_weighted
  
  # Number white
  num_white_list <- acs_2017_wide$estimate.B02001_002[a]
  num_white_weighted <- stats::weighted.mean(num_white_list,
                                             as.vector(Gaussian_template),
                                             na.rm = TRUE)
  percent_white_weighted <- num_white_weighted / population_weighted
  new_features[i, 6] <- percent_white_weighted
  
  # Number males
  num_males_list <- acs_2017_wide$estimate.B01001_002[a]
  num_males_weighted <- stats::weighted.mean(num_males_list,
                                             as.vector(Gaussian_template),
                                             na.rm = TRUE)
  percent_males_weighted <- num_males_weighted / population_weighted
  new_features[i, 7] <- percent_males_weighted
  
  # Number children
  num_children_list <- acs_2017_wide$estimate.B09001_001[a]
  num_children_weighted <- stats::weighted.mean(num_children_list,
                                                as.vector(Gaussian_template),
                                                na.rm = TRUE)
  percent_kids_weighted <- num_children_weighted / population_weighted
  new_features[i, 8] <- percent_kids_weighted
  num_adults <- population_weighted - num_children_weighted
  
  # Number with high school level edu
  num_high_school_list <- (acs_2017_wide$estimate.B15002_011[a] +
                             acs_2017_wide$estimate.B15002_028[a])
  num_high_school_weighted <- stats::weighted.mean(num_high_school_list,
                                                   as.vector(Gaussian_template),
                                                   na.rm = TRUE)
  percent_high_school_weighted <- num_high_school_weighted / num_adults
  new_features[i, 9] <- percent_high_school_weighted
  
  # Number with associates level edu
  num_assoc_list <- (acs_2017_wide$estimate.B15002_014[a] +
                       acs_2017_wide$estimate.B15002_031[a])
  num_assoc_weighted <- stats::weighted.mean(num_assoc_list,
                                             as.vector(Gaussian_template),
                                             na.rm = TRUE)
  percent_assoc_weighted <- num_assoc_weighted / num_adults
  new_features[i, 10] <- percent_assoc_weighted
  
  # Number with bachelors level edu
  num_bach_list <- (acs_2017_wide$estimate.B15002_015[a] +
                      acs_2017_wide$estimate.B15002_032[a])
  num_bach_weighted <- stats::weighted.mean(num_bach_list,
                                            as.vector(Gaussian_template),
                                            na.rm = TRUE)
  percent_bach_weighted <- num_bach_weighted / num_adults
  new_features[i, 11] <- percent_bach_weighted
  
  # Number with graduate level edu
  num_grad_list <- (acs_2017_wide$estimate.B15002_016[a] +
                      acs_2017_wide$estimate.B15002_017[a] +
                      acs_2017_wide$estimate.B15002_018[a] +
                      acs_2017_wide$estimate.B15002_033[a] +
                      acs_2017_wide$estimate.B15002_034[a] +
                      acs_2017_wide$estimate.B15002_035[a])
  num_grad_weighted <- stats::weighted.mean(num_grad_list,
                                            as.vector(Gaussian_template),
                                            na.rm = TRUE)
  percent_grad_weighted <- num_grad_weighted / num_adults
  new_features[i, 12] <- percent_grad_weighted
  
  # Number senior citizens
  num_senr_list <- (acs_2017_wide$estimate.B01001_020[a] +
                      acs_2017_wide$estimate.B01001_021[a] +
                      acs_2017_wide$estimate.B01001_022[a] +
                      acs_2017_wide$estimate.B01001_023[a] +
                      acs_2017_wide$estimate.B01001_024[a] +
                      acs_2017_wide$estimate.B01001_025[a] +
                      acs_2017_wide$estimate.B01001_044[a] +
                      acs_2017_wide$estimate.B01001_045[a] +
                      acs_2017_wide$estimate.B01001_046[a] +
                      acs_2017_wide$estimate.B01001_047[a] +
                      acs_2017_wide$estimate.B01001_048[a] +
                      acs_2017_wide$estimate.B01001_049[a])
  num_senr_weighted <- stats::weighted.mean(num_senr_list,
                                            as.vector(Gaussian_template),
                                            na.rm = TRUE)
  percent_senr_weighted <- num_senr_weighted / population_weighted
  new_features[i, 13] <- percent_senr_weighted
  
  # Lower quartile home value
  lowerq_home_list <- acs_2017_wide$estimate.B25076_001[a]
  lowerq_home_weighted <- stats::weighted.mean(lowerq_home_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  new_features[i, 14] <- lowerq_home_weighted
  
  # Median home value
  median_home_list <- acs_2017_wide$estimate.B25077_001[a]
  median_home_weighted <- stats::weighted.mean(median_home_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  new_features[i, 15] <- median_home_weighted
  
  # Upper quartile home value
  upperq_home_list <- acs_2017_wide$estimate.B25078_001[a]
  upperq_home_weighted <- stats::weighted.mean(upperq_home_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  new_features[i, 16] <- upperq_home_weighted
  
  # Num in poverty in last 12 months
  num_poverty_list <- acs_2017_wide$estimate.B17001_001[a]
  num_poverty_weighted <- stats::weighted.mean(num_poverty_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  percent_poverty_weighted <- num_poverty_weighted / population_weighted
  new_features[i, 17] <- percent_poverty_weighted
  
  # Gini
  gini_index_list <- acs_2017_wide$estimate.B19083_001[a]
  gini_index_weighted <- stats::weighted.mean(gini_index_list,
                                              as.vector(Gaussian_template),
                                              na.rm = TRUE)
  new_features[i, 18] <- gini_index_weighted
  
  # Number not working
  not_working_list <- (acs_2017_wide$estimate.B12006_006[a] +
                         acs_2017_wide$estimate.B12006_007[a] +
                         acs_2017_wide$estimate.B12006_011[a] +
                         acs_2017_wide$estimate.B12006_012[a] +
                         acs_2017_wide$estimate.B12006_017[a] +
                         acs_2017_wide$estimate.B12006_018[a] +
                         acs_2017_wide$estimate.B12006_022[a] +
                         acs_2017_wide$estimate.B12006_023[a] +
                         acs_2017_wide$estimate.B12006_028[a] +
                         acs_2017_wide$estimate.B12006_029[a] +
                         acs_2017_wide$estimate.B12006_033[a] +
                         acs_2017_wide$estimate.B12006_034[a] +
                         acs_2017_wide$estimate.B12006_039[a] +
                         acs_2017_wide$estimate.B12006_040[a] +
                         acs_2017_wide$estimate.B12006_044[a] +
                         acs_2017_wide$estimate.B12006_045[a] +
                         acs_2017_wide$estimate.B12006_050[a] +
                         acs_2017_wide$estimate.B12006_051[a] +
                         acs_2017_wide$estimate.B12006_055[a] +
                         acs_2017_wide$estimate.B12006_056[a])
  not_working_weighted <- stats::weighted.mean(not_working_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  percent_not_working_weighted <- not_working_weighted / num_adults
  new_features[i, 19] <- percent_not_working_weighted
  
  # Number with commute 30mins or greater
  commuter_30_list <- (acs_2017_wide$estimate.B08303_008[a] +
                         acs_2017_wide$estimate.B08303_009[a] +
                         acs_2017_wide$estimate.B08303_010[a] +
                         acs_2017_wide$estimate.B08303_011[a] +
                         acs_2017_wide$estimate.B08303_012[a] +
                         acs_2017_wide$estimate.B08303_013[a])
  commuter_30_weighted <- stats::weighted.mean(commuter_30_list,
                                               as.vector(Gaussian_template),
                                               na.rm = TRUE)
  percent_commuter_30_weighted <- commuter_30_weighted / num_adults
  new_features[i, 20] <- percent_commuter_30_weighted
  
  # Number with odd-hour commute
  odd_commuter_list <- acs_2017_wide$estimate.B08011_002[a]
  odd_commuter_weighted <- stats::weighted.mean(odd_commuter_list,
                                                as.vector(Gaussian_template),
                                                na.rm = TRUE)
  percent_odd_commuter_weighted <- odd_commuter_weighted / num_adults
  new_features[i, 21] <- percent_odd_commuter_weighted
}








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


# Match population count to the land area data.frame
land_area$INCOME <- income_data$estimate[match(toupper(do.call(paste, land_area_county_state_year)),
                                               paste(toupper(sub("\\s+\\w+,", "",
                                                                 income_data$NAME)),
                                                     income_data$YEAR))]


# Make land area numeric
land_area$LND010200D <- as.numeric(land_area$LND010200D)


# Get mobile home density
land_area$MOB_HOM_DENS <- land_area$MOB_HOM_COUNT / land_area$LND010200D


# Get population density
land_area$POP_DENS <- land_area$POP_COUNT / land_area$LND010200D


# Get the county, state, and year for matching
grid_county_state_year <- dplyr::select(grid_with_LC,
                                       c(county_name,
                                         state_abbrev,
                                         YEAR))

# Format land_area$LOC
land_area$LOC <- gsub("\\s+", " ", land_area$LOC) %>%
  substring(first = 2)


# Match ACS data to grid points
grid_with_LC$MOB_HOME_DENS <- land_area$MOB_HOM_DENS[match(toupper(do.call(paste, grid_county_state_year)),
                                                     paste(toupper(sub(",", "",
                                                                       land_area$LOC)),
                                                           land_area$YEAR))]

grid_with_LC$POP_DENS <- land_area$POP_DENS[match(toupper(do.call(paste, grid_county_state_year)),
                                            paste(toupper(sub(",", "",
                                                              land_area$LOC)),
                                                  land_area$YEAR))]

grid_with_LC$INCOME <- land_area$INCOME[match(toupper(do.call(paste, grid_county_state_year)),
                                              paste(toupper(sub(",", "",
                                                                land_area$LOC)),
                                                    land_area$YEAR))]


# Some counties don't appear in all data sources, therefore making some Inf values
# Getting rid of the NAs and Infs
grid_with_LC <- dplyr::filter(grid_with_LC,
                              POP_DENS != Inf) %>%
  na.omit()


# Import the unprocessed model data
unprocessed_tor_df <- read.csv('data/raw/tor_data_with_derived.csv')


# For non-beforehand variables, assume the mean
grid_with_LC$assumed_duration <- rep(mean(unprocessed_tor_df$DURATION_SECONDS),
                                     nrow(grid_with_LC))

grid_with_LC$assumed_tor_length <- rep(mean(unprocessed_tor_df$TOR_LENGTH),
                                       nrow(grid_with_LC))

grid_with_LC$assumed_tor_width <- rep(mean(unprocessed_tor_df$TOR_WIDTH),
                                      nrow(grid_with_LC))

grid_with_LC$time <- rep(mean(unprocessed_tor_df$BEGIN_TIME),
                         nrow(grid_with_LC)) %>%
  floor()

grid_with_LC$mult_vort_ind <- rep(mean(unprocessed_tor_df$MULTI_VORT_IND),
                                  nrow(grid_with_LC))


# Assigning state rank the same way it was done the first time
# Before these were different data sets, no longer the case
# Refuse to rename throughout
tor_df <- unprocessed_tor_df
DamPerState <- aggregate(tor_df$DAMAGE_PROPERTY,
                         by = list(Category = tor_df$STATE),
                         FUN = sum)

# Order them
cum_dam_order <- sort(DamPerState$x, decreasing = TRUE)

# Make the ordered damage a dataframe
cum_dam_rank <- as.data.frame(cum_dam_order)

# Create a rank dataframe
rank <- as.data.frame(c(1:nrow(cum_dam_rank)))

# Assign rank
cum_dam_rank <- cbind(rank, cum_dam_rank)

# Make them have same column name
DamPerState$cum_dam_order <- DamPerState$x

# Get rank matched with state name
dam_per_state_rank <- merge(x = DamPerState,
                            y = cum_dam_rank,
                            by = "cum_dam_order")

# Name state and rank correctly
dam_per_state_rank$STATE <- dam_per_state_rank$Category

dam_per_state_rank$STATE_RANK <- dam_per_state_rank$`c(1:nrow(cum_dam_rank))`

# Get only state name and rank
dam_per_state_rank <- dplyr::select(dam_per_state_rank,
                                    c(STATE, STATE_RANK))

colnames(grid_with_LC)[colnames(grid_with_LC) == 'state'] <- 'STATE'

# Merge this back to the original dataframe
grid_with_LC <- merge(x = grid_with_LC,
                      y = dam_per_state_rank,
                      by = "STATE")


# Produce tornado area
grid_with_LC$assumed_area <- grid_with_LC$assumed_tor_length * grid_with_LC$assumed_tor_width


# Produce total developed intensity
grid_with_LC$TOT_DEV_INT <- grid_with_LC$DEV_OPEN_PROP * 0.10 +
  grid_with_LC$DEV_LOW_PROP * 0.35 +
  grid_with_LC$DEV_MED_PROP * 0.65 +
  grid_with_LC$DEV_HIGH_PROP * 0.90


# Produce total wooded prop
grid_with_LC$TOT_WOOD_AREA <- grid_with_LC$WOOD_WETLAND_PROP +
  grid_with_LC$DECID_FOREST_PROP +
  grid_with_LC$EVERGR_FOREST_PROP +
  grid_with_LC$MIXED_FOREST_PROP


# Produce total wood-dev interaction
grid_with_LC$WOOD_DEV_INT <- grid_with_LC$TOT_DEV_INT * grid_with_LC$TOT_WOOD_AREA


# Produce an approximation of wealth contained in the the tornado area
grid_with_LC$EXP_INC_AREA <- grid_with_LC$INCOME * grid_with_LC$assumed_area


# For the predictions, we will do the 15th of each month
julian_days <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)


# Have this temp_df for for-loop purposes
temp_df <- grid_with_LC


# Copy all the grid data for all 12 of the '<month> the 15th'
for (i in 1:length(julian_days)) {
  
  if (i == 1) {
    
    grid_with_LC$julian_day <- rep(julian_days[i], nrow(grid_with_LC))
    
  } else {
    
    temp_df$julian_day <- rep(julian_days[i], nrow(temp_df))
    grid_with_LC <- rbind(grid_with_LC, temp_df)
    
  }
  
}


# Remove the temporary data.frame
rm(temp_df)


# Match grid values to the reference basis spline data set
# Import the reference data sets
time_ref_df <- read.csv('data/raw/time_splines_ref.csv')
julian_ref_df <- read.csv('data/raw/julian_splines_ref.csv')


# Merge the time-based basis splines
grid_with_LC <- base::merge(x = grid_with_LC,
                            y = time_ref_df,
                            by.x = 'time',
                            by.y = 'BEGIN_TIME')


# Merge julian-based basis splines
grid_with_LC <- base::merge(x = grid_with_LC,
                            y = julian_ref_df,
                            by.x = 'julian_day',
                            by.y = 'JULIAN_DAY')


# Keep names the same as the original model data
colnames(grid_with_LC)[colnames(grid_with_LC) == 'LON'] <- 'BEGIN_LON'
colnames(grid_with_LC)[colnames(grid_with_LC) == 'LAT'] <- 'BEGIN_LAT'
colnames(grid_with_LC)[colnames(grid_with_LC) == 'assumed_duration'] <- 'DURATION_SECONDS'
colnames(grid_with_LC)[colnames(grid_with_LC) == 'assumed_tor_length'] <- 'TOR_LENGTH'
colnames(grid_with_LC)[colnames(grid_with_LC) == 'assumed_tor_width'] <- 'TOR_WIDTH'
colnames(grid_with_LC)[colnames(grid_with_LC) == 'mult_vort_ind'] <- 'MULTI_VORT_IND'
colnames(grid_with_LC)[colnames(grid_with_LC) == 'assumed_area'] <- 'TOR_AREA'
colnames(grid_with_LC)[colnames(grid_with_LC) == 'time'] <- 'BEGIN_TIME'
colnames(grid_with_LC)[colnames(grid_with_LC) == 'julian_day'] <- 'JULIAN_DAY'


# Importing unprocessed model data to do the processing
# With the same means and standard deviations
tor_df <- read.csv("data/raw/tor_data_with_derived.csv")


# Define a simple mean normalization function
mean_normalize <- function(col_name){
  
  # descr:  simple mean normalization... (x - mean(x))/sd(x)
  # arg:    thing to normalize
  # return: that thing normalized
  
  numerator <- grid_with_LC[, col_name] - mean(tor_df[, col_name])
  
  normalized <- numerator / sd(tor_df[, col_name])
  
  return(normalized)
  
}


# Define a mean normalization following a log-transformation
mean_norm_log_xform <- function(col_name) {
  
  # descr:  log transform (base e) then mean normalize
  # arg:    thing to process
  # return: that thing processed
  
  log_xformed <- log(grid_with_LC[, col_name] + 1,
                     base = exp(1))
  
  numerator <- log_xformed - mean(log(tor_df[, col_name] + 1,
                                      base = exp(1)))
  
  log_and_normalized <- numerator / sd(log(tor_df[, col_name] + 1,
                                           base = exp(1)))
  
  return(log_and_normalized)
  
}


# Define a mean normalization following a log-transformation
# following a multiplication
mean_norm_log_xform_prop <- function(col_name) {
  
  # descr:  multiple by 10000, then log transform (base e), then mean normalize
  #         this is for proportions, the 10000 multiplications makes the log
  #         transformation more effective
  # arg:    thing to process
  # return: that thing processed
  
  processed_10000 <- grid_with_LC[, col_name] * 10000
  
  log_xformed <- log(processed_10000 + 1,
                     base = exp(1))
  
  numerator <- log_xformed - mean(log((10000 * tor_df[, col_name]) + 1,
                                      base = exp(1)))
  
  log_and_normalized_10000 <- numerator / sd(log((10000 * tor_df[, col_name]) + 1,
                                                 base = exp(1)))
  
  return(log_and_normalized_10000)
  
}



# Process the variables
grid_with_LC$DURATION_SECONDS <- mean_norm_log_xform('DURATION_SECONDS')

grid_with_LC$BEGIN_LAT <- mean_normalize('BEGIN_LAT')

grid_with_LC$BEGIN_LON <- mean_normalize('BEGIN_LON')

grid_with_LC$TOR_LENGTH <- mean_norm_log_xform('TOR_LENGTH')

grid_with_LC$TOR_WIDTH <- mean_norm_log_xform('TOR_WIDTH')

grid_with_LC$OPEN_WATER_PROP <- mean_norm_log_xform_prop('OPEN_WATER_PROP')

grid_with_LC$DEV_OPEN_PROP <- mean_norm_log_xform_prop('DEV_OPEN_PROP')

grid_with_LC$DEV_LOW_PROP <- mean_norm_log_xform_prop('DEV_LOW_PROP')

grid_with_LC$DEV_MED_PROP <- mean_norm_log_xform_prop('DEV_MED_PROP')

grid_with_LC$DEV_HIGH_PROP <- mean_norm_log_xform_prop('DEV_HIGH_PROP')

grid_with_LC$DECID_FOREST_PROP <- mean_norm_log_xform_prop('DECID_FOREST_PROP')

grid_with_LC$EVERGR_FOREST_PROP <- mean_norm_log_xform_prop('EVERGR_FOREST_PROP')

grid_with_LC$MIXED_FOREST_PROP <- mean_norm_log_xform_prop('MIXED_FOREST_PROP')

grid_with_LC$SHRUB_SCRUB_PROP <- mean_norm_log_xform_prop('SHRUB_SCRUB_PROP')

grid_with_LC$GRASS_LAND_PROP <- mean_norm_log_xform_prop('GRASS_LAND_PROP')

grid_with_LC$PASTURE_HAY_PROP <- mean_norm_log_xform_prop('PASTURE_HAY_PROP')

grid_with_LC$CULT_CROPS_PROP <- mean_norm_log_xform_prop('CULT_CROPS_PROP')

grid_with_LC$WOOD_WETLAND_PROP <- mean_norm_log_xform_prop('WOOD_WETLAND_PROP')

grid_with_LC$HERB_WETLAND_PROP <- mean_norm_log_xform_prop('HERB_WETLAND_PROP')

grid_with_LC$BARREN_LAND_PROP <- mean_norm_log_xform_prop('BARREN_LAND_PROP')

grid_with_LC$INCOME <- mean_norm_log_xform('INCOME')

grid_with_LC$TOR_AREA <- mean_norm_log_xform_prop('TOR_AREA')

grid_with_LC$TOT_DEV_INT <- mean_norm_log_xform_prop('TOT_DEV_INT')

grid_with_LC$TOT_WOOD_AREA <- mean_norm_log_xform_prop('TOT_WOOD_AREA')

grid_with_LC$WOOD_DEV_INT <- mean_norm_log_xform_prop('WOOD_DEV_INT')

grid_with_LC$EXP_INC_AREA <- mean_norm_log_xform('EXP_INC_AREA')

grid_with_LC$STATE_RANK <- mean_norm_log_xform_prop('STATE_RANK')

grid_with_LC$YEAR <- mean_normalize('YEAR')

grid_with_LC$MOB_HOME_DENS <- mean_norm_log_xform_prop('MOB_HOME_DENS')

grid_with_LC$POP_DENS <- mean_norm_log_xform('POP_DENS')


# Reshape the gridded data.frame so that we can see all
# The distributions easily
grid_hist_data <- melt(grid_with_LC)

grid_hist_data <- dplyr::select(grid_hist_data,
                                -c(STATE,
                                   county_name,
                                   state_abbrev))

grid_hist_data <- dplyr::filter(grid_hist_data,
                                variable != "ID")
grid_hist_data <- dplyr::filter(grid_hist_data,
                                variable != "time")
grid_hist_data <- dplyr::filter(grid_hist_data,
                                variable != "julian_day")


# Plot the distributions
ggplot(grid_hist_data,
       aes(x = value)) +
  geom_histogram(bins = 100,
                 fill = "dark red") +
  facet_wrap(~variable) +
  theme_bw()


# Import the training data to get the same column order
train_data <- read.csv("data/raw/tor_train_set.csv")


# They need to have matching column names, so adding an all-missing
# Property Damage column to the grid data
grid_with_LC$DAMAGE_PROPERTY <- rep(NA, nrow(grid_with_LC))
final_grid_df <- dplyr::select(grid_with_LC,
                               colnames(train_data))


# Save the gridded data.frame so we can predict off it in PyTorch
write_csv(final_grid_df,
          'data/raw/final_grid_df.csv')


