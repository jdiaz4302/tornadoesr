


# Packages
library(sp)
library(raster)
library(dplyr)
library(readr)
library(maps)
library(maptools)
library(reshape2)
library(tidycensus)
library(ggplot2)


# Import the cities data
cities_df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')


# Only use cities with a population greater than 100,000
cities_df <- dplyr::filter(cities_df,
                           pop > 100000)


# Storing this for later remerge
stored_df <- dplyr::select(cities_df,
                           c(name, pop))


# Assign unique identifier to each row
cities_df$name <- 1:nrow(cities_df)


# Now we only need coordinates
cities_df <- dplyr::select(cities_df,
                           c(lon, lat, name))


# Get those points as a Spatial object
cities_points <- SpatialPointsDataFrame(coords = cities_df[, 1:2],
                                        data = as.data.frame(cities_df$name),
                                        proj4string = CRS('+init=epsg:4326'))


# Read the census-provided United States shapefile
# Convert its crs
# And crop it to only the continental US
us_shape <- shapefile('data/raw/cb_2015_us_nation_5m.shp') %>%
  spTransform(CRSobj = crs(cities_points)) %>%
  crop(extent(-125, -66, 23, 50))


# Only points within the continental US
cities_points <- cities_points[us_shape, ]


# Import the NLCD raster
NLCD_2011 <- raster("data/raw/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img")


# Convert the  cities's crs to that of the NLCD
cities_NLCD_crs <- spTransform(cities_points,
                               crs(NLCD_2011))

# Get the extraction buffer
source("Complete_Workflow/03_clean_StormEvents_files.R")
avg_tor_length_meters <- mean(tor_df$TOR_LENGTH) * 1609.34


# Extract the LC values for each pixel
Sys.time()
cities_LC_values <- raster::extract(NLCD_2011,
                                    cities_NLCD_crs,
                                    buffer = avg_tor_length_meters)
Sys.time()


# Get the proportion of LC values in the buffer area
# Of each lat/lon combo
LC_prop_cities <- lapply(cities_LC_values, function(x) {
  
  prop.table(table(x))
  
  })

Sys.time()


# Make it into a data.frame
LC_cities_df <- data.frame(ID = rep(cities_points$`cities_df$name`,
                                    lapply(LC_prop_cities,
                                           length)),
                           cover = names(unlist(LC_prop_cities)),
                           percent = unlist(LC_prop_cities))

colnames(LC_cities_df)[1] <- 'name'


# Get rid of this error classification
LC_cities_df <- dplyr::filter(LC_cities_df,
                              cover != 0)

# Get rid of the snow/ice classification
LC_cities_df <- dplyr::filter(LC_cities_df,
                              cover != 12)


# Reshape the data.frame
LC_cities_df <- reshape(LC_cities_df,
                        idvar = "name",
                        timevar = "cover",
                        direction = "wide")


# All NA's should be 0's because it's 'missing value'
# For a land cover classification; ie, none of that classification
LC_cities_df[is.na(LC_cities_df)] <- 0


# Merge the LC proportions to their lat/lon combos
cities_with_LC <- merge(x = cities_df,
                        y = LC_cities_df,
                        by = 'name')


# Give the LC values a name
colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.11"] <- "OPEN_WATER_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.21"] <- "DEV_OPEN_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.22"] <- "DEV_LOW_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.23"] <- "DEV_MED_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.24"] <- "DEV_HIGH_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.41"] <- "DECID_FOREST_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.42"] <- "EVERGR_FOREST_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.43"] <- "MIXED_FOREST_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.52"] <- "SHRUB_SCRUB_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.71"] <- "GRASS_LAND_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.81"] <- "PASTURE_HAY_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.82"] <- "CULT_CROPS_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.90"] <- "WOOD_WETLAND_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.95"] <- "HERB_WETLAND_PROP"

colnames(cities_with_LC)[colnames(cities_with_LC) == "percent.31"] <- "BARREN_LAND_PROP"


# Import the 2015 census-provided US shapefile for counties
# Get it in the right CRS
# And crop it to only the continental US
us_counties <- shapefile('data/raw/cb_2015_us_county_5m.shp') %>%
  spTransform(CRSobj = crs(cities_points)) %>%
  crop(extent(-125, -66, 23, 50))


# Remove cities points not within the continental US
# And make county names capitalized
cities_with_LC$county_name <- over(cities_points, us_counties)$NAME %>%
  toupper()


# Define a function to name the state that a coordinate belongs to
# Provided via stackoverflow
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill = TRUE,
                col = "transparent",
                plot = FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states,
                                   IDs = IDs,
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


# cities the state of the cities points
# And capitalize it
cities_with_LC$state <- latlong2state(cities_with_LC[2:3]) %>%
  toupper()


# Remove NA values
cities_with_LC <- na.omit(cities_with_LC)


# Assign the year 2018 to all rows
cities_with_LC$YEAR <- rep(2018, nrow(cities_with_LC))


# Change the state names to abbreviations
cities_with_LC$state_abbrev <- state.abb[match(cities_with_LC$state,
                                               toupper(state.name))]


# Get the county, state, and year of each cities point
cities_county_state_year <- dplyr::select(cities_with_LC,
                                          c(county_name, state_abbrev, YEAR))


# Get location and year of each cities point as one 'column'
cities_county_state_year_together <- do.call(paste, cities_county_state_year)


# Import the census-provided county area data.frame
land_area <- read.csv("data/raw/LND01.csv") %>%
  select(c(ï..Areaname, LND010200D))


################################################
########## PROCESSSING LAND AREA DATA ##########
################################################
# I'm only interested in county entries, which always contain a comma
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
year_2018 <- rep(2018, nrow(land_area))


# Give land_area the year 2018
land_area$YEAR <- year_2018


# Get the county and state
land_area_county_state_year <- select(land_area,
                                      c(county,
                                        state_name,
                                        YEAR))
################################################
###### END OF PROCESSSING LAND AREA DATA #######
################################################


# Set census API
census_api_key("ENTER YOUR CENSUS API KEY")


# Writing a function to get ACS data of interest
get_ACS_mob_hom_and_pop <- function(end_year) {
  
  # description: get mobile home count and population of each county from ACS
  # argument: end-year of the ACS
  # return: a dataframe containing mobile home count and population of each county
  
  acs_df <- get_acs(geography = "county",
                    year = end_year,
                    variables = c("B25024_010E",   # Total mobile homes
                                  "B01003_001E",   # Total population
                                  "B19013_001E"))  # Median household income
  
  acs_df$YEAR <- rep(end_year,
                     nrow(acs_df))
  
  return(acs_df)
  
}


# Get ACS data
acs_2015 <- get_ACS_mob_hom_and_pop(2015)
acs_df <- acs_2015

acs_df$YEAR <- rep(2018, nrow(acs_df))
rm(acs_2015)

acs_df <- dplyr::select(acs_df,
                        -c(GEOID, moe))


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
cities_county_state_year <- dplyr::select(cities_with_LC,
                                          c(county_name,
                                            state_abbrev,
                                            YEAR))

# Format land_area$LOC
land_area$LOC <- gsub("\\s+", " ", land_area$LOC) %>%
  substring(first = 2)


# Match ACS data to cities points
cities_with_LC$MOB_HOME_DENS <- land_area$MOB_HOM_DENS[match(toupper(do.call(paste, cities_county_state_year)),
                                                     paste(toupper(sub(",", "",
                                                                       land_area$LOC)),
                                                           land_area$YEAR))]

cities_with_LC$POP_DENS <- land_area$POP_DENS[match(toupper(do.call(paste, cities_county_state_year)),
                                            paste(toupper(sub(",", "",
                                                              land_area$LOC)),
                                                  land_area$YEAR))]

cities_with_LC$INCOME <- land_area$INCOME[match(toupper(do.call(paste, cities_county_state_year)),
                                                paste(toupper(sub(",", "",
                                                                  land_area$LOC)),
                                                      land_area$YEAR))]

# Some counties don't appear in all data sources, therefore making some Inf values
# Getting rid of the NAs and Infs
cities_with_LC <- dplyr::filter(cities_with_LC,
                                POP_DENS != Inf) %>%
  na.omit()


# Import the unprocessed model data
unprocessed_tor_df <- read.csv('data/raw/tor_data_with_derived.csv')


# For non-beforehand variables, assume the mean
cities_with_LC$assumed_duration <- rep(mean(unprocessed_tor_df$DURATION_SECONDS),
                                       nrow(cities_with_LC))

cities_with_LC$assumed_tor_length <- rep(mean(unprocessed_tor_df$TOR_LENGTH),
                                         nrow(cities_with_LC))

cities_with_LC$assumed_tor_width <- rep(mean(unprocessed_tor_df$TOR_WIDTH),
                                        nrow(cities_with_LC))

cities_with_LC$time <- rep(mean(unprocessed_tor_df$BEGIN_TIME),
                           nrow(cities_with_LC)) %>%
  floor()

cities_with_LC$mult_vort_ind <- rep(mean(unprocessed_tor_df$MULT),
                                    nrow(cities_with_LC))


# Assigning state rank the same way it was done the first time
tor_df <- read.csv("data/raw/tor_data_with_ACS.csv")
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

colnames(cities_with_LC)[colnames(cities_with_LC) == 'state'] <- 'STATE'

# Merge this back to the original dataframe
cities_with_LC <- merge(x = cities_with_LC,
                        y = dam_per_state_rank,
                        by = "STATE")


# Produce tornado area
cities_with_LC$assumed_area <- cities_with_LC$assumed_tor_length * cities_with_LC$assumed_tor_width


# Produce total developed intensity
cities_with_LC$TOT_DEV_INT <- cities_with_LC$DEV_OPEN_PROP * 0.10 +
  cities_with_LC$DEV_LOW_PROP * 0.35 +
  cities_with_LC$DEV_MED_PROP * 0.65 +
  cities_with_LC$DEV_HIGH_PROP * 0.90


# Produce total wooded prop
cities_with_LC$TOT_WOOD_AREA <- cities_with_LC$WOOD_WETLAND_PROP +
  cities_with_LC$DECID_FOREST_PROP +
  cities_with_LC$EVERGR_FOREST_PROP +
  cities_with_LC$MIXED_FOREST_PROP


# Produce total wood-dev interaction
cities_with_LC$WOOD_DEV_INT <- cities_with_LC$TOT_DEV_INT * cities_with_LC$TOT_WOOD_AREA


# Produce an approximation of wealth contained in the the tornado area
cities_with_LC$EXP_INC_AREA <- cities_with_LC$INCOME * cities_with_LC$assumed_area


# For the predictions, we will do the 15th of each month
julian_days <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)


# Have this temp_df for for-loop purposes
temp_df <- cities_with_LC


# Copy all the grid data for all 12 of the '<month> the 15th'
for (i in 1:length(julian_days)) {
  
  if (i == 1) {
    
    cities_with_LC$julian_day <- rep(julian_days[i], nrow(cities_with_LC))
    
  } else {
    
    temp_df$julian_day <- rep(julian_days[i], nrow(temp_df))
    cities_with_LC <- rbind(cities_with_LC, temp_df)
    
  }
  
}


# Remove the temporary data.frame
rm(temp_df)


# Match grid values to the reference basis spline data set
# Import the reference data sets
time_ref_df <- read.csv('data/raw/time_splines_ref.csv')
julian_ref_df <- read.csv('data/raw/julian_splines_ref.csv')


# Merge the time-based basis splines
cities_with_LC <- base::merge(x = cities_with_LC,
                              y = time_ref_df,
                              by.x = 'time',
                              by.y = 'BEGIN_TIME')


# Merge julian-based basis splines
cities_with_LC <- base::merge(x = cities_with_LC,
                              y = julian_ref_df,
                              by.x = 'julian_day',
                              by.y = 'JULIAN_DAY')


# Keep names the same as the original model data
colnames(cities_with_LC)[colnames(cities_with_LC) == 'lon'] <- 'BEGIN_LON'
colnames(cities_with_LC)[colnames(cities_with_LC) == 'lat'] <- 'BEGIN_LAT'
colnames(cities_with_LC)[colnames(cities_with_LC) == 'assumed_duration'] <- 'DURATION_SECONDS'
colnames(cities_with_LC)[colnames(cities_with_LC) == 'assumed_tor_length'] <- 'TOR_LENGTH'
colnames(cities_with_LC)[colnames(cities_with_LC) == 'assumed_tor_width'] <- 'TOR_WIDTH'
colnames(cities_with_LC)[colnames(cities_with_LC) == 'mult_vort_ind'] <- 'MULTI_VORT_IND'
colnames(cities_with_LC)[colnames(cities_with_LC) == 'assumed_area'] <- 'TOR_AREA'
colnames(cities_with_LC)[colnames(cities_with_LC) == 'time'] <- 'BEGIN_TIME'
colnames(cities_with_LC)[colnames(cities_with_LC) == 'julian_day'] <- 'JULIAN_DAY'


# Importing unprocessed model data to do the processing
# With the same means and standard deviations
tor_df <- read.csv("data/raw/tor_data_with_derived.csv")


# Define a simple mean normalization function
mean_normalize <- function(col_name){
  
  # descr:  simple mean normalization... (x - mean(x))/sd(x)
  # arg:    thing to normalize
  # return: that thing normalized
  
  numerator <- cities_with_LC[, col_name] - mean(tor_df[, col_name])
  
  normalized <- numerator / sd(tor_df[, col_name])
  
  return(normalized)
  
}


# Define a mean normalization following a log-transformation
mean_norm_log_xform <- function(col_name) {
  
  # descr:  log transform (base e) then mean normalize
  # arg:    thing to process
  # return: that thing processed
  
  log_xformed <- log(cities_with_LC[, col_name] + 1,
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
  
  processed_10000 <- cities_with_LC[, col_name] * 10000
  
  log_xformed <- log(processed_10000 + 1,
                     base = exp(1))
  
  numerator <- log_xformed - mean(log((10000 * tor_df[, col_name]) + 1,
                                      base = exp(1)))
  
  log_and_normalized_10000 <- numerator / sd(log((10000 * tor_df[, col_name]) + 1,
                                                 base = exp(1)))
  
  return(log_and_normalized_10000)
  
}



# Process the variables
cities_with_LC$DURATION_SECONDS <- mean_norm_log_xform('DURATION_SECONDS')

cities_with_LC$BEGIN_LAT <- mean_normalize('BEGIN_LAT')

cities_with_LC$BEGIN_LON <- mean_normalize('BEGIN_LON')

cities_with_LC$TOR_LENGTH <- mean_norm_log_xform('TOR_LENGTH')

cities_with_LC$TOR_WIDTH <- mean_norm_log_xform('TOR_WIDTH')

cities_with_LC$OPEN_WATER_PROP <- mean_norm_log_xform_prop('OPEN_WATER_PROP')

cities_with_LC$DEV_OPEN_PROP <- mean_norm_log_xform_prop('DEV_OPEN_PROP')

cities_with_LC$DEV_LOW_PROP <- mean_norm_log_xform_prop('DEV_LOW_PROP')

cities_with_LC$DEV_MED_PROP <- mean_norm_log_xform_prop('DEV_MED_PROP')

cities_with_LC$DEV_HIGH_PROP <- mean_norm_log_xform_prop('DEV_HIGH_PROP')

cities_with_LC$DECID_FOREST_PROP <- mean_norm_log_xform_prop('DECID_FOREST_PROP')

cities_with_LC$EVERGR_FOREST_PROP <- mean_norm_log_xform_prop('EVERGR_FOREST_PROP')

cities_with_LC$MIXED_FOREST_PROP <- mean_norm_log_xform_prop('MIXED_FOREST_PROP')

cities_with_LC$SHRUB_SCRUB_PROP <- mean_norm_log_xform_prop('SHRUB_SCRUB_PROP')

cities_with_LC$GRASS_LAND_PROP <- mean_norm_log_xform_prop('GRASS_LAND_PROP')

cities_with_LC$PASTURE_HAY_PROP <- mean_norm_log_xform_prop('PASTURE_HAY_PROP')

cities_with_LC$CULT_CROPS_PROP <- mean_norm_log_xform_prop('CULT_CROPS_PROP')

cities_with_LC$WOOD_WETLAND_PROP <- mean_norm_log_xform_prop('WOOD_WETLAND_PROP')

cities_with_LC$HERB_WETLAND_PROP <- mean_norm_log_xform_prop('HERB_WETLAND_PROP')

cities_with_LC$BARREN_LAND_PROP <- mean_norm_log_xform_prop('BARREN_LAND_PROP')

cities_with_LC$INCOME <- mean_norm_log_xform('INCOME')

cities_with_LC$TOR_AREA <- mean_norm_log_xform_prop('TOR_AREA')

cities_with_LC$TOT_DEV_INT <- mean_norm_log_xform_prop('TOT_DEV_INT')

cities_with_LC$TOT_WOOD_AREA <- mean_norm_log_xform_prop('TOT_WOOD_AREA')

cities_with_LC$WOOD_DEV_INT <- mean_norm_log_xform_prop('WOOD_DEV_INT')

cities_with_LC$EXP_INC_AREA <- mean_norm_log_xform('EXP_INC_AREA')

cities_with_LC$STATE_RANK <- mean_norm_log_xform_prop('STATE_RANK')

cities_with_LC$YEAR <- mean_normalize('YEAR')

cities_with_LC$MOB_HOME_DENS <- mean_norm_log_xform_prop('MOB_HOME_DENS')

cities_with_LC$POP_DENS <- mean_norm_log_xform('POP_DENS')


# Reshape the gridded data.frame so that we can see all
# The distributions easily
cities_hist_data <- melt(cities_with_LC)

cities_hist_data <- dplyr::select(cities_hist_data,
                                -c(STATE,
                                   county_name,
                                   state_abbrev))

cities_hist_data <- dplyr::filter(cities_hist_data,
                                variable != "ID")
cities_hist_data <- dplyr::filter(cities_hist_data,
                                variable != "BEGIN_TIME")
cities_hist_data <- dplyr::filter(cities_hist_data,
                                variable != "JULIAN_DAY")
cities_hist_data <- dplyr::filter(cities_hist_data,
                                variable != "name")



# Plot the distributions
ggplot(cities_hist_data,
       aes(x = value)) +
  geom_histogram(bins = 100,
                 fill = "dark red") +
  facet_wrap(~variable) +
  theme_bw()


# Import the training data to get the same column order
train_data <- read.csv("data/raw/tor_train_set.csv")


# They need to have matching column names, so adding an all-missing
# Property Damage column to the grid data
cities_with_LC$DAMAGE_PROPERTY <- rep(NA, nrow(cities_with_LC))

# Getting city names reintegrated
colnames(stored_df)[colnames(stored_df) == 'name'] <- 'city_name'
stored_df$name <- 1:nrow(stored_df)

cities_with_LC <- merge(x = cities_with_LC,
                        y = stored_df,
                        by = 'name')

cities_with_LC <- dplyr::select(cities_with_LC,
                                -name)

colnames(cities_with_LC)[colnames(cities_with_LC) == 'city_name'] <- 'name'

# Have city name, then the same order of features as the training data
final_cities_df <- dplyr::select(cities_with_LC,
                                 c('name',
                                   'pop',
                                   colnames(train_data)))


# Save the city data.frame so we can predict off it in PyTorch
write_csv(final_cities_df,
          'data/raw/final_cities_df.csv')


