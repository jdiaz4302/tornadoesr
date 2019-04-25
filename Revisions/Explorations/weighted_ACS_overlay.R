


# Packages
library(tidycensus)
library(raster)
library(sf)


# Census API key
census_api_key("REDACTED")


# Import the NLCD raster for its CRS so we don't have to recode much of this process
NLCD <- raster::raster(paste0('data/raw/nlcd_2001_landcover_2011_edition_2014_10_10/',
                              'nlcd_2001_landcover_2011_edition_2014_10_10.img'))


# Testing with the med. year homes were built variable
median_year_struct_built <- get_acs(geography = "county", year = 2010,
                                    variables = c("B25035_001"), geometry = TRUE)
# Getting it as the proper class
med_year_struct_built_geo <- as_Spatial(median_year_struct_built$geometry)


# Import the tornado data
tor_df <- read.csv('data/raw/tor_data_with_NLCD.csv')
# Prototyping on 1 year
tor_df_2010 <- dplyr::filter(tor_df,
                             YEAR == 2010)


# Getting the coordinates for spatial data formalization
tor_coord_2010 <- dplyr::select(tor_df_2010,
                                c(BEGIN_LON,
                                  BEGIN_LAT))
# Spatial data formalization
tor_Spatial_2010 <- SpatialPointsDataFrame(tor_coord_2010,
                                           tor_df_2010,
                                           proj4string = CRS("+init=epsg:4326"))
# Giving it the same CRS as NLCD
tor_Spatial_2010 <- spTransform(tor_Spatial_2010,
                                crs(NLCD))


# Getting the transformed coordinates for the template's manual georeferencing
coords_2010 <- tor_Spatial_2010@coords
coords_2010 <- coords_2010[1:(length(coords_2010) / 2), ]





# Storing the standard deviation of tornado path length
stand_dev_of_tor_length <- sd(tor_df$TOR_LENGTH) / 10 * 1609.34


# Creating a Gaussian template with NLCD CRS and tornado path length sd
Gaussian_template <- focalWeight(NLCD,
                                 d = stand_dev_of_tor_length,
                                 type = 'Gauss')


# Defining function for the weighted extraction
weighted_extract <- function(coords, ACS_data) {
  # Creating grid of lats and lon
  coords_1 <- seq(as.numeric(coords[1]) - dim(Gaussian_template)[1] / 2 * 30,
                  as.numeric(coords[1]) + dim(Gaussian_template)[1] / 2 * 30,
                  by = 30)
  coords_2 <- seq(as.numeric(coords[2]) - dim(Gaussian_template)[2] / 2 * 30,
                  as.numeric(coords[2]) + dim(Gaussian_template)[2] / 2 * 30,
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
  
  # Extract the all social values within the Gaussian template extent
  Gaussian_template_area_extraction <- over(Gaussian_template_for_tor,
                                            as_Spatial(ACS_data$geometry))
  return(Gaussian_template_area_extraction)
}


# Testing out the process - Let's make sure the counties are right and see how many counties we
# tend to get
for (i in 1:50) {
  testing_output <- weighted_extract(coords_2010[i, ], median_year_struct_built)
  print(length(unique(testing_output)))
  print('')
}


