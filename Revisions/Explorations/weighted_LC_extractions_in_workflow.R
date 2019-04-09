


# Get packages
library(readr)
library(dplyr)
library(raster)
install.packages('questionr')
library(questionr)


# Import tornado data
source("Revisions/Complete_Workflow/03_clean_StormEvents_files.R")


# Storing the standard deviation of tornado path length
stand_dev_of_tor_length <- sd(tor_df$TOR_LENGTH) * 1609.34


# Separate it for most accurate NLCD extractions
tor_df_2001 <- dplyr::filter(tor_df,
                             YEAR < 2006)

tor_df_2006 <- dplyr::filter(tor_df,
                             YEAR >= 2006 & YEAR < 2011)

tor_df_2011 <- dplyr::filter(tor_df,
                             YEAR >= 2011)


# Importing NLCD data
NLCD_2001 <- raster("data/raw/nlcd_2001_landcover_2011_edition_2014_10_10/nlcd_2001_landcover_2011_edition_2014_10_10.img")

NLCD_2006 <- raster("data/raw/nlcd_2006_landcover_2011_edition_2014_10_10/nlcd_2006_landcover_2011_edition_2014_10_10.img")

NLCD_2011 <- raster("data/raw/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img") 


# Creating a Gaussian template with NLCD CRS and tornado path length sd
Gaussian_template <- focalWeight(NLCD_2001,
                                 d = stand_dev_of_tor_length,
                                 type = 'Gauss')

# Get the coordinates
tor_coord_2001 <- dplyr::select(tor_df_2001,
                                c(BEGIN_LON,
                                  BEGIN_LAT))

tor_coord_2006 <- dplyr::select(tor_df_2006,
                                c(BEGIN_LON,
                                  BEGIN_LAT))

tor_coord_2011 <- dplyr::select(tor_df_2011,
                                c(BEGIN_LON,
                                  BEGIN_LAT))


# Make them into SpatialPointsDataFrame's
tor_Spatial_2001 <- SpatialPointsDataFrame(tor_coord_2001,
                                           tor_df_2001,
                                           proj4string = CRS("+init=epsg:4326"))

tor_Spatial_2006 <- SpatialPointsDataFrame(tor_coord_2006,
                                           tor_df_2006,
                                           proj4string = CRS("+init=epsg:4326"))

tor_Spatial_2011 <- SpatialPointsDataFrame(tor_coord_2011,
                                           tor_df_2011,
                                           proj4string = CRS("+init=epsg:4326"))


# Make StormEvents has the same CRS as NLCD
tor_Spatial_2001 <- spTransform(tor_Spatial_2001,
                                crs(NLCD_2001))

tor_Spatial_2006 <- spTransform(tor_Spatial_2006,
                                crs(NLCD_2006))

tor_Spatial_2011 <- spTransform(tor_Spatial_2011,
                                crs(NLCD_2011))


# We need the coordinates for the weighted extractions and Guassian template georef.
coords_2001 <- tor_Spatial_2001@coords
coords_2001 <- coords_2001[1:(length(coords_2001) / 2), ]

coords_2006 <- tor_Spatial_2006@coords
coords_2006 <- coords_2006[1:(length(coords_2006) / 2), ]

coords_2011 <- tor_Spatial_2011@coords
coords_2011 <- coords_2011[1:(length(coords_2011) / 2), ]


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
                                                      proj4string = crs(tor_Spatial_2001))
  
  # Extract the all LC values within the Gaussian template extent
  Gaussian_template_area_extraction <- raster::extract(NLCD_2001,
                                                       extent(Gaussian_template_for_tor))
  return(Gaussian_template_area_extraction)
}


# Performing the weighted extraction
wtd.table(weighted_extract(coords_2011[1, ]), weights = Gaussian_template)
sum(wtd.table(weighted_extract(coords_2011[1, ]), weights = Gaussian_template))


