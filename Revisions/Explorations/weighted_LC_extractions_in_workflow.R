


# Get packages
library(readr)
library(dplyr)
library(raster)


# Import tornado data
source("Revisions/Complete_Workflow/03_clean_StormEvents_files.R")


# Storing the standard deviation of tornado path length
stand_dev_of_tor_length <- sd(tor_df$TOR_LENGTH) * 1609.34


# Separate it for most accurate NLCD extractions
tor_df_2001 <- dplyr::filter(tor_df,
                             YEAR < 2006)
# Just working with 1 tornado for now
tor_df_2001 <- tor_df_2001[1, ]


# Importing NLCD data
NLCD_2001 <- raster("data/raw/nlcd_2001_landcover_2011_edition_2014_10_10/nlcd_2001_landcover_2011_edition_2014_10_10.img")


# Creating a Gaussian template with NLCD CRS and tornado path length sd
Gaussian_template <- focalWeight(NLCD_2001,
                                 d = stand_dev_of_tor_length,
                                 type = 'Gauss')

# Get the coordinates
tor_coord_2001 <- dplyr::select(tor_df_2001,
                                c(BEGIN_LON,
                                  BEGIN_LAT))


# Make them into a SpatialPointsDataFrame
tor_Spatial_2001 <- SpatialPointsDataFrame(tor_coord_2001,
                                           tor_df_2001,
                                           proj4string = CRS("+init=epsg:4326"))


# Make StormEvents have the same CRS as NLCD
tor_Spatial_2001 <- spTransform(tor_Spatial_2001,
                                crs(NLCD_2001))


# Creating a Gaussian template w.r.t.
# tornado path length distribution and NLCD CRS
Gaussian_template <- focalWeight(NLCD_2001,
                                 d = 30,               # Fix this, prototyping
                                 type = 'Gauss')


# Manually georeferencing the template
# Creating grid of lats and lon
coords_1 <- seq(tor_Spatial_2001@coords[1] - dim(Gaussian_template)[1] / 2 * 30,
                tor_Spatial_2001@coords[1] + dim(Gaussian_template)[1] / 2 * 30,
                by = 30)
coords_2 <- seq(tor_Spatial_2001@coords[2] - dim(Gaussian_template)[2] / 2 * 30,
                tor_Spatial_2001@coords[2] + dim(Gaussian_template)[2] / 2 * 30,
                by = 30)
# Making them into data.frame columns
lon <- as.vector(t(replicate(dim(Gaussian_template)[2], coords_1)))
lat <- as.vector(replicate(dim(Gaussian_template)[1], coords_2))
coords_df <- data.frame(lon, lat)
# Formalizing the spatial object
Gaussian_template_for_tor <- SpatialPointsDataFrame(coords_df,
                                                    coords_df,
                                                    proj4string = crs(tor_Spatial_2001))


# Viewing the area under consideration by the Gaussian template
plot(crop(NLCD_2001, extent(Gaussian_template_for_tor)))


# Extract the all LC values within the Gaussian template extent
Gaussian_template_area_extraction <- raster::extract(NLCD_2001,
                                                     extent(Gaussian_template_for_tor))


# Using a nifty little packaged for calculating the weighted counts of LC
install.packages('questionr')
library(questionr)


# Calculate and view the weighted-extraction LC proportions
wtd.table(Gaussian_template_area_extraction, weights = Gaussian_template)
# They're already proportions!
sum(wtd.table(Gaussian_template_area_extraction, weights = Gaussian_template))

