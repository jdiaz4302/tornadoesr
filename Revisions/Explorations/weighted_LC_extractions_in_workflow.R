


# Get packages
library(readr)
library(dplyr)
library(raster)
library(tidyr)
library(plyr)
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
# Keeping the EVENT_IDs too
coords_2001 <- tor_Spatial_2001@coords
coords_2001 <- coords_2001[1:(length(coords_2001) / 2), ]
IDs_2001 <- tor_Spatial_2001@data$EVENT_ID

coords_2006 <- tor_Spatial_2006@coords
coords_2006 <- coords_2006[1:(length(coords_2006) / 2), ]
IDs_2006 <- tor_Spatial_2006@data$EVENT_ID

coords_2011 <- tor_Spatial_2011@coords
coords_2011 <- coords_2011[1:(length(coords_2011) / 2), ]
IDs_2011 <- tor_Spatial_2011@data$EVENT_ID


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


# Sitting up the data.frames for weighted extraction percentages
NLCD_2001_factor_levels <- levels(NLCD_2001)
NLCD_2001_factor_levels_filtered <- NLCD_2001_factor_levels[[1]][NLCD_2001_factor_levels[[1]]$COUNT != 0, ]$ID
NLCD_2001_weighted_LC_prop <- matrix(ncol = length(NLCD_2001_factor_levels_filtered) + 1,
                                     nrow = 0) %>%
  data.frame()
colnames(NLCD_2001_weighted_LC_prop) <- c(paste0('percent.', NLCD_2001_factor_levels_filtered),
                                          'EVENT_ID')

NLCD_2006_factor_levels <- levels(NLCD_2006)
NLCD_2006_factor_levels_filtered <- NLCD_2006_factor_levels[[1]][NLCD_2006_factor_levels[[1]]$COUNT != 0, ]$ID
NLCD_2006_weighted_LC_prop <- matrix(ncol = length(NLCD_2006_factor_levels_filtered) + 1,
                                     nrow = 0) %>%
  data.frame()
colnames(NLCD_2006_weighted_LC_prop) <- c(paste0('percent.', NLCD_2006_factor_levels_filtered),
                                          'EVENT_ID')

NLCD_2011_factor_levels <- levels(NLCD_2011)
NLCD_2011_factor_levels_filtered <- NLCD_2011_factor_levels[[1]][NLCD_2011_factor_levels[[1]]$COUNT != 0, ]$ID
NLCD_2011_weighted_LC_prop <- matrix(ncol = length(NLCD_2011_factor_levels_filtered) + 1,
                                     nrow = 0) %>%
  data.frame()
colnames(NLCD_2011_weighted_LC_prop) <- c(paste0('percent.', NLCD_2011_factor_levels_filtered),
                                          'EVENT_ID')


# Performing the weighted extractions and filling in data.frame rows
for (i in 1:nrow(coords_2001)) {
  weighted_extractions <- wtd.table(weighted_extract(coords_2001[i, ]), weights = Gaussian_template)
  curr_df <- data.frame(weighted_extractions)
  curr_df <- spread(curr_df, key = 'Var1', value = 'Freq', sep = "")
  new_colnames <- c()
  for (j in strsplit(colnames(curr_df), 'Var1')[]) {
    new_colnames <- c(new_colnames, paste0('percent.', (j[2])))
  }                             
  colnames(curr_df) <- new_colnames
  NLCD_2001_weighted_LC_prop <- rbind.fill(NLCD_2001_weighted_LC_prop, curr_df)
  NLCD_2001_weighted_LC_prop[i, 'EVENT_ID'] <- IDs_2001[i]
}

for (i in 1:nrow(coords_2006)) {
  weighted_extractions <- wtd.table(weighted_extract(coords_2006[i, ]), weights = Gaussian_template)
  curr_df <- data.frame(weighted_extractions)
  curr_df <- spread(curr_df, key = 'Var1', value = 'Freq', sep = "")
  new_colnames <- c()
  for (j in strsplit(colnames(curr_df), 'Var1')[]) {
    new_colnames <- c(new_colnames, paste0('percent.', (j[2])))
  }                             
  colnames(curr_df) <- new_colnames
  NLCD_2006_weighted_LC_prop <- rbind.fill(NLCD_2006_weighted_LC_prop, curr_df)
  NLCD_2006_weighted_LC_prop[i, 'EVENT_ID'] <- IDs_2006[i]
}

for (i in 1:nrow(coords_2011)) {
  weighted_extractions <- wtd.table(weighted_extract(coords_2011[i, ]), weights = Gaussian_template)
  curr_df <- data.frame(weighted_extractions)
  curr_df <- spread(curr_df, key = 'Var1', value = 'Freq', sep = "")
  new_colnames <- c()
  for (j in strsplit(colnames(curr_df), 'Var1')[]) {
    new_colnames <- c(new_colnames, paste0('percent.', (j[2])))
  }                             
  colnames(curr_df) <- new_colnames
  NLCD_2011_weighted_LC_prop <- rbind.fill(NLCD_2011_weighted_LC_prop, curr_df)
  NLCD_2011_weighted_LC_prop[i, 'EVENT_ID'] <- IDs_2011[i]
}


# Put them all together
LC_df <- rbind(NLCD_2001_weighted_LC_prop,
               NLCD_2006_weighted_LC_prop,
               NLCD_2011_weighted_LC_prop)


# NA's really mean 0, "missing land cover" = "none of that land cover"
LC_df[is.na(LC_df)] <- 0


# Classification value of 0 is not valid, so git rid of that as well
LC_df <- dplyr::select(LC_df,
                       -percent.0)


# Merge the LC values with the tornado events
tor_LC_df <- merge(x = tor_df,
                   y = LC_df,
                   by = 'EVENT_ID')


# Give the LC values a name
colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.11"] <- "OPEN_WATER_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.12"] <- "ICE_SNOW_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.21"] <- "DEV_OPEN_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.22"] <- "DEV_LOW_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.23"] <- "DEV_MED_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.24"] <- "DEV_HIGH_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.41"] <- "DECID_FOREST_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.42"] <- "EVERGR_FOREST_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.43"] <- "MIXED_FOREST_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.52"] <- "SHRUB_SCRUB_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.71"] <- "GRASS_LAND_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.81"] <- "PASTURE_HAY_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.82"] <- "CULT_CROPS_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.90"] <- "WOOD_WETLAND_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.95"] <- "HERB_WETLAND_PROP"

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.31"] <- "BARREN_LAND_PROP"


# Then save the dataframe as .csv
# readr::write_csv(tor_LC_df, "data/raw/tor_data_with_NLCD.csv")


