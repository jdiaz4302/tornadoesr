


# Get packages
library(readr)
library(dplyr)
library(raster)


# Import tornado data
source("Complete_Workflow/03_clean_StormEvents_files.R")


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


# For extraction
avg_tor_length_meters <- mean(tor_df$TOR_LENGTH) * 1609.34


# Do the extraction and time it
Sys.time()

tor_events_LC_2001 <- raster::extract(NLCD_2001,
                                      tor_Spatial_2001,
                                      buffer = avg_tor_length_meters)

Sys.time()

tor_events_LC_2006 <- raster::extract(NLCD_2006,
                                      tor_Spatial_2006,
                                      buffer = avg_tor_length_meters)

Sys.time()

tor_events_LC_2011 <- raster::extract(NLCD_2011,
                                      tor_Spatial_2011,
                                      buffer = avg_tor_length_meters)


# Get the LC proportions, rather than list of all values
# and time it
Sys.time()

LC_prop_2001 <- lapply(tor_events_LC_2001, function(x) {
  prop.table(table(x))
  }
  )

Sys.time()

LC_prop_2006 <- lapply(tor_events_LC_2006, function(x) {
  prop.table(table(x))
}
)

Sys.time()

LC_prop_2011 <- lapply(tor_events_LC_2011, function(x) {
  prop.table(table(x))
}
)

Sys.time()


# Arrange the proportions into a data.frame, rather than nested list
LC_df_2001 <- data.frame(EVENT_ID = rep(tor_df_2001$EVENT_ID,
                                        lapply(LC_prop_2001,
                                               length)),
                         cover = names(unlist(LC_prop_2001)),
                         percent = unlist(LC_prop_2001))

LC_df_2006 <- data.frame(EVENT_ID = rep(tor_df_2006$EVENT_ID,
                                        lapply(LC_prop_2006,
                                               length)),
                         cover = names(unlist(LC_prop_2006)),
                         percent = unlist(LC_prop_2006))

LC_df_2011 <- data.frame(EVENT_ID = rep(tor_df_2011$EVENT_ID,
                                        lapply(LC_prop_2011,
                                               length)),
                         cover = names(unlist(LC_prop_2011)),
                         percent = unlist(LC_prop_2011))


# More appropriate format, makes cover a variable rather than a value
LC_df_2001 <- reshape(LC_df_2001,
                      idvar = "EVENT_ID",
                      timevar = "cover",
                      direction = "wide")

LC_df_2006 <- reshape(LC_df_2006,
                      idvar = "EVENT_ID",
                      timevar = "cover",
                      direction = "wide")

LC_df_2011 <- reshape(LC_df_2011,
                      idvar = "EVENT_ID",
                      timevar = "cover",
                      direction = "wide")


# There are literally only 2 tornado events that have NLCD 
# classification 12 (perennial ice/snow), and this is messing up the
# rbind(), so I'm just getting rid of it
LC_df_2011 <- dplyr::select(LC_df_2011,
                            -percent.12)


# Put them all together
LC_df <- rbind(LC_df_2001,
               LC_df_2006,
               LC_df_2011)


# NA's really mean 0, "missing land cover" = "none of that land cover"
LC_df[is.na(LC_df)] <- 0


# Classification value of 0 is not valid, so git rid of that as well
LC_df <- dplyr::select(LC_df,
                       -percent.0)


# Fix the indices
rownames(LC_df) <- seq(length = nrow(LC_df))


# Merge the LC values with the tornado events
tor_LC_df <- merge(x = tor_df,
                   y = LC_df)


# Give the LC values a name
colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.11"] <- "OPEN_WATER_PROP"

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


