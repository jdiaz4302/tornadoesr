


# Get packages
library(raster)


# Import tornado data
source("R/3_format_for_python.R")


# Importing NLCD data
NLCD <- raster("data/raw/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img") 


# Get the coords
tor_coord <- dplyr::select(tor_df,
                           c(BEGIN_LON,
                             BEGIN_LAT))


# Make it a SpatialPointsDataFrame
tor_Spatial <- SpatialPointsDataFrame(tor_coord,
                                      tor_df,
                                      proj4string = CRS("+init=epsg:4326"))

# Make them have the same CRS
tor_Spatial <- spTransform(tor_Spatial,
                           crs(NLCD))


# For extraction
avg_tor_length_meters <- mean(tor_df$TOR_LENGTH) * 1e3


# Do the extraction and time it
Sys.time()

tor_events_LC <- raster::extract(NLCD,
                                 tor_Spatial,
                                 buffer = avg_tor_length_meters)

Sys.time()


# Get the LC proportions, rather than list of all values
# and time it
Sys.time()

LC_prop <- lapply(tor_events_LC, function(x) {
  prop.table(table(x))
  }
  )

Sys.time()


# Arrange the proportions into a data.frame, rather than nested list
LC_df <- data.frame(EVENT_ID = rep(tor_df$EVENT_ID,
                                   lapply(LC_prop,
                                          length)),
                    cover = names(unlist(LC_prop)),
                    percent = unlist(LC_prop))


# More appropriate format, makes cover a variable rather than a value
LC_df <- reshape(LC_df,
                 idvar = "EVENT_ID",
                 timevar = "cover",
                 direction = "wide")


# Fix the indices
rownames(LC_df) <- seq(length = nrow(LC_df))


# Merge the LC values with the tornado events
tor_LC_df <- merge(x = tor_df,
                   y = LC_df)


# All NA's were removed from tor_df
# All existing NA's come from proportions of a LC value
# That did not exist in an event
# So, all NA's should be 0
tor_LC_df[is.na(tor_LC_df)] <- 0


# "0" isn't actually a LC value, so we'll remove that error
tor_LC_df <- dplyr::select(tor_LC_df,
                           -c(percent.0))


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

colnames(tor_LC_df)[colnames(tor_LC_df) == "percent.12"] <- "ICE_SNOW_PROP"


# Then save the dataframe as .csv
# write_csv(tor_LC_df, "data/raw/Tor_data_with_LC.csv")


