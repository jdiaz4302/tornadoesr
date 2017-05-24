


# Packages
library(readr)


# Import tornado events with LC proportions
tor_LC_df <- read.csv("data/raw/Tor_data_with_LC.csv")


# Defining a simple mean normalization function
mean_normalize <- function(to_normalize){
  
  # descr:  simple mean normalization... (x - mean(x))/sd(x)
  # arg:    thing to normalize
  # return: that thing normalized
  
  numerator <- to_normalize - mean(to_normalize)
  
  normalized <- numerator / sd(to_normalize)
  
  return(normalized)
  
}


# Normalizing property damage
tor_LC_df$DAMAGE_PROPERTY <- mean_normalize(tor_LC_df$DAMAGE_PROPERTY)


# Normalizing predicting variables
tor_LC_df$DURATION_SECONDS <- mean_normalize(tor_LC_df$DURATION_SECONDS)

tor_LC_df$BEGIN_LAT <- mean_normalize(tor_LC_df$BEGIN_LAT)

tor_LC_df$BEGIN_LON <- mean_normalize(tor_LC_df$BEGIN_LON)

tor_LC_df$TOR_LENGTH <- mean_normalize(tor_LC_df$TOR_LENGTH)

tor_LC_df$TOR_WIDTH <- mean_normalize(tor_LC_df$TOR_WIDTH)

tor_LC_df$BEGIN_DATE_TIME <- mean_normalize(tor_LC_df$BEGIN_DATE_TIME)

tor_LC_df$OPEN_WATER_PROP <- mean_normalize(tor_LC_df$OPEN_WATER_PROP)

tor_LC_df$DEV_OPEN_PROP <- mean_normalize(tor_LC_df$DEV_OPEN_PROP)

tor_LC_df$DEV_LOW_PROP <- mean_normalize(tor_LC_df$DEV_LOW_PROP)

tor_LC_df$DEV_MED_PROP <- mean_normalize(tor_LC_df$DEV_MED_PROP)

tor_LC_df$DEV_HIGH_PROP <- mean_normalize(tor_LC_df$DEV_HIGH_PROP)

tor_LC_df$DECID_FOREST_PROP <- mean_normalize(tor_LC_df$DECID_FOREST_PROP)

tor_LC_df$EVERGR_FOREST_PROP <- mean_normalize(tor_LC_df$EVERGR_FOREST_PROP)

tor_LC_df$MIXED_FOREST_PROP <- mean_normalize(tor_LC_df$MIXED_FOREST_PROP)

tor_LC_df$SHRUB_SCRUB_PROP <- mean_normalize(tor_LC_df$SHRUB_SCRUB_PROP)

tor_LC_df$GRASS_LAND_PROP <- mean_normalize(tor_LC_df$GRASS_LAND_PROP)

tor_LC_df$PASTURE_HAY_PROP <- mean_normalize(tor_LC_df$PASTURE_HAY_PROP)

tor_LC_df$CULT_CROPS_PROP <- mean_normalize(tor_LC_df$CULT_CROPS_PROP)

tor_LC_df$WOOD_WETLAND_PROP <- mean_normalize(tor_LC_df$WOOD_WETLAND_PROP)

tor_LC_df$HERB_WETLAND_PROP <- mean_normalize(tor_LC_df$HERB_WETLAND_PROP)

tor_LC_df$BARREN_LAND_PROP <- mean_normalize(tor_LC_df$BARREN_LAND_PROP)

tor_LC_df$ICE_SNOW_PROP <- mean_normalize(tor_LC_df$ICE_SNOW_PROP)


# Save it
# write_csv(tor_LC_df, "data/raw/Tor_data_with_LC_norm.csv")


