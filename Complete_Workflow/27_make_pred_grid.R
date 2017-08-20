


# Packages
library(sp)
library(raster)
library(dplyr)
library(readr)


# Get all possible longitude values
LON <- seq(-125, -66)


# Iteratively add each possible latitude value in an
# Adjacent column to the longitude values
for (i in seq(23, 50)) {
  
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


# Get those points as a Spatial object
grid_points <- SpatialPoints(grid_df,
                             proj4string = CRS('+init=epsg:4326'))


# Read the census-provided United States shapefile
# Convert its crs
# And crop it to only the continental US
us_shape <- shapefile('data/raw/cb_2016_us_nation_5m.shp') %>%
  spTransform(CRSobj = crs(grid_points)) %>%
  crop(extent(-125, -66, 23, 50))


# Only points within the continental US
grid_points <- grid_points[us_shape, ]


# Save the gridded points as a .csv for prediction
as.data.frame(grid_points) %>%
  write_csv('data/raw/grid_for_pred.csv')


