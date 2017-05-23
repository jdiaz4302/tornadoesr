

# Get packages
library(raster)


# Import tornado data
source("R/format_for_python.R")


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


# Do the extraction and time it
Sys.time()

tor_events_LC <- raster::extract(NLCD,
                                 tor_Spatial,
                                 buffer = 3500)

Sys.time()


# Get the LC proportions, rather than list of all values
LC_prop <- lapply(tor_events_LC,
                  function(x){
                    prop.table(table(x))
                    }
                  )


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


# Then save the dataframe as .csv
# But I'm not going to put that here just in case


