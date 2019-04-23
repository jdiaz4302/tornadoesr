


install.packages('tidycensus')
library(tidycensus)
library(raster)


census_api_key("REDACTED")

# Point estimate year home built - SECURE
median_year_struct_built <- get_acs(geography = "county", year = 2010, variables = c("B25035_001"), geometry = TRUE)
aa <- median_year_struct_built$estimate

library(sf)
med_year_struct_built_geo <- as_Spatial(median_year_struct_built$geometry)
plot(med_year_struct_built_geo)
crs(med_year_struct_built_geo)

NLCD <- raster('data/raw/nlcd_2001_landcover_2011_edition_2014_10_10/nlcd_2001_landcover_2011_edition_2014_10_10.img')
plot(NLCD)
crs(NLCD)

a <- spTransform(med_year_struct_built_geo, crs(NLCD))
plot(a)

a_ID <- sapply(slot(a, "polygons"), function(a) slot(a, "ID"))
a_df <- data.frame(ID = 1:length(a), row.names = a_ID) 
a_df$estimate <- median_year_struct_built$estimate
a2 <- SpatialPolygonsDataFrame(a, a_df)
class(a2) 
plot(a2)

mid_x <- (extent(NLCD)[1] + extent(NLCD)[2]) / 2 
mid_y <- (extent(NLCD)[3] + extent(NLCD)[4]) / 2 

small_extent <- extent(mid_x - 100000, mid_x + 100000,
                       mid_y - 100000, mid_y + 100000)

small_NLCD <- crop(NLCD, small_extent)
plot(small_NLCD)

small_a2 <- crop(a2, small_extent)
plot(small_a2)

start <- Sys.time()
small_a2_rasterized <- rasterize(small_a2, small_NLCD)
end <- Sys.time()
end - start

small_a2_rasterized

midpoint_df <- data.frame(mid_x, mid_y)

small_extraction <- extract(small_a2_rasterized, midpoint_df)
small_extraction
real_small_extraction <- levels(small_a2_rasterized)[[1]][
  levels(small_a2_rasterized)[[1]]$ID == small_extraction, 'estimate'
  ]
real_small_extraction
plot(small_a2_rasterized)






library('parallel')
# Calculate the number of cores
no_cores <- detectCores() - 1
# Number of polygons features in SPDF
features <- 1:nrow(small_a2[,])
# Split features in n parts
n <- 13
parts <- split(features, cut(features, n))
parts
# Initiate cluster (after loading all the necessary object to R environment: BRA_adm2, parts, r.raster, n)
cl <- makeCluster(no_cores, type = "FORK")
print(cl)
# Parallelize rasterize function
system.time(rParts <- parLapply(cl = cl, X = 1:n, fun = function(x) rasterize(small_a2[parts[[x]],], small_NLCD)))
rParts
# Finish
stopCluster(cl)
# Merge all raster parts
rMerge <- do.call(raster::merge, rParts)
# Plot raster
plot(rMerge)
extract(rMerge, midpoint_df)


