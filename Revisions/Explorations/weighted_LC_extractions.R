


library(raster)


# Setting proper working directory
setwd('tornadoesr/')


rasterOptions(chunksize = 5e+10,
              maxmemory = 5e+10)


# Importing a NLCD raster
NLCD_2001 <- raster(paste0('data/raw/nlcd_2001_landcover_2011_edition_2014_10_10/',
                           'nlcd_2001_landcover_2011_edition_2014_10_10.img'))
# View it
plot(NLCD_2001)


# Defining a Guassian kernel of specified sigma and spatial refernce
Gaussian_template <- focalWeight(NLCD_2001, 5000, type = 'Gauss')
# View it
image(Gaussian_template)


# Speed testing on a subset
NLCD_2001_subset <- crop(NLCD_2001,
                         extent(as.integer(NLCD_2001@extent[1] + (NLCD_2001@extent[2] - NLCD_2001@extent[1])*0.5),
                                as.integer(NLCD_2001@extent[1] + (NLCD_2001@extent[2] - NLCD_2001@extent[1])*0.52),
                                as.integer(NLCD_2001@extent[3] + (NLCD_2001@extent[4] - NLCD_2001@extent[3])*0.5),
                                as.integer(NLCD_2001@extent[3] + (NLCD_2001@extent[4] - NLCD_2001@extent[3])*0.52)))
plot(NLCD_2001_subset)


# Appling the template via convolution
NLCD_2001_subset_focal <- focal(NLCD_2001_subset,
                                w = Gaussian_template,
                                pad = TRUE)



