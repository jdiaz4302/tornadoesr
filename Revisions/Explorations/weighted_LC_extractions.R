


library(raster)


# Setting proper working directory
setwd('tornadoesr/')


# Importing a NLCD raster
NLCD_2001 <- raster(paste0('data/raw/nlcd_2001_landcover_2011_edition_2014_10_10/',
                           'nlcd_2001_landcover_2011_edition_2014_10_10.img'))
# View it
plot(NLCD_2001)


# Defining a Guassian kernel of specified sigma and spatial refernce
Gaussian_template <- focalWeight(NLCD_2001, 5000, type = 'Gauss')
# View it
image(Gaussian_template)


# Appling the template via convolution
NLCD_2001_focal <- focal(NLCD_2001,
                         w = Gaussian_template,
                         pad = TRUE)



