


# Packages
library(dplyr)


# Function to get NLCD data
get_NLCD <- function(NLCD_URL) {
  
  # description: function to get USGS NLCD data
  # argument: NLCD_URL - the download URL of the specific NLCD raster
  # return: a saved folder containing the NLCD raster of interest
  
  
  # Setup temporary file for the .zip
  zipped_file <- tempfile()
  
  # Download the .zip
  download.file(NLCD_URL, zipped_file)
  
  # Unzip it
  unzip(zipped_file,
        exdir = "data/raw")
  
  
}


# 2001
get_NLCD("http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2001v2&FNAME=nlcd_2001_landcover_2011_edition_2014_10_10.zip")


# 2006
get_NLCD("http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2006&FNAME=nlcd_2006_landcover_2011_edition_2014_10_10.zip")


# 2011
get_NLCD("http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2011&FNAME=nlcd_2011_landcover_2011_edition_2014_10_10.zip")


