library(RCurl)
library(tidyverse)


# Identify all of the files in the ftp directory --------------------------
ftp_prefix <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
download.file(ftp_prefix, destfile = "out.txt")
lines <- readLines("out.txt")

# Find the filenames for storm detail csvs --------------------------------
storm_detail_indices <- grep("StormEvents_details", lines)
storm_lines <- lines[storm_detail_indices]
filenames <- substring(storm_lines, 56)


# Generate full paths to the data files -----------------------------------
full_paths <- paste0(ftp_prefix, filenames)

local_prefix <- file.path("data", "raw")
local_paths <- file.path(local_prefix, filenames)



# Download all of the data files ------------------------------------------
for (i in seq_along(full_paths)) {
  download.file(full_paths[i], local_paths[i])
}
