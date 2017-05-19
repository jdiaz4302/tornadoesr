


# Set the Storm Events URL
ftp_prefix <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"


# Download that page as a .txt
download.file(ftp_prefix,
              destfile = "out.txt")


# Import all the lines from that .txt
lines <- readLines("out.txt")


# Get all the line indices that contain relevant .csv's
storm_detail_indices <- grep("StormEvents_details",
                             lines)


# Subset the total lines to only relevant .csv lines
storm_lines <- lines[storm_detail_indices]


# Find where the .csv file name starts
filename_start_index <- unlist(gregexpr(" StormEvents",
                                        storm_lines))


# Sets all_same_index to a T/F value, TRUE if only 1 index
all_same_index <- length(unique(filename_start_index)) == 1


# Throws error if they're not the same
if (!all_same_index) {
  stop("The filenames begin at different indices in 'storm_lines'.")
  }


# Subset the lines down to only the files that we wish to download
filenames <- substring(storm_lines,
                       unique(filename_start_index))


# Gets rid of some empty space that exists due to an earlier
# encountered problem
filenames <- substring(filenames,
                       2)


# Generate full URL of the files
full_paths <- paste0(ftp_prefix,
                     filenames)


# Where it's going in your directory
local_prefix <- file.path("data", "raw")


# Maintaining their original names in your directory
local_paths <- file.path(local_prefix, filenames)


# Download all of the data files if they don't already exist
for (i in seq_along(full_paths)) {
  if (!file.exists(local_paths[i])) {
    download.file(full_paths[i],
                  local_paths[i])
    }
  }


