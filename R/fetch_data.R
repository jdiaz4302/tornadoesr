# Download storm event data from NOAA NWS ---------------------------------

# Identify all of the files in the ftp directory --------------------------
ftp_prefix <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
download.file(ftp_prefix, destfile = "out.txt")
lines <- readLines("out.txt")

# Find the filenames for storm detail csvs --------------------------------
storm_detail_indices <- grep("StormEvents_details", lines)
storm_lines <- lines[storm_detail_indices]

filename_start_index <- unlist(gregexpr("StormEvents", storm_lines))

all_same_index <- length(unique(filename_start_index)) == 1
if (!all_same_index) {
  stop("The filenames begin at different indices in 'storm_lines'.")
}

filenames <- substring(storm_lines, unique(filename_start_index))


# Generate full paths to the data files -----------------------------------
full_paths <- paste0(ftp_prefix, filenames)

local_prefix <- file.path("data", "raw")
local_paths <- file.path(local_prefix, filenames)



# Download all of the data files if they don't already exist ----------------
for (i in seq_along(full_paths)) {
  if (!file.exists(local_paths[i])) {
    download.file(full_paths[i], local_paths[i])
  }
}
