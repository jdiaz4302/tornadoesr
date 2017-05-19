


# Packages
library(dplyr)
library(readr)


# Find all of the csv.gz files
files <- list.files(path = file.path("data", "raw"), 
                    pattern = ".csv.gz", 
                    full.names = TRUE)


# Get the index of where the file contains the last update date
last_update_index <- gregexpr(pattern = "_c[:numbers:]*.",
                              files) %>%
  unlist %>%
  unique


# Get the update dates and format them appropriately
file_updated <- substring(files, 
                          first = last_update_index + 2, 
                          last = last_update_index + 9) %>%
  as.Date(format = "%Y%m%d")


# Get the index of where the file contains the .csv year
year_index <- gregexpr(pattern = "_d[0-9]{4}",
                       files) %>%
  unlist %>%
  unique


# Get the .csv years
file_year <- substring(files, 
                       first = year_index + 2, 
                       last = year_index + 5) %>%
  as.numeric

# Subset the data to only the most recently updated data files
# For each .csv / year
file_df <- tibble(files,
                  file_updated,
                  file_year) %>%
  group_by(file_year) %>%
  mutate(most_recent_change = max(file_updated), 
         is_most_recent = file_updated == most_recent_change) %>%
  filter(is_most_recent)


# Delete old files that have been updated
to_delete <- setdiff(files,
                     file_df$files)

file.remove(to_delete)


# Merge the data
tor_df <- lapply(files,
                 read_csv,
                 col_types = cols(DAMAGE_CROPS = "c",
                                  EPISODE_ID = "c",
                                  TOR_OTHER_CZ_FIPS = "c",
                                  TOR_WIDTH = "d")) %>%
  bind_rows() %>%
  as.data.frame


