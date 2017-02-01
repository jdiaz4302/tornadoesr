# Merge all of the yearly storm event data --------------------------------
library(tidyverse)

# Find the most recent versions of all data files -------------------------

# find all of the csv.gz files
files <- list.files(path = file.path("data", "raw"), 
                    pattern = ".csv.gz", 
                    full.names = TRUE)

# find out when the data files were last updated
last_update_index <- gregexpr(pattern = "_c[:numbers:]*.", files) %>%
  unlist %>%
  unique

file_updated <- substring(files, 
                          first = last_update_index + 2, 
                          last = last_update_index + 9) %>%
  as.Date(format = "%Y%m%d")

# find out which year each data file is associated with
year_index <- gregexpr(pattern = "_d[0-9]{4}", files) %>%
  unlist %>%
  unique

file_year <- substring(files, 
                       first = year_index + 2, 
                       last = year_index + 5) %>%
  as.numeric

# subset the data to only the most recent data files
file_df <- tibble(files, file_updated, file_year) %>%
  group_by(file_year) %>%
  mutate(most_recent_change = max(file_updated), 
         is_most_recent = file_updated == most_recent_change) %>%
  filter(is_most_recent)

# delete old files that have been updated
to_delete <- setdiff(files, file_df$files)
file.remove(to_delete)




# Merging the data --------------------------------------------------------
df <- lapply(files, 
             read_csv, 
             col_types = cols(DAMAGE_CROPS = "c", 
                              EPISODE_ID = "c", 
                              TOR_OTHER_CZ_FIPS = "c", 
                              TOR_WIDTH = "d")) %>%
  bind_rows() # bind the result together into one data frame
