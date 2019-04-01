


# Packages
library(dplyr)
library(readr)


# Find all of the csv.gz files
files <- list.files(path = file.path("data", "raw"), 
                    pattern = ".csv.gz", 
                    full.names = TRUE)


# Merge them
tor_df <- lapply(files,
                 read_csv,
                 col_types = cols(DAMAGE_CROPS = "c",
                                  EPISODE_ID = "c",
                                  TOR_OTHER_CZ_FIPS = "c",
                                  TOR_WIDTH = "d",
                                  CATEGORY = 'i')) %>%
  bind_rows() %>%
  as.data.frame



