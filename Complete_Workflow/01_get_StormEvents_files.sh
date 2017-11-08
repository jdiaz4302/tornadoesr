#!/bin/bash

# Remove all previous raw storm events data files
rm data/raw/StormEvents*.csv.gz

# Download all compressed csv files that contain storm events from the NOAA ftp server
wget -nd -P data/raw -r -np -A "StormEvents_details*.csv.gz*" ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles

