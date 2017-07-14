


# Setup temporary file for the .zip
zipped_file <- tempfile()


# Download the .zip
download.file("http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2011&FNAME=nlcd_2011_landcover_2011_edition_2014_10_10.zip",
              zipped_file)


# Unzip
unzip(zipped_file)


# Move it to the right place
file.rename("nlcd_2011_landcover_2011_edition_2014_10_10",
            "data/raw/nlcd_2011_landcover_2011_edition_2014_10_10")


