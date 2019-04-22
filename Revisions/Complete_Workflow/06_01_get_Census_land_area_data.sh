


# Get the Census land area data
cd data/raw/
wget https://www2.census.gov/library/publications/2011/compendia/usa-counties/excel/LND01.xls
cd ../../


# Install gnumeric - for converting xls to csv
sudo apt-get install gnumeric


# Convert the xls to csv
ssconvert data/raw/LND01.xls data/raw/LND01.csv


