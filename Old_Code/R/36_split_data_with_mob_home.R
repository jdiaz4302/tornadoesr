


# Packages
library(readr)


# Import dataset to split
tor_df <- read.csv("data/raw/Tor_data_with_mob_home_processed.csv")


# Shuffle it, for safety
tor_df <- tor_df[sample(nrow(tor_df)), ]

rownames(tor_df) <- NULL


# Set split percentages
fractionTraining <- 0.60
fractionValidation <- 0.20
fractionTest <- 0.20


# Getting the sample sizes, +1's are to grab remainders
sampleSizeTraining <- floor(fractionTraining * nrow(tor_df))

sampleSizeValidation <- floor(fractionValidation * nrow(tor_df)) + 1

sampleSizeTest <- floor(fractionTest * nrow(tor_df))


# Get indices for training set
indicesTraining <- sort(sample(seq_len(nrow(tor_df)),
                               size = sampleSizeTraining))


# Remove the training indices from possible indices
indicesNotTraining <- setdiff(seq_len(nrow(tor_df)),
                              indicesTraining)


# Get indices for validation
indicesValidation <- sort(sample(indicesNotTraining,
                                 size = sampleSizeValidation))


# Give test set what indices remain
indicesTest <- setdiff(indicesNotTraining,
                       indicesValidation)


# Subset the data
test_set <- tor_df[indicesTest, ]             # Keep all columns

cv_set <- tor_df[indicesValidation, ]

train_set <- tor_df[indicesTraining, ]

# Run this until the data sets have very similar means
# and distributions
# Easily done with R/17_...


# Save them
# write_csv(test_set, "data/raw/tor_test_set_mob_home.csv")
# write_csv(cv_set, "data/raw/tor_cv_set_mob_home.csv")
# write_csv(train_set, "data/raw/tor_train_set_mob_home.csv")


