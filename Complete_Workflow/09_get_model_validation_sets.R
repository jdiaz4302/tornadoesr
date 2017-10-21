


# Packages
library(readr)


# Import dataset to split
tor_df <- read.csv("data/raw/tor_data_preprocessed.csv")


# Shuffle it, for safety
tor_df <- tor_df[sample(nrow(tor_df)), ]


# Reset index
rownames(tor_df) <- NULL


# Set split percentages
fractionTraining <- 0.60
fractionValidation <- 0.20
fractionTest <- 0.20


# Getting the sample sizes, +1's are to grab remainders
sampleSizeTraining <- floor(fractionTraining * nrow(tor_df))

sampleSizeValidation <- floor(fractionValidation * nrow(tor_df)) + 1

sampleSizeTest <- floor(fractionTest * nrow(tor_df)) + 1


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


# Get rid of the unnecessary 'NotTraining'
rm(indicesNotTraining)


# Subset the data
test_set <- tor_df[indicesTest, ]             # Keep all columns

cv_set <- tor_df[indicesValidation, ]

train_set <- tor_df[indicesTraining, ]


# Save them
# write_csv(test_set, "data/raw/tor_test_set.csv")
# write_csv(cv_set, "data/raw/tor_cv_set.csv")
# write_csv(train_set, "data/raw/tor_train_set.csv")


