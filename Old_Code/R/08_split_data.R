


# Packages
library(readr)


# Import dataset to split
tor_LC_df <- read.csv("data/raw/Tor_data_with_LC_processed.csv")


# Shuffle it, for safety
tor_LC_df <- tor_LC_df[sample(nrow(tor_LC_df)),
                       ]

rownames(tor_LC_df) <- NULL


# Set split percentages
fractionTraining <- 0.60

fractionValidation <- 0.20

fractionTest <- 0.20


# Getting the sample sizes, +1's are to grab remainders
sampleSizeTraining <- floor(fractionTraining * nrow(tor_LC_df))

sampleSizeValidation <- floor(fractionValidation * nrow(tor_LC_df)) + 1

sampleSizeTest <- floor(fractionTest * nrow(tor_LC_df)) + 1


# Get indices for training set
indicesTraining <- sort(sample(seq_len(nrow(tor_LC_df)),
                               size = sampleSizeTraining))


# Remove the training indices from possible indices
indicesNotTraining <- setdiff(seq_len(nrow(tor_LC_df)),
                              indicesTraining)


# Get indices for validation
indicesValidation <- sort(sample(indicesNotTraining,
                                 size = sampleSizeValidation))


# Give test set what indices remain
indicesTest <- setdiff(indicesNotTraining,
                       indicesValidation)


# Subset the data
test_set <- tor_LC_df[indicesTest,
                      ]                            # Keep all columns

cv_set <- tor_LC_df[indicesValidation,
                    ]

train_set <- tor_LC_df[indicesTraining,
                       ]

# Run this until the data sets have very similar means
# and distributions
# Easily done with R/17_...


# Save them
# write_csv(test_set, "data/raw/tor_test_set.csv")
# write_csv(cv_set, "data/raw/tor_cv_set.csv")
# write_csv(train_set, "data/raw/tor_train_set.csv")


