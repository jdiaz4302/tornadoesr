


# Packages
library(readr)
library(dplyr)


# Read in the .csv's
test_set <- read.csv("data/raw/tor_test_set_interact.csv")

cv_set <- read.csv("data/raw/tor_cv_set_interact.csv")

training_set <- read.csv("data/raw/tor_train_set_interact.csv")


# 0 is the lowest DAMAGE_PROPERTY value
# Its also by-far the most abundant
# So, the minimum value of the data.frames
# Will be a true-value of 0
zero_damage <- min(test_set$DAMAGE_PROPERTY)


# Filter out zeros
test_set_wout_zeros <- filter(test_set,
                              DAMAGE_PROPERTY > zero_damage)

cv_set_wout_zeros <- filter(cv_set,
                            DAMAGE_PROPERTY > zero_damage)

training_set_wout_zeros <- filter(training_set,
                                  DAMAGE_PROPERTY > zero_damage)


# Save them
# write_csv(test_set_wout_zeros, "data/raw/tor_test_set_no_zeros_interact.csv")
# write_csv(cv_set_wout_zeros, "data/raw/tor_cv_set_no_zeros_interact.csv")
# write_csv(training_set_wout_zeros, "data/raw/tor_train_set_no_zeros_interact.csv")


