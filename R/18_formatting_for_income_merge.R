


# Packages
library(readr)


# Import all the data
train_no_0 <- read.csv("data/raw/tor_train_set_no_zeros.csv")

train_set <- read.csv("data/raw/tor_train_set.csv")

cv_no_0 <- read.csv("data/raw/tor_cv_set_no_zeros.csv")

cv_set <- read.csv("data/raw/tor_cv_set.csv")

test_no_0 <- read.csv("data/raw/tor_test_set_no_zeros.csv")

test_set <- read.csv("data/raw/tor_test_set.csv")

county_income_data <- read.csv("data/raw/GeoFRED_Per_Capita_Personal_Income_by_County_Dollars.csv")


# Change the state names to abbreviations
train_no_0$state_abbrev <- state.abb[match(train_no_0$STATE,
                                           toupper(state.name))]

train_set$state_abbrev <- state.abb[match(train_set$STATE,
                                          toupper(state.name))]

cv_no_0$state_abbrev <- state.abb[match(cv_no_0$STATE,
                                        toupper(state.name))]

cv_set$state_abbrev <- state.abb[match(cv_set$STATE,
                                       toupper(state.name))]

test_no_0$state_abbrev <- state.abb[match(test_no_0$STATE,
                                        toupper(state.name))]

test_set$state_abbrev <- state.abb[match(test_set$STATE,
                                         toupper(state.name))]


# Add income to the other data
train_no_0_names <- dplyr::select(train_no_0,
                                  c(CZ_NAME, state_abbrev))

train_no_0$INCOME <- county_income_data$X.2[match(do.call(paste, train_no_0_names),
                                                  toupper(sub("\\s+\\w+,", "",
                                                              county_income_data$X)))]


train_set_names <- dplyr::select(train_set,
                                 c(CZ_NAME, state_abbrev))

train_set$INCOME <- county_income_data$X.2[match(do.call(paste, train_set_names),
                                                 toupper(sub("\\s+\\w+,", "",
                                                             county_income_data$X)))]


cv_no_0_names <- dplyr::select(cv_no_0,
                               c(CZ_NAME, state_abbrev))

cv_no_0$INCOME <- county_income_data$X.2[match(do.call(paste, cv_no_0_names),
                                               toupper(sub("\\s+\\w+,", "",
                                                           county_income_data$X)))]


cv_set_names <- dplyr::select(cv_set,
                              c(CZ_NAME, state_abbrev))

cv_set$INCOME <- county_income_data$X.2[match(do.call(paste, cv_set_names),
                                              toupper(sub("\\s+\\w+,", "",
                                                          county_income_data$X)))]


test_no_0_names <- dplyr::select(test_no_0,
                                 c(CZ_NAME, state_abbrev))

test_no_0$INCOME <- county_income_data$X.2[match(do.call(paste, test_no_0_names),
                                                 toupper(sub("\\s+\\w+,", "",
                                                             county_income_data$X)))]


test_set_names <- dplyr::select(test_set,
                                c(CZ_NAME, state_abbrev))

test_set$INCOME <- county_income_data$X.2[match(do.call(paste, test_set_names),
                                                toupper(sub("\\s+\\w+,", "",
                                                            county_income_data$X)))]


# Get rid of the newly-created state abbreviation variable
train_no_0 <- dplyr::select(train_no_0,
                            -state_abbrev)

train_set <- dplyr::select(train_set,
                           -state_abbrev)

cv_no_0 <- dplyr::select(cv_no_0,
                         -state_abbrev)

cv_set <- dplyr::select(cv_set,
                        -state_abbrev)

test_no_0 <- dplyr::select(test_no_0,
                           -state_abbrev)

test_set <- dplyr::select(test_set,
                          -state_abbrev)


# Save them
# write_csv(train_no_0, "data/raw/train_with_econ_wout_zeros.csv")
# write_csv(train_set, "data/raw/train_with_econ_with_zeros.csv")
# write_csv(cv_no_0, "data/raw/cv_with_econ_wout_zeros.csv")
# write_csv(cv_set, "data/raw/cv_with_econ_with_zeros.csv")
# write_csv(test_no_0, "data/raw/test_with_econ_wout_zeros.csv")
# write_csv(test_set, "data/raw/test_with_econ_with_zeros.csv")


