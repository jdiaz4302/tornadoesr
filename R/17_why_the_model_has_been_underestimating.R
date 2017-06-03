


# The models are regularly predicting values that
# Are lower than the true outcomes
# Seeing if there's an easy reason why


# Import the data
train_set <- read.csv("data/raw/tor_train_set.csv")

test_set <- read.csv("data/raw/tor_test_set.csv")

cv_set <- read.csv("data/raw/tor_cv_set.csv")


# Check the means
mean(train_set$DAMAGE_PROPERTY)
# result: 0.0065

mean(cv_set$DAMAGE_PROPERTY)
# result: -0.0162


# Welp, the means on the non-training data are
# Lower than the training data - not by much though
# The model is trained on a higher mean than the
# Data that it is predicting on


# The histograms
hist(train_set$DAMAGE_PROPERTY,
     breaks = 50,
     xlim = c(-2, 2))

hist(cv_set$DAMAGE_PROPERTY,
     breaks = 50,
     xlim = c(-2, 2))


# The distribution of cross validation data
# Is shifted to the left, lower, than that of the
# Training data
# To elaborate, the lower half of the distribution
# of the cross valiadation data is nearly exclusively
# larger, with the larger half of the distribution is
# nearly exclusively smaller.


