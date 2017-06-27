


# Packages
library(rethinking)
library(viridis)


# Import the data
train_set <- read.csv("data/raw/tor_train_set_no_zeros_mob_home.csv")


# Setup and fit the multvariate regression
dam_model <- map(alist(DAMAGE_PROPERTY ~ dnorm(mu, sigma),
                       mu <- a + b1*DURATION_SECONDS + b2*BEGIN_LAT + b3*BEGIN_LON + b4*TOR_LENGTH +
                         b5*TOR_WIDTH + b6*YEAR + b7*OPEN_WATER_PROP + b8*DEV_OPEN_PROP +
                         b9*DEV_LOW_PROP + b10*DEV_MED_PROP + b11*DEV_HIGH_PROP + b12*DECID_FOREST_PROP +
                         b13*EVERGR_FOREST_PROP + b14*MIXED_FOREST_PROP + b15*SHRUB_SCRUB_PROP +
                         b16*GRASS_LAND_PROP + b17*PASTURE_HAY_PROP + b18*CULT_CROPS_PROP +
                         b19*WOOD_WETLAND_PROP + b20*HERB_WETLAND_PROP + b21*BARREN_LAND_PROP +
                         b22*INCOME + b23*TOR_AREA + b24*TOT_DEV_INT + b25*TOT_WOOD_AREA +
                         b26*WOOD_DEV_INT + b27*EXP_INC_AREA + b28*DAY_OF_YEAR + b29*STATE_RANK +
                         b30*MOB_HOM_COUNT + b31*TIME + b32*MONTH_MEAN,
                       b1 ~ dnorm(0, 1),
                       b2 ~ dnorm(0, 1),
                       b3 ~ dnorm(0, 1),
                       b4 ~ dnorm(0, 1),
                       b5 ~ dnorm(0, 1),
                       b6 ~ dnorm(0, 1),
                       b7 ~ dnorm(0, 1),
                       b8 ~ dnorm(0, 1),
                       b9 ~ dnorm(0, 1),
                       b10 ~ dnorm(0, 1),
                       b11 ~ dnorm(0, 1),
                       b12 ~ dnorm(0, 1),
                       b13 ~ dnorm(0, 1),
                       b14 ~ dnorm(0, 1),
                       b15 ~ dnorm(0, 1),
                       b16 ~ dnorm(0, 1),
                       b17 ~ dnorm(0, 1),
                       b18 ~ dnorm(0, 1),
                       b19 ~ dnorm(0, 1),
                       b20 ~ dnorm(0, 1),
                       b21 ~ dnorm(0, 1),
                       b22 ~ dnorm(0, 1),
                       b23 ~ dnorm(0, 1),
                       b24 ~ dnorm(0, 1),
                       b25 ~ dnorm(0, 1),
                       b26 ~ dnorm(0, 1),
                       b27 ~ dnorm(0, 1),
                       b28 ~ dnorm(0, 1),
                       b29 ~ dnorm(0, 1),
                       b30 ~ dnorm(0, 1),
                       b31 ~ dnorm(0, 1),
                       b32 ~ dnorm(0, 1),
                       a ~ dnorm(0, 10),
                       sigma ~ dunif(0, 20)),
                 data = train_set)


# View coefficients
precis(dam_model, digits = 4)


# Sample average property damage values from the posterior for the training data
mu <- link(dam_model)

# Characterise those samples 
mu_mean <- apply(mu, 2, mean)

# Add that to the df
train_set$mu_mean <- mu_mean

# Plot pred versus reality
ggplot(train_set, aes(x = DAMAGE_PROPERTY,
                      y = mu_mean)) +
  geom_jitter(width = 0.05,
              height = 0.05,
              alpha = 0.5,
              size = 2) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, col = "red", size = 1.5) +
  scale_x_continuous(limits = c(-1, 3)) +
  scale_y_continuous(limits = c(-1, 3))
  

