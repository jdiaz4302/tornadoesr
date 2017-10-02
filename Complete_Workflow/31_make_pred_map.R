


# Packages
library(dplyr)
library(sp)
library(raster)
library(ggplot2)


# Import the grid points
# And remove the pandas-induced index column
grid_with_pred <- read.csv('Complete_Workflow/grid_with_predictions.csv') %>%
  dplyr::select(-X)


# Import the unprocessed model data
tor_df <- read.csv("data/raw/tor_data_with_interact_effects.csv")


# Undo the processing to get lat and lon in natural scale
grid_with_pred$BEGIN_LAT <- grid_with_pred$BEGIN_LAT * sd(tor_df$BEGIN_LAT) +
  mean(tor_df$BEGIN_LAT)
grid_with_pred$BEGIN_LON <- grid_with_pred$BEGIN_LON * sd(tor_df$BEGIN_LON) +
  mean(tor_df$BEGIN_LON)


# Undo only the mean normalization on property damage, exponentiate,
# Then convert to log10 scale
grid_with_pred$DAMAGE_PROPERTY <- (grid_with_pred$DAMAGE_PROPERTY * sd(log(tor_df$DAMAGE_PROPERTY + 1,
                                                                          base = exp(1))) +
  mean(log(tor_df$DAMAGE_PROPERTY + 1,
           base = exp(1)))) %>%
  exp() %>%
  log10()
 
 
# Have the gridded points as a SpatialPointsDataFrame
grid_points <- SpatialPointsDataFrame(coords = grid_with_pred[, 4:3],
                                      data = dplyr::select(grid_with_pred,
                                                           -c(BEGIN_LAT,
                                                              BEGIN_LON)),
                                      proj4string = CRS('+init=epsg:4326'))


# Import the 2015 census-provided US country shape
# Get it in the same CRS
# And crop it to only the continental US
us_shape <- shapefile('data/raw/cb_2015_us_state_5m.shp') %>%
  spTransform(CRSobj = crs(grid_points)) %>%
  crop(extent(-125, -66, 23, 50))


# Make month a factor
grid_with_pred$MONTH <- as.factor(grid_with_pred$MONTH)


# Specify that it's the 15th day of each month
levels(grid_with_pred$MONTH) <- paste(month.name, '15th')


# Get cities data
cities_df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')

cities_df <- dplyr::filter(cities_df,
                           pop > 100000)


# Plot a map for each month
ggplot(data = grid_with_pred) +
  theme_bw() +
  geom_polygon(data = us_shape,
               aes(long, lat, group = group),
               col = 'grey80', fill = 'black') +
  geom_point(aes(x = grid_with_pred$BEGIN_LON,
                 y = grid_with_pred$BEGIN_LAT,
                 col = grid_with_pred$DAMAGE_PROPERTY),
             size = 1,
             pch = 15,
             alpha = 0.95) +
  facet_wrap(~MONTH, ncol = 3) +
  viridis::scale_color_viridis('Predicted, Log-scale\nProperty Damage\n(US dollars + 1)') +
  theme(plot.title = element_text(size = 19, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 9/16,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'black'),
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(colour = 'grey80')) +
  labs(title = 'Maps of 2018 Tornado-Induced Property Damage Predictions',
       subtitle = 'Derived from the Best-Performing Neural Network') +
  geom_point(data = cities_df,
             aes(x = lon,
                 y = lat),
             col = 'black',
             pch = 16,
             size = 1/4)


