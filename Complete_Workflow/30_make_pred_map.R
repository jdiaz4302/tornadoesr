


# Packages
library(dplyr)
library(sp)
library(raster)
library(ggplot2)
library(animation)
library(gganimate)


# Import the grid points
# And remove the pandas-induced index column
grid_with_pred <- read.csv('Complete_Workflow/grid_with_expectated_values.csv') %>%
  dplyr::select(-X)


# Import the unprocessed model data
tor_df <- read.csv("data/raw/tor_data_with_derived.csv")


# Undo the processing to get lat and lon in natural scale
grid_with_pred$BEGIN_LAT <- grid_with_pred$BEGIN_LAT * sd(tor_df$BEGIN_LAT) +
  mean(tor_df$BEGIN_LAT)
grid_with_pred$BEGIN_LON <- grid_with_pred$BEGIN_LON * sd(tor_df$BEGIN_LON) +
  mean(tor_df$BEGIN_LON)


# Undo only the mean normalization on property damage, exponentiate,
# Then convert to log10 scale
grid_with_pred$DAMAGE_PROPERTY <- log10(grid_with_pred$DAMAGE_PROPERTY)
 

# Damages by month
ggplot(grid_with_pred,
       aes(x = JULIAN_DAY,
           y = DAMAGE_PROPERTY)) +
  geom_point(alpha = 0.2) +
  theme_bw() +
  labs(x = 'Day of the Year',
       y = 'Expected Tornado-Induced Property Damage\n(Log-scale US dollars)') +
  stat_smooth()

 
# Have the gridded points as a SpatialPointsDataFrame
grid_points <- SpatialPointsDataFrame(coords = grid_with_pred[, c('BEGIN_LON',
                                                                  'BEGIN_LAT')],
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


# Make the Julian Day integers into a factor
# And give them better label names (X 15th); where X = a month
grid_with_pred$JULIAN_DAY <- factor(grid_with_pred$JULIAN_DAY)

levels(grid_with_pred$JULIAN_DAY) <- paste(month.name,
                                           rep('15th'))


# Save the output figure with great resolution
tiff("Figure5.tiff", width = 8, height = 8, units = 'in', res = 450)


# Plot a map for each month
ggplot(grid_with_pred,
       aes(x = BEGIN_LON,
           y = BEGIN_LAT,
           col = DAMAGE_PROPERTY)) +
  theme_bw() +
  geom_polygon(data = us_shape,
               aes(long, lat, group = group),
               col = 'grey20', fill = 'black') +
  geom_point(size = 0.65,
             pch = 15) +
  facet_wrap(~JULIAN_DAY, ncol = 3) +
  viridis::scale_color_viridis('Expected\nLog10-Scale\nProperty Damage\n(US dollars + 1)') +
  theme(plot.title = element_text(size = 19, hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 9/16,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'black'),
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(colour = 'grey80'))


# Finish the save
dev.off()


# Some stuff that may be useful in the future

# id_total <- nrow(grid_with_pred) / 12
# grid_with_pred$ID <- rep(1:id_total, 12)
# grid_with_pred$EASE <- rep("cubic-in-out",
#                           nrow(grid_with_pred))
# grid_with_pred$MONTH <- as.numeric(grid_with_pred$MONTH)

# library(tweenr)
# levels(grid_with_pred$MONTH) <- 1:12

# grid_tween <- tween_elements(grid_with_pred,
#                             time = 'MONTH', group = 'ID',
#                             ease = 'EASE', nframes = 96)


# Make a gif with 2018 months as frames
testing <- ggplot(grid_with_pred,
                  aes(x = BEGIN_LON,
                      y = BEGIN_LAT,
                      frame = as.Date(grid_with_pred$JULIAN_DAY,
                                      origin = '2017-12-31'))) +
  geom_point(aes(col = DAMAGE_PROPERTY),
             pch = 16, size = 2.75) +
  viridis::scale_color_viridis('Log Scale\nProperty Damage', option = 'B', direction = -1,
                               breaks = c(3, 4, 5, 6),
                               labels = c('$1,000', '$10,000', '$100,000', '$1,000,000')) +
  theme_minimal() +
  ggtitle("Expected Tornado-Induced Property Damage,") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 9/16,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22, color = 'white'),
        panel.background = element_rect(fill = 'black'),
        plot.background = element_rect(fill = 'black'),
        legend.text = element_text(size = 16, color = 'white'),
        legend.title = element_text(size = 16, color = 'white'))


# Animate the gif
gganimate(testing,
          filename = 'prediction_map.gif',
          interval = 0.3,
          ani.width = 1725,
          ani.height = 950)


