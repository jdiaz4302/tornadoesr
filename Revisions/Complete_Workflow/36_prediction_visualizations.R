


# Produced via:
# jupyter nbconvert --to script Revisions/Complete_Workflow/36_prediction_visualizations.ipynb
# Then minimally altered

library(ggplot2)

grid_df <- read.csv('data/raw/grid_with_predictions.csv')
cities_df <- read.csv('data/raw/cities_with_predictions.csv')

old_df <- read.csv('data/raw/tor_data_inflation_adj.csv')

lat_mu <- mean(old_df$BEGIN_LAT)
lon_mu <- mean(old_df$BEGIN_LON)
lat_sd <- sd(old_df$BEGIN_LAT)
lon_sd <- sd(old_df$BEGIN_LON)

grid_df$BEGIN_LON <- grid_df$BEGIN_LON*lon_sd+lon_mu
grid_df$BEGIN_LAT <- grid_df$BEGIN_LAT*lat_sd+lat_mu
cities_df$BEGIN_LON <- cities_df$BEGIN_LON*lon_sd+lon_mu
cities_df$BEGIN_LAT <- cities_df$BEGIN_LAT*lat_sd+lat_mu

pred_df <- rbind(grid_df, cities_df)

cmin <-  10000
cmax <- 250000

origin = paste0('2019-01-01')
out <- as.Date(pred_df$JULIAN_DAY, origin = origin)
months <- substr(as.character(out), start = 6, stop =7)

pred_df$MONTH <- months
worst_month <- which.max(aggregate(DAMAGE_PROPERTY~MONTH, pred_df, sum)$DAMAGE_PROPERTY)
worst_month

sub_grid_df <- grid_df[grid_df$JULIAN_DAY == unique(grid_df$JULIAN_DAY)[worst_month], ]
sub_cities_df <- cities_df[cities_df$JULIAN_DAY == unique(grid_df$JULIAN_DAY)[worst_month], ]

pred_df <- rbind(sub_grid_df, sub_cities_df)

states <- map_data("state")

png('1.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 0.05) +
  viridis::scale_color_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                               direction = -1, option = 'B',
                               breaks = c(200000,
                                          600000,
                                          1000000,
                                          1400000,
                                          1800000,
                                          2200000),
                               labels = c('$200,000',
                                          '$600,000',
                                          '$1,000,000',
                                          '$1,400,000',
                                          '$1,800,000',
                                          '$2,200,000')) + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = sub_grid_df, aes(x = BEGIN_LON,
                                     y = BEGIN_LAT,
                                     color = DAMAGE_PROPERTY),
             size = 0.2) +
  geom_point(data = sub_cities_df, aes(x = BEGIN_LON,
                                       y = BEGIN_LAT,
                                       color = DAMAGE_PROPERTY),
             size = 0.6, pch = 22) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0(worst_month, '-15-2019'))
dev.off()

library(akima)
library(reshape2)

fld <- with(pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROPERTY, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('2.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = states) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_fill_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black', direction = -1, option = 'B',
                              breaks = c(200000,
                                         400000,
                                         600000,
                                         800000,
                                         1000000,
                                         1200000,
                                         1400000,
                                         1600000,
                                         1800000),
                              labels = c('$200,000', '$400,000', '$600,000', '$800,000', '$1,000,000',
                                         '$1,200,000', '$1,400,000', '$1,600,000', '$1,800,000')) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0(worst_month, '-15-2019'))
dev.off()

png('3.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 0.05) +
  viridis::scale_color_viridis('Probability\nof damage', direction = -1, option = 'B',
                               limit = c(-0.01, 1.01)) + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = sub_grid_df, aes(x = BEGIN_LON,
                                     y = BEGIN_LAT,
                                     color = DAMAGE_PROB),
             size = 0.2) +
  geom_point(data = sub_cities_df, aes(x = BEGIN_LON,
                                       y = BEGIN_LAT,
                                       color = DAMAGE_PROB),
             size = 0.6, pch = 22) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0(worst_month, '-15-2019'))
dev.off()

fld <- with(pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROB, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('4.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = states) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = 'NA', lwd = 0.5) +
  viridis::scale_fill_viridis('Probability\nof damage', direction = -1, option = 'B',
                              limit = c(-0.01, 1.01)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0(worst_month, '-15-2019'))
dev.off()

KS <- subset(states, region %in% c("kansas"))

KS_grid_df <- dplyr::filter(grid_df, (min(KS$lat) - 1) <= BEGIN_LAT &
                              BEGIN_LAT <= (max(KS$lat) + 1))
KS_grid_df <- dplyr::filter(KS_grid_df, (min(KS$long) - 1) <= BEGIN_LON &
                              BEGIN_LON <= (max(KS$long) + 1))

KS_cities_df <- dplyr::filter(cities_df, (min(KS$lat) - 1) <= BEGIN_LAT &
                                BEGIN_LAT <= (max(KS$lat) + 1))
KS_cities_df <- dplyr::filter(KS_cities_df, (min(KS$long) - 1) <= BEGIN_LON &
                                BEGIN_LON <= (max(KS$long) + 1))

pred_df <- rbind(KS_grid_df, KS_cities_df)

origin = paste0('2019-01-01')
out <- as.Date(pred_df$JULIAN_DAY, origin = origin)
months <- substr(as.character(out), start = 6, stop =7)

pred_df$MONTH <- months
worst_month <- which.max(aggregate(DAMAGE_PROPERTY~MONTH, pred_df, sum)$DAMAGE_PROPERTY)
worst_month

KS_grid_df <- KS_grid_df[KS_grid_df$JULIAN_DAY == unique(KS_grid_df$JULIAN_DAY)[worst_month], ]
KS_cities_df <- KS_cities_df[KS_cities_df$JULIAN_DAY == unique(KS_cities_df$JULIAN_DAY)[worst_month], ]

KS_pred_df <- rbind(KS_grid_df, KS_cities_df)

png('5.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = KS) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                               breaks = c(#100000,
                                 200000,
                                 #300000,
                                 400000,
                                 #500000,
                                 600000,
                                 #700000,
                                 800000,
                                 #900000,
                                 1000000),
                               labels = c(#'$100,000',
                                 '$200,000',
                                 #'$300,000',
                                 '$400,000',
                                 #'$500,000',
                                 '$600,000',
                                 #'$700,000',
                                 '$800,000',
                                 #'$900,000',
                                 '$1,000,000'),
                               direction = -1, option = 'B') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = KS_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROPERTY),
             size = 2.5) +
  geom_point(data = KS_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROPERTY),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Kansas, ', worst_month, '-15-2019'))
dev.off()

fld <- with(KS_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROPERTY, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('6.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = KS) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                              breaks = c(#100000,
                                200000,
                                #300000,
                                400000,
                                #500000,
                                600000,
                                #700000,
                                800000,
                                #900000,
                                1000000),
                              labels = c(#'$100,000',
                                '$200,000',
                                #'$300,000',
                                '$400,000',
                                #'$500,000',
                                '$600,000',
                                #'$700,000',
                                '$800,000',
                                #'$900,000',
                                '$1,000,000'),
                              direction = -1, option = 'B') +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(KS$long) - 1, max(KS$long) + 1) +
  ylim(min(KS$lat) - 1, max(KS$lat) + 1) +
  labs(title = paste0('Kansas, ', worst_month, '-15-2019'))
dev.off()

png('7.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = KS) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Probability\nof damage', direction = -1, option = 'B',
                               limit = c(-0.01, 1.01)) + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = KS_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROB),
             size = 2.5) +
  geom_point(data = KS_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROB),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Kansas, ', worst_month, '-15-2019'))
dev.off()

fld <- with(KS_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROB, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('8.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = KS) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Probability\nof damage', direction = -1, option = 'B',
                              limit = c(-0.01, 1.01)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(KS$long) - 1, max(KS$long) + 1) +
  ylim(min(KS$lat) - 1, max(KS$lat) + 1) +
  labs(title = paste0('Kansas, ', worst_month, '-15-2019'))
dev.off()

round(min(KS_pred_df$DAMAGE_PROPERTY))
round(mean(KS_pred_df$DAMAGE_PROPERTY))
round(max(KS_pred_df$DAMAGE_PROPERTY))

mean(KS_pred_df$DAMAGE_PROB)

IL <- subset(states, region %in% c("illinois"))

IL_grid_df <- dplyr::filter(grid_df, (min(IL$lat) - 1) <= BEGIN_LAT &
                              BEGIN_LAT <= (max(IL$lat) + 1))
IL_grid_df <- dplyr::filter(IL_grid_df, (min(IL$long) - 1) <= BEGIN_LON &
                              BEGIN_LON <= (max(IL$long) + 1))

IL_cities_df <- dplyr::filter(cities_df, (min(IL$lat) - 1) <= BEGIN_LAT &
                                BEGIN_LAT <= (max(IL$lat) + 1))
IL_cities_df <- dplyr::filter(IL_cities_df, (min(IL$long) - 1) <= BEGIN_LON &
                                BEGIN_LON <= (max(IL$long) + 1))

pred_df <- rbind(IL_grid_df, IL_cities_df)
origin = paste0('2019-01-01')
out <- as.Date(pred_df$JULIAN_DAY, origin = origin)
months <- substr(as.character(out), start = 6, stop =7)

pred_df$MONTH <- months
worst_month <- which.max(aggregate(DAMAGE_PROPERTY~MONTH, pred_df, sum)$DAMAGE_PROPERTY)
worst_month

IL_grid_df <- IL_grid_df[IL_grid_df$JULIAN_DAY == unique(IL_grid_df$JULIAN_DAY)[worst_month], ]
IL_cities_df <- IL_cities_df[IL_cities_df$JULIAN_DAY == unique(IL_cities_df$JULIAN_DAY)[worst_month], ]

IL_pred_df <- rbind(IL_grid_df, IL_cities_df)

png('9.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = IL) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                               breaks = c(#100000,
                                 200000,
                                 #300000,
                                 400000,
                                 #500000,
                                 600000,
                                 #700000,
                                 800000,
                                 #900000,
                                 1000000),
                               labels = c(#'$100,000',
                                 '$200,000',
                                 #'$300,000',
                                 '$400,000',
                                 #'$500,000',
                                 '$600,000',
                                 #'$700,000',
                                 '$800,000',
                                 #'$900,000',
                                 '$1,000,000'),
                               direction = -1, option = 'B') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = IL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROPERTY),
             size = 2.5) +
  geom_point(data = IL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROPERTY),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Illinois, ', worst_month, '-15-2019'))
dev.off()

fld <- with(IL_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROPERTY, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('10.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = IL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                              breaks = c(#100000,
                                200000,
                                #300000,
                                400000,
                                #500000,
                                600000,
                                #700000,
                                800000,
                                #900000,
                                1000000),
                              labels = c(#'$100,000',
                                '$200,000',
                                #'$300,000',
                                '$400,000',
                                #'$500,000',
                                '$600,000',
                                #'$700,000',
                                '$800,000',
                                #'$900,000',
                                '$1,000,000'),
                              direction = -1, option = 'B')+
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(IL$long) - 1, max(IL$long) + 1) +
  ylim(min(IL$lat) - 1, max(IL$lat) + 1) +
  labs(title = paste0('Illinois, ', worst_month, '-15-2019'))
dev.off()

png('11.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = IL) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Probability\nof damage', direction = -1, option = 'B',
                               limit = c(-0.01, 1.01)) + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = IL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROB),
             size = 2.5) +
  geom_point(data = IL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROB),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Illinois, ', worst_month, '-15-2019'))
dev.off()

fld <- with(IL_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROB, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('12.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = IL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Probability\nof damage', direction = -1, option = 'B',
                              limit = c(-0.01, 1.01)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(IL$long) - 1, max(IL$long) + 1) +
  ylim(min(IL$lat) - 1, max(IL$lat) + 1) +
  labs(title = paste0('Illinois, ', worst_month, '-15-2019'))
dev.off()

round(min(IL_pred_df$DAMAGE_PROPERTY))
round(mean(IL_pred_df$DAMAGE_PROPERTY))
round(max(IL_pred_df$DAMAGE_PROPERTY))

mean(IL_pred_df$DAMAGE_PROB)

AL <- subset(states, region %in% c("alabama"))

AL_grid_df <- dplyr::filter(grid_df, (min(AL$lat) - 1) <= BEGIN_LAT &
                              BEGIN_LAT <= (max(AL$lat) + 1))
AL_grid_df <- dplyr::filter(AL_grid_df, (min(AL$long) - 1) <= BEGIN_LON &
                              BEGIN_LON <= (max(AL$long) + 1))

AL_cities_df <- dplyr::filter(cities_df, (min(AL$lat) - 1) <= BEGIN_LAT &
                                BEGIN_LAT <= (max(AL$lat) + 1))
AL_cities_df <- dplyr::filter(AL_cities_df, (min(AL$long) - 1) <= BEGIN_LON &
                                BEGIN_LON <= (max(AL$long) + 1))

pred_df <- rbind(AL_grid_df, AL_cities_df)
origin = paste0('2019-01-01')
out <- as.Date(pred_df$JULIAN_DAY, origin = origin)
months <- substr(as.character(out), start = 6, stop =7)

pred_df$MONTH <- months
worst_month <- which.max(aggregate(DAMAGE_PROPERTY~MONTH, pred_df, sum)$DAMAGE_PROPERTY)
worst_month

AL_grid_df <- AL_grid_df[AL_grid_df$JULIAN_DAY == unique(AL_grid_df$JULIAN_DAY)[worst_month], ]
AL_cities_df <- AL_cities_df[AL_cities_df$JULIAN_DAY == unique(AL_cities_df$JULIAN_DAY)[worst_month], ]

AL_pred_df <- rbind(AL_grid_df, AL_cities_df)

png('13.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = AL) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                               breaks = c(#100000,
                                 200000,
                                 #300000,
                                 400000,
                                 #500000,
                                 600000,
                                 #700000,
                                 800000,
                                 #900000,
                                 1000000),
                               labels = c(#'$100,000',
                                 '$200,000',
                                 #'$300,000',
                                 '$400,000',
                                 #'$500,000',
                                 '$600,000',
                                 #'$700,000',
                                 '$800,000',
                                 #'$900,000',
                                 '$1,000,000'),
                               direction = -1, option = 'B') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = AL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROPERTY),
             size = 2.5) +
  geom_point(data = AL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROPERTY),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Alabama, ', worst_month, '-15-2019'))
dev.off()

fld <- with(AL_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROPERTY, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('14.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = AL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                              breaks = c(#100000,
                                200000,
                                #300000,
                                400000,
                                #500000,
                                600000,
                                #700000,
                                800000,
                                #900000,
                                1000000),
                              labels = c(#'$100,000',
                                '$200,000',
                                #'$300,000',
                                '$400,000',
                                #'$500,000',
                                '$600,000',
                                #'$700,000',
                                '$800,000',
                                #'$900,000',
                                '$1,000,000'),
                              direction = -1, option = 'B')+
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(AL$long) - 1, max(AL$long) + 1) +
  ylim(min(AL$lat) - 1, max(AL$lat) + 1) +
  labs(title = paste0('Alabama, ', worst_month, '-15-2019'))
dev.off()

png('15.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = AL) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Probability\nof damage', direction = -1, option = 'B',
                               limit = c(-0.01, 1.01)) + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = AL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROB),
             size = 2.5) +
  geom_point(data = AL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROB),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Alabama, ', worst_month, '-15-2019'))
dev.off()

fld <- with(AL_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROB, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('16.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = AL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Probability\nof damage', direction = -1, option = 'B',
                              limit = c(-0.01, 1.01)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(AL$long) - 1, max(AL$long) + 1) +
  ylim(min(AL$lat) - 1, max(AL$lat) + 1) +
  labs(title = paste0('Alabama, ', worst_month, '-15-2019'))
dev.off()

round(min(AL_pred_df$DAMAGE_PROPERTY))
round(mean(AL_pred_df$DAMAGE_PROPERTY))
round(max(AL_pred_df$DAMAGE_PROPERTY))

mean(AL_pred_df$DAMAGE_PROB)

png('1.png', height = 1, width = 2, units = 'in', res = 300)
ggplot(data = AL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Probability\nof damage', direction = -1, option = 'B',
                              limit = c(-0.01, 1.01), alpha = 0.33) +
  geom_point(data = AL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROB),
             size = 2.5) +
  geom_point(data = AL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROB),
             size = 6, pch = 22, stroke = 1.5) +
  viridis::scale_color_viridis('Probability\nof damage', direction = -1, option = 'B',
                               limit = c(-0.01, 1.01)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(AL$long) - 1, max(AL$long) + 1) +
  ylim(min(AL$lat) - 1, max(AL$lat) + 1) +
  labs(title = paste0('Florida, ', worst_month, '-15-2019'))
dev.off()

OK <- subset(states, region %in% c("oklahoma"))

OK_grid_df <- dplyr::filter(grid_df, (min(OK$lat) - 1) <= BEGIN_LAT &
                              BEGIN_LAT <= (max(OK$lat) + 1))
OK_grid_df <- dplyr::filter(OK_grid_df, (min(OK$long) - 1) <= BEGIN_LON &
                              BEGIN_LON <= (max(OK$long) + 1))

OK_cities_df <- dplyr::filter(cities_df, (min(OK$lat) - 1) <= BEGIN_LAT &
                                BEGIN_LAT <= (max(OK$lat) + 1))
OK_cities_df <- dplyr::filter(OK_cities_df, (min(OK$long) - 1) <= BEGIN_LON &
                                BEGIN_LON <= (max(OK$long) + 1))

pred_df <- rbind(OK_grid_df, OK_cities_df)
origin = paste0('2019-01-01')
out <- as.Date(pred_df$JULIAN_DAY, origin = origin)
months <- substr(as.character(out), start = 6, stop =7)

pred_df$MONTH <- months
worst_month <- which.max(aggregate(DAMAGE_PROPERTY~MONTH, pred_df, sum)$DAMAGE_PROPERTY)
worst_month

OK_grid_df <- OK_grid_df[OK_grid_df$JULIAN_DAY == unique(OK_grid_df$JULIAN_DAY)[worst_month], ]
OK_cities_df <- OK_cities_df[OK_cities_df$JULIAN_DAY == unique(OK_cities_df$JULIAN_DAY)[worst_month], ]

OK_pred_df <- rbind(OK_grid_df, OK_cities_df)

png('17.png', height = 2, width = 2, units = 'in', res = 300)

ggplot(data = OK) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                               breaks = c(#100000,
                                 0, 200000,
                                 #300000,
                                 400000,
                                 #500000,
                                 600000,
                                 #700000,
                                 800000,
                                 #900000,
                                 1000000,
                                 1200000,
                                 1400000),
                               labels = c(#'$100,000',
                                 '$0', '$200,000',
                                 #'$300,000',
                                 '$400,000',
                                 #'$500,000',
                                 '$600,000',
                                 #'$700,000',
                                 '$800,000',
                                 #'$900,000',
                                 '$1,000,000',
                                 '$1,200,000', '$1,400,000'),
                               direction = -1, option = 'B') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = OK_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROPERTY),
             size = 2.5) +
  geom_point(data = OK_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROPERTY),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Oklahoma, ', worst_month, '-15-2019'))
dev.off()

fld <- with(OK_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROPERTY, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('18.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = OK) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                              breaks = c(#100000,
                                0, 200000,
                                #300000,
                                400000,
                                #500000,
                                600000,
                                #700000,
                                800000,
                                #900000,
                                1000000,
                                1200000, 1400000),
                              labels = c(#'$100,000',
                                '$0',
                                '$200,000',
                                #'$300,000',
                                '$400,000',
                                #'$500,000',
                                '$600,000',
                                #'$700,000',
                                '$800,000',
                                #'$900,000',
                                '$1,000,000',
                                '$1,200,000', '$1,400,000'),
                              direction = -1, option = 'B') +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(OK$long) - 1, max(OK$long) + 1) +
  ylim(min(OK$lat) - 1, max(OK$lat) + 1) +
  labs(title = paste0('Oklahoma, ', worst_month, '-15-2019'))
dev.off()

png('19.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = OK) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Probability\nof damage', direction = -1, option = 'B',
                               limit = c(-0.01, 1.01)) + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = OK_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROB),
             size = 2.5) +
  geom_point(data = OK_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROB),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Oklahoma, ', worst_month, '-15-2019'))
dev.off()

fld <- with(OK_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROB, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('20.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = OK) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Probability\nof damage', direction = -1, option = 'B',
                              limit = c(-0.01, 1.01)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(OK$long) - 1, max(OK$long) + 1) +
  ylim(min(OK$lat) - 1, max(OK$lat) + 1) +
  labs(title = paste0('Oklahoma, ', worst_month, '-15-2019'))
dev.off()

round(min(OK_pred_df$DAMAGE_PROPERTY))
round(mean(OK_pred_df$DAMAGE_PROPERTY))
round(max(OK_pred_df$DAMAGE_PROPERTY))

mean(OK_pred_df$DAMAGE_PROB)

FL <- subset(states, region %in% c("florida"))

FL_grid_df <- dplyr::filter(grid_df, (min(FL$lat) - 1) <= BEGIN_LAT &
                              BEGIN_LAT <= (max(FL$lat) + 1))
FL_grid_df <- dplyr::filter(FL_grid_df, (min(FL$long) - 1) <= BEGIN_LON &
                              BEGIN_LON <= (max(FL$long) + 1))

FL_cities_df <- dplyr::filter(cities_df, (min(FL$lat) - 1) <= BEGIN_LAT &
                                BEGIN_LAT <= (max(FL$lat) + 1))
FL_cities_df <- dplyr::filter(FL_cities_df, (min(FL$long) - 1) <= BEGIN_LON &
                                BEGIN_LON <= (max(FL$long) + 1))

pred_df <- rbind(FL_grid_df, FL_cities_df)
origin = paste0('2019-01-01')
out <- as.Date(pred_df$JULIAN_DAY, origin = origin)
months <- substr(as.character(out), start = 6, stop =7)

pred_df$MONTH <- months
worst_month <- which.max(aggregate(DAMAGE_PROPERTY~MONTH, pred_df, sum)$DAMAGE_PROPERTY)
worst_month

FL_grid_df <- FL_grid_df[FL_grid_df$JULIAN_DAY == unique(FL_grid_df$JULIAN_DAY)[worst_month], ]
FL_cities_df <- FL_cities_df[FL_cities_df$JULIAN_DAY == unique(FL_cities_df$JULIAN_DAY)[worst_month], ]

FL_pred_df <- rbind(FL_grid_df, FL_cities_df)

png('21.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = FL) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                               breaks = c(#100000,
                                 200000,
                                 #300000,
                                 400000,
                                 #500000,
                                 600000,
                                 #700000,
                                 800000,
                                 #900000,
                                 1000000),
                               labels = c(#'$100,000',
                                 '$200,000',
                                 #'$300,000',
                                 '$400,000',
                                 #'$500,000',
                                 '$600,000',
                                 #'$700,000',
                                 '$800,000',
                                 #'$900,000',
                                 '$1,000,000'),
                               direction = -1, option = 'B') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = FL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROPERTY),
             size = 2.5) +
  geom_point(data = FL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROPERTY),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Florida, ', worst_month, '-15-2019'))
dev.off()

fld <- with(FL_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROPERTY, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('22.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = FL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Conditional property\ndamage', limit = c(0, 250000), na.value = 'black',
                              breaks = c(#100000,
                                200000,
                                #300000,
                                400000,
                                #500000,
                                600000,
                                #700000,
                                800000,
                                #900000,
                                1000000),
                              labels = c(#'$100,000',
                                '$200,000',
                                #'$300,000',
                                '$400,000',
                                #'$500,000',
                                '$600,000',
                                #'$700,000',
                                '$800,000',
                                #'$900,000',
                                '$1,000,000'),
                              direction = -1, option = 'B') +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(FL$long) - 1, max(FL$long) + 1) +
  ylim(min(FL$lat) - 1, max(FL$lat) + 1) +
  labs(title = paste0('Florida, ', worst_month, '-15-2019'))
dev.off()

png('23.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = FL) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "cyan", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Probability\nof damage', direction = -1, option = 'B',
                               limit = c(-0.01, 1.01)) + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = FL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROB),
             size = 2.5) +
  geom_point(data = FL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROB),
             size = 6, pch = 22, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  labs(title = paste0('Florida, ', worst_month, '-15-2019'))
dev.off()

fld <- with(FL_pred_df, interp(x = BEGIN_LON, y = BEGIN_LAT, z = DAMAGE_PROB, duplicate = TRUE))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "dam")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('24.png', height = 2, width = 2, units = 'in', res = 300)
ggplot(data = FL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Probability\nof damage', direction = -1, option = 'B',
                              limit = c(-0.01, 1.01))+
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(FL$long) - 1, max(FL$long) + 1) +
  ylim(min(FL$lat) - 1, max(FL$lat) + 1) +
  labs(title = paste0('Florida, ', worst_month, '-15-2019'))
dev.off()

round(min(FL_pred_df$DAMAGE_PROPERTY))
round(mean(FL_pred_df$DAMAGE_PROPERTY))
round(max(FL_pred_df$DAMAGE_PROPERTY))
mean(FL_pred_df$DAMAGE_PROB)


ggplot(data = FL) + 
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0, color = "white", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Conditional\nproperty\ndamage', limit = c(cmin - 1, cmax),
                               direction = -1, option = 'B',
                               breaks = c(cmin - 0.1, 50000,
                                          100000, 150000,
                                          200000, 250000),
                               labels = c('less than\n$10,000', '$50,000',
                                          '$100,000', '$150,000',
                                          '$200,000', 'greater than\n$250,000')) + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = FL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROPERTY),
             size = 3, alpha = 0) +
  geom_point(data = FL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROPERTY),
             size = 6, pch = 22, stroke = 1.5, alpha = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'top',
        legend.key.size = unit(1, 'in'))

ggplot(data = FL) + 
  geom_polygon(aes(x = long, y = lat, group = group), alpha = 0, color = "white", fill = 'NA', lwd = 0.5) +
  viridis::scale_color_viridis('Probability\nof damage', direction = -1, option = 'B',
                               limit = c(-0.0001, 1.0001), na.value = 'black') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = FL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROB),
             size = 3, alpha = 0) +
  geom_point(data = FL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROB),
             size = 6, pch = 22, stroke = 1.5, alpha = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'top',
        legend.key.size = unit(1, 'in'))

png('2.png', height = 8, width = 8, units = 'in', res = 300)
ggplot(data = FL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = dam)) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Probability\nof damage', direction = -1, option = 'B',
                              limit = c(-0.01, 1.01), alpha = 0.33) +
  geom_point(data = FL_grid_df, aes(x = BEGIN_LON,
                                    y = BEGIN_LAT,
                                    color = DAMAGE_PROB),
             size = 2.5) +
  geom_point(data = FL_cities_df, aes(x = BEGIN_LON,
                                      y = BEGIN_LAT,
                                      color = DAMAGE_PROB),
             size = 6, pch = 22, stroke = 1.5) +
  viridis::scale_color_viridis('Probability\nof damage', direction = -1, option = 'B',
                               limit = c(-0.01, 1.01)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(FL$long) - 1, max(FL$long) + 1) +
  ylim(min(FL$lat) - 1, max(FL$lat) + 1) +
  labs(title = paste0('Florida, ', worst_month, '-15-2019'))
dev.off()


