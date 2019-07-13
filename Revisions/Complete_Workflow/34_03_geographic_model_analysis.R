
test_set_perf <- read.csv('data/raw/final_test_set_perf.csv')

old_df <- read.csv('data/raw/tor_data_inflation_adj.csv')

lat_mu <- mean(old_df$BEGIN_LAT)
lat_sd <- sd(old_df$BEGIN_LAT)
lon_mu <- mean(old_df$BEGIN_LON)
lon_sd <- sd(old_df$BEGIN_LON)
dam_mu <- mean(log(old_df$DAMAGE_PROPERTY + 1))
dam_sd <- sd(log(old_df$DAMAGE_PROPERTY + 1))

test_set_perf$lat <- test_set_perf$lat*lat_sd+lat_mu
test_set_perf$lon <- test_set_perf$lon*lon_sd+lon_mu

test_set_perf$true <- exp(test_set_perf$true*dam_sd+dam_mu) - 1
test_set_perf$pred <- exp(test_set_perf$pred*dam_sd+dam_mu) - 1

test_set_perf$resid_col <- log10(sqrt((test_set_perf$pred - test_set_perf$true)^2) + 1)
test_set_perf[test_set_perf$resid_col < 3, ]$resid_col <- 2.9
test_set_perf[test_set_perf$resid_col > 6, ]$resid_col <- 6.1

head(test_set_perf)

library(ggplot2)

states <- map_data("state")

library(akima)
library(reshape2)

fld <- with(test_set_perf, interp(x = lon, y = lat, z = log10(sqrt((true - pred)^2) + 1), duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('1.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = states) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 0.5) +
  viridis::scale_fill_viridis('Absolute\nresidual\n\n',
                              breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                              labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                              direction = -1, option = 'B', na.value = 'cyan') +
  viridis::scale_color_viridis('Absolute\nresidual\n\n', limit = c(2.8, 6.2),
                               breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                               labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                               direction = -1, option = 'B', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = test_set_perf, aes(x = lon,
                                       y = lat,
                                       color = resid_col),
             size = 0.5, pch = 16, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none')
dev.off()


fld <- with(test_set_perf, interp(x = lon, y = lat, z = (dam_ind - probs)^2, duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('3.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = states) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 0.5) +
  viridis::scale_fill_viridis('Squared\nresidual\n\n',
                              breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                              direction = -1, option = 'D', na.value = 'cyan') +
  viridis::scale_color_viridis('Squared\nresidual\n\n',
                               breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                               direction = -1, option = 'D', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = test_set_perf, aes(x = lon,
                                       y = lat,
                                       color = (dam_ind - probs)^2),
             size = 0.5, pch = 16, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none')
dev.off()

KS <- subset(states, region %in% c("kansas"))

KS_test_df <- dplyr::filter(test_set_perf, (min(KS$lat) - 1) <= lat &
                              lat <= (max(KS$lat) + 1))
KS_test_df <- dplyr::filter(KS_test_df, (min(KS$long) - 1) <= lon &
                              lon <= (max(KS$long) + 1))


fld <- with(KS_test_df, interp(x = lon, y = lat, z = log10(sqrt((true - pred)^2) + 1), duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('2.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = KS) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Absolute\nresidual\n\n',
                              breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                              labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                              direction = -1, option = 'B', na.value = 'cyan') +
  viridis::scale_color_viridis('Absolute\nresidual\n\n', limit = c(2.8, 6.2),
                               breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                               labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                               direction = -1, option = 'B', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = KS_test_df, aes(x = lon,
                                    y = lat,
                                    color = resid_col),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(KS$long) - 1, max(KS$long) + 1) +
  ylim(min(KS$lat) - 1, max(KS$lat) + 1) +
  labs(title = 'Kansas')
dev.off()



fld <- with(KS_test_df, interp(x = lon, y = lat, z = (dam_ind - probs)^2, duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('4.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = KS) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Squared\nresidual\n\n',
                              breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                              direction = -1, option = 'D', na.value = 'cyan') +
  viridis::scale_color_viridis('Squared\nresidual\n\n',
                               breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                               direction = -1, option = 'D', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = KS_test_df, aes(x = lon,
                                    y = lat,
                                    color = (dam_ind - probs)^2),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(KS$long) - 1, max(KS$long) + 1) +
  ylim(min(KS$lat) - 1, max(KS$lat) + 1) +
  labs(title = 'Kansas')
dev.off()

IL <- subset(states, region %in% c("illinois"))

IL_test_df <- dplyr::filter(test_set_perf, (min(IL$lat) - 1) <= lat &
                              lat <= (max(IL$lat) + 1))
IL_test_df <- dplyr::filter(IL_test_df, (min(IL$long) - 1) <= lon &
                              lon <= (max(IL$long) + 1))


fld <- with(IL_test_df, interp(x = lon, y = lat, z = log10(sqrt((true - pred)^2) + 1), duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('5.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = IL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Absolute\nresidual\n\n',
                              breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                              labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                              direction = -1, option = 'B', na.value = 'cyan') +
  viridis::scale_color_viridis('Absolute\nresidual\n\n', limit = c(2.8, 6.2),
                               breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                               labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                               direction = -1, option = 'B', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = IL_test_df, aes(x = lon,
                                    y = lat,
                                    color = resid_col),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(IL$long) - 1, max(IL$long) + 1) +
  ylim(min(IL$lat) - 1, max(IL$lat) + 1) +
  labs(title = 'Illinois')
dev.off()


fld <- with(IL_test_df, interp(x = lon, y = lat, z = (dam_ind - probs)^2, duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('7.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = IL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Squared\nresidual\n\n',
                              breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                              direction = -1, option = 'D', na.value = 'cyan') +
  viridis::scale_color_viridis('Squared\nresidual\n\n',
                               breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                               direction = -1, option = 'D', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = IL_test_df, aes(x = lon,
                                    y = lat,
                                    color = (dam_ind - probs)^2),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(IL$long) - 1, max(IL$long) + 1) +
  ylim(min(IL$lat) - 1, max(IL$lat) + 1) +
  labs(title = 'Illinois')
dev.off()

AL <- subset(states, region %in% c("alabama"))

AL_test_df <- dplyr::filter(test_set_perf, (min(AL$lat) - 1) <= lat &
                              lat <= (max(AL$lat) + 1))
AL_test_df <- dplyr::filter(AL_test_df, (min(AL$long) - 1) <= lon &
                              lon <= (max(AL$long) + 1))



fld <- with(AL_test_df, interp(x = lon, y = lat, z = log10(sqrt((true - pred)^2) + 1), duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('6.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = AL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Absolute\nresidual\n\n',
                              breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                              labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                              direction = -1, option = 'B', na.value = 'cyan') +
  viridis::scale_color_viridis('Absolute\nresidual\n\n', limit = c(2.8, 6.2),
                               breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                               labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                               direction = -1, option = 'B', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = AL_test_df, aes(x = lon,
                                    y = lat,
                                    color = resid_col),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(AL$long) - 1, max(AL$long) + 1) +
  ylim(min(AL$lat) - 1, max(AL$lat) + 1) +
  labs(title = 'Alabama')
dev.off()

fld <- with(AL_test_df, interp(x = lon, y = lat, z = (dam_ind - probs)^2, duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('8.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = AL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Squared\nresidual\n\n',
                              breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                              direction = -1, option = 'D', na.value = 'cyan') +
  viridis::scale_color_viridis('Squared\nresidual\n\n',
                               breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                               direction = -1, option = 'D', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = AL_test_df, aes(x = lon,
                                    y = lat,
                                    color = (dam_ind - probs)^2),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(AL$long) - 1, max(AL$long) + 1) +
  ylim(min(AL$lat) - 1, max(AL$lat) + 1) +
  labs(title = 'Alabama')
dev.off()

OK <- subset(states, region %in% c("oklahoma"))

OK_test_df <- dplyr::filter(test_set_perf, (min(OK$lat) - 1) <= lat &
                              lat <= (max(OK$lat) + 1))
OK_test_df <- dplyr::filter(OK_test_df, (min(OK$long) - 1) <= lon &
                              lon <= (max(OK$long) + 1))


fld <- with(OK_test_df, interp(x = lon, y = lat, z = log10(sqrt((true - pred)^2) + 1), duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('9.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = OK) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Absolute\nresidual\n\n',
                              breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                              labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                              direction = -1, option = 'B', na.value = 'cyan') +
  viridis::scale_color_viridis('Absolute\nresidual\n\n', limit = c(2.8, 6.2),
                               breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                               labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                               direction = -1, option = 'B', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = OK_test_df, aes(x = lon,
                                    y = lat,
                                    color = resid_col),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(OK$long) - 1, max(OK$long) + 1) +
  ylim(min(OK$lat) - 1, max(OK$lat) + 1) +
  labs(title = 'Oklahoma')
dev.off()

fld <- with(OK_test_df, interp(x = lon, y = lat, z = (dam_ind - probs)^2, duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('11.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = OK) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Squared\nresidual\n\n',
                              breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                              direction = -1, option = 'D', na.value = 'cyan') +
  viridis::scale_color_viridis('Squared\nresidual\n\n',
                               breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                               direction = -1, option = 'D', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = OK_test_df, aes(x = lon,
                                    y = lat,
                                    color = (dam_ind - probs)^2),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(OK$long) - 1, max(OK$long) + 1) +
  ylim(min(OK$lat) - 1, max(OK$lat) + 1) +
  labs(title = 'Oklahoma')
dev.off()

FL <- subset(states, region %in% c("florida"))

FL_test_df <- dplyr::filter(test_set_perf, (min(FL$lat) - 1) <= lat &
                              lat <= (max(FL$lat) + 1))
FL_test_df <- dplyr::filter(FL_test_df, (min(FL$long) - 1) <= lon &
                              lon <= (max(FL$long) + 1))

fld <- with(FL_test_df, interp(x = lon, y = lat, z = log10(sqrt((true - pred)^2) + 1), duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('10.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = FL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Absolute\nresidual\n\n',
                              breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                              labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                              direction = -1, option = 'B', na.value = 'cyan') +
  viridis::scale_color_viridis('Absolute\nresidual\n\n', limit = c(2.8, 6.2),
                               breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                               labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                               direction = -1, option = 'B', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = FL_test_df, aes(x = lon,
                                    y = lat,
                                    color = resid_col),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(FL$long) - 1, max(FL$long) + 1) +
  ylim(min(FL$lat) - 1, max(FL$lat) + 1) +
  labs(title = 'Florida')
dev.off()

fld <- with(FL_test_df, interp(x = lon, y = lat, z = (dam_ind - probs)^2, duplicate = 'mean'))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "resid")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]
png('12.png', height = 4, width = 4, units = 'in', res = 300)
ggplot(data = FL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "gray50", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Squared\nresidual\n\n',
                              breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                              direction = -1, option = 'D', na.value = 'cyan') +
  viridis::scale_color_viridis('Squared\nresidual\n\n',
                               breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                               direction = -1, option = 'D', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = FL_test_df, aes(x = lon,
                                    y = lat,
                                    color = (dam_ind - probs)^2),
             size = 1.5, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'none') +
  xlim(min(FL$long) - 1, max(FL$long) + 1) +
  ylim(min(FL$lat) - 1, max(FL$lat) + 1) +
  labs(title = 'Florida')
dev.off()

png('legend1.png', height = 4, width = 8, units = 'in', res = 300)
ggplot(data = FL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "NA", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Absolute\nresidual\n\n',
                              breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                              labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                              direction = -1, option = 'B', na.value = 'cyan') +
  viridis::scale_color_viridis('Absolute\nresidual\n\n', limit = c(2.8, 6.2),
                               breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                               labels = c('$10', '$100', 'less than\n$1,000', '$10,000', '$100,000', 'greater than\n$1,000,000', '$10,000,000', '$100,000,000', '$1,000,000,000'),
                               direction = -1, option = 'B', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = FL_test_df, aes(x = lon,
                                    y = lat,
                                    color = resid_col),
             size = 1.5, pch = 21, stroke = 1.5, alpha = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 14),
        axis.title = element_blank(), legend.title = element_text(size = 14),
        axis.text = element_blank(), legend.text = element_text(size = 12),
        axis.ticks = element_blank(), legend.position = 'top',
        legend.key.height = unit(0.5, 'in'),
        legend.key.width = unit(1, 'in'))
dev.off()


png('legend2.png', height = 4, width = 8, units = 'in', res = 300)
ggplot(data = FL) + 
  coord_quickmap() +
  geom_tile(data = df, aes(x = Lon, y = Lat, fill = resid), alpha = 0) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "NA", fill = 'NA', lwd = 1) +
  viridis::scale_fill_viridis('Squared\nresidual\n\n',
                              breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                              direction = -1, option = 'D', na.value = 'cyan') +
  viridis::scale_color_viridis('Squared\nresidual\n\n',
                               breaks = c(0, 0.25, 0.5, 0.75, 1), limit = c(-0.01, 1.01),
                               direction = -1, option = 'D', na.value = 'cyan') + 
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = FL_test_df, aes(x = lon,
                                    y = lat,
                                    color = (dam_ind - probs)^2),
             size = 1.5, pch = 21, stroke = 1.5, alpha = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 14),
        axis.title = element_blank(), legend.title = element_text(size = 14),
        axis.text = element_blank(), legend.text = element_text(size = 12),
        axis.ticks = element_blank(), legend.position = 'top',
        legend.key.height = unit(0.5, 'in'),
        legend.key.width = unit(1, 'in'))
dev.off()
