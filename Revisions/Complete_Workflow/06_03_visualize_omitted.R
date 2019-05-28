


# Viz package
library(ggplot2)


# Import the data with social variables that has not be na.omit'd yet
tor_df <- read.csv('data/raw/tor_data_with_ACS.csv')


# Find the rows that are omitted due to na.omit
na_df <- tor_df[!complete.cases(tor_df), ]


# Map of missing values
png('omitted_map.png', width = 8, height = 8, units = 'in', res = 300)
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = 'NA', lwd = 0.05) + 
  viridis::scale_color_viridis('Reported property\ndamage', direction = -1,
                               breaks = c(0, 2, 4, 6),
                               labels = c('$0', '$100', '$10,000', '$1,000,000')) +
  coord_quickmap() +
  guides(fill = FALSE) +
  geom_point(data = na_df, aes(x = BEGIN_LON,
                               y = BEGIN_LAT,
                               col = log10(DAMAGE_PROPERTY + 1)),
             size = 10, pch = 21, stroke = 1.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = 'Omitted tornadoes')
dev.off()


# Distribution of retrained property damages
png('omitted_dist.png', width = 8, height = 8, units = 'in', res = 300)
par(pty = 's')
hist(log10(acs_tor_df$DAMAGE_PROPERTY + 1),
     breaks = seq(0, 10, length.out = 25),
     col = rgb(red = 0, green = 0, blue = 1, alpha = 0.75),
     xlab = 'Reported property damage', xaxt = "n", freq = FALSE,
     ylim = c(0, 1), main = '')
abline(v = mean(log10(acs_tor_df$DAMAGE_PROPERTY + 1)), col = 'blue', lwd = 5)
# Giving the axis readable ticks
axis(1, c(0, 2, 4, 6, 8),
     labels = c('$0', '$100', '$10,000', '$1,000,000', '$100,000,000'),
     tick = FALSE, padj= -1.5)
# Adding the distribution of omitted property damages
hist(log10(na_df$DAMAGE_PROPERTY + 1),
     freq = FALSE, breaks = seq(0, 10, length.out = 25),
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5),
     add = TRUE)
abline(v = mean(log10(na_df$DAMAGE_PROPERTY + 1)), col = 'red', lwd = 5)
# Legend for reference
legend("topright",
       c("Retained (n = 22,048)", "Omitted (n = 75)"),
       fill=c(rgb(red = 0, green = 0, blue = 1, alpha = 0.75),
              rgb(red = 1, green = 0, blue = 0, alpha = 0.75)))
dev.off()


