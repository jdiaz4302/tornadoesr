


# Packages
library(ggplot2)
library(gganimate)
library(ggmap)
library(viridis)
library(animation)


# Call the data
source("R/3_format_for_crop_dam_visualizations.R")


# Get rid of 2017 since its an incomplete year
tor_df <- dplyr::filter(tor_df,
                        YEAR != 2017)


# Get property damage per year
DamPerState <- aggregate(tor_df$DAMAGE_CROPS,
                         by = list(Category = tor_df$STATE),
                         FUN = sum)


# Log transform the property damage total per state
DamPerState$x <- log(DamPerState$x + 1,
                     base = exp(1))


# Some formatting
# Make state a factor rather than character
DamPerState$Category <- as.factor(DamPerState$Category)


# Get the value for the state with the 10th most
# tornado-induced property damage
top_10_states_cutoff <- sort(DamPerState$x, decreasing = TRUE)[10]


# Keep only the top 10 states
DamPerState <- filter(DamPerState,
                      x >= top_10_states_cutoff)


# Get those state names
state_names <- DamPerState$Category


# Reset factor levels
state_names <- as.character(state_names)


# Get dataframe for each state
ALA_df <- filter(tor_df,
                 STATE == state_names[1])

ARK_df <- filter(tor_df,
                 STATE == state_names[2])

GA_df <- filter(tor_df,
                STATE == state_names[3])

ILL_df <- filter(tor_df,
                 STATE == state_names[4])

KANSAS_df <- filter(tor_df,
                    STATE == state_names[5])

MISSIP_df <- filter(tor_df,
                    STATE == state_names[6])

MISSOURI_df <- filter(tor_df,
                      STATE == state_names[7])

OK_df <- filter(tor_df,
                STATE == state_names[8])

TENN_df <- filter(tor_df,
                  STATE == state_names[9])

TX_df <- filter(tor_df,
                STATE == state_names[10])


# Combine them
intermediate_1 <- rbind(TX_df, TENN_df)

inter_2 <- rbind(intermediate_1, OK_df)

inter_3 <- rbind(inter_2, KANSAS_df)

inter_4 <- rbind(inter_3, MISSOURI_df)

inter_5 <- rbind(inter_4, MISSIP_df)

inter_6 <- rbind(inter_5, ILL_df)

inter_7 <- rbind(inter_6, GA_df)

inter_8 <- rbind(inter_7, ARK_df)

tor_top_10_df <- rbind(inter_8, ALA_df)


# Log transform property damage for better visualization
tor_top_10_df$DAMAGE_CROPS <- log(tor_top_10_df$DAMAGE_CROPS + 1,
                                     base = exp(1))

# Make STATE a factor
tor_top_10_df$STATE <- as.factor(tor_top_10_df$STATE)


# Plot crop damage by state
ggplot(tor_top_10_df,
       aes(x = STATE,
           y = DAMAGE_CROPS,
           col = STATE)) +
  theme_bw() +
  geom_jitter(width = 0.10,
              height = 0.10,
              size = 3,
              alpha = .4) +
  stat_summary(aes(y = DAMAGE_CROPS, group = 1),
               fun.y = mean,
               col = "grey25",
               group = 1,
               geom = "point",
               size = 10,
               shape = 15,
               alpha = 0.85) +
  labs(x = "State",
       y = "Log-Transformed Crop Damage (US dollars)",
       title = "Tornado-Induced Crop Damage",
       subtitle = "Per Event in the Top 10 Most Damaged States") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 14),
        legend.position = "none",
        aspect.ratio = 15/27)


# Making another gif
# Set bounds for continental US
US_bounds <- c(left = -125, bottom = 23, right = -66, top = 50)


# Get the base map
US_map <- get_stamenmap(US_bounds, zoom = 5, maptype = "toner-lite")


# Plot the tornado events on it
map_plot <- ggmap(US_map) +
  geom_jitter(data = tor_df,
              aes(x = BEGIN_LON,
                  y = BEGIN_LAT,
                  fill = log(DAMAGE_CROPS + 1,
                             base = exp(1))),
              width = 0.05,
              height = 0.05,
              pch = 21,
              col = "black",
              size = 2) +
  theme_bw() +
  scale_fill_gradientn(colours = inferno(5, direction = 1)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Tornado-Induced Crop Damage by Event",
       fill = "Log Transformed Crop Damage (US dollars)",
       size = "Log Transformed Crop Damage (US dollars)") +
  theme(plot.title = element_text(hjust = 0.5, size = 27),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))


# View it
map_plot


# Map gif - by year
map_gif <- ggmap(US_map) +
  geom_jitter(data = tor_df,
              aes(x = BEGIN_LON,
                  y = BEGIN_LAT,
                  fill = log(DAMAGE_CROPS + 1,
                             base = exp(1)),
                  frame = YEAR),
              width = 0.05,
              height = 0.05,
              pch = 21,
              col = "black",
              size = 5) +
  theme_bw() +
  scale_fill_gradientn(colours = inferno(5, direction = 1)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Tornado-Induced Crop Damage by Event in:",
       fill = "Log Transformed Crop Damage (US dollars)",
       size = "Log Transformed Crop Damage (US dollars)") +
  theme(plot.title = element_text(hjust = 0.5, size = 27),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))


# Animate it
gganimate::gganimate(map_gif,
                     ani.width = 1725,
                     ani.height = 950,
                     interval = 1.75)
# Save it
# "images/plot6.gif"


# Format month as factor
tor_df$MONTH_NAME <- as.factor(tor_df$MONTH_NAME)


# Give it proper sequence
levels(tor_df$MONTH_NAME) <- month.name


# Map gif - by month
map_gif <- ggmap(US_map) +
  geom_jitter(data = tor_df,
              aes(x = BEGIN_LON,
                  y = BEGIN_LAT,
                  fill = log(DAMAGE_CROPS + 1,
                             base = exp(1)),
                  frame = MONTH_NAME),
              width = 0.05,
              height = 0.05,
              pch = 21,
              col = "black",
              size = 5) +
  theme_bw() +
  scale_fill_gradientn(colours = inferno(5, direction = 1)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Tornado-Induced Crop Damage by Event in:",
       fill = "Log Transformed Crop Damage (US dollars)",
       size = "Log Transformed Crop Damage (US dollars)") +
  theme(plot.title = element_text(hjust = 0.5, size = 27),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))


# Animate it
gganimate::gganimate(map_gif,
                     ani.width = 1725,
                     ani.height = 950,
                     interval = 1.75)
# Save it
# "images/plot8.gif"


# Lets do cumulative tornado-induced property damage
tor_df <- tor_df[with(tor_df, order(BEGIN_DATE_TIME, DAMAGE_CROPS)), ]

rownames(tor_df) <- NULL

tor_df$CUM_DAM_CROP <- zoo::rollapplyr(tor_df$DAMAGE_CROPS,
                                       length(tor_df$DAMAGE_CROPS),
                                       partial = TRUE,
                                       FUN = sum)

# Make the GIF
saveGIF( {
  for (i in 1:20) {
    print(ggplot(tor_df) +
            theme_bw() +
            geom_line(aes(x = BEGIN_DATE_TIME,
                          y = CUM_DAM_CROP),
                      lwd = 1,
                      col = "dark blue") +
            labs(x = "Date",
                 y = "Crop Damage (US dollars)",
                 title = "Cumulative Tornado-Induced Crop Damage since Jan 1, 1997") +
            theme(plot.title = element_text(hjust = 0.5, size = 20),
                  axis.title = element_text(size = 18),
                  axis.text = element_text(size = 15)) +
            xlim(range(tor_df$BEGIN_DATE_TIME)[1],
                 range(tor_df$BEGIN_DATE_TIME)[1] + i * ((range(tor_df$BEGIN_DATE_TIME)[2] - range(tor_df$BEGIN_DATE_TIME)[1]) / 20)) +
            scale_y_continuous(breaks=c(1e8, 2e8, 3e8),
                               labels=c("100 Million", "200 Million", "300 Million")))
  }
},
interval = 0.75,
ani.width = 1250,
ani.height = 1000)

#,
#movie.name = "images/keepers/plot13.gif")


