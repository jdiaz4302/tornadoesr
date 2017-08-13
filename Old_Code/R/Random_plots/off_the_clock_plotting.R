


# Packages
library(ggplot2)
library(gganimate)
library(ggmap)
library(viridis)
library(animation)


# Call the data
source("R/Random_plots/3_format_for_prop_dam_visualizations.R")


# Get rid of 2017 since its an incomplete year
tor_df <- dplyr::filter(tor_df,
                        YEAR != 2017)


# Get property damage per year
DamPerState <- aggregate(tor_df$DAMAGE_PROPERTY,
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
tor_top_10_df$DAMAGE_PROPERTY <- log(tor_top_10_df$DAMAGE_PROPERTY + 1,
                                     base = exp(1))

# Make STATE a factor
tor_top_10_df$STATE <- as.factor(tor_top_10_df$STATE)


# Plot property damage by state
ggplot(tor_top_10_df,
       aes(x = STATE,
           y = DAMAGE_PROPERTY,
           col = STATE)) +
  theme_bw() +
  geom_jitter(width = 0.10,
              height = 0.10,
              size = 3,
              alpha = .4) +
  stat_summary(aes(y = DAMAGE_PROPERTY, group = 1),
               fun.y = mean,
               col = "grey25",
               group = 1,
               geom = "point",
               size = 10,
               shape = 15,
               alpha = 0.85) +
  labs(x = "State",
       y = "Log-Transformed Property Damage (US dollars)",
       title = "Tornado-Induced Property Damage",
       subtitle = "Per Event in the Top 10 Most Damaged States") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 14),
        legend.position = "none",
        aspect.ratio = 15/27)


# Get the average per year per state
#avg_ALA_by_year <- aggregate(ALA_df$DAMAGE_PROPERTY,
#                             list(Category = ALA_df$YEAR),
#                             mean)
#
#state <- c(rep("ALABAMA", nrow(avg_ALA_by_year)))
#
#avg_ALA_by_year <- cbind(avg_ALA_by_year,
#                         state)
#
#
#avg_ARK_by_year <- aggregate(ARK_df$DAMAGE_PROPERTY,
#                             list(Category = ARK_df$YEAR),
#                             mean)
#
#state <- c(rep("ARKANSAS", nrow(avg_ARK_by_year)))
#
#avg_ARK_by_year <- cbind(avg_ARK_by_year,
#                         state)
#
#
#avg_GA_by_year <- aggregate(GA_df$DAMAGE_PROPERTY,
#                            list(Category = GA_df$YEAR),
#                            mean)
#
#state <- c(rep("GEORGIA", nrow(avg_GA_by_year)))
#
#avg_GA_by_year <- cbind(avg_GA_by_year,
#                        state)
#
#
#avg_ILL_by_year <- aggregate(ILL_df$DAMAGE_PROPERTY,
#                             list(Category = ILL_df$YEAR),
#                             mean)
#
#state <- c(rep('ILLINOIS', nrow(avg_ILL_by_year)))
#
#avg_ILL_by_year <- cbind(avg_ILL_by_year,
#                         state)
#
#
#avg_MISSIP_by_year <- aggregate(MISSIP_df$DAMAGE_PROPERTY,
#                                list(Category = MISSIP_df$YEAR),
#                                mean)
#
#state <- c(rep("MISSISSIPPI", nrow(avg_MISSIP_by_year)))
#
#avg_MISSIP_by_year <- cbind(avg_MISSIP_by_year,
#                            state)
#
#
#avg_MISSOURI_by_year <- aggregate(MISSOURI_df$DAMAGE_PROPERTY,
#                                  list(Category = MISSOURI_df$YEAR),
#                                  mean)
#
#state <- c(rep("MISSOURI", nrow(avg_MISSOURI_by_year)))
#
#avg_MISSOURI_by_year <- cbind(avg_MISSOURI_by_year,
#                              state)
#
#
#avg_KANSAS_by_year <- aggregate(KANSAS_df$DAMAGE_PROPERTY,
#                                list(Category = KANSAS_df$YEAR),
#                                mean)
#
#state <- c(rep("KANSAS", nrow(avg_KANSAS_by_year)))
#
#avg_KANSAS_by_year <- cbind(avg_KANSAS_by_year,
#                            state)
#
#
#avg_OK_by_year <- aggregate(OK_df$DAMAGE_PROPERTY,
#                            list(Category = OK_df$YEAR),
#                            mean)
#
#state <- c(rep("OKLAHOMA", nrow(avg_OK_by_year)))
#
#avg_OK_by_year <- cbind(avg_OK_by_year,
#                        state)
#
#
#avg_TENN_by_year <- aggregate(TENN_df$DAMAGE_PROPERTY,
#                              list(Category = TENN_df$YEAR),
#                              mean)
#
#state <- c(rep("TENNESSEE", nrow(avg_TENN_by_year)))
#
#avg_TENN_by_year <- cbind(avg_TENN_by_year,
#                          state)
#
#
#avg_TX_by_year <- aggregate(TX_df$DAMAGE_PROPERTY,
#                            list(Category = TX_df$YEAR),
#                            mean)
#
#state <- c(rep("TEXAS", nrow(avg_TX_by_year)))
#
#avg_TX_by_year <- cbind(avg_TX_by_year,
#                        state)
#
# Combine them all
#intermediate_11 <- rbind(avg_ALA_by_year, avg_ARK_by_year)
#
#intermediate_12 <- rbind(intermediate_11, avg_GA_by_year)
#
#intermediate_13 <- rbind(intermediate_12, avg_ILL_by_year)
#
#intermediate_14 <- rbind(intermediate_13, avg_MISSIP_by_year)
#
#intermediate_15 <- rbind(intermediate_14, avg_MISSOURI_by_year)
#
#intermediate_16 <- rbind(intermediate_15, avg_KANSAS_by_year)
#
#intermediate_17 <- rbind(intermediate_16, avg_OK_by_year)
#
#intermediate_18 <- rbind(intermediate_17, avg_TENN_by_year)
#
#avg_by_state_by_year <- rbind(intermediate_18, avg_TX_by_year)
#
#
# Log transform it
#avg_by_state_by_year$x <- log(avg_by_state_by_year$x + 1,
#                              base = exp(1))
#
#
# Better looking variable name for the plot
#avg_by_state_by_year$STATE <- avg_by_state_by_year$state
#
#
# Plot it
#ggplot(avg_by_state_by_year,
#       aes(x = Category,
#           y = x,
#           group = STATE,
#           col = STATE)) +
#  geom_line(size = 1.15,
#            alpha = 0.8) +
#  geom_jitter(width = 0.05,
#              height = 0.05,
#              size = 5) +
#  theme_bw() +
#  labs(x = "Year",
#       y = "Log-Transformed Property Damage (US dollars)",
#       title = "Average Tornado-Induced Property Damage",
#       subtitle = "Per Event in the Top 10 Most Damaged States") +
#  theme(plot.title = element_text(hjust = 0.5, size = 24),
#        plot.subtitle = element_text(hjust = 0.5, size = 15),
#        axis.title = element_text(size = 17),
#        axis.text = element_text(size = 14),
#        aspect.ratio = 8/11) + 
#  scale_color_brewer(palette = "Paired") +
#  coord_cartesian(ylim = c(0, 20)) 
#
#
# Let's make a gif
#dam_by_event_per_year <- ggplot(tor_top_10_df,
#                                aes(x = STATE,
#                                    y = DAMAGE_PROPERTY,
#                                    col = STATE,
#                                    frame = YEAR)) +
#  theme_bw() +
#  geom_jitter(width = 0.15,
#              height = 0.15,
#              size = 6,
#              alpha = 0.55) +
#  labs(x = "State",
#       y = "Log-Transformed Property Damage (US dollars)",
#       title = "Tornado-Induced Property Damage by Event in:",
#       subtitle = "Per Event in the Top 10 Most Damaged States") +
#  theme(plot.title = element_text(hjust = 0.5, size = 27),
#        plot.subtitle = element_text(hjust = 0.5, size = 18),
#        axis.title = element_text(size = 20),
#        axis.text = element_text(size = 18),
#        legend.position = "none",
#        aspect.ratio = 15/27)
#
#
# Animate the gif
#gganimate::gganimate(dam_by_event_per_year,
#                     ani.width = 1725,
#                     ani.height = 950)
# To save it
# "images/plot4.gif")


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
                  fill = log(DAMAGE_PROPERTY + 1,
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
       title = "Tornado-Induced Property Damage by Event",
       fill = "Log Transformed Property Damage (US dollars)",
       size = "Log Transformed Property Damage (US dollars)") +
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
                  fill = log(DAMAGE_PROPERTY + 1,
                             base = exp(1)),
                  frame = YEAR),
              width = 0.05,
              height = 0.05,
              pch = 21,
              col = "black",
              size = 5) +
  theme_bw() +
  scale_fill_gradientn(colours = inferno(22, direction = 1)) +
  labs(title = "Tornado-Induced Property Damage by Event in:",
       fill = "L.T.P.D.") +
  theme(plot.title = element_text(hjust = 0.5, size = 27),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))


# Animate it
gganimate::gganimate(map_gif,
                     ani.width = 1725,
                     ani.height = 950,
                     interval = 1.75)
# Save it
# "images/keepers/plot6.gif"


# Format month as factor
tor_df$MONTH_NAME <- factor(tor_df$MONTH_NAME, month.name)


# Map gif - by month
map_gif <- ggmap(US_map) +
  geom_jitter(data = tor_df,
              aes(x = BEGIN_LON,
                  y = BEGIN_LAT,
                  fill = log(DAMAGE_PROPERTY + 1,
                             base = exp(1)),
                  frame = MONTH_NAME),
              width = 0.05,
              height = 0.05,
              pch = 21,
              col = "black",
              size = 5) +
  theme_bw() +
  scale_fill_gradientn(colours = inferno(22, direction = 1)) +
  labs(title = "Tornado-Induced Property Damage by Event in:",
       fill = "L.T.P.D.") +
  theme(plot.title = element_text(hjust = 0.5, size = 27),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))


# Animate it
gganimate::gganimate(map_gif,
                     ani.width = 1725,
                     ani.height = 950,
                     interval = 1.75)
# Save it
# "images/keepers/plot8.gif"


# Lets do cumulative tornado-induced property damage
tor_df <- tor_df[with(tor_df, order(BEGIN_DATE_TIME, DAMAGE_PROPERTY)), ]

rownames(tor_df) <- NULL

tor_df$CUM_DAM_PROP <- zoo::rollapplyr(tor_df$DAMAGE_PROPERTY,
                                       length(tor_df$DAMAGE_PROPERTY),
                                       partial = TRUE,
                                       FUN = sum)

# Make the GIF
saveGIF( {
  for (i in 1:20) {
    print(ggplot(tor_df) +
            theme_bw() +
            geom_line(aes(x = BEGIN_DATE_TIME,
                          y = CUM_DAM_PROP),
                      lwd = 1,
                      col = "dark blue") +
            labs(x = "Date",
                 y = "Property Damage (US dollars)",
                 title = "Cumulative Tornado-Induced Property Damage since Jan 1, 1997") +
            theme(plot.title = element_text(hjust = 0.5, size = 20),
                  axis.title = element_text(size = 18),
                  axis.text = element_text(size = 15)) +
            xlim(range(tor_df$BEGIN_DATE_TIME)[1],
                 range(tor_df$BEGIN_DATE_TIME)[1] + i * ((range(tor_df$BEGIN_DATE_TIME)[2] - range(tor_df$BEGIN_DATE_TIME)[1]) / 20)) +
            scale_y_continuous(breaks=c(1e10, 2e10, 3e10),
                               labels=c("10 Billion", "20 Billion", "30 Billion")))
  }
},
interval = 0.75,
ani.width = 1250,
ani.height = 1000)

#,
#movie.name = "images/plot7.gif")


