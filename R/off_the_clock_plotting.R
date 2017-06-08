


# Packages
library(ggplot2)
library(gganimate)
library(ggmap)


# Call the data
source("R/3_format_for_python.R")


# Get rid of 2017 since its an incomplete year
tor_df <- dplyr::filter(tor_df,
                        YEAR != 2017)


# Get property damage per year
DamPerState <- aggregate(tor_df$DAMAGE_PROPERTY,
                         by = list(Category = tor_df$STATE),
                         FUN = sum)


# Log transform the property damage
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

MISSIP_df <- filter(tor_df,
                    STATE == state_names[5])

MISSOURI_df <- filter(tor_df,
                      STATE == state_names[6])

NC_df <- filter(tor_df,
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

inter_3 <- rbind(inter_2, NC_df)

inter_4 <- rbind(inter_3, MISSOURI_df)

inter_5 <- rbind(inter_4, MISSIP_df)

inter_6 <- rbind(inter_5, ILL_df)

inter_7 <- rbind(inter_6, GA_df)

inter_8 <- rbind(inter_7, ARK_df)

tor_top_10_df <- rbind(inter_8, ALA_df)


# Log transform it for better visualization
tor_top_10_df$DAMAGE_PROPERTY <- log(tor_top_10_df$DAMAGE_PROPERTY + 1,
                                     base = exp(1))

# Make STATE a factor
tor_top_10_df$STATE <- as.factor(tor_top_10_df$STATE)


# Get an anova model
anova_model <- lm(DAMAGE_PROPERTY ~ 0 + STATE,
                  data = tor_top_10_df)


# Get the 95% confidence intervals on the mean as a dataframe
confint_d <- as.data.frame(confint(anova_model))


# Assign the dataframe proper STATE variable
confint_d$STATE <- levels(tor_top_10_df$STATE)


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
  geom_segment(aes(xend = STATE, y = `2.5 %`, yend = `97.5 %`), 
               color = 'grey35', data = confint_d, size = 7) +
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
avg_ALA_by_year <- aggregate(ALA_df$DAMAGE_PROPERTY,
                             list(Category = ALA_df$YEAR),
                             mean)

state <- c(rep("ALABAMA", nrow(avg_ALA_by_year)))

avg_ALA_by_year <- cbind(avg_ALA_by_year,
                         state)


avg_ARK_by_year <- aggregate(ARK_df$DAMAGE_PROPERTY,
                             list(Category = ARK_df$YEAR),
                             mean)

state <- c(rep("ARKANSAS", nrow(avg_ARK_by_year)))

avg_ARK_by_year <- cbind(avg_ARK_by_year,
                         state)


avg_GA_by_year <- aggregate(GA_df$DAMAGE_PROPERTY,
                            list(Category = GA_df$YEAR),
                            mean)

state <- c(rep("GEORGIA", nrow(avg_GA_by_year)))

avg_GA_by_year <- cbind(avg_GA_by_year,
                        state)


avg_ILL_by_year <- aggregate(ILL_df$DAMAGE_PROPERTY,
                             list(Category = ILL_df$YEAR),
                             mean)

state <- c(rep('ILLINOIS', nrow(avg_ILL_by_year)))

avg_ILL_by_year <- cbind(avg_ILL_by_year,
                         state)


avg_MISSIP_by_year <- aggregate(MISSIP_df$DAMAGE_PROPERTY,
                                list(Category = MISSIP_df$YEAR),
                                mean)

state <- c(rep("MISSISSIPPI", nrow(avg_MISSIP_by_year)))

avg_MISSIP_by_year <- cbind(avg_MISSIP_by_year,
                            state)


avg_MISSOURI_by_year <- aggregate(MISSOURI_df$DAMAGE_PROPERTY,
                                  list(Category = MISSOURI_df$YEAR),
                                  mean)

state <- c(rep("MISSOURI", nrow(avg_MISSOURI_by_year)))

avg_MISSOURI_by_year <- cbind(avg_MISSOURI_by_year,
                              state)


avg_NC_by_year <- aggregate(NC_df$DAMAGE_PROPERTY,
                            list(Category = NC_df$YEAR),
                            mean)

state <- c(rep("NORTH CAROLINA", nrow(avg_NC_by_year)))

avg_NC_by_year <- cbind(avg_NC_by_year,
                        state)


avg_OK_by_year <- aggregate(OK_df$DAMAGE_PROPERTY,
                            list(Category = OK_df$YEAR),
                            mean)

state <- c(rep("OKLAHOMA", nrow(avg_OK_by_year)))

avg_OK_by_year <- cbind(avg_OK_by_year,
                        state)


avg_TENN_by_year <- aggregate(TENN_df$DAMAGE_PROPERTY,
                              list(Category = TENN_df$YEAR),
                              mean)

state <- c(rep("TENNESSEE", nrow(avg_TENN_by_year)))

avg_TENN_by_year <- cbind(avg_TENN_by_year,
                          state)


avg_TX_by_year <- aggregate(TX_df$DAMAGE_PROPERTY,
                            list(Category = TX_df$YEAR),
                            mean)

state <- c(rep("TEXAS", nrow(avg_TX_by_year)))

avg_TX_by_year <- cbind(avg_TX_by_year,
                        state)

# Combine them all
intermediate_11 <- rbind(avg_ALA_by_year, avg_ARK_by_year)

intermediate_12 <- rbind(intermediate_11, avg_GA_by_year)

intermediate_13 <- rbind(intermediate_12, avg_ILL_by_year)

intermediate_14 <- rbind(intermediate_13, avg_MISSIP_by_year)

intermediate_15 <- rbind(intermediate_14, avg_MISSOURI_by_year)

intermediate_16 <- rbind(intermediate_15, avg_NC_by_year)

intermediate_17 <- rbind(intermediate_16, avg_OK_by_year)

intermediate_18 <- rbind(intermediate_17, avg_TENN_by_year)

avg_by_state_by_year <- rbind(intermediate_18, avg_TX_by_year)


# Log transform it
avg_by_state_by_year$x <- log(avg_by_state_by_year$x + 1,
                              base = exp(1))


# Better looking variable name for the plot
avg_by_state_by_year$STATE <- avg_by_state_by_year$state


# Plot it
ggplot(avg_by_state_by_year,
       aes(x = Category,
           y = x,
           group = STATE,
           col = STATE)) +
  geom_line(size = 1.15,
            alpha = 0.8) +
  geom_jitter(width = 0.05,
              height = 0.05,
              size = 5) +
  theme_bw() +
  labs(x = "Year",
       y = "Log-Transformed Property Damage (US dollars)",
       title = "Average Tornado-Induced Property Damage",
       subtitle = "Per Event in the Top 10 Most Damaged States") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 14),
        aspect.ratio = 8/11) + 
  scale_color_brewer(palette = "Paired")


# For each year selection
#tor_top_10_df_00 <- dplyr::filter(tor_top_10_df,
#                                  YEAR == 2000)
#
#tor_top_10_df_05 <- dplyr::filter(tor_top_10_df,
#                                  YEAR == 2005)
#
#tor_top_10_df_10 <- dplyr::filter(tor_top_10_df,
#                                  YEAR == 2010)
#
#tor_top_10_df_15 <- dplyr::filter(tor_top_10_df,
#                                  YEAR == 2015)
#
#
# 2000
# Get an anova model
#anova_model_00 <- lm(DAMAGE_PROPERTY ~ 0 + STATE,
#                     data = tor_top_10_df_00)
#
#
# Get the 95% confidence intervals on the mean as a dataframe
#confint_d_00 <- as.data.frame(confint(anova_model_00))
#
#
# Assign the dataframe proper STATE variable
#confint_d_00$STATE <- levels(factor(tor_top_10_df_00$STATE))
#
#
# 2005
# Get an anova model
#anova_model_05 <- lm(DAMAGE_PROPERTY ~ 0 + STATE,
#                     data = tor_top_10_df_05)
#
#
# Get the 95% confidence intervals on the mean as a dataframe
#confint_d_05 <- as.data.frame(confint(anova_model_05))
#
#
# Assign the dataframe proper STATE variable
#confint_d_05$STATE <- levels(factor(tor_top_10_df_05$STATE))
#
#
# 2010
# Get an anova model
#anova_model_10 <- lm(DAMAGE_PROPERTY ~ 0 + STATE,
#                     data = tor_top_10_df_10)
#
#
# Get the 95% confidence intervals on the mean as a dataframe
#confint_d_10 <- as.data.frame(confint(anova_model_10))
#
#
# Assign the dataframe proper STATE variable
#confint_d_10$STATE <- levels(factor(tor_top_10_df_10$STATE))
#
#
# 2015
# Get an anova model
#anova_model_15 <- lm(DAMAGE_PROPERTY ~ 0 + STATE,
#                     data = tor_top_10_df_15)
#
#
# Get the 95% confidence intervals on the mean as a dataframe
#confint_d_15 <- as.data.frame(confint(anova_model_15))
#
#
# Assign the dataframe proper STATE variable
#confint_d_15$STATE <- levels(factor(tor_top_10_df_15$STATE))
#
#  
#
#
#ggplot(tor_top_10_df,
#       aes(x = STATE,
#           y = DAMAGE_PROPERTY)) +
#  theme_bw() +
#  geom_jitter(width = 0.10,
#              height = 0.10,
#              size = 3,
#              alpha = .4) +
#  geom_segment(aes(xend = STATE, y = `2.5 %`, yend = `97.5 %`), 
#               color = 'dark red', data = confint_d_00, size = 7,
#               alpha = 0.55) +
#  geom_segment(aes(xend = STATE, y = `2.5 %`, yend = `97.5 %`), 
#               color = 'orange', data = confint_d_05, size = 6,
#               alpha = 0.55) +
#  geom_segment(aes(xend = STATE, y = `2.5 %`, yend = `97.5 %`), 
#               color = 'green', data = confint_d_10, size = 5,
#               alpha = 0.55) +
#  geom_segment(aes(xend = STATE, y = `2.5 %`, yend = `97.5 %`), 
#               color = 'dark blue', data = confint_d_15, size = 4,
#               alpha = 0.55) +
#  labs(x = "State",
#       y = "Log-Transformed Property Damage (US dollars)",
#       title = "Tornado-Induced Property Damage",
#       subtitle = "Per Event in the Top 10 Most Damaged States") +
#  theme(plot.title = element_text(hjust = 0.5, size = 24),
#        plot.subtitle = element_text(hjust = 0.5, size = 15),
#        axis.title = element_text(size = 17),
#        axis.text = element_text(size = 14),
#        legend.position = "none",
#        aspect.ratio = 7/13) +
#  coord_cartesian(ylim = c(0, 20))


# Let's make a gif
dam_by_event_per_year <- ggplot(tor_top_10_df,
                                aes(x = STATE,
                                    y = DAMAGE_PROPERTY,
                                    col = STATE,
                                    frame = YEAR)) +
  theme_bw() +
  geom_jitter(width = 0.10,
              height = 0.10,
              size = 6,
              alpha = 0.55) +
  labs(x = "State",
       y = "Log-Transformed Property Damage (US dollars)",
       title = "Tornado-Induced Property Damage by Event in:",
       subtitle = "Per Event in the Top 10 Most Damaged States") +
  theme(plot.title = element_text(hjust = 0.5, size = 27),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.position = "none",
        aspect.ratio = 15/27)


# Animate the gif
gganimate::gganimate(dam_by_event_per_year,
                     ani.width = 1688,
                     ani.height = 1100)
# To save it
# "images/plot3.gif")


# Lets make another gif
dam_by_event_per_state <- ggplot(tor_df,
                                 aes(x = as.Date(as.POSIXct(BEGIN_DATE_TIME,
                                                 origin = "1970-01-01")),
                                     y = log(DAMAGE_PROPERTY + 1,
                                             base = exp(1)),
                                     frame = STATE,
                                     col = log(DAMAGE_PROPERTY + 1,
                                               base = exp(1)))) +
  theme_bw() +
  geom_jitter(width = 0.10,
              height = 0.10,
              size = 6,
              alpha = 0.55) +
  scale_colour_gradientn(colours = rainbow(10)) +
  labs(x = "Date",
       y = "Log-Transformed Property Damage (US dollars)",
       title = "Tornado-Induced Property Damage by Event in:") +
  theme(plot.title = element_text(hjust = 0.5, size = 27),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.position = "none",
        aspect.ratio = 2/3)


# Animate the gif
gganimate::gganimate(dam_by_event_per_state,
                     ani.width = 1500,
                     ani.height = 1000)
# To save it
# "images/plot4.gif"


# Another gif
US_bounds <- c(left = -125, bottom = 25.75, right = -67, top = 49)

US_map <- get_stamenmap(US_bounds, zoom = 5, maptype = "toner-lite")

map_plot <- ggmap(US_map) +
  geom_jitter(data = tor_df,
              aes(x = BEGIN_LON,
                  y = BEGIN_LAT,
                  col = log(DAMAGE_PROPERTY + 1,
                            base = exp(1)),
                  frame = YEAR,
                  size = log(DAMAGE_PROPERTY + 1,
                             base = exp(1))),
              width = 0.05,
              height = 0.05,
              alpha = 0.35) +
  theme_bw() +
  scale_colour_gradientn(colours = rainbow(7))


gganimate::gganimate(map_plot,
                     ani.width = 1500,
                     ani.height = 1000)


