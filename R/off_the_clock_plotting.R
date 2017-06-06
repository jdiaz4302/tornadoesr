


# Packages
library(ggplot2)


# Call the data
source("R/3_format_for_python.R")


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
top_10_states <- sort(DamPerState$x, decreasing = TRUE)[10]

# Keep only the top 10 states
DamPerState <- filter(DamPerState,
                      x >= top_10_states)

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
  geom_point(size = 3,
             alpha = .4) +
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
        aspect.ratio = 7/13)




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


# Plot it
ggplot(avg_by_state_by_year,
       aes(x = Category,
           y = x,
           group = state,
           col = state)) +
  geom_line(size = 1.25,
            alpha = 0.3) +
  geom_point(size = 3) +
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


