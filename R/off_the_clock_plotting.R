


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


# Plot all property
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


