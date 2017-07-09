


# Packages
library(ggplot2)
library(viridis)


# Call the data
source("R/Random_plots/3_format_for_prop_dam_visualizations.R")


# Get rid of 2017 since its an incomplete year
tor_df <- dplyr::filter(tor_df,
                        YEAR != 2017)


# Get property damage per year
DamPerState <- aggregate(tor_df$DAMAGE_PROPERTY,
                         by = list(Category = tor_df$STATE),
                         FUN = sum)


# Manually assigning census region
DamPerState$Region <- c("South",
                        "West",
                        "South",
                        "West",
                        "West",
                        "Northeast",
                        "South",
                        "South",
                        "South",
                        "South",
                        "West",
                        "Midwest",
                        "Midwest",
                        "Midwest",
                        "Midwest",
                        "South",
                        "South",
                        "Northeast",
                        "South",
                        "Northeast",
                        "Midwest",
                        "Midwest",
                        "South",
                        "Midwest",
                        "West",
                        "Midwest",
                        "West",
                        "Northeast",
                        "Northeast",
                        "West",
                        "Northeast",
                        "South",
                        "Midwest",
                        "Midwest",
                        "South",
                        "West",
                        "Northeast",
                        "Northeast",
                        "South",
                        "Midwest",
                        "South",
                        "South",
                        "West",
                        "Northeast",
                        "South",
                        "West",
                        "South",
                        "Midwest",
                        "West") %>%
  factor(levels = c("South", "Midwest", "West", "Northeast"))


# Get the bar plot as decreasing values
DamPerState <- within(DamPerState,
                      Category <- factor(Category,
                                         levels = names(sort(table(Category), 
                                                        decreasing = TRUE))))


# The plot
ggplot(DamPerState,
       aes(x = reorder(Category, -x),
           y = x,
           fill = Region)) +
  theme_bw() +
  geom_bar(stat = "identity",
           col = "black") +
  theme(axis.text.x = element_text(angle = 65,
                                   hjust = 1),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 22,
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 17,
                                     hjust = 0.5),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(breaks = c(0e0, 1e9, 2e9, 3e9, 4e9, 5e9),
                     labels = c("Zero", "One Billion", "Two Billion",
                                "Three Billion", "Four Billion", "Five Billion")) +
  scale_fill_viridis("US Census Region",
                     discrete = TRUE) +
  labs(x = "State",
       y = "Property Damage (US dollars)",
       title = "Cumulative Tornado-Induced Property Damage",
       subtitle = "Since Jan 1, 1997")


