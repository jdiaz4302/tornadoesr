


# Packages
library(dplyr)
library(ggplot2)
library(reshape2)


# Import the data
tor_df <- read.csv("data/raw/tor_data_processed.csv")


# Melt the dataframe into a 2 column df
# with variables and values as columns
tor_hist_data <- melt(tor_df)


# Change variables for better facet titles
levels(tor_hist_data$variable) <- c("Property Damage",
                                    "Tornado Duration",
                                    "Beginning Latitude",
                                    "Beginning Longitude",
                                    "Tornado Length",
                                    "Tornado Width",
                                    "Year",
                                    "Multivortex Indicator",
                                    "Open Water Prop.",
                                    "Dev. Open Space Prop.",
                                    "Dev. Low Int. Prop.",
                                    "Dev. Medium Int. Prop.",
                                    "Dev. High Int. Prop.",
                                    "Barren Land Prop.",
                                    "Deciduous Forest Prop.",
                                    "Evergreen Forest Prop.",
                                    "Mixed Forest Prop.",
                                    "Shrub/Scrub Prop.",
                                    "Grassland/Herb. Prop.",
                                    "Pasture/Hay Prop.",
                                    "Cultivated Crops Prop.",
                                    "Woody Wet. Prop.",
                                    "Emer. Herb. Wet. Prop.",
                                    "County Income",
                                    "County Mobile Home Dens.",
                                    "County Population Dens.",
                                    "Tornado Area",
                                    "Total Dev. Int.",
                                    "Total Wooded Proportion",
                                    "Wooded-Dev. Interaction",
                                    "Median Income of Area",
                                    "Day of Year",
                                    "Month",
                                    "State Rank",
                                    "Time")


# Plot them
ggplot(tor_hist_data,
       aes(x = value)) +
  geom_histogram(bins = 75,
                 fill = "black") +
  facet_wrap(~variable) +
  theme_bw() +
  labs(x = "",
       y = "",
       title = "Distribution of Processsed Variables") +
  theme(plot.title = element_text(size = 22, hjust = 0.5),
        strip.text = element_text(size = 8),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        aspect.ratio = 8/11) +
  scale_x_continuous(limits = c(-7, 7)) +
  scale_y_continuous(expand = c(0, 0))


