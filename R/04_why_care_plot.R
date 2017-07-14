


# Packages
library(plyr)
library(ggplot2)


# Calling the data
source("R/Random_plots/3_format_for_prop_dam_visualizations.R")


# Get property damage per year
DamPerYear <- aggregate(tor_df$DAMAGE_PROPERTY,
                        by = list(Category = tor_df$YEAR),
                        FUN = sum)

# Don't include 2017 since it's not complete
DPY <- filter(DamPerYear,
              Category != 2017)

DPY$Damage <- DPY$x


# Plot it
ggplot(DPY,
       aes(x = Category,
           y = log(Damage))) +
  geom_area(fill = "red",
            alpha = 0.25) +
  geom_line(lwd = 1.25,
            col = "grey65") +
  coord_cartesian(ylim = c(18, 24)) +
  scale_x_continuous(breaks = 1997:2016) +
  scale_y_continuous(minor_breaks = seq(1, 24, 1)) +
  geom_hline(aes(yintercept = 19),
             lty = 2,
             lwd = 1.5,
             colour = "black") +
  geom_hline(aes(yintercept = 23),
             lty = 2,
             lwd = 1.5,
             colour = "black") +
  geom_point(size = 4,
             colour = "red4") +
  geom_text(aes(2001,
                23,
                label = "Over $9,700,000,000.00",
                vjust = -.75),
                size = 5) +
  geom_text(aes(2014,
                19,
                label = "Over $178,000,000.00",
                vjust = 1.5,
                hjust = 1),
                size = 5) +
  xlab("Year") +
  ylab("Log-Transformed Property Damage (US dollars)") +
  ggtitle("Tornado-Induced Property Damage per Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 22),
        axis.title = element_text(size = 17),
        legend.position = "none",
        axis.text = element_text(size = 15),
        aspect.ratio = 8/13)
 

