# Exploring data, plot by individual, species, means, etc

library(readr)
library(dplyr)
library(ggplot2)

# Read in data
flux <- read_csv("data_clean/flux_combined.csv")
str(flux)

# Plot ts by site and species
flux %>%
  ggplot(aes(x = date, y = `water use per basal area`)) +
  geom_point(aes(color = as.factor(ind)),
             size = 0.1) +
  facet_grid(rows = vars(site),
             cols = vars(species)) +
  theme_bw() +
  guides(color = "none")
