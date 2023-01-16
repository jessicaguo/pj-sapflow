# Check presence of met data at 4 sites

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plantecophys)

met_raw <- read_csv("data_raw/sapflow_csv/probe_meta.csv",
                    locale = locale(tz = "America/Los_Angeles")) %>%
  select(-record, -par_avg, -par_total, -externalbatt_min) %>%
  pivot_wider(names_from = site_name,
              values_from = c(airtc_avg, rh_avg),
              names_glue = "{site_name}.{.value}") %>%
  mutate(left.both = ifelse(!is.na(left.airtc_avg) & !is.na(left.rh_avg), TRUE, FALSE),
         right.both = ifelse(!is.na(right.airtc_avg) & !is.na(right.rh_avg), TRUE, FALSE),
         upland.both = ifelse(!is.na(upland.airtc_avg) & !is.na(upland.rh_avg), TRUE, FALSE),
         valley.both = ifelse(!is.na(valley.airtc_avg) & !is.na(valley.rh_avg), TRUE, FALSE))


sum(met_raw$left.both)
sum(met_raw$right.both)
sum(met_raw$upland.both)
sum(met_raw$valley.both)


#### Plot present/missing data from each site by year ####
met_raw %>%
  select(timestamp, ends_with("both")) %>%
  mutate(year = year(timestamp)) %>%
  filter(year <= 2016) %>%
  pivot_longer(cols = 2:5,
               names_to = "site_name",
               values_to = "both",
               names_pattern = "(.*).both") %>%
  ggplot(aes(x = timestamp, y = site_name)) +
  geom_point(aes(color = both)) +
  facet_wrap(~year,
             ncol = 3,
             scales = "free_x")

met_raw %>%
  select(timestamp, ends_with("both")) %>%
  mutate(year = year(timestamp)) %>%
  filter(year > 2016) %>%
  pivot_longer(cols = 2:5,
               names_to = "site_name",
               values_to = "both",
               names_pattern = "(.*).both") %>%
  ggplot(aes(x = timestamp, y = site_name)) +
  geom_point(aes(color = both)) +
  facet_wrap(~year,
             ncol = 3,
             scales = "free_x")


#### Create composite subdaily VPD with right and left ####

# Skip 2011-2012, replace -40 with NA (error in right)

met <- met_raw %>%
  select(timestamp,
         starts_with("left"),
         starts_with("right"),
         starts_with("upland"),
         starts_with("valley")) %>%
  mutate(year = year(timestamp)) %>%
  filter(year > 2012) %>%
  mutate(r_l = ifelse(right.both == FALSE & left.both == TRUE, TRUE, FALSE),
         r_u = ifelse(right.both == FALSE & upland.both == TRUE, TRUE, FALSE),
         r_v = ifelse(right.both == FALSE & valley.both == TRUE, TRUE, FALSE),
         right.airtc_avg = ifelse(right.airtc_avg == -40, NA, right.airtc_avg))

# Check overlap by year
met %>%
  group_by(year) %>%
  summarize(randl = sum(r_l),
            randu = sum(r_u),
            randv = sum(r_v),
            right_NA = sum(!right.both),
            right_n = sum(right.both),
            left_NA = sum(!left.both),
            left_n = sum(left.both))

# Seems like upland sensors were wonky, only use left
# valley has less complete data

met %>%
  ggplot(aes(x = timestamp)) +
  geom_line(aes(y = left.airtc_avg, color = "left")) +
  geom_line(aes(y = right.airtc_avg, color = "right")) +
  # geom_line(aes(y = valley.airtc_avg, color = "valley")) +
  facet_wrap(~year, ncol = 2, scales = "free_x") +
  theme_bw()

met %>%
  ggplot(aes(x = timestamp)) +
  geom_line(aes(y = left.rh_avg, color = "left")) +
  geom_line(aes(y = right.rh_avg, color = "right")) +
  # geom_line(aes(y = valley.rh_avg, color = "valley")) +
  facet_wrap(~year, ncol = 2, scales = "free_x") +
  theme_bw()


# Take mean between left and right to create composite or average climate

met_composite <- met %>%
  mutate(airtc = rowMeans(select(met, left.airtc_avg, right.airtc_avg), na.rm = TRUE),
         rh = rowMeans(select(met, left.rh_avg, right.rh_avg), na.rm = TRUE)) %>%
  select(timestamp, year, airtc, rh) %>%
  mutate(vpd = RHtoVPD(rh, airtc),
         date = as.Date(timestamp,
                        tz = "America/Los_Angeles"),
         doy = yday(timestamp))

saveRDS(met_composite, file = "app/met_composite.RDS")




