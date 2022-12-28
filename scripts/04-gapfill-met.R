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


# mutate(vpd = RHtoVPD(rh_avg, airtc_avg),
  #        date = as.Date(timestamp,
  #                       tz = "America/Los_Angeles"),
  #        year = year(timestamp), 
  #        doy = yday(timestamp))
