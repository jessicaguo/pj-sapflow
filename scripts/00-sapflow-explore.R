# Understand and join database csv's
# Wade's database output as csv for Susan
# Tables located in data_raw/sapflow_csv/
# P is Pinus monophylla
# J is Juniperus osteosperma

library(readr)
library(dplyr)
library(tidyr)
library(plantecophys)
library(lubridate)

#### Tables ####
# veg.csv
veg <- read_csv("data_raw/sapflow_csv/veg.csv")
veg %>%
  filter(veg_type %in% c("juniper", "pinyon")) %>%
  group_by(site_name, veg_type) %>%
  count()
# 70 records, but includes sagebrush and depth variable individuals
# Left: 3 J, 9 P,
# Right: 6 J, 10 P
# Upland: 10 P
# Valley: 7 J, 7 P
colnames(veg)

# sizeclass.csv
sizeclass <- read_csv("data_raw/sapflow_csv/sizeclass.csv")
# Looks to be same info as veg: diameter, radius, size class (S or M)
# veg has more complete info on individuals and more variety of labels

# site.csv
site <- read_csv("data_raw/sapflow_csv/site.csv")
# 6 records, some metadata on instrumentation details

# site_date.csv
site_date <- read_csv("data_raw/sapflow_csv/site_date.csv")
# 5 records of start and end dates, but looks to be outdated

# sapflow.csv
sapflow <- read_csv("data_raw/sapflow_csv/sapflow.csv",
                    locale = locale(tz = "America/Los_Angeles"))
# 24 million records of raw 15 minutely delta voltages for each tree and probe

# probe.csv
probe <- read_csv("data_raw/sapflow_csv/probe.csv")
# 130 records of unique probe IDs, including at multiple depths

# probe_meta.csv
probe_meta <- read_csv("data_raw/sapflow_csv/probe_meta.csv",
                       locale = locale(tz = "America/Los_Angeles"))
tapply(probe_meta$timestamp, probe_meta$site_name, range, na.rm = TRUE)
# Micromet records for six sites, 2011-2019 for left and right,
# 2012-2019 for upland, 2012-2018 for valley

# This version for battery levels, to be joined
probe_batt <- read_csv("data_raw/sapflow_csv/probe_meta.csv",
                      locale = locale(tz = "America/Los_Angeles")) %>%
  select(site_name, timestamp, externalbatt_min)

# This version for standalone met data
met <- probe_meta %>%
  mutate(vpd = RHtoVPD(rh_avg, airtc_avg),
         date = as.Date(timestamp,
                        tz = "America/Los_Angeles"),
         year = year(timestamp), 
         doy = yday(timestamp))
  
# probe_date.csv
probe_date <- read_csv("data_raw/sapflow_csv/probe_date.csv")
# 143 records of time periods of bad data (?) for individual probes
# Need to email Wade to understand how it was generated
# And if that data can be thrown out

# Join together relevant tables for raw data page of app
unique(sapflow$probe_id) %in% unique(probe$probe_id)
unique(probe$veg_id) %in% unique(veg$veg_id)
unique(veg$site_name) %in% unique(site$site_name) # no need to join site table
sum(!is.na(unique(sapflow$timestamp) %in% unique(probe_meta$timestamp)))

sap_all <- sapflow %>%
  left_join(probe, by = "probe_id") %>%
  select(-starts_with("vdelta_")) %>%
  left_join(veg, by = "veg_id") %>%
  left_join(probe_batt, by = c("site_name", "timestamp"))

colnames(sap_all)

# saveRDS(sap_all, file = "app/sap_all.RDS")

# alternatively, if too big, save separate tables
saveRDS(sapflow, file = "app/sapflow.RDS")
saveRDS(probe, file = "app/probe.RDS")
saveRDS(veg, file = "app/veg.RDS")
saveRDS(probe_batt, file = "app/probe_batt.RDS")

saveRDS(met, file = "app/met.RDS")
