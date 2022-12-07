# Understand and join database csv's
# Wade's database output as csv for Susan
# Tables located in data_raw/sapflow_csv/
# P is Pinus monophylla
# J is Juniperus osteosperma

# Also prep snotel data

library(readr)
library(dplyr)
library(tidyr)
library(plantecophys)
library(lubridate)
library(readxl)

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
probe_date <- read_csv("data_raw/sapflow_csv/probe_date.csv",
                       locale = locale(tz = "America/Los_Angeles"))
# 143 records of time periods of bad data (?) for individual probes
# Data can be eliminated from sapflow


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

# Clunky way of non-equi join: for loop and anti-join
sap_remove <- list()
for(i in 1:nrow(probe_date)){
  sap_remove[[i]] <- sap_all |> 
    filter(probe_id == probe_date$probe_id[i],
           timestamp >= probe_date$date_begin[i], 
           timestamp <= probe_date$date_end[i])
}
sap_to_trim <- do.call(rbind, sap_remove)
nrow(sap_to_trim)/nrow(sap_all) # 5.87% of observations removed

sap_trimmed <- sap_all |> 
  anti_join(sap_to_trim)

##### SNOTEL #####

sno <- read_xlsx("data_raw/Porter_SNOTEL_daily summary.xlsx",
                 skip = 3) %>%
  mutate(Date = as.Date(Date),
         date = as.Date(force_tz(Date, tz = "America/Los_Angeles")) - 1) %>%
  relocate(date, .after = Date) %>%
  rename(tavg_C = `TAVG.D-1 (degC)`)

saveRDS(sno, file = "data_clean/snotel.RDS")

