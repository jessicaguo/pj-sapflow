# Prepare cleaned/joined table for app
library(readr)
library(dplyr)
library(tidyr)
library(plantecophys)
library(lubridate)

`%nin%` <- Negate(`%in%`)

#### Snotel ####
sno <- readRDS("data_clean/snotel.RDS") %>%
  filter(tavg_C <=0)

#### Tables ####
# veg.csv
veg <- read_csv("data_raw/sapflow_csv/veg.csv")

# sapflow.csv
sapflow <- read_csv("data_raw/sapflow_csv/sapflow.csv",
                    locale = locale(tz = "America/Los_Angeles")) %>%
  mutate(date = as.Date(timestamp,
                        tz = "America/Los_Angeles")) %>%
  filter(date %nin% sno$date)
# probe.csv
probe <- read_csv("data_raw/sapflow_csv/probe.csv")

# probe_meta.csv, convert to met data
met <- read_csv("data_raw/sapflow_csv/probe_meta.csv",
                       locale = locale(tz = "America/Los_Angeles")) %>%
  mutate(vpd = RHtoVPD(rh_avg, airtc_avg),
         date = as.Date(timestamp,
                        tz = "America/Los_Angeles"),
         year = year(timestamp), 
         doy = yday(timestamp))

# probe_meta.csv, convert to battery levels
probe_batt <- read_csv("data_raw/sapflow_csv/probe_meta.csv",
                       locale = locale(tz = "America/Los_Angeles")) %>%
  select(site_name, timestamp, externalbatt_min)


# probe_date.csv, use to remove bad data periods/probes
probe_date <- read_csv("data_raw/sapflow_csv/probe_date.csv",
                       locale = locale(tz = "America/Los_Angeles"))

# Join together relevant tables for raw data page of app
unique(sapflow$probe_id) %in% unique(probe$probe_id)
unique(probe$veg_id) %in% unique(veg$veg_id)
sum(!is.na(unique(sapflow$timestamp) %in% unique(probe_batt$timestamp)))

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

sapflux <- sap_all |> 
  anti_join(sap_to_trim) %>%
  mutate(year = year(timestamp),
         doy = yday(timestamp)) %>%
  relocate(year, date, doy, .after = timestamp)

# Save as RDS files in app/
saveRDS(veg, file = "app/veg.RDS")
saveRDS(met, file = "app/met.RDS")
saveRDS(sapflux, file = "app/sapflux.RDS")
