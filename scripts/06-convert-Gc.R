#### Process to tree-level conductance ####
library(dplyr)
library(readr)
library(udunits2)
library(ggplot2)
library(suncalc)

# Function for conductance coefficient Kg
# Phillips and Oren 1998
Kg <- function(t_C) {
  return(115.8 + 0.4236*t_C)
  # Units kPa m^3 kg^-1
}

#  Read in sap flux density data
sfd_combined <- readRDS(file = "data_clean/SFD_all.RDS")
# Read in met data
met_composite <- readRDS(file = "app/met_composite.RDS")

##### Convert to Gc using simplified Penman-Monteith inversion #####

# Average north and south probes
# No correction for leaf area applied - reported on a per sapwood area basis
sfd_avg <- sfd_combined %>%
  group_by(veg_id, timestamp, veg_type, site_name) %>%
  summarize(sfd_mean = mean(sfd, na.rm = TRUE),
            # Convert to kg so units will cancel in later calculations
            sfd_kg = ud.convert(sfd_mean, "g cm^-2 h^-1", "kg m^-2 s^-1")) 

# Add conductance coefficient Kg
met_composite <- met_composite %>%
  mutate(cond_coef = Kg(airtc))

#  Join sfd_avg and met_composite, all weather data is present
cond_hourly <- sfd_avg %>%
  left_join(select(met_composite, timestamp,
                   year, date, airtc, cond_coef, vpd),
            join_by(timestamp)) %>%
  mutate(gc_ms = cond_coef * sfd_kg / vpd) # yields Gc in m s^-1

##### Filtering data #####
# create sunrise/sunset dataframe to join by
sun <- getSunlightTimes(
  date = seq(as.Date("2013-01-01"), as.Date("2018-12-31"),
             by = "day"),
  lat = 39 + 28/60, # Porter Canyon Snotel
  lon = -117 - 37/60,
  keep = c("sunrise", "sunset"),
  tz = "America/Los_Angeles"
)

# Filter by VPD > 1 and daytime
cond_daytime <- cond_hourly %>%
  semi_join(sun, join_by(date, between(timestamp, sunrise, sunset))) %>%
  filter(vpd >= 1)

saveRDS(cond_daytime, file = "data_clean/gc_daytime_hourly.RDS")

# Summarize to daily as means if n >= 12
cond_daytime <- readRDS("data_clean/gc_daytime_hourly.RDS")
# Highest density of data from April through October

cond_daily <- cond_daytime %>%
  group_by(veg_id, date, veg_type, site_name) %>%
  summarize(n = n(),
            sfd_mean = mean(sfd_mean, na.rm = TRUE), # flux density g cm^-2 h^-1
            gc_ms = mean(gc_ms, na.rm = TRUE), # stomatal conductance m/s
            vpd_mean = mean(vpd, na.rm = TRUE), # mean daytime vpd
            vpd_max = max(vpd, na.rm = TRUE), # max daytime vpd
            vpd_min = min(vpd, na.rm = TRUE)) %>% # min daytime vpd
  filter(n >= 12) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  filter(month %in% 4:10) %>%
  ungroup()

# Check start and end dates
cond_daily %>%
  group_by(year) %>%
  summarize(st = min(date),
            en = max(date))

# Check number of tree-days per month
cond_daily %>%
  mutate(month = lubridate::month(date)) %>%
  count(month)

#### Detecting outliers ####
# narrowed down to 2015 in June, for both P and J
#and 2016 ine June, P only

# Plot in gc vs. log vpd space, by year/month
cond_daily %>%
  filter(year == 2015,
         month == 6) %>%
  ggplot(aes(x = log(vpd_mean), 
             y = gc_ms,
             col = veg_type)) +
  geom_point(alpha = 0.3) +
  facet_grid(rows = vars(veg_type),
             cols = vars(year),
             scales = "free") 

# Plot timeseries for each year by tree

cond_daily %>%
  filter(year %in% 2015:2016,
         month == 6) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = gc_ms,
             color = veg_id)) +
  geom_line(aes(y = gc_ms,
                group = veg_id,
                color = veg_id)) +
  geom_point(aes(y = vpd_max),
             color = "black") +
  geom_point(aes(y = vpd_min),
             color = "skyblue") +
  facet_grid(rows = vars(veg_type),
             cols = vars(year),
             scales = "free") +
  theme(legend.position = "none")

# Identify individuals for diff time periods
# Pinyon June 2016
p16 <- cond_daily %>%
  filter(veg_type == "pinyon",
         date >= as.Date("2016-06-20"),
         date <= as.Date("2016-06-30")) %>%
  arrange(desc(gc_ms))

# check p41v
p16_hourly <- cond_daytime %>%
  filter(veg_id == "p41v",
         date >= as.Date("2016-06-19"),
         date <= as.Date("2016-06-23")) 

ggplot(p16_hourly, aes(x = timestamp,
                       y = sfd_mean)) +
  geom_point()

# Pinyon June 2015
p15 <- cond_daily %>%
  filter(veg_type == "pinyon",
         date >= as.Date("2015-06-15"),
         date <= as.Date("2015-06-22")) %>%
  arrange(desc(gc_ms))

# check "p61u": pu61s remove blips in raw from 2015-06-16 to 2015-06-18
# "p64u":  pu64s remove blips in raw from 2015-06-16 to 2015-06-18
# "p10l": sap46_p10n remove 2015-06-17
# "p11l": sap37_p11n remove 2015-06-17
# "p06l": 
p15_hourly <- cond_daytime %>%
  filter(veg_id %in% c("p61u", "p64u", "p10l", "p11l", "p06l"),
         date >= as.Date("2015-06-15"),
         date <= as.Date("2015-06-22"))

ggplot(p15_hourly, aes(x = timestamp)) +
  geom_point(aes(y = sfd_mean,
                 color = veg_id)) +
  geom_point(aes(y = vpd*75),
             color = "black")

# Juniper June 2015
j15 <- cond_daily %>%
  filter(veg_type == "juniper",
         date >= as.Date("2015-06-15"),
         date <= as.Date("2015-06-22")) %>%
  arrange(desc(gc_ms))

# Any 3 day peak is indicative of a baselining problem
# Likely a blip in the board that results in a 3-day shift due to the baselining algorithm

# sfd or leaf transpiration may be converted to mmol m^-2 s^-1
# Gc or canopy conductance may be converted to mmol m^-2 s^-1

##### Screen data #####

# Possible criteria include days when min temp < 0
# days when rained
# half-hours when VPD < 0.1 kPa


##### Summarize Js (sum) and Gc (mean) to daily/diurnal #####
# Diurnal is easy enough, group by date
# Daily/daytime might require sunrise and sunset times for that latitude