#### Process to tree-level conductance ####
library(dplyr)
library(readr)
library(udunits2)
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

# sfd or leaf transpiration may be converted to mmol m^-2 s^-1
# Gc or canopy conductance may be converted to mmol m^-2 s^-1

##### Screen data #####

# Possible criteria include days when min temp < 0
# days when rained
# half-hours when VPD < 0.1 kPa


##### Summarize Js (sum) and Gc (mean) to daily/diurnal #####
# Diurnal is easy enough, group by date
# Daily/daytime might require sunrise and sunset times for that latitude