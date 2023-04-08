# Apply baseline algorithm
# Calculate sap flux
library(dplyr)
library(TREXr)
library(lubridate)
library(readr)
library(zoo)

# Load data to baseline
baseline <- readRDS("app/baseline.RDS")
range(lapply(baseline, length))
# 92 sensors with between 2 and 58 consecutive time chunks

# Load identifying tables
probes <- read_csv("data_raw/sapflow_csv/probe.csv") # to join_by(probe_id), links to veg_id
veg <- read_csv("data_raw/sapflow_csv/veg.csv") # full table, but missing some saplengths
saplength <- read_csv("data_raw/PJ_sapwood_depths.csv",
                      col_select = 1:5) %>%
  tidyr::drop_na() # only PJ but includes saplength

# Add saplength_cm together
veg <- veg %>%
  left_join(select(saplength, veg_id, saplength_cm), 
            by = "veg_id") %>%
  mutate(saplength_cm = case_when(saplength_cm.x == saplength_cm.y ~ saplength_cm.y,
                                  is.na(saplength_cm.x) ~ saplength_cm.y,
                                  is.na(saplength_cm.y) ~ saplength_cm.x)) %>%
  select(-saplength_cm.x, -saplength_cm.y)

# Join probes with veg tables
probes_veg <- probes %>%
  left_join(select(veg, veg_id, veg_type, site_name, radius_cm, saplength_cm, diameter_cm),
            by = "veg_id")

probes_veg$probe_id[which(probes_veg$saplength_cm < 2)]

# Loop to baseline with moving window and 3 day interval
sfd_list <- list()
for(i in 1:length(baseline)) { # number of sensors
  # Bind all time chunks 
  temp <- do.call(rbind, baseline[[i]])
  
  # Baseline with 3-day moving window
  baselined <- tdm_dt.max(temp,
                          methods = c("mw"),
                          interpolate = FALSE,
                          max.days = 3,
                          df = FALSE)
  
  sapwood_depth <- probes_veg$saplength_cm[which(probes_veg$probe_id == names(baseline)[[i]])]
  # Clearwater correction if saplength < 2
  if(sapwood_depth < 2){
    baselined <- tdm_hw.cor(baselined, probe.length = 20,
                      sapwood.thickness = sapwood_depth*10)
  }
  
  
  # Calculate SFD
  sfd <- tdm_cal.sfd(baselined, 
                     df = FALSE, 
                     wood = "Coniferous") # Using 7 calibrations of coniferous wood
  

  # Extract relevant outputs to dataframe (including probe_id and parameters)
  sfd_list[[i]] <- cbind.data.frame(probe_id = names(baseline)[i],
                                    # Convert timestamp back to America/Los_Angeles
                                    timestamp = with_tz(index(baselined$input), tzone = "America/Los_Angeles"),
                                    deltaV = as.numeric(baselined$input),
                                    max.mw = as.numeric(baselined$max.mw),
                                    k = sfd$sfd.mw$k,
                                    sfd = sfd$sfd.mw$sfd,
                                    q025 = sfd$sfd.mw$q025,
                                    q975 = sfd$sfd.mw$q975)
  
  print(paste0("Sensor ", i, " of 92 completed"))
}

# Bind all sfd for all 92 sensors
sfd_all <- do.call(rbind, sfd_list)


sfd_combined <- sfd_all %>%
  left_join(select(probes_veg, probe_id, veg_id, veg_type, 
                   site_name, saplength_cm, diameter_cm), by = "probe_id")

saveRDS(sfd_combined, file = "data_clean/SFD_all.RDS")

