# Apply baseline algorithm
# Calculate sap flux
library(dplyr)
library(TREXr)
library(lubridate)
library(readr)

# Load data to baseline
baseline <- readRDS("app/baseline.RDS")
range(lapply(baseline, length))
# 92 sensors with between 2 and 58 consecutive time chunks


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
                          df = TRUE)
  
  # Calculate SFD
  sfd <- tdm_cal.sfd(baselined, 
                     df = FALSE, 
                     wood = "Coniferous") # Using 7 calibrations of coniferous wood
  

  # Extract relevant outputs to dataframe (including probe_id and parameters)
  sfd_list[[i]] <- cbind.data.frame(probe_id = names(baseline)[i],
                                    # Convert timestamp back to America/Los_Angeles
                                    timestamp = with_tz(as.POSIXct(baselined$input$timestamp, tz = "GMT"), tzone = "America/Los_Angeles"),
                                    deltaV = baselined$input$value,
                                    max.mw = baselined$max.mw$value,
                                    k = sfd$sfd.mw$k,
                                    sfd = sfd$sfd.mw$sfd,
                                    q025 = sfd$sfd.mw$q025,
                                    q975 = sfd$sfd.mw$q975)
  
  print(paste0("Sensor ", i, " of 92 completed"))
}

# Bind all sfd for all 92 sensors
sfd_all <- do.call(rbind, sfd_list)

# Load and join with identifying tables
probes <- read_csv("data_raw/sapflow_csv/probe.csv") # to join_by(probe_id), links to veg_id
veg <- read_csv("data_raw/sapflow_csv/veg.csv")

sfd_combined <- sfd_all %>%
  left_join(select(probes, probe_id, veg_id), by = "probe_id") %>%
  left_join(select(veg, veg_id, veg_type, site_name, diameter_cm), by = "veg_id")

saveRDS(sfd_combined, file = "data_clean/SFD_all.RDS")

