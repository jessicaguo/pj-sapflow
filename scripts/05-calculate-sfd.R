# Apply baseline algorithm
# Calculate sap flux
library(dplyr)
library(TREXr)
library(ggplot2)

l# Load data to baseline
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
                                    timestamp = as.POSIXct(baselined$input$timestamp, tz = "GMT"),
                                    deltaV = baselined$input$value,
                                    max.mw = baselined$max.mw$value,
                                    k = sfd$sfd.mw$k,
                                    sfd = sfd$sfd.mw$sfd,
                                    q025 = sfd$sfd.mw$q025,
                                    q975 = sfd$sfd.mw$q975)
}

# Bind all sfd for all 92 sensors
sfd_all <- do.call(rbind, sfd_list)

