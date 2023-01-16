# Prepare curated sensor data to be baselined
# Add manual curation and checks 

library(readr)
library(lubridate)
library(TREXr)
library(dplyr)
library(ggplot2)
# pak::pak("tidyverse/dplyr") # For trying development version of dplyr
library(dplyr)
#### Validation and testing of manual curation ####
# Read in delineated times
clip <- read_csv("data_raw/sapflow_clip.csv")

# Test whether years match by each row
test_year <- clip %>% 
  mutate(yr = year(date_start),
         yr2 = year(date_end),
         yr_match1 = ifelse(year == yr, TRUE, FALSE),
         yr_match2 = ifelse(year == yr2, TRUE, FALSE),
         yr_match3 = ifelse(yr == yr2, TRUE, FALSE))
sum(test_year$yr_match1) == nrow(clip)
sum(test_year$yr_match2) == nrow(clip)
sum(test_year$yr_match3) == nrow(clip)

# Test if duration is at least 7 days
test_dur <- clip %>% 
  mutate(dur = difftime(date_end, date_start, units = "days") %>%
           as.numeric() + 1)
length(which(test_dur$dur < 7)) == 0

# Test if periods are non-overlapping
test_overlap <- clip %>%
  group_by(probe_id) %>%
  mutate(date_start_next = lead(date_start),
         non_overlap = ifelse(date_start_next > date_end, TRUE, FALSE))
sum(test_overlap$non_overlap, na.rm = TRUE) == sum(!is.na(test_overlap$date_start_next))

# Test if probe labels are correct
probe <- read_csv("data_raw/sapflow_csv/probe.csv")

sum(unique(clip$probe_id) %in% probe$probe_id) == length(unique(clip$probe_id))

####  Develop data for app, round 1 ####
# Read in data
sapflux <- readRDS("app/sapflux.RDS")
attr(sapflux$timestamp, "tzone")

# Constrain by criteria:
# Only from 4 PJ sites
# Only 0-2 depth
# Only 2013-2018 inclusive
sap <- sapflux %>%
  filter(site_name %in% c("left", "right", "upland", "valley"),
         depth_upper == 0,
         depth_lower == 2,
         year %in% 2013:2018)

sensors <- unique(sap$probe_id) 
length(sensors) # 94 sensors
length(unique(sap$veg_id)) # 47 trees

# Use first pass of manual sapflow_clip 

dat1 <- data.frame(probes = rep(c("x", "y", "z"), each = 30),
                   date = rep(seq(as.Date("2023-01-01"), as.Date("2023-01-30"), length.out = 30),
                              3))
dat2 <- data.frame(probes = c("x", "y", "z"),
                   date_st = c(as.Date("2023-01-02"), as.Date("2023-01-03"), as.Date("2023-01-04")),
                   date_en = c(as.Date("2023-01-19"), as.Date("2023-01-20"), as.Date("2023-01-21")))
test <- dat1 |>
  right_join(dat2, join_by(date >= date_st, date <= date_en, probes))

test <- sap %>%
  right_join(clip, join_by(date >= date_start, date <= date_end, probe_id))

nrow(sap) - nrow(test)
# Set up empty list of lists - will have 94 elements, each a list

sap_list <- list()

for(i in 1:length(sensors)){
  # Subset for each sensor
  foo <- sap %>%
    filter(probe_id == sensors[i]) %>%
    select(timestamp, vdelta) %>%
    rename(value = vdelta) %>%
    mutate(timestamp = with_tz(timestamp, tzone = "GMT")) # must convert to GMT, then convert back

  # Convert to raw data format needed for TREX package
  raw <- is.trex(foo,
                 tz = "GMT", # 
                 time.format = '%Y-%m-%d %H:%M:%S', # 
                 solar.time = FALSE,
                 long.deg = FALSE,
                 ref.add = FALSE,
                 df = TRUE)

  # Adjust timesteps and linearly interpolate gaps
  input <- dt.steps(input = raw,
                    start = as.character(min(foo$timestamp)),
                    end = as.character(max(foo$timestamp)),
                    time.int = 30, # summarize to 30-minutely
                    max.gap = 60,
                    decimals = 5,
                    df = TRUE)

  # Remove obvious outliers at threshold of 0.2
  input$value[which(input$value < 0.2)]<- NA 
  
  ### Separate into discrete chunks of time
  # Calculate difftime between consecutive rows
  input2 <- input %>%
    mutate(ts = as.POSIXct(timestamp, tz = "GMT")) %>%
    filter(!is.na(value)) %>%
    mutate(diff = difftime(ts, lag(ts), units = "mins") %>%
             as.numeric())
  
  # Create grouping for consecutive time chunks
  ind_group <- c(1, which(input2$diff > 30), nrow(input2) + 1)
  times_group <- (ind_group - lag(ind_group))[-1]
  input2$group_consec <- rep(1:length(times_group), times = times_group)
  
  
  # Use group_split to  separate inputs by group
  input_list <- input2 %>%
    select(-ts, -diff) %>%
    group_split(group_consec, .keep = FALSE)
  
  # Use lapply to turn back into plain data.frame (TREX does not work with tibbles)
  input_list <- lapply(input_list, FUN = as.data.frame)
  
  # Select only list elements with 7 days or more of consecutive data 
  # (minimum required by the 'dr' baselining option)
  ind_7 <- which(times_group > 7*48)
  input_list <- input_list[ind_7]
  
  sap_list[[i]] <- input_list
  
  print(paste("Sensor", sensors[i], "complete,", i, "of 94"))
}
names(sap_list) <- sensors

saveRDS(sap_list, file = "app/baseline.RDS")



#### Sandbox for app to baseline ####

dr_list <- list()
for(i in 1:length(sap_list[[sensors[6]]])) { # input$sensor
  baselined <- tdm_dt.max(as.data.frame(sap_list[[sensors[6]]][[i]]), 
                          methods = c("dr"),
                          interpolate = FALSE,
                          max.days = 3, # input$dr_interval
                          df = TRUE)
  
  dr_list[[i]] <- cbind.data.frame(timestamp = as.POSIXct(baselined$input$timestamp, tz = "GMT"),
                        deltaV = baselined$input$value, 
                        max.dr = baselined$max.dr$value)
}

dr <- do.call(rbind, dr_list)

ggplot(dr, aes(x = timestamp)) +
  geom_point(aes(y = deltaV, col = "delta_V")) +
  geom_step(aes(y = max.dr, col = "Baseline"), na.rm = TRUE)
