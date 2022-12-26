# TREX package to baseline raw delta V data
library(lubridate)
library(TREXr)
library(dplyr)
library(plotly)

sapflux <- readRDS("app/sapflux.RDS")
attr(sapflux$timestamp, "tzone")


# sensor_url <- RCurl::getURL("https://github.com/deep-org/workshop_data/blob/master/esa-workshop2020/TDP%20data/N13Ad_S1_dV%20(mv).txt")
sensor <- read.csv("test.csv")
sensor <- data.frame(timestamp = mdy_hms(sensor$Timestamp, tz = "GMT"), 
                     value = sensor$dV)

test <- is.trex(sensor,
                tz="GMT",
                time.format="%Y-%m-%d %H:%M:%S",
                solar.time=FALSE,
                long.deg=FALSE,
                ref.add=FALSE,
                df=TRUE)


#### Test on single sensor ####
sensors <- unique(sapflux$probe_id) 

foo <- sapflux %>%
  filter(probe_id == sensors[1]) %>%
  select(timestamp, vdelta) %>%
  rename(value = vdelta) %>%
  mutate(timestamp = with_tz(timestamp, tzone = "GMT")) # must convert to GMT
hist(foo$value, breaks = 60)
attr(foo$timestamp, "tzone")

# Check raw data format
raw <- is.trex(foo,
               tz = "GMT", # 
               time.format = '%Y-%m-%d %H:%M:%S', # 
               solar.time = FALSE,
               long.deg = FALSE,
               ref.add = FALSE,
               df = TRUE)
sum(is.na(raw$value))

# Adjust timesteps and linearly interpolate gaps
input <- dt.steps(input = raw,
                  start = as.character(min(foo$timestamp)),
                  end = as.character(max(foo$timestamp)),
                  time.int = 30, # summarize to 30-minutely
                  max.gap = 60,
                  decimals = 5,
                  df = TRUE)
str(input)
sum(is.na(input$value)) #21484

# Remove obvious outliers
input$value[which(input$value < 0.2)]<- NA # Check with Susan - is this the right threshold?
sum(is.na(input$value)) # 27109

# how to label only consecutive chunks of time?
input2 <- input %>%
  mutate(ts = as.POSIXct(timestamp, tz = "GMT")) %>%
  filter(!is.na(value)) %>%
  mutate(diff = difftime(ts, lag(ts), units = "mins") %>%
           as.numeric())

ind_group <- c(1, which(input2$diff > 30), nrow(input2) + 1)
times_group <- (ind_group - lag(ind_group))[-1]
sum(times_group)

test <- rep(1:length(times_group), times = times_group)
nrow(input2)
length(test)

input2$group_consec <- rep(1:length(times_group), times = times_group)

which(times_group > 7*48)
# Use group_split to  separate inputs by group

input_list <- input2 %>%
  select(-ts, -diff) %>%
  group_split(group_consec, .keep = FALSE)
length(input_list)
# Shiny app to interactively remove outliers
# outlier()


#### Add Baselining step to app - allow user to adjust the number of days?

# Baselining - max delta T values
# Only predawn 'pd' works with gaps
# moving window 'mw' and double regression 'dr' require complete dataset (?)
baselined <- tdm_dt.max(as.data.frame(input_list[[11]]), 
                        methods = c("dr"),
                        det.pd = TRUE,
                        interpolate = FALSE,
                        max.days = 5,
                        df = FALSE)
str(baselined)

plot(baselined$input, ylab = expression(Delta*italic("V")))

lines(baselined$max.dr, col = "green")
