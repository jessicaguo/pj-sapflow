# TREX package to baseline raw delta V data
library(lubridate)
library(TREXr)
library(dplyr)

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
                  time.int = 15,
                  max.gap = 60,
                  decimals = 5,
                  df = TRUE)
sum(is.na(input$value)) #42970
# Remove obvious outliers
input$value[which(input$value<0.2)]<- NA # Check with Susan - is this the right threshold?
sum(is.na(input$value)) # 54256

# Baselining - max delta T values

baselined <- tdm_dt.max(input, 
                        methods = c("pd", "mw", "dr"),
                        det.pd = TRUE,
                        interpolate = FALSE,
                        max.days = 10,
                        df = FALSE)

