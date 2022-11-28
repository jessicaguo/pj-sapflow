# Raw mV data from loggers
# 15-minutely, from Left site

library(readxl)
library(dplyr)
library(lubridate)
library(plantecophys)
library(ggplot2)
library(tidyr)

# Read in 1st sheet
l1 <- read_excel("data_raw/Compile Raw - LeftFifteen.xlsx",
                 sheet = "Left_fifteen_2012",
                 na = "NAN") %>%
  mutate(dt = force_tz(TIMESTAMP, "America/Los_Angeles"),
         date = date(dt),
         hour = hour(dt),
         month = month(dt))
str(l1)
attr(l1$ts, "tzone")
is.Date(l1$date)

#### Filtering criteria - general ####
l1_filter <- l1 %>%
  mutate(VPD = RHtoVPD(RH_Avg, AirTC_Avg)) %>%
  group_by(date) %>%
  mutate(AirTC_daily = mean(AirTC_Avg, na.rm = TRUE),
         AirTC_n = sum(!is.na(AirTC_Avg))) %>%
  filter(externalbatt_Min > 11.2,
         AirTC_daily > 0) %>%
  ungroup()

length(unique(l1$date)) # 349
length(unique(l1_filter$date)) # 262

#### Plot and test function ####
ggplot(l1_filter) +
  geom_point(aes(x = dt, y = SAP33_P9S)) +
  geom_point(aes(x = dt, y = VPD/10, color = "VPD"))

l1_Vmax <- l1_filter %>%
  filter(hour >= 0, hour <= 5) %>% # predawn only
  pivot_longer(starts_with("SAP"), 
               names_to = "Ind", 
               values_to = "V") %>%
  group_by(Ind, date) %>%
  mutate(Vmax = max(V, na.rm = TRUE),
         Vmean = mean(V, na.rm = TRUE),
         Vsd = sd(V, na.rm = TRUE),
         Dmin = min(VPD, na.rm = TRUE),
         Dmean = mean(VPD, na.rm = TRUE),
         Dsd = sd(VPD, na.rm = TRUE))


l1_Vmax %>%
  filter(Ind == "SAP33_P9S") %>%
  ggplot() +
  geom_point(aes(x = date, y = Vmax, col = "Vmax")) +
  geom_pointrange(aes(x = date, y = Vmean, 
                      ymin = Vmean - Vsd,
                      ymax = Vmean + Vsd,
                      col = "Vmean"),
                  size = 0.25) +
  # geom_point(aes(x = date, y = Dmin/10, col = "Dmin/10")) +
  # geom_pointrange(aes(x = date, y = Dmean/10, 
  #                     ymin = Dmean/10 - Dsd/10,
  #                     ymax = Dmean/10 + Dsd/10,
  #                     col = "Dmean/10"),
  #                 size = 0.25) +
  theme_bw()


ggplot() +
  geom_line(data = filter(l1_filter, month == 7),
            aes(x = dt, y = SAP33_P9S)) +
  geom_point(data = filter(l1_Vmax, Ind == "SAP48_P2N",
                           month == 7), 
             aes(x = dt, y = Vmax, color = "Vmax")) +
  geom_pointrange(data = filter(l1_Vmax, Ind == "SAP48_P2N",
                           month == 7), 
             aes(x = dt, y = Vmean,
                 ymin = Vmean - Vsd,
                 ymax = Vmean + Vsd,
                 col = "Vmean"),
             size = 0.25) +
  theme_bw()

ggplot(filter(l1_filter, month == 7)) +
  geom_point(aes(x = dt, y = SAP33_P9S)) +
  geom_point(aes(x = dt, y = SAP44_P6N)) +
  geom_point(aes(x = dt, y = SAP42_P1S)) +
  geom_point(aes(x = dt, y = VPD/10, color = "VPD"))
