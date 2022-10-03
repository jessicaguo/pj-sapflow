# Clean and understand raw data files

#### Merge data from 7 flux files ####

library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(plantecophys)
library(ggplot2)
library(tidyr)

# Read in data from data_raw
fns <- list.files("data_raw/") # first 7 are sapflux files
raw_list <- list()
for(i in 1:7) {
  temp <- read_xlsx(paste0("data_raw/", fns[i]), 
                    sheet = "all data")
  raw_list[[i]] <- temp
}
raw_df <- do.call(rbind, raw_list)

clean_df <- raw_df %>%
  mutate(date = ymd(paste(year, month, day, sep = "-")),
         species = str_sub(`tree id`, 1, 1),
         species = case_when(species == "p" ~ "Pinyon",
                             species == "j" ~ "Juniper"),
         ind = as.numeric(str_sub(`tree id`, 2, 3)),
         site = str_sub(`tree id`, 4, 4),
         site2 = str_sub(`tree id`, 5, 5), 
         ind2 = case_when(site2 == "" ~ ind,
                          site == "a" ~ ind, # keep 53a as 53
                          site == "b" ~ 56), # rename 53b as 56
         site = case_when(site2 == "" ~ site,
                          site %in% c("a", "b") ~ site2),
         site = case_when(site == 'v' ~ "Valley",
                          site == "l" ~ "Left",
                          site == "r" ~ "Right",
                          site == "u" ~ "Upland")) %>%
  select(-ind, -site2) %>%
  rename(ind = ind2,
         doy = `julian day`,
         tree_id = `tree id`)

clean_df %>%
  group_by(species, site) %>%
  summarize(n_ind = length(unique(ind)),
            n_obs = n())
# More pinyons (29) than junipers (16)


# Save as .csv
write_csv(clean_df, file = "data_clean/flux_combined.csv")

#### Rename and structure env file ####
sheets <- c("left 2017", "right 2017", "upland 2017", "valley 2017",
            "left 2018", "right 2018", "upland 2018", "valley 2018")

env_list <- list()
for(i in 1:length(sheets)) {
  if(sheets[i] == "valley 2017") {
    raw_temp <- read_xlsx(paste0("data_raw/", fns[8]),
                          sheet = sheets[i]) %>%
      mutate(dt = as_datetime(`timestamp...2`) %>%
               force_tz(tzone = "America/Los_Angeles")) %>%
      mutate(par_avg = ifelse(par_total == 0, 0, `par_avg NO ZEROS`)) %>%
      select(-`timestamp...2`, -`timestamp...3`, -`par_avg NO ZEROS`) %>%
      relocate(par_avg, .before = par_total) %>%
      mutate(par_total = as.numeric(par_total),
             par_avg = as.numeric(par_avg))
      
  } else {
  raw_temp <- read_xlsx(paste0("data_raw/", fns[8]),
                        sheet = sheets[i]) %>%
    mutate(dt = as_datetime(`timestamp...2`) %>%
             force_tz(tzone = "America/Los_Angeles")) %>%
    select(-`timestamp...2`, -`timestamp...3`)%>%
    mutate(par_total = as.numeric(par_total),
           par_avg = as.numeric(par_avg))
  }
  
  env_list[[i]] <- raw_temp
}
lapply(env_list, colnames)
lapply(env_list, str)

# Combine into dataframe, add date and d_avg
env_df <- do.call(rbind, env_list) %>%
  mutate(date = paste(year, sprintf("%02d", month), sprintf("%02d", day),
                      sep = "-") %>%
           ymd(tz = "America/Los_Angeles"),
         d_avg = RHtoVPD(rh_avg, airtc_avg),
         site = str_to_sentence(site_name)) %>%
  # Summarize to daily by site and date
  group_by(site, date) %>%
  summarize(d_max = max(d_avg, na.rm = TRUE),
            d_mean = mean(d_avg, na.rm = TRUE),
            d_min = min(d_avg, na.rm = TRUE),
            d_n = sum(!is.na(d_avg)),
            par_sum = sum(par_avg, na.rm = TRUE),
            par_n = sum(!is.na(par_avg)),
            t_max = max(airtc_avg, na.rm = TRUE),
            t_mean = mean(airtc_avg, na.rm = TRUE),
            t_min = min(airtc_avg, na.rm = TRUE),
            t_n = sum(!is.na(airtc_avg)))

# Check sample sizes
env_df %>%
  select(site, date, d_n, par_n, t_n) %>%
  pivot_longer(-1:-2, 
               names_to = "type",
               values_to = "n") %>%
  ggplot(aes(x = date, y = n, color = site)) +
  geom_point() +
  facet_grid(rows = vars(site),
             cols = vars(type))
         
# Save as .csv
write_csv(env_df, file = "data_clean/env_combined.csv")
