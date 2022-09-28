# Merge data from 7 .csv files

library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)

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
