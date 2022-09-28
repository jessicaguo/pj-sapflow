# Merge data from 7 .csv files

library(readxl)
library(dplyr)
library(lubridate)
library(stringr)

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
         ind
         
         
         
         %>%
           case_when(. == 'v' ~ "Valley",
                     . == "l" ~ "Left",
                     . == "r" ~ "Right",
                     . == "u" ~ "Upland"))
