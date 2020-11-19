
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(googlesheets)
library(janitor)
library(ggthemes)
library(extrafont)
library(htmltools)
library(leaflet)
library(DT)
library(stringr)


# Survey data -------------------------------------------------------------

bird_data_url <-
  gs_url("https://docs.google.com/spreadsheets/d/1M2hSw0TBxRry3Z7FA4ttU3QAvnoWHhb2MKV7J4tgUbQ/edit#gid=0")

bird_data <-
  bird_data_url %>%
  gs_read(., ws = 1) %>%
  clean_names() %>%
  mutate(date_mm_dd_yyyy = lubridate::mdy(date_mm_dd_yyyy)) %>%
  select(-collector_name_s)

dataset_name <-
  paste("Data/UIUC_bird_survey_data_backup_",
        lubridate::year(bird_data_url$updated),
        ".csv",
        sep = "")

write.csv(bird_data, dataset_name)
