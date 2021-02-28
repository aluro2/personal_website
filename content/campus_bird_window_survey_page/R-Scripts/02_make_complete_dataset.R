
# Load packages -----------------------------------------------------------
library(tidyverse)

survey_data_paths <-
  list.files("Data", "UIUC_bird_survey_data", full.names = TRUE)

full_data <-
  bind_rows(
    lapply(survey_data_paths[!grepl("all_years", survey_data_paths)], read_csv)) %>%
    mutate(year = as.factor(lubridate::year(date_mm_dd_yyyy)),
           month = as.factor(lubridate::month(date_mm_dd_yyyy)),
           month_day = as.factor(format(date_mm_dd_yyyy, format="%m-%d"))) %>%
    filter(year %in% c(2019,2020)) %>%
    select(year,
           month,
           month_day,
           date_mm_dd_yyyy,
           any_birds_found_on_route_shift,
           route_shift,
           route_name,
           building_name,
           building_face_direction,
           species,
           found_dead_alive)

write_csv(full_data, "Data/UIUC_bird_survey_data_all_years.csv")
