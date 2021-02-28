#  Load packages ----------------------------------------------------------
library(tidyverse)
library(googlesheets)
library(janitor)
library(ggthemes)
library(extrafont)
library(plotly)
library(ggmap)

# Import survey data ------------------------------------------------------

building_face_coords <-
  read_csv("Data/NorthQuad-BuildingFace-Coords.csv")

northquad_bldgs <-
  read_csv("Data/UIUC_bird_survey_data_all_years.csv") %>%
  filter(route_name== "North Quad") %>%
  distinct(building_name) %>%
  na.omit() %>%
  pull(building_name)

northquad_data <-
  read_csv("Data/UIUC_bird_survey_data_all_years.csv") %>%
  filter(building_name %in% northquad_bldgs,
         any_birds_found_on_route_shift == "Yes") %>%
  left_join(building_face_coords) %>%
  select(building_name, building_face_direction, lat, lon) %>%
  group_by(building_name,lat, lon)

northquad_count <-
  read_csv("Data/UIUC_bird_survey_data_all_years.csv") %>%
  filter(building_name %in% northquad_bldgs,
         any_birds_found_on_route_shift == "Yes") %>%
  left_join(building_face_coords) %>%
  select(building_name, building_face_direction, lat, lon) %>%
  group_by(building_name,lat, lon) %>%
  summarise(n_birds = n()) %>%
  filter(!building_name == "Everitt Electrical & Computer Engineering Laboratory_37",
         n_birds > 10)


# Save summary datast -----------------------------------------------------

read_csv("Data/UIUC_bird_survey_data_all_years.csv") %>%
  filter(building_name %in% northquad_bldgs,
         any_birds_found_on_route_shift == "Yes") %>%
  left_join(building_face_coords) %>%
  select(building_name, building_face_direction) %>%
  group_by(building_name, building_face_direction) %>%
  summarise(n_birds = n()) %>%
  write_csv("Data/northquad-bird-counts-FA2019-2020.csv")


# Get map -----------------------------------------------------------------

north_quad_map <-
  get_googlemap(
    "Hydro-Systems Laboratory",
    #center = c(lat =40.11449934131326, lon = -88.22650031333202),
    zoom = 17,
    maptype = "satellite",
    color = "bw",
    size = c(640, 640))

north_quad_map %>%
  ggmap() +
  geom_jitter(data = northquad_data,
             aes(x = lon,
                 y = lat),
             shape = 4,
             stroke = 0.5,
             size = 3,
             alpha = 1,
             fill = "yellow",
             color = "yellow",
             width = 0.0002,
             height = 0.0001) +
  geom_label(data = northquad_count,
            aes(label=n_birds),
            hjust=0.5,
            vjust=-1,
            color = "yellow",
            fontface = "bold",
            size = 5,
            alpha = 0.5,
            fill = "black")+
  labs(title = "Number of Window-Killed Birds at UIUC North Campus",
       subtitle = "Survey conducted in September-November of 2019-2020",
       caption = "Yellow X represents single bird found") +
  guides(size = FALSE) +
  theme_void() +
  theme(title = element_text(size = 14))

ggsave("Figs/beckman_collisions.png",
       width = 12,
       height = 12,
       units = "in",
       dpi = 300)
