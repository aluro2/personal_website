
#  Load packages ----------------------------------------------------------


library(tidyverse)
library(googlesheets)
library(janitor)
library(ggthemes)
library(extrafont)
library(plotly)

loadfonts()

##Only need to run if fonts haven't been added to your computer yet (Windows)
# font_import()
#

# Import survey data ------------------------------------------------------

survey_data_paths <-
  list.files("Data", "UIUC_bird_survey_data", full.names = TRUE)

bird_data <-
  bind_rows(
    lapply(survey_data_paths, read_csv)) %>%
  filter(any_birds_found_on_route_shift == "Yes") %>%
  mutate(year = as.factor(lubridate::year(date_mm_dd_yyyy)),
         month = as.factor(lubridate::month(date_mm_dd_yyyy)),
         month_day = as.factor(format(date_mm_dd_yyyy, format="%m-%d"))) %>%
  select(year,
         month,
         month_day,
         date_mm_dd_yyyy,
         route_shift,
         route_name,
         building_name,
         building_face_direction,
         species,
         found_dead_alive)


unique(bird_data$species)
names(bird_data)

# Birds found by date/year ------------------------------------------------


bird_data %>%
  filter(
         !species == "NA",
         !species == "Unidentifiable") %>%
  select(species, year, month_day) %>%
  table(.) %>%
  as_tibble(.) %>%
  mutate(species = str_extract(species, "[^_]+")) %>%
  arrange(., order(month_day), species) %>%
  mutate(species = factor(species, levels = unique(species))) %>%
  select(year, month_day, n) %>%
  group_by(year, month_day) %>%
  summarise(n_birds_found = sum(n)) %>%
  ggplot(., aes(x = month_day, y = n_birds_found, group = year, fill = year)) +
  geom_area(alpha = 0.5) +
  scale_fill_fivethirtyeight() +
  xlab("Date") +
  ylab("Number of Birds Found") +
  theme_fivethirtyeight() +
  scale_y_continuous(
    breaks = function(x) unique(
      floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  theme_fivethirtyeight() +
  theme(
    #panel.background = element_rect(fill = "black",
    #colour = "black",
    #size = 0.5, linetype = "solid"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(size = 20, face = "bold"),
    text = element_text(family = "Calibri", size = 20),
    legend.position = "top",
    panel.grid.major.x = element_blank())



# Species Plot ------------------------------------------------------------

species_plot <-
  bird_data %>%
    filter(
           !species %in% c(
             "NA","Unidentifiable", "unknown", "thrush sp.", "Thrush", "Warbler"),
           !str_detect(species, "[?]")) %>%
    select(species) %>%
    mutate(species = str_trim(species, side = "both"),
           species = str_extract(species, "[^_]+")) %>%
    table(.) %>%
    as_tibble(.) %>%
    rename(Species = ".",
           "Number of Birds Found" = n ) %>%
    arrange(., desc(`Number of Birds Found`), Species) %>%
    mutate(Species = factor(Species, levels = unique(Species)),
           Total = sum(`Number of Birds Found`),
           `Percent of Birds Found` = round(`Number of Birds Found`/Total*100, 1)) %>%
  ggplot(aes(
    text = paste(
      "Species: ", Species, "\n",
      "Total: ", `Number of Birds Found`, "\n",
      "% of Total: ", `Percent of Birds Found`, "\n",
      sep = ""
    ))) +
  geom_linerange(
    aes(x = Species, ymin = 0, ymax = `Percent of Birds Found`),
    color = "darkgray",
    size = 1.5) +
  geom_point(aes(fill = Species, x = Species, y = `Percent of Birds Found`),
             color = "darkgrey",  size = 7, shape = 21 ) +
  scale_fill_viridis_d(direction = -1, option = "plasma") +
  scale_y_continuous(
      breaks = function(x) unique(
        floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  theme_fivethirtyeight() +
  theme(#panel.background = element_rect(fill = "black",
                                        #colour = "black",
                                       #size = 0.5, linetype = "solid"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Calibri", size = 20),
        legend.position = "none",
        panel.grid.major.x = element_blank())

ggplotly(species_plot, tooltip = "text")


ggsave("Week_1_species.png", dpi = 600, width = 10, height = 8, device = "png")

## Area plot

cols <- colorRampPalette(c("darkred", "gray"))

colors <- cols(4)

bird_data %>%
  filter(any_birds_found_on_route_shift == "Yes") %>%
  select(building_face_direction) %>%
  table(.) %>%
  as_tibble(.) %>%
  rename("Building Face Direction" = ".",
         "Number of Birds Found" = n ) %>%
  arrange(., desc(`Number of Birds Found`), `Building Face Direction`) %>%
  mutate(`Building Face Direction` = factor(`Building Face Direction`, levels = unique(`Building Face Direction`))) %>%
  ggplot(., aes(x = `Building Face Direction`, y = `Number of Birds Found`)) +
  geom_linerange(
    aes(x = `Building Face Direction`, ymin = 0, ymax = `Number of Birds Found`),
    color = "darkgray",
    size = 1.5) +
  geom_point(size = 10, color =  colors) +
  ggtitle("Week 1 \n Birds Found by Building Face") +
  theme_fivethirtyeight() +
  theme(#panel.background = element_rect(fill = "black",
    #colour = "black",
    #size = 0.5, linetype = "solid"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    text = element_text(family = "Calibri"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_blank())

ggsave("Week_1_building_face.png", dpi = 600, width = 10, height = 8, device = "png")



# Map of bird strikes -----------------------------------------------------

library(ggmap)
library(ggrepel)

building_locations <- read_csv("Data/campus_building_locations.csv")

bird_data_map <-
  bird_data %>%
  filter(any_birds_found_on_route_shift == "Yes") %>%
  left_join(., building_locations, by = "building_name") %>%
  select(building_name, lat, lon) %>%
  group_by(building_name, lat, lon) %>%
  summarise(n_birds = n())

UIUC <- get_googlemap("Illini Union",  zoom = 15, maptype = "terrain", color = "bw", size = c(640, 640))

saveRDS(UIUC, "Data/UIUC_map.rds")
UIUC <- readRDS("Data/UIUC_map.rds")

UIUC_stamen <-
  get_googlemap("Illini Union",  zoom = 11, maptype = "satellite")

UIUC_stamen <-
  bb2bbox(attr(UIUC_stamen, "bb")) %>%
  get_stamenmap(maptype = "toner", zoom = 16)

ggmap(UIUC_stamen)

## Map with alternating labels


ggmap(UIUC) +
  geom_point(data = bird_data_map,
             aes(x = lon,
                 y = lat,
                 size = n_birds),
             fill = "blue",
             color = "orange",
             shape = 21,
             stroke = 4,
             alpha = 0.8) +
  labs(size = "Number of Birds Found") +
  scale_size_area(max_size = 12) +
  geom_label_repel(data = subset(bird_data_map, lat < 40.110),
                   aes(x = lon,
                       y = lat,
                       label = building_name),
                   force = 15,
                   family = "Calibri",
                   label.size = 0.01,
                   nudge_x = 1,
                   direction = "y",
                   hjust = 0,
                   segment.size = 0.7,
                   segment.color = "blue",
                   color = "blue",
                   alpha = 0.8,
                   size = 5) +
  geom_label_repel(data = subset(bird_data_map, lat > 40.110),
                   aes(x = lon,
                       y = lat,
                       label = building_name),
                   force = 15,
                   family = "Calibri",
                   label.size = 0.01,
                   nudge_x = -1,
                   direction = "y",
                   hjust = 1,
                   segment.size = 0.7,
                   segment.color = "blue",
                   color = "blue",
                   alpha = 0.8,
                   size = 5) +
  theme_void() +
  theme(
    text = element_text(family = "Calibri", size = 20))

## Map without building labels

ggmap(UIUC) +
  geom_point(data = bird_data_map,
             aes(x = lon,
                 y = lat,
                 size = n_birds,
                 fill = n_birds),
             shape = 21,
             stroke = 3,
             alpha = 0.95) +
  #scale_fill_viridis_c(option = "plasma") +
  scale_fill_gradient2(low = "blue",
                       mid = "red",
                       high = "yellow",
                       midpoint = 45,
                       breaks = seq(20, 80, 20),
                       labels = seq(20, 80, 20)) +
  labs(fill = "Number of \n Window-Killed Birds",
       subtitle = "Survey conducted from September-November 2019") +
  geom_label_repel(data = subset(bird_data_map, n_birds > 10),
                   aes(x = lon,
                       y = lat,
                       label = building_name),
                   force = 15,
                   family = "Calibri",
                   label.size = 0.01,
                   nudge_x = -1,
                   direction = "y",
                   hjust = 0,
                   segment.size = 1,
                   segment.color = "black",
                   fill = "black",
                   color = "white",
                   alpha = 1,
                   size = 7) +
  guides(size = FALSE,
         fill = guide_colorbar(barwidth = 2,
                               barheight = 10,
                               #label.position = "left",
                               ticks.colour = "black",
                               ticks.linewidth = 2,
                               direction = "vertical",
                               frame.colour = "black",
                               frame.linewidth = 3,
                               label.theme = element_text(colour = "white",
                                                          size = 18,
                                                          angle = 0))) +
  scale_size_area(max_size = 20) +
  coord_cartesian(expand = TRUE) +
  theme_void() +
  theme(
    text = element_text(size = 24),
    legend.title = element_text(color = "white"),
    # Legend Title center-align
    legend.title.align = 0.5,
    # Change legend background color
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(1, "in"),
    legend.key.width = unit(1.25,"in"),
    legend.margin = margin(t = 0.5, b = 0.25, l = 0.5, r = 0.5, unit = "in"),
    # Change Legend position
    legend.position = c(0.85,0.75),
    plot.subtitle = element_text(size = 20, face = "italic", hjust = 0.5),
    panel.background = element_rect(fill = "black", colour = NA)
  )

ggsave("Figs/birds_by_location-2.png", dpi = 220, height = 9, width = 13, units = "in")

## Interactive leaflet map

library(leaflet)

bird_data %>%
  filter(any_birds_found_on_route_shift == "Yes") %>%
  left_join(., building_locations, by = "building_name") %>%
  select(species, lat, lon) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())




