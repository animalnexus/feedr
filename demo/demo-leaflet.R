
library(feedr)
library(dplyr)

v <- chickadees %>%
  group_by(experiment) %>%
  do(visits(.))

m <- v %>%
  group_by(experiment) %>%
  do(move(.))

p <- v %>%
  group_by(experiment) %>%
  do(presence(.))

## Movements
m_all <- m %>%
  group_by(experiment, logger_id, move_path, lat, lon) %>%
  summarise(path_use = length(move_path) / unique(animal_n)) %>%
  ungroup(m_all)

## Presence averaged
p_all <- p %>%
  group_by(experiment, logger_id) %>%
  summarize(amount = sum(length) / unique(animal_n)) %>%
  ungroup(p_all)

## Map
map_leaflet(p = p_all, m = m_all)