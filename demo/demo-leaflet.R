library(feedr)
library(dplyr)
library(tidyr)
library(purrr)

bouts <- chickadees %>%
  nest(.by = "experiment") %>%
  mutate(v = map(data, visits), m = map(v, move), p = map(v, presence))

## Movements
m_all <- bouts %>%
  select(experiment, m) %>%
  unnest(m) %>%
  summarise(
    path_use = length(move_path) / unique(animal_n),
    .by = c("experiment", "logger_id", "move_path", "lat", "lon")
  )

## Presence averaged
p_all <- bouts %>%
  select(experiment, p) %>%
  unnest(p) %>%
  summarize(
    amount = sum(length) / unique(animal_n),
    .by = c("experiment", "logger_id")
  )

## Map
map_leaflet(p = p_all, m = m_all)
