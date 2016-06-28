library(feedr)
library(dplyr)

v <- visits(finches) %>% group_by(bird_id)

f <- v %>% do(feeding(., bw = 15))
m <- v %>% do(move(.))
l <- unique(f[, c("feeder_id", "lat", "lon")])

# Summarise data for visualization (use totals):
f.all <- f %>%
  group_by(feeder_id) %>%
  summarize(amount = sum(feed_length) / bird_n[1])

m.all <- m %>%
  group_by(feeder_id, lat, lon, move_path) %>%
  summarize(path_use = length(move_path) / bird_n[1])

# Map Prep
prep <- feedr:::map_prep(p = m.all, u = f.all)
map <- map_leaflet_base(locs = prep[['locs']])

path_layer(map, p = prep[['p']])
use_layer(map, u = prep[['u']])

# Look at total summary maps
map_leaflet(u = f.all, p = m.all)
map_ggmap(u = f.all, p = m.all)

