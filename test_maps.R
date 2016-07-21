library(feedr)
library(dplyr)

#########################################################
## Finches
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
  summarize(path_use = length(move_path) / bird_n[1] / 2)
##########################################################
## Chickadee data
v <- visits(chickadees)
v <- v[v$experiment == "exp1", ] ## Only look at first experiment

v <- ddply(v, "experiment", transform, bird_n_exp = length(unique(bird_id)))
m <- ddply(v, "bird_id", move)
f <- ddply(v, "bird_id", feeding)

## Feeder Use (u)
f.all <- ddply(f, "feeder_id", summarise,
               amount = sum(feed_length) / bird_n_exp[1])

## Movements along paths (p)
m.all <- ddply(m, c("feeder_id", "move_path"), summarise,
               path_use = length(move_path) / bird_n_exp[1] / 2)

## Locations (l)
l <- unique(f[, c("feeder_id", "lat", "lon")])
l <- rbind(l, data.frame(feeder_id = "exp1-GR11", lat = 53.89304, lon = -122.81827))

f.all <- merge(f.all, l, all.x = TRUE)


#######################################################
# Map Prep
prep <- feedr:::map_prep(p = m.all, u = f.all)
map <- map_leaflet_base(locs = prep[['locs']])

path_layer(map, p = prep[['p']])
use_layer(map, u = prep[['u']])

# Look at total summary maps
map_leaflet(u = f.all, p = m.all)
map_ggmap(u = f.all, p = m.all)

# Look at individual maps

