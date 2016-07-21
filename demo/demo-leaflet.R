
library(feedr)
library(plyr)

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

## Map
map_leaflet(u = f.all, p = m.all, locs = l)
