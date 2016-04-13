
library(feedr)
library(plyr)

v <- visits(chickadees)
v <- ddply(v, "experiment", transform, bird_n_exp = length(unique(bird_id)))
m <- ddply(v, "bird_id", move)
f <- ddply(v, "bird_id", feeding)
f.all <- ddply(f, "feeder_id", summarise,
               feed_length = sum(feed_length) / bird_n_exp[1])
m.all <- ddply(m, "move_path", summarise,
               path_use = length(move_path) / bird_n_exp[1] / 2)
l <- unique(f[, c("feeder_id", "lat", "lon")])
l <- rbind(l, data.frame(feeder_id = "exp1-GR11", lat = 53.89304, lon = -122.81827))
map.leaflet(f = f.all, m = m.all, locs = l)
