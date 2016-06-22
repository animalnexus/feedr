library(feedr)
library(magrittr)
context("leaflet maps")

v <- visits(finches) %>%
  group_by(bird_id)

f.all <- v %>%
  do(feeding(., bw = 15)) %>%
  group_by(feeder_id) %>%
  summarize(amount = sum(feed_length) / bird_n[1])

m.all <- v %>%
  do(move(.))%>%
  group_by(feeder_id, lat, lon, move_path) %>%
  summarize(path_use = length(move_path) / bird_n[1])

test_that("map.leaflet.base returns leaflet map", {
  map <- map.leaflet.base(locs = prep[['locs']])

  expect_is(map, c("leaflet", "htmlwidget"))
})
