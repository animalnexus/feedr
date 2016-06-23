library(magrittr)
context("leaflet maps")

v <- visits(finches) %>%
  dplyr::group_by(bird_id)

f <- v %>%
  dplyr::do(feeding(., bw = 15))

f.all <- f %>%
  dplyr::group_by(feeder_id) %>%
  dplyr::summarize(amount = sum(feed_length) / bird_n[1])

m <- v %>%
  dplyr::do(move(.))

m.all <- m %>%
  dplyr::group_by(feeder_id, lat, lon, move_path) %>%
  dplyr::summarize(path_use = length(move_path) / bird_n[1])

f.indiv <- plyr::ddply(f, c("bird_id", "feeder_id", "lat", "lon"), plyr::summarise,
                 amount = sum(feed_length))

m.indiv <- plyr::ddply(m, c("bird_id", "move_path"), plyr::summarise,
                 path_use = length(move_path))

test_that("maps.leaflets return maps without errors", {
  expect_error(map <- map.leaflet.base(locs = unique(f.indiv[, c("lat", "lon")])), NA)
  expect_is(map, c("leaflet", "htmlwidget"))
  expect_error(map <- map.leaflet(u = f.indiv, p = m.indiv), NA)
  expect_is(map, c("leaflet", "htmlwidget"))
})

test_that("ggmap returns map", {
  expect_message(expect_error(map <- map.ggmap(u = f.indiv, p = m.indiv), NA), "(Some birds)|(Map from URL)|(You have specified)")
  expect_is(map, c("gg", "ggplot"))
})
