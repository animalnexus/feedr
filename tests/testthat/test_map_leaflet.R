library(magrittr)
context("Mapping Visualizations")

test_that("maps_leaflet_base return maps", {
  expect_error(map <- map_leaflet_base(locs = unique(finches[, c("feeder_id", "lat", "lon")])), NA)
  expect_is(map, c("leaflet", "htmlwidget"))

  expect_true(all(attr(map$x, "leafletData") == unique(finches[, c("feeder_id", "lat", "lon")])))

})

test_that("ggmap and leaflet map return map", {
  f.indiv <- visits(finches) %>%
    dplyr::group_by(bird_id) %>%
    dplyr::do(feeding(., bw = 15)) %>%
    dplyr::group_by(bird_id, feeder_id, lat, lon) %>%
    dplyr::summarize(amount = sum(feed_length))

  m.indiv <- visits(finches) %>%
    dplyr::group_by(bird_id) %>%
    dplyr::do(move(.)) %>%
    dplyr::group_by(bird_id, feeder_id, move_path) %>%
    dplyr::summarize(path_use = length(move_path))

  m.error <- visits(finches) %>%
    dplyr::group_by(bird_id) %>%
    dplyr::do(move(.)) %>%
    dplyr::group_by(bird_id, move_path) %>%
    dplyr::summarize(path_use = length(move_path))

  expect_error(map <- map_leaflet(u = f.indiv, p = m.indiv), NA)
  expect_is(map, c("leaflet", "htmlwidget"))

  expect_error(map_ggmap(u = f.indiv, p = m.error), "requires three columns:")
  expect_message(expect_error(map <- map_ggmap(u = f.indiv, p = m.indiv), NA), "(Some birds)|(Map from URL)|(You have specified)")
  expect_is(map, c("gg", "ggplot"))
})
