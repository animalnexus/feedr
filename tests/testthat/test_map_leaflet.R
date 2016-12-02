library(magrittr)
context("Mapping Visualizations")

test_that("maps_leaflet_base return maps", {
  expect_error(map <- map_leaflet_base(locs = unique(finches[, c("logger_id", "lat", "lon")])), NA)
  expect_is(map, c("leaflet", "htmlwidget"))

  expect_true(all(attr(map$x, "leafletData") == unique(finches[, c("logger_id", "lat", "lon")])))

})

test_that("ggmap and leaflet map return map", {
  p_indiv <- presence(visits(finches), bw = 15) %>%
    dplyr::group_by(animal_id, logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length))

  m_indiv <- move(visits(finches)) %>%
    dplyr::group_by(animal_id, logger_id, move_path) %>%
    dplyr::summarize(path_use = length(move_path))

  m_error <- move(visits(finches)) %>%
    dplyr::group_by(animal_id, move_path) %>%
    dplyr::summarize(path_use = length(move_path))

  expect_error(map <- map_leaflet(p = p_indiv, m = m_indiv), NA)
  expect_is(map, c("leaflet", "htmlwidget"))

  expect_error(map_ggmap(p = p_indiv, m = m_error), "requires three columns:")
  expect_message(expect_error(map <- map_ggmap(p = p_indiv, m = m_indiv), NA), "(Some animals)|(Map from URL)|(You have specified)")
  expect_is(map, c("gg", "ggplot"))
})
