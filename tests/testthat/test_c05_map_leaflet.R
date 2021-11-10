
test_that("maps_leaflet_base return maps", {
  skip_on_cran()

  expect_silent(map <- map_leaflet_base(locs = unique(finches[, c("logger_id", "lat", "lon")])))
  expect_s3_class(map, c("leaflet", "htmlwidget"))
  expect_true(all(attr(map$x, "leafletData") == unique(finches[, c("logger_id", "lat", "lon")])))
  expect_equal_to_leaflet_reference(map, "leaflet_base_map.rds")
})

test_that("map_leaflet() returns summary map", {
  skip_on_cran()

  expect_error(map <- map_leaflet(p = presence(visits(finches)),
                                  m = move(visits(finches)),
                                  summary = "sum"), NA)
  expect_s3_class(map, c("leaflet", "htmlwidget"))
  expect_equal_to_leaflet_reference(map, "leaflet_sum_map.rds")

  expect_error(map <- map_leaflet(p = presence(visits(finches)),
                                  m = move(visits(finches)),
                                  summary = "sum_indiv"), NA)
  expect_s3_class(map, c("leaflet", "htmlwidget"))
  expect_equal_to_leaflet_reference(map, "leaflet_sum_indiv_map.rds")

  p2 <- presence(visits(finches)) %>%
    dplyr::group_by(logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length)/logger_n[1], .groups = "drop")

  m2 <- move(visits(finches)) %>%
    dplyr::group_by(logger_id, move_path, lat, lon) %>%
    dplyr::summarize(path_use = length(move_path)/logger_n[1], .groups = "drop")

  expect_error(map <- map_leaflet(p = p2, m = m2), NA)
  expect_s3_class(map, c("leaflet", "htmlwidget"))
  expect_equal_to_leaflet_reference(map, "leaflet_none_map.rds")
})

test_that("map_leaflet() returns summary map of individuals", {
  skip_on_cran()

  p_indiv <- summaries(presence(visits(finches)), "indiv")
  m_indiv <- summaries(move(visits(finches)), "indiv")

  m_error <- move(visits(finches)) %>%
    dplyr::group_by(animal_id, move_path) %>%
    dplyr::summarize(path_use = length(move_path), .groups = "drop")

  expect_error(map <- map_leaflet(p = p_indiv, m = m_indiv), NA)
  expect_s3_class(map, c("leaflet", "htmlwidget"))
  expect_equal_to_leaflet_reference(map, "leaflet_indiv_map.rds")
})

test_that("map_leaflet() scale, pal, title", {
  skip_on_cran()

  expect_error(map <- map_leaflet(
    p = presence(visits(finches)),
    m = move(visits(finches)),
    summary = "sum_indiv",
    m_scale = 3, p_scale = 4,
    m_title = "PATH", p_title = "TIME",
    m_pal = c("blue", "white"), p_pal = c("green", "yellow")), NA)
  expect_s3_class(map, c("leaflet", "htmlwidget"))
  expect_equal_to_leaflet_reference(map, "leaflet_args_map.rds")
})
