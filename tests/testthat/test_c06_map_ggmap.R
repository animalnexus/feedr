context("map_ggmap Visualizations")

test_that("map_ggmap() returns summary maps", {
  skip_on_cran()
  expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                m = move(visits(finches)),
                                summary = "sum"), NA)
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_sum_map.png")

  expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                m = move(visits(finches)),
                                summary = "sum_indiv"), NA)
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_sum_indiv_map.png")

  p2 <- presence(visits(finches)) %>%
    dplyr::group_by(logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length)/logger_n[1])

  m2 <- move(visits(finches)) %>%
    dplyr::group_by(logger_id, move_path, lat, lon) %>%
    dplyr::summarize(path_use = length(move_path)/logger_n[1])

  expect_message(expect_error(map <- map_ggmap(p = p2, m = m2), NA))
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_none_map.png")

  # Summary by hand is equal to function summary
  p2 <- presence(visits(finches)) %>%
    dplyr::group_by(logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length)/animal_n[1])

  m2 <- move(visits(finches)) %>%
    dplyr::group_by(logger_id, move_path, lat, lon) %>%
    dplyr::summarize(path_use = length(move_path)/animal_n[1])

  expect_message(expect_error(map <- map_ggmap(p = p2, m = m2), NA))
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_sum_indiv_map.png")

})

test_that("map_ggmap() returns map of individuals", {
  skip_on_cran()

  p_indiv <- summaries(presence(visits(finches)), "indiv")
  m_indiv <- summaries(move(visits(finches)), "indiv")

  m_error <- move(visits(finches)) %>%
    dplyr::group_by(animal_id, move_path) %>%
    dplyr::summarize(path_use = length(move_path))

  expect_error(map_ggmap(p = p_indiv, m = m_error)) # No logger_id

  expect_message(expect_error(map <- map_ggmap(p = p_indiv, m = m_indiv), NA), "(Some animals)|(Map from URL)|(You have specified)")
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_indiv1_map.png")
  suppressMessages({map2 <- map_ggmap(p = presence(visits(finches)),
                                      m = move(visits(finches)),
                                      summary = "indiv")})
  expect_equal_to_ggplot_reference(map, "ggmap_indiv1_map.png")

  expect_message(expect_error(map <- map_ggmap(p = p_indiv, m = m_indiv, which = c("06200004F8")), NA), "(Map from URL)|(You have specified)")
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_indiv2_map.png")
})

test_that("map_ggmap() scale, pal, title", {
  skip_on_cran()

  expect_message(expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                               m = move(visits(finches)),
                                               summary = "sum_indiv",
                                               m_scale = 3, p_scale = 4,
                                               m_title = "PATH", p_title = "TIME",
                                               m_pal = c("blue", "white"), p_pal = c("green", "yellow")),
                              NA))
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_args1_map.png")
})

test_that("map_ggmap() maptype, mapsource, zoom", {
  skip_on_cran()

  expect_message(expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                               m = move(visits(finches)),
                                               summary = "sum_indiv",
                                               maptype = "terrain",
                                               mapsource = "google",
                                               zoom = 15), NA))
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_args2_map.png")

  expect_message(expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                               m = move(visits(finches)),
                                               summary = "sum_indiv",
                                               maptype = "watercolor",
                                               mapsource = "stamen",
                                               zoom = 16), NA))
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_args3_map.png")
})
