context("map_ggmap Visualizations")

test_that("map_ggmap() returns summary maps", {
  expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                m = move(visits(finches)),
                                summary = "sum"), NA)
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_sum_map.rds")

  expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                m = move(visits(finches)),
                                summary = "sum_indiv"), NA)
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_sum_indiv_map.rds")

  p2 <- presence(visits(finches)) %>%
    dplyr::group_by(logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length)/logger_n[1])

  m2 <- move(visits(finches)) %>%
    dplyr::group_by(logger_id, move_path, lat, lon) %>%
    dplyr::summarize(path_use = length(move_path)/logger_n[1])

  expect_message(expect_error(map <- map_ggmap(p = p2, m = m2), NA))
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_none_map.rds")
})

test_that("map_ggmap() returns map of individuals", {
  p_indiv <- summaries(presence(visits(finches)), "indiv")
  m_indiv <- summaries(move(visits(finches)), "indiv")

  m_error <- move(visits(finches)) %>%
    dplyr::group_by(animal_id, move_path) %>%
    dplyr::summarize(path_use = length(move_path))

  expect_error(map_ggmap(p = p_indiv, m = m_error)) # No logger_id

  expect_message(expect_error(map <- map_ggmap(p = p_indiv, m = m_indiv), NA), "(Some animals)|(Map from URL)|(You have specified)")
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_indiv1_map.rds")
  suppressMessages({map2 <- map_ggmap(p = presence(visits(finches)),
                                      m = move(visits(finches)),
                                      summary = "indiv")})
  expect_equivalent(map[-8], map2[-8]) #Don't compare plot environment

  expect_message(expect_error(map <- map_ggmap(p = p_indiv, m = m_indiv, which = c("06200004F8")), NA), "(Map from URL)|(You have specified)")
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_indiv2_map.rds")
})

test_that("map_ggmap() scale, pal, title", {
  expect_message(expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                               m = move(visits(finches)),
                                               summary = "sum_indiv",
                                               m_scale = 3, p_scale = 4,
                                               m_title = "PATH", p_title = "TIME",
                                               m_pal = c("blue", "white"), p_pal = c("green", "yellow")),
                              NA))
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_args1_map.rds")
})

test_that("map_ggmap() maptype, mapsource, zoom", {
  expect_message(expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                               m = move(visits(finches)),
                                               summary = "sum_indiv",
                                               maptype = "terrain",
                                               mapsource = "google",
                                               zoom = 15), NA))
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_args2_map.rds")

  expect_message(expect_error(map <- map_ggmap(p = presence(visits(finches)),
                                               m = move(visits(finches)),
                                               summary = "sum_indiv",
                                               maptype = "watercolor",
                                               mapsource = "stamen",
                                               zoom = 16), NA))
  expect_is(map, c("gg", "ggplot"))
  expect_equal_to_ggplot_reference(map, "ggmap_args3_map.rds")
})
