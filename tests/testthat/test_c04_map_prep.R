test_that("map_prep", {
   m <- move(visits(finches))
   p <- presence(visits(finches))

   m_miss <- dplyr::select(m, -lat, -lon)
   p_miss <- dplyr::select(p, -lat, -lon)

   m_sum <- summaries(m)
   p_sum <- summaries(p)

   l <- unique(p[, c("logger_id", "lat", "lon")])

   # Summaries
   expect_error(map_prep(p = p, m = m, summary = "none"),
                "If not supplying a summary type")
   expect_error(map_prep(p = p_sum, m = m_sum, summary = "none"), NA)

   expect_equal(map_prep(p = p_sum, m = m_sum, summary = "none"),
                map_prep(p = p, m = m, summary = "sum_indiv"),
                ignore_attr = TRUE)

   # Missing columns
   expect_error(
     map_prep(p = dplyr::select(p, -logger_id), m = m, summary = "sum"),
     "The presence dataframe \\(p\\)")

   expect_error(
     map_prep(p = p, m = dplyr::select(m, -logger_id), summary = "sum"),
     "The movement dataframe \\(m\\)")

   expect_error(
     map_prep(p = p, m = dplyr::select(m, -move_path), summary = "sum"),
     "The movement dataframe \\(m\\)")

   expect_error(
     map_prep(p = dplyr::select(p, -logger_id),
              m = dplyr::select(m, -logger_id), summary = "sum"),
     "The presence dataframe \\(p\\) requires the column: 'logger_id'")

   # Missing data raises problems
   expect_error(
     map_prep(p_miss, m_miss),
     "Data is missing latitude \\(lat\\) or longitude \\(lon\\) or both.")

   # Deal with one or the other missing
   expect_equal(map_prep(p = p, m = m, summary = "sum"),
                map_prep(p = p_miss, m = m, summary = "sum"),
                ignore_attr = TRUE)
   expect_equal(map_prep(p = p, m = m, summary = "sum"),
                map_prep(p = p, m = m_miss, summary = "sum"),
                ignore_attr = TRUE)
   expect_equal(map_prep(p = p, m = m, summary = "sum"),
                map_prep(p = p_miss, m = m_miss, locs = l, summary = "sum"),
                ignore_attr = TRUE)
})

test_that("Custom summaries", {
  p <- presence(visits(finches)) %>%
    dplyr::group_by(logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length) / logger_n[1], .groups = "drop")

  p_indiv <- presence(visits(finches)) %>%
    dplyr::group_by(animal_id, logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length) / logger_n[1], .groups = "drop")

  m <- move(visits(finches)) %>%
    dplyr::group_by(logger_id, move_path, lat, lon) %>%
    dplyr::summarize(path_use = length(move_path) / logger_n[1], .groups = "drop")

  m_indiv <- move(visits(finches)) %>%
    dplyr::group_by(animal_id, logger_id, move_path, lat, lon) %>%
    dplyr::summarize(path_use = length(move_path), .groups = "drop")

  m_error <- move(visits(finches)) %>%
    dplyr::group_by(date, logger_id, move_path, lat, lon) %>%
    dplyr::summarize(path_use = length(move_path), .groups = "drop") %>%
    dplyr::group_by(logger_id, move_path, lat, lon) %>%
    dplyr::summarize(path_use = mean(path_use), .groups = "drop")

  p_error <- presence(visits(finches)) %>%
    dplyr::group_by(sex, logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length) / logger_n[1], .groups = "drop")

  p_error2 <- presence(visits(finches)) %>%
    dplyr::group_by(animal_id, date, logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length) / logger_n[1], .groups = "drop")

  # Catch problems
  expect_error(map_prep(p = p, m = m_error, summary = "none"),
               "Movement data should have identical path_use")
  expect_error(map_prep(p = p_error, m = m, summary = "none"),
               "Presence data should be summarized to have one row")
  expect_error(map_prep(p = p_error2, m = m, summary = "none"),
               "Movements \\(m\\) and presence \\(p\\) data do not both have")
  expect_error(map_prep(p = p_error2, m = m_indiv, summary = "none"),
               "Presence data should be summarized to have at most")

  # No problems
  expect_silent(map_prep(p = p, m = m, summary = "none"))
  expect_silent(map_prep(p = p_indiv, m = m_indiv, summary = "none"))
})
