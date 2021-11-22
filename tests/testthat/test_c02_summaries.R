m <- move(visits(finches))
p <- presence(visits(finches))

test_that("summaries detect data type and summary type", {
  n_m <- c("logger_id", "move_path", "lat", "lon", "path_use")
  n_p <- c("logger_id", "lat", "lon", "amount")

  expect_named(summaries(m), n_m)
  expect_named(summaries(p), n_p)

  expect_named(summaries(m, "sum"), n_m)
  expect_named(summaries(p, "sum"), n_p)

  expect_named(summaries(m, "indiv"), c("animal_id", n_m))
  expect_named(summaries(p, "indiv"), c("animal_id", n_p))
})

test_that("summaries return correct summary", {

  # summary = "sum_indiv"
  expect_equal(summaries(p), {
    dplyr::group_by(p, .data$logger_id, .data$lat, .data$lon) %>%
      dplyr::summarise(amount = sum(.data$length) / .data$animal_n[1], .groups = "drop")
  }, ignore_attr = TRUE)

  expect_equal(summaries(m), {
   dplyr::group_by(m, .data$logger_id, .data$move_path, .data$lat, .data$lon) %>%
      dplyr::summarise(path_use = length(.data$move_path) / .data$animal_n[1], .groups = "drop")
  }, ignore_attr = TRUE)

  # summary = "sum"
  expect_equal(summaries(p, summary = "sum"), {
    dplyr::group_by(p, logger_id, lat, lon) %>%
      dplyr::summarise(amount = sum(length), .groups = "drop")
  }, ignore_attr = TRUE)

  expect_equal(summaries(m, summary = "sum"), {
    dplyr::group_by(m, logger_id, move_path, lat, lon) %>%
      dplyr::summarise(path_use = length(move_path), .groups = "drop")
  }, ignore_attr = TRUE)

  # summary = "indiv"
  expect_equal(summaries(p, summary = "indiv"), {
    dplyr::group_by(p, animal_id, logger_id, lat, lon) %>%
      dplyr::summarise(amount = sum(length), .groups = "drop")
  }, ignore_attr = TRUE)

  expect_equal(summaries(m, summary = "indiv"), {
    dplyr::group_by(m, animal_id, logger_id, move_path, lat, lon) %>%
      dplyr::summarise(path_use = length(move_path), .groups = "drop")
  }, ignore_attr = TRUE)
})
