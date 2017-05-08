context("Test summary functions")

m <- move(visits(finches))
p <- presence(visits(finches))

test_that("summaries detect data type and summary type", {
  n_m <- c("move_path", "logger_id", "lat", "lon", "path_use")
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
  expect_equivalent(summaries(p), {
    dplyr::group_by(p, logger_id, lat, lon) %>%
      dplyr::summarise(amount = sum(length) / animal_n[1])
  })

  expect_equivalent(summaries(m), {
   dplyr::group_by(m, logger_id, move_path, lat, lon) %>%
      dplyr::summarise(path_use = length(move_path) / animal_n[1])
  })

  # summary = "sum"
  expect_equivalent(summaries(p, summary = "sum"), {
    dplyr::group_by(p, logger_id, lat, lon) %>%
      dplyr::summarise(amount = sum(length))
  })

  expect_equivalent(summaries(m, summary = "sum"), {
    dplyr::group_by(m, logger_id, move_path, lat, lon) %>%
      dplyr::summarise(path_use = length(move_path))
  })

  # summary = "indiv"
  expect_equivalent(summaries(p, summary = "indiv"), {
    dplyr::group_by(p, animal_id, logger_id, lat, lon) %>%
      dplyr::summarise(amount = sum(length))
  })

  expect_equivalent(summaries(m, summary = "indiv"), {
    dplyr::group_by(m, animal_id, logger_id, move_path, lat, lon) %>%
      dplyr::summarise(path_use = length(move_path))
  })
})
