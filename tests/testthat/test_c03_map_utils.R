context("Check map utility functions")

test_that("get_locs handles custom data types", {
  p <- presence(visits(finches)) %>%
    dplyr::group_by(.data$logger_id, .data$lat, .data$lon) %>%
    dplyr::summarize(amount = sum(.data$length) / .data$logger_n[1],
                     .groups = "drop")

  m <- move(visits(finches)) %>%
    dplyr::group_by(.data$logger_id, .data$move_path, .data$lat, .data$lon) %>%
    dplyr::summarize(path_use = length(.data$move_path) / .data$logger_n[1],
                     .groups = "drop")

  expect_error(lp <- get_locs(d = p), NA)
  expect_error(lm <- get_locs(d = m), NA)

  expect_gte(length(unique(finches$logger_id)), nrow(lp))
  expect_gte(length(unique(m$move_path))*2, nrow(lm))
})

test_that("add_locs handles different data types", {

  l <- unique(finches[, c("logger_id", "lon", "lat")])
  l_miss <- l
  l_miss$lat[l_miss$logger_id == "2100"] <- NA
  l_miss2 <- l_miss
  l_miss2$lon[l_miss2$logger_id == "2400"] <- NA

  p <- presence(visits(finches))
  p_none <- dplyr::select(p, -lat, -lon)
  m <- move(visits(finches))
  m_none <- dplyr::select(m, -lat, -lon)

  # No change when already have locs
  expect_equal(p, add_locs(p, l), ignore_attr = TRUE)
  expect_equal(m, add_locs(m, l), ignore_attr = TRUE)

  # Add locs
  expect_equal(p, add_locs(p_none, l), ignore_attr = TRUE)
  expect_equal(m, add_locs(m_none, l), ignore_attr = TRUE)

  # Remove correct loggers with missing locs
  expect_message(a <- add_locs(p_none, l_miss),
                 "Removed logger 2100 due to missing lat or lon.")
  expect_equal(dplyr::filter(p, logger_id != 2100), a, ignore_attr = TRUE)

  expect_message(a <- add_locs(m_none, l_miss),
                 "Removed logger 2100 due to missing lat or lon.")
  expect_equal(dplyr::filter(m, !(stringr::str_detect(move_path, "(2100)"))), a,
               ignore_attr = TRUE)

  expect_message(a <- add_locs(p_none, l_miss2),
                 "Removed loggers 2100, 2400 due to missing lat or lon.")
  expect_equal(dplyr::filter(p, !(logger_id  %in% c(2100, 2400))), a,
               ignore_attr = TRUE)

  expect_message(a <- add_locs(m_none, l_miss2),
                 "Removed loggers 2100, 2400 due to missing lat or lon.")


   expect_equal(dplyr::filter(m, !(stringr::str_detect(move_path, "(2100)|(2400)"))), a, ignore_attr = TRUE)

})

