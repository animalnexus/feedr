context("Check map utility functions")

test_that("get_locs handles custom data types", {
  p <- presence(visits(finches)) %>%
    dplyr::group_by(logger_id, lat, lon) %>%
    dplyr::summarize(amount = sum(length) / logger_n[1])

  m <- move(visits(finches)) %>%
    dplyr::group_by(logger_id, move_path, lat, lon) %>%
    dplyr::summarize(path_use = length(move_path) / logger_n[1])

  expect_error(lp <- get_locs(d = p), NA)
  expect_error(lm <- get_locs(d = m), NA)

  expect_gte(length(unique(finches$logger_id)), nrow(lp))
  expect_gte(length(unique(m$move_path))*2, nrow(lm))
})

test_that("add_locs handles different data types", {

  l <- unique(finches[, c("logger_id", "lat", "lon")])
  l_miss <- l
  l_miss$lat[l_miss$logger_id == "2100"] <- NA
  l_miss2 <- l_miss
  l_miss2$lon[l_miss2$logger_id == "2400"] <- NA

  p <- presence(visits(finches))
  p_none <- dplyr::select(p, -lat, -lon)
  m <- move(visits(finches))
  m_none <- dplyr::select(m, -lat, -lon)

  # No change when already have locs
  expect_equivalent(p, add_locs(p, l))
  expect_equivalent(m, add_locs(m, l))

  # Add locs
  expect_equivalent(p, add_locs(p_none, l))
  expect_equivalent(m, add_locs(m_none, l))

  # Remove correct loggers with missing locs
  expect_message(a <- add_locs(p_none, l_miss), "Removed logger 2100 due to missing lat or lon.")
  expect_equivalent(dplyr::filter(p, logger_id != 2100), a)

  expect_message(a <- add_locs(m_none, l_miss), "Removed logger 2100 due to missing lat or lon.")
  expect_equivalent(dplyr::filter(m, !(stringr::str_detect(move_path, "(2100)"))), a)

  expect_message(a <- add_locs(p_none, l_miss2), "Removed loggers 2100, 2400 due to missing lat or lon.")
  expect_equivalent(dplyr::filter(p, !(logger_id  %in% c(2100, 2400))), a)

  expect_message(a <- add_locs(m_none, l_miss2), "Removed loggers 2100, 2400 due to missing lat or lon.")
  expect_equivalent(dplyr::filter(m, !(stringr::str_detect(move_path, "(2100)|(2400)"))), a)

})

