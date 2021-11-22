# presence() single
test_that("presence() handles single animal", {

  p <- visits(finches) %>%
    dplyr::filter(animal_id == "0620000514") %>%
    presence() %>%
    expect_silent()

  ## Format
  expect_s3_class(p, "data.frame")
  expect_length(p, 14)
  expect_match(names(p)[1:5], "^logger_id$|^animal_id$|^date$|^start$|^end$|^length$")
  expect_s3_class(p$animal_id, "factor")
  expect_s3_class(p$logger_id, "factor")
  expect_s3_class(p$date, "Date")
  expect_s3_class(p$start, "POSIXct")

  expect_equal(p$date, as.Date(p$start, tz = lubridate::tz(p$start)))

  ## Not impossible nor missing
  expect_equal(sum(is.na(p)), 0)
  expect_true(all(p$end >= p$start))
})



# presence() multiple
test_that("presence() handles multiple animals", {

  p <- finches %>%
    visits() %>%
    presence() %>%
    expect_silent()

  ## Format
  expect_s3_class(p, "data.frame")
  expect_length(p, 14)
  expect_match(names(p)[1:5], "^logger_id$|^animal_id$|^date$|^start$|^end$|^length$")
  expect_s3_class(p$animal_id, "factor")
  expect_s3_class(p$logger_id, "factor")
  expect_s3_class(p$date, "Date")
  expect_s3_class(p$start, "POSIXct")

  ## Not impossible nor missing
  expect_equal(sum(is.na(p)), 0)
  expect_true(all(p$end >= p$start))
})

test_that("presence() pass", {
  expect_length(presence(visits(finches), pass = FALSE), 6)
  expect_length(presence(visits(finches), pass = TRUE), 14)
})

