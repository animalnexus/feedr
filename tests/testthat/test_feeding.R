library(magrittr)
context("Transformations to feeding bouts")

# feeding() single
test_that("feeding() handles single bird", {

  expect_silent(f <- feeding(visits(finches) %>% dplyr::filter(bird_id == "0620000514")))

  ## Format
  expect_is(f, "data.frame")
  expect_length(f, 10)
  expect_match(names(f)[1:5], "^feeder_id$|^bird_id$|^feed_start$|^feed_end$|^feed_length$")
  expect_is(f$bird_id, "factor")
  expect_is(f$feeder_id, "factor")
  expect_is(f$feed_start, "POSIXct")

  ## Not impossible nor missing
  expect_equal(sum(is.na(f)), 0)
  expect_true(all(f$feed_end >= f$feed_start))
})



# feeding() multiple
test_that("feeding() handles multiple birds", {

  expect_silent(f <- feeding(visits(finches)))

  ## Format
  expect_is(f, "data.frame")
  expect_length(f, 10)
  expect_match(names(f)[1:5], "^feeder_id$|^bird_id$|^feed_start$|^feed_end$|^feed_length$")
  expect_is(f$bird_id, "factor")
  expect_is(f$feeder_id, "factor")
  expect_is(f$feed_start, "POSIXct")

  ## Not impossible nor missing
  expect_equal(sum(is.na(f)), 0)
  expect_true(all(f$feed_end >= f$feed_start))
})

test_that("feeder() pass", {
  expect_length(feeding(visits(finches), pass = FALSE), 5)
  expect_length(feeding(visits(finches), pass = TRUE), 10)
})

