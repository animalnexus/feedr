library(magrittr)
context("Transformations to presence bouts")

# presence() single
test_that("presence() handles single animal", {

  expect_silent(p <- presence(visits(finches) %>% dplyr::filter(animal_id == "0620000514")))

  ## Format
  expect_is(p, "data.frame")
  expect_length(p, 12)
  expect_match(names(p)[1:5], "^logger_id$|^animal_id$|^date$|^start$|^end$|^length$")
  expect_is(p$animal_id, "factor")
  expect_is(p$logger_id, "factor")
  expect_is(p$start, "POSIXct")

  ## Not impossible nor missing
  expect_equal(sum(is.na(p)), 0)
  expect_true(all(p$end >= p$start))
})



# presence() multiple
test_that("presence() handles multiple animals", {

  expect_silent(p <- presence(visits(finches)))

  ## Format
  expect_is(p, "data.frame")
  expect_length(p, 12)
  expect_match(names(p)[1:5], "^logger_id$|^animal_id$|^date$|^start$|^end$|^length$")
  expect_is(p$animal_id, "factor")
  expect_is(p$logger_id, "factor")
  expect_is(p$start, "POSIXct")

  ## Not impossible nor missing
  expect_equal(sum(is.na(p)), 0)
  expect_true(all(p$end >= p$start))
})

test_that("presence() pass", {
  expect_length(presence(visits(finches), pass = FALSE), 6)
  expect_length(presence(visits(finches), pass = TRUE), 11)
})

