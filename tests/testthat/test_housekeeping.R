library(feedr)
context("Checking Data for Errors")

# check_ids()
test_that("check_ids catches and removes correctly", {
  b <- read.csv(system.file("extdata", "bird_index.csv", package = "feedr"))

  expect_message(check_ids(finches, b), "All ids in your data are also in your bird_id index")
  expect_message(check_ids(finches, b), "All ids in your bird_id index are also in your data")
  expect_message(check_ids(finches, b), "No ids have been omitted")

  b$species <- as.character(b$species)
  b$species[1] <- "wand"
  b$species[2] <- "error"
  expect_message(check_ids(finches, b), "All ids in your data are also in your bird_id index")
  expect_message(check_ids(finches, b), "All ids in your bird_id index are also in your data")
  expect_message(check_ids(finches, b), "The following bird ids have been omitted: 0620000514, 041868D861")
  expect_equal(nrow(check_ids(finches, b)[check_ids(finches, b)$bird_id == "0620000514",]), 0)
  expect_equal(nrow(check_ids(finches, b)[check_ids(finches, b)$bird_id == "041868D861",]), 0)

  b <- b[b$bird_id != "0620000514",]
  expect_message(check_ids(finches, b), "Some ids present in your data do not exist in the bird_id index: 0620000514")

  r <- finches[!(finches$bird_id %in% c("0620000514", "06200004F8")),]
  expect_message(check_ids(r, b), "Some ids present in your bird_id index, are not in your data: 06200004F8")

  expect_is(check_ids(r, b)$bird_id, "factor")
  expect_is(check_ids(r, b)$feeder_id, "factor")
  expect_is(check_ids(r, b)$time, "POSIXct")

  r$bird_id <- as.character(r$bird_id)
  r$bird_id[1] <- substr(r$bird_id[1], 1, 4)
  expect_error(check_ids(r, b), "You have some bird_ids in your read data that are not 10 characters long")
})

# check_problems()
test_that("check_problems catches and removes correctly", {
  p <- read.csv(system.file("extdata", "problems.csv", package = "feedr"))

  expect_message(check_problems(finches, p), "The following bird ids have been corrected:")
  expect_is(check_problems(finches, p)$bird_id, "factor")
  expect_is(check_problems(finches, p)$feeder_id, "factor")
  expect_is(check_problems(finches, p)$time, "POSIXct")
  expect_equal(sum(levels(check_problems(finches, p)$bird_id) == "041B6BEF6B"), 1)
  expect_equal(sum(levels(check_problems(finches, p)$bird_id) == "041B999F6B"), 1)

  p$original_id <- as.character(p$original_id)
  p$original_id <- p$corrected_id
  expect_message(check_problems(finches, p), "No bird ids needed to be fixed")
})
