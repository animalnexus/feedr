library(feedr)
context("Internal formating and error checks")

# check.name()
test_that("check.name returns error if name missing", {
  d <- data.frame(bird_id = NA, feeder_id = NA, start = 2, end = 2)
  expect_error(check.name(d, c("bird_ID", "feeDer_id")))
  expect_silent(check.name(d, c("bird_id", "feeder_id")))
})

# check.time()
test_that("check.time returns error if col not time", {
  d <- data.frame(bird_id = NA, feeder_id = NA, start = as.POSIXct("2015-01-01 12:34:00"), end = 54)
  expect_error(check.time(d, c("start", "end")))
  expect_silent(check.name(d, c("start")))

  expect_error(check.time(d, c("start", "end")), "This data frame should have been created")
  expect_error(check.time(d, c("start", "end"), internal = FALSE), "Consider as.POSIXct()")
})

# check.indiv()
test_that("check.indiv returns error if more than one bird_id", {
  d <- data.frame(bird_id = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514"),
                  feeder_id = NA)
  expect_error(check.indiv(d))
  expect_silent(check.indiv(data.frame(bird_id = rep("041868D396", 3), feeder_id = 45)))
})

# check.format()
test_that("check.format returns error if '_' in column values", {
  d <- data.frame(bird_id = c("041868D396", "041868D861", "041868FF93", "062000__043E", "0620__0004F8", "0620000514"),
                  feeder_id = c("456", "5678", "TR_567", "GH_563", "YU848", "56_67"))
  expect_message(check.format(d))
  expect_message(check.format(d, n = "bird_id"))
  expect_silent(check.format(d[1:2,]))
  expect_silent(check.format(d[1:2,], n = "bird_id"))
  expect_message(check.format(d, n = c("bird_id", "feeder_id")))
})
