library(magrittr)
context("Transformations to movements")

# move()
test_that("move() returns appropriate, non-empty dataframe", {

  ## Errors
  expect_silent(m <- move(visits(finches)))

  ## Format
  expect_is(m, "data.frame")
  expect_length(m, 14)
  expect_equal(nrow(m), 30)
  expect_equal(sum(is.na(m)), 0)
  expect_match(names(m)[1:3], "^bird_id$|^date$|^time$|^feeder_id$|^direction$|^move_dir$|^move_path$|^strength$")
  expect_is(m$bird_id, "factor")
  expect_is(m$feeder_id, "factor")
  expect_is(m$time, "POSIXct")
})

test_that("move() returns non-impossible data", {
  m <- move(visits(finches))
  expect_true(all(m$time[m$direction == "arrived"] >= m$time[m$direction == "left"]))
  expect_true(all(m$feeder_id[m$direction == "arrived"] != m$feeder_id[m$direction == "left"]))
})

test_that("move() returns expected data", {
  m <- move(visits(finches))
  expect_equal(unique(m$bird_id[1]), factor("041868D396", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(m$feeder_id[1:2], factor(c("2400", "2100"), levels = c("2100", "2200", "2400", "2700")))
  expect_equal(m$time[1:2], as.POSIXct(c("2016-01-29 08:35:26", "2016-01-29 11:21:23"), tz = "America/Vancouver"))
  expect_equal(m$bird_n[1], 6)
  expect_equal(m$feeder_n[1], 4)
  expect_equal(round(m$lon[2], 4), -120.3624)
  expect_equal(round(m$lat[2], 5), 50.66896)
  expect_equal(m$move_dir[1], "2400_2100")
  expect_equal(m$move_path[1], factor("2100_2400", levels = c("2100_2200", "2100_2400", "2100_2700", "2200_2400", "2200_2700", "2400_2700")))
  expect_equal(m$species[1], "House Finch")
  expect_true(length(unique(m$move_path)) != length(unique(m$move_dir))) ## 47 visits > 0 seconds
})

test_that("move() handles zero movements and single bird", {
  ## No movements (all = FALSE)
  expect_silent(m <- visits(finches) %>% dplyr::filter(bird_id == "062000043E") %>% move())
  expect_is(m, "data.frame")
  expect_equal(nrow(m), 0)

  ## No movements (all = TRUE)
  expect_silent(m <- visits(finches) %>% dplyr::filter(bird_id == "062000043E") %>% move(., all = TRUE))
  expect_is(m, "data.frame")
  expect_length(m, 14)
  expect_equal(nrow(m), 1)
  expect_equal(m$bird_id[1], factor("062000043E", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
})

test_that("move() pass", {
  expect_length(move(visits(finches), pass = FALSE), 9)
  expect_length(move(visits(finches), pass = TRUE), 14)
})

