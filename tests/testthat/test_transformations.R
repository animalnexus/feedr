library(feedr)
library(magrittr)
context("Transformations between data types: Visits")

# visits()
test_that("visits() returns appropriate, non-empty dataframe", {
  expect_silent(v <- visits(finches))
  expect_is(v, "data.frame")
  expect_equal(sum(is.na(v)), 0)
  expect_match(names(v)[1:3], "^bird_id$|^start$|^end$|^feeder_id$")
  expect_is(v$bird_id, "factor")
  expect_is(v$feeder_id, "factor")
  expect_is(v$start, "POSIXct")
  expect_is(v$end, "POSIXct")

  expect_equal(v$bird_id[2], factor("041868D396", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))
  expect_equal(v$feeder_id[2], factor("2100", levels = c("2100", "2200", "2400", "2700")))
  expect_equal(v$start[2], as.POSIXct("2016-01-29 11:21:23", tz = "America/Vancouver"))
  expect_equal(v$end[2], as.POSIXct("2016-01-29 11:21:26", tz = "America/Vancouver"))
  expect_equal(v$bird_n[2], 5)
  expect_equal(v$feeder_n[2], 4)
  expect_equal(round(v$lon[2], 4), -120.3624)
  expect_equal(round(v$lat[2], 5), 50.66896)
  expect_equal(v$species[2], "House Finch")
  expect_equal(nrow(v[v$end != v$start,]), 49) ## 47 visits > 0 seconds
})

# move()
test_that("move() returns appropriate, non-empty dataframe", {

  ## No errors
  expect_silent(m <- visits(finches) %>% dplyr::group_by(bird_id) %>% dplyr::do(move(.)))

  ## Format
  expect_is(m, "data.frame")
  expect_length(m, 12)
  expect_equal(nrow(m), 30)
  expect_equal(sum(is.na(m)), 0)
  expect_match(names(m)[1:3], "^bird_id$|^time$|^feeder_id$|^direction$|^move_dir$|^move_path$|^strength$")
  expect_is(m$bird_id, "factor")
  expect_is(m$feeder_id, "factor")
  expect_is(m$time, "POSIXct")

  ## Data
  expect_equal(unique(m$bird_id[1]), factor("041868D396", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))
  expect_equal(m$feeder_id[1:2], factor(c("2400", "2100"), levels = c("2100", "2200", "2400", "2700")))
  expect_equal(m$time[1:2], as.POSIXct(c("2016-01-29 08:35:26", "2016-01-29 11:21:23"), tz = "America/Vancouver"))
  expect_equal(m$bird_n[1], 5)
  expect_equal(m$feeder_n[1], 4)
  expect_equal(round(m$lon[2], 4), -120.3624)
  expect_equal(round(m$lat[2], 5), 50.66896)
  expect_equal(m$move_dir[1], "2400_2100")
  expect_equal(m$move_path[1], factor("2100_2400", levels = c("2100_2200", "2100_2400", "2100_2700", "2200_2400", "2200_2700", "2400_2700")))
  expect_equal(m$species[1], "House Finch")
  expect_true(length(unique(m$move_path)) != length(unique(m$move_dir))) ## 47 visits > 0 seconds

  ## No movements (all = FALSE)
  expect_silent(m <- visits(finches) %>% dplyr::filter(bird_id == "062000043E") %>% move())
  expect_is(m, "data.frame")
  expect_length(m, 0)

  ## No movements (all = TRUE)
  expect_silent(m <- visits(finches) %>% dplyr::filter(bird_id == "062000043E") %>% move(., all = TRUE))
  expect_is(m, "data.frame")
  expect_length(m, 12)
  expect_equal(nrow(m), 1)
  expect_equal(m$bird_id[1], factor("062000043E", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))

})
