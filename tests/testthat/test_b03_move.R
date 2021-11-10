test_that("move() returns appropriate, non-empty dataframe", {

  ## Errors
  m <- finches %>%
    visits() %>%
    move() %>%
    expect_silent()

  ## Format
  expect_s3_class(m, "data.frame")
  expect_length(m, 17)
  expect_equal(nrow(m), 30)
  expect_equal(sum(is.na(m)), 0)
  expect_match(names(m)[1:3], "^animal_id$|^date$|^time$|^logger_id$|^direction$|^move_dir$|^move_path$|^strength$")
  expect_s3_class(m$animal_id, "factor")
  expect_s3_class(m$logger_id, "factor")
  expect_s3_class(m$date, "Date")
  expect_s3_class(m$time, "POSIXct")


})

test_that("move() returns non-impossible data", {
  m <- move(visits(finches))
  expect_true(all(m$time[m$direction == "arrived"] >= m$time[m$direction == "left"]))
  expect_true(all(m$logger_id[m$direction == "arrived"] != m$logger_id[m$direction == "left"]))
})

test_that("move() returns expected data", {
  m <- move(visits(finches))
  expect_equal(unique(m$animal_id[1]), factor("041868D396", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))
  expect_equal(m$logger_id[1:2], factor(c("2400", "2100"), levels = c("2100", "2200", "2400", "2700")))
  expect_equal(m$time[1:2], as.POSIXct(c("2016-01-29 08:35:26", "2016-01-29 11:21:23"),
                                       tz = "Etc/GMT+8"))
  expect_equal(m$date[2], as.Date("2016-01-29"))
  expect_equal(m$date, as.Date(m$time, tz = lubridate::tz(m$time)))
  expect_equal(m$animal_n[1], 5)
  expect_equal(m$logger_n[1], 4)
  expect_equal(round(m$lon[2], 4), -120.3624)
  expect_equal(round(m$lat[2], 5), 50.66896)
  expect_equal(m$move_dir[1], factor("2400_2100", levels = c("2200_2100", "2400_2100", "2700_2100", "2100_2200", "2400_2200", "2700_2200", "2100_2400", "2200_2400", "2700_2400", "2100_2700", "2200_2700", "2400_2700")))
  expect_equal(m$move_path[1], factor("2100_2400", levels = c("2100_2200", "2100_2400", "2100_2700", "2200_2400", "2200_2700", "2400_2700")))
  expect_equal(m$species[1], "House Finch")
  expect_true(length(unique(m$move_path)) != length(unique(m$move_dir))) ## 47 visits > 0 seconds
})

test_that("move() handles zero movements and single animal", {
  ## No movements (all = FALSE)
  expect_silent(m <- visits(finches) %>% dplyr::filter(animal_id == "062000043E") %>% move())
  expect_s3_class(m, "data.frame")
  expect_equal(nrow(m), 0)

  ## No movements (all = TRUE)
  expect_silent(m <- visits(finches) %>% dplyr::filter(animal_id == "062000043E") %>% move(., all = TRUE))
  expect_s3_class(m, "data.frame")
  expect_length(m, 17)
  expect_equal(nrow(m), 1)
  expect_equal(m$animal_id[1], factor("062000043E", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))
})

test_that("move() pass", {
  expect_length(move(visits(finches), pass = FALSE), 9)
  expect_length(move(visits(finches), pass = TRUE), 17)
})

