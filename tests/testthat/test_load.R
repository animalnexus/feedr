library(feedr)
context("Loading files")

# load.web()
test_that("load.web loads and formats data correctly", {
  f <- "../test_load_web.csv"
  expect_is(load.web(f), "data.frame")
  expect_match(names(load.web(f))[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load.web(f)$bird_id, "factor")
  expect_is(load.web(f)$feeder_id, "factor")
  expect_is(load.web(f)$time, "POSIXct")

  expect_equal(load.web(f)$bird_id[1], factor("062000031A", levels = c("062000031A","0620000500")))
  expect_equal(load.web(f)$feeder_id[1], factor("2100", levels = c("2100","2700")))
  expect_equal(load.web(f)$time[1], as.POSIXct("2015-09-02 15:25:58"))
})

# load.raw()
test_that("load.raw loads and formats data correctly", {
  f <- "../raw_tests/exp1/GR12DATA.csv"
  expect_is(load.raw(f), "data.frame")
  expect_match(names(load.raw(f))[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load.raw(f)$bird_id, "character")
  expect_is(load.raw(f)$feeder_id, "character")
  expect_is(load.raw(f)$time, "POSIXct")

  expect_equal(load.raw(f)$bird_id[1], "06200004BB")
  expect_equal(load.raw(f)$feeder_id[1], "GR12")
  expect_equal(load.raw(f)$time[1], as.POSIXct("2015-12-09 15:20:59"))
})

# load.raw.all()
test_that("load.raw.all loads and formats data correctly", {
  load <- load.raw.all(r_dir = "../raw_tests/", extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment")
  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load$bird_id, "factor")
  expect_is(load$feeder_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$bird_id[1], factor("06200004BB", levels = c("06200001F0", "06200003C3", "0620000418", "06200004BB")))
  expect_equal(load$feeder_id[1], factor("GR12", levels = c("GR11","GR12")))
  expect_equal(load$time[1], as.POSIXct("2015-12-09 15:20:59"))
})

# get.data()
test_that("get.data loads and formats data correctly", {
  data <- get.data(start = "2016-03-17", end = "2016-03-17 09:00:00", sites = "Kamloops")
  expect_is(data, "data.frame")
  expect_match(names(data)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(data$bird_id, "factor")
  expect_is(data$feeder_id, "factor")
  expect_is(data$time, "POSIXct")

  expect_equal(data$bird_id[1], factor("062000043E", levels = c("062000043E")))
  expect_equal(data$feeder_id[1], factor("1500", levels = c("1500")))
  expect_equal(data$time[1], as.POSIXct("2016-03-17 07:54:29"))
})


#     ## ---- testing1 ----
#     source("fun_load.R")
#     r <- loadWeb("../Data/Dec01_Dec14_Data.csv")
#     bird_ids <- unique(data.frame(bird_id = r[,3], species = as.character("bcch")))
#     bird_ids$species <- as.character(bird_ids$species)
#
#     # Test perfect match
#     b <- bird_ids
#     t <- checkIds(r, bird_ids)
#
#     # Test omit wand:
#     b <- bird_ids
#     b$species[1:3] <- "wand"
#     t <- checkIds(r, b)
#
#     # Test more in index than data
#     b <- bird_ids
#     t <- checkIds(r[r$bird_id!=b$bird_id[1],], b)
#
#     # Test more in data than in index
#     b <- bird_ids[-c(1:5),]
#     t <- checkIds(r, b)
#
#     # Test combos:
#     b <- bird_ids[-c(1:5),]
#     b$species[1:3] <- "wand"
#     t <- checkIds(r[r$bird_id!=b$bird_id[1],], b)
#
#     # wand should be omitted
#     r[r$bird_id == b[1,1],]
#     t[t$bird_id == b[1,1],]
#     t[t$bird_id == "041868EF6B",] # in data but not in index
#
#     ## ---- testing2 ----
#     problems <- read.csv("../Data/problems_test.csv")
#
#     # No problem
#     temp <- r[r$bird_id != as.character(problems$original_id[1]),]
#     test <- checkProblems(temp, problems)
#     temp[temp$bird_id == as.character(problems$original_id[1]),]
#
#     # Problem
#     temp <- r
#     temp[temp$bird_id == as.character(problems$original_id[1]),]
#     test <- checkProblems(temp, problems = "../Data/problems_test.csv")
#     test[test$bird_id == as.character(problems$original_id[1]),]
#
#     ## Levels changed as they should
#     levels(temp$bird_id)
#     levels(test$bird_id)
