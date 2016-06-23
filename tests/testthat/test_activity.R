library(magrittr)
context("Transformations to activity")

# activity()
test_that("activity() in general", {
  f <- visits(finches) %>% dplyr::group_by(bird_id) %>% dplyr::do(feeding(.))
  a <- dplyr::do(f, activity(.))
  #a <- dplyr::do(f[f$bird_id == "06200004F8",], activity(.))
  expect_message(plyr::ddply(f, c("bird_id"), activity),
                 "041868D396: Skipping. Individual has less than 24hrs of data")

  expect_message(plyr::ddply(f, c("bird_id"), activity),
                 "0620000514: 88.89% of obs")

  expect_message(plyr::ddply(f, c("bird_id"), activity, res = 5),
                 "0620000514: 55.56% of obs")

   expect_is(a, "data.frame")
   expect_match(names(a)[1:6], "^bird_id$|^time$|^date$|^activity$|^activity_c$|^feeder_id$")
   expect_is(a$bird_id, "factor")
   expect_is(a$feeder_id, "factor")
   expect_is(a$time, "POSIXct")

   expect_equal(a$bird_id[1], factor("06200004F8", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
   expect_equal(a$feeder_id[1], factor(NA, levels = c("2100", "2200", "2400", "2700")))
   expect_equal(a$time[1], as.POSIXct("2016-01-28", tz = "America/Vancouver"))
   expect_equal(nrow(a), 386)
})

# activity()
test_that("activity() no missing, by feeder", {
  f <- plyr::ddply(visits(finches), c("bird_id"), feeding)
  a <- plyr::ddply(f, c("bird_id"), activity, by_feeder = TRUE)

  expect_equal(a$feeder_id[1], factor(2100, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(nrow(a), 1544)
})

# activity()
test_that("activity() missing", {
  f <- plyr::ddply(visits(finches), c("bird_id"), feeding)
  m <- read.csv("../data/missing.csv")

  a <- plyr::ddply(f, c("bird_id"), activity, by_feeder = TRUE, missing = m)
  expect_equal(nrow(a[a$activity_c == "unknown",]), 260)

  a <- plyr::ddply(f, c("bird_id"), activity, missing = m)
  expect_equal(nrow(a[a$activity_c == "unknown",]), 130)

  a <- plyr::ddply(f, c("bird_id"), activity, missing = "../data/missing.csv")
  expect_equal(nrow(a[a$activity_c == "unknown",]), 130)

  expect_error(plyr::ddply(f, c("bird_id"), activity, missing = c(1, 2)), "'missing' must be")
})

# daily()
test_that("daily() by_feeder == FALSE", {
  f <- plyr::ddply(visits(finches), c("bird_id"), feeding)
  a <- plyr::ddply(f, c("bird_id"), activity)
  d <- plyr::ddply(a, c("bird_id"), daily)

  expect_equal(d$bird_id[1], factor("06200004F8", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(d$feeder_id[1], factor(NA, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(d$time[1], as.POSIXct("1970-01-01", tz = "UTM"))
  expect_equal(nrow(d), 192)

  a <- plyr::ddply(f, c("bird_id"), activity, by_feeder = TRUE)
  d <- plyr::ddply(a, c("bird_id"), daily)

  expect_equal(d$bird_id[1], factor("06200004F8", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(d$feeder_id[1], factor(2100, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(d$time[1], as.POSIXct("1970-01-01", tz = "UTM"))
  expect_equal(nrow(d), 768)
})
