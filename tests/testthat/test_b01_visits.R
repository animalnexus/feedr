library(magrittr)
context("Transformations to visits")

# visits()
test_that("visits() returns appropriate, non-empty dataframe", {
  expect_silent(v <- visits(finches))
  expect_is(v, "data.frame")
  expect_equal(sum(is.na(v)), 0)
  expect_match(names(v)[1:3], "^animal_id$|^date$|^start$|^end$|^logger_id$")
  expect_is(v$animal_id, "factor")
  expect_is(v$logger_id, "factor")
  expect_is(v$start, "POSIXct")
  expect_is(v$end, "POSIXct")
  expect_true(all(as.numeric(v$end - v$start) >= 0))
})

test_that("visits() has correct spacing", {
  t <- data.frame(n = c(1, 1, 8, 3),
                  bw = c(3, 10, 30, 3600),
                  start = as.POSIXct(c("2016-01-28 12:35:52",
                                       "2016-01-28 12:35:52",
                                       "2016-01-29 09:53:53",
                                       "2016-01-28 13:23:40"), tz = "America/Vancouver"),
                  end = as.POSIXct(c("2016-01-28 12:35:52",
                                     "2016-01-28 12:36:12",
                                     "2016-01-29 09:55:37",
                                     "2016-01-28 15:09:19"), tz = "America/Vancouver"))

  # First two visits interrupted by other birds arriving, last is not (at the same feeder at least)

  for(i in 1:nrow(t)){
    v <- visits(finches, bw = t$bw[i]) %>%
      dplyr::filter(animal_id == "06200004F8")
    expect_true(v$start[t$n[i]] == t$start[i])
    expect_true(v$end[t$n[i]] == t$end[i])
  }

})

test_that("visits() jumps over obs of diff animals at diff loggers", {
  # Expect 06200004F8 observations to consolidate, even though inbetween obs at a different logger
  expect_equal(nrow(visits(finches[40:75,], bw = 6000)), 5)

  # Expect no two rows to have the same animal_id UNLESS the logger_id is different
  for(r in list(finches, finches_lg, chickadees)) {
    v <- visits(r, bw = 10000000000000000000) %>%
      dplyr::arrange(start) %>%
      dplyr::mutate(okay = (animal_id != dplyr::lead(animal_id)) |
                      (animal_id == dplyr::lead(animal_id) & logger_id != dplyr::lead(logger_id)))
    expect_true(sum(!v$okay, na.rm = TRUE) == 0)
  }

  # Expect no two visits at the same logger to overlap

  library(lubridate)

  error <- data.frame()

  for(q in c("finches", "finches_lg")) {
    r <- get(q)
    if(q == "chickadees") r <- dplyr::filter(r, experiment == "exp2")
    v <- visits(r, bw = 100000000000000000000) %>%
      dplyr::arrange(start) %>%
      dplyr::mutate(visits = lubridate::interval(start, end))

    # Expect no two visits at the same logger to overlap
    for(l in unique(v$logger_id)){
     # message(q, ": ", l)
      for(i in 1:length(v$visits[v$logger_id == l])){
        if(any(v$visits[v$logger_id == l][i] %within% v$visits[v$logger_id == l][-i])) {
          error <- rbind(error, data.frame(data = q, type = "logger", visits = v$visits[v$logger_id == l][i], id = l))
        }
      }
    }

    # Expect no two visits by the same individual to overlap
    for(a in unique(v$animal_id)){
    # message(q, ": ", a)
      for(i in 1:length(v$visits[v$logger_id == l])){
        if(any(v$visits[v$logger_id == l][i] %within% v$visits[v$logger_id == l][-i])) {
          error <- rbind(error, data.frame(data = q, type = "animal_id", visits = v$visits[v$logger_id == l][i], id = l))
        }
      }
    }
  }

  expect_true(nrow(error) == 0, info = "At least one animal_id or logger_id has overlapping visits")

})

test_that("visits() returns correct data", {
  v <- visits(finches)
  expect_equal(v$animal_id[2], factor("041868D396", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))
  expect_equal(v$logger_id[2], factor("2100", levels = c("2100", "2200", "2400", "2700")))
  expect_equal(v$start[2], as.POSIXct("2016-01-29 11:21:23", tz = "America/Vancouver"))
  expect_equal(v$end[2], as.POSIXct("2016-01-29 11:21:26", tz = "America/Vancouver"))
  expect_equal(v$animal_n[2], 5)
  expect_equal(v$logger_n[2], 4)
  expect_equal(round(v$lon[2], 4), -120.3624)
  expect_equal(round(v$lat[2], 5), 50.66896)
  expect_equal(v$species[2], "House Finch")
  expect_equal(nrow(v[v$end != v$start,]), 49) ## 49 visits > 0 seconds
})

test_that("visits() allow_imp", {
  f <- rbind(finches, finches[1:5,] %>% dplyr::mutate(logger_id = "2400"))
  expect_error(visits(f), "Impossible visits found \\(n = 10\\)")
  expect_silent(visits(f, allow_imp = TRUE))
})

test_that("visits() na_rm", {
  f <- finches
  f[4, 'time'] <- NA
  expect_error(visits(f), "NAs")
  expect_silent(v2 <- visits(f, na_rm = TRUE))
  expect_equal(nrow(v2) + 1, nrow(visits(finches)))
})

test_that("visits() pass", {
  expect_length(visits(finches, pass = FALSE), 7)
  expect_length(visits(finches, pass = TRUE), 11)
})

test_that("visits() with group_by", {
  coords <- read.csv(system.file("extdata", "chickadees_logger_index.csv", package = "feedr")) %>%
    dplyr::mutate(logger_id = paste0(experiment, "-", logger_name))

  c <- load_raw_all(system.file("extdata", "raw", package = "feedr"),
                    extra_pattern = "exp[0-9]{1,2}",
                    extra_name = "experiment", tz = "America/Vancouver") %>%
    dplyr::mutate(logger_id = paste0(experiment, "-", logger_id)) %>%
    dplyr::left_join(coords, by = c("logger_id", "experiment"))

  expect_silent(visits(c))
  expect_silent(dplyr::group_by(c, experiment) %>% dplyr::do(visits(., bw = 3, allow_imp = TRUE)))

})
