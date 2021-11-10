# check_ids()
test_that("check_ids catches and removes correctly", {
  b <- read.csv(system.file("extdata", "animal_index.csv", package = "feedr"))

  check_ids(finches, b) %>%
    expect_message("All animal_ids in your data are also in your animal_id index") %>%
    expect_message("All animal_ids in your animal_id index are also in your data") %>%
    expect_message("animal_id index \\(id\\) data frame doesn't contain any animal_ids to omit") %>%
    expect_message("No animal_ids have been omitted")

  b$species <- as.character(b$species)
  b$species[1] <- "wand"
  b$species[2] <- "error"

  expect_message(f <- check_ids(finches, b),
                 "All animal_ids in your data are also in your animal_id index") %>%
    expect_message("All animal_ids in your animal_id index are also in your data") %>%
    expect_message("The following animal_ids have been omitted: 0620000514, 041868D861")

  expect_equal(nrow(f[f$animal_id == "0620000514",]), 0)
  expect_equal(nrow(f[f$animal_id == "041868D861",]), 0)

  b <- b[c(b$animal_id %in% c("06200004F8", "041868D396")),]
  check_ids(finches, b) %>%
    expect_message("Some animal_ids present in your data do not exist in the animal_id index: 0620000514, 041868D861, 062000043E") %>%
    suppressMessages()

  r <- finches[!(finches$animal_id %in% c("0620000514", "06200004F8")),]
  expect_message(f <- check_ids(r, b),
                 paste0("Some animal_ids present in your animal_id index, ",
                        "are not in your data: 06200004F8")) %>%
    suppressMessages()

  expect_s3_class(f$animal_id, "factor")
  expect_s3_class(f$logger_id, "factor")
  expect_s3_class(f$time, "POSIXct")

  r$animal_id <- as.character(r$animal_id)
  r$animal_id[1] <- substr(r$animal_id[1], 1, 4)
  expect_error(check_ids(r, b), "You have some ids in your read data that are not 10 characters long")
  expect_error(check_ids(r, b, id_length = 4),
"You have some ids in your animal_id index that are not 4 characters long
  You have some ids in your read data that are not 4 characters long")

})

# check_problems()
test_that("check_problems catches and removes correctly", {
  p <- read.csv(system.file("extdata", "problems.csv", package = "feedr"))

  expect_message(f <- check_problems(finches, p),
                 "The following animal ids have been corrected:") %>%
    suppressMessages()

  expect_s3_class(f$animal_id, "factor")
  expect_s3_class(f$logger_id, "factor")
  expect_s3_class(f$time, "POSIXct")
  expect_equal(sum(levels(f$animal_id) == "041B6BEF6B"), 1)
  expect_equal(sum(levels(f$animal_id) == "041B999F6B"), 1)

  p$original_id <- as.character(p$original_id)
  p$original_id <- p$corrected_id
  expect_message(check_problems(finches, p), "No animal ids needed to be fixed")
})
