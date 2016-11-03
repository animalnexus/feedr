
finches <- get.data(start = "2016-01-28", end = "2016-01-30")

write.csv(finches, "./data-raw/finches.csv", row.names = FALSE)
devtools::use_data(finches, overwrite = TRUE)
