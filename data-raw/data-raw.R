library(dplyr)

## Finches
finches <- get.data(start = "2016-01-28", end = "2016-01-30")

write.csv(finches, "./data-raw/finches.csv", row.names = FALSE)
devtools::use_data(finches, overwrite = TRUE)

### Chickadees
chickadees <- load_raw_all(system.file("extdata", "raw", package = "feedr"), extra_pattern = "exp[0-9]{1}", extra_name = "experiment")
chickadees$feeder_id <- paste0(chickadees$experiment, "-", chickadees$feeder_id)  ##Make feeder id unique

locs <- read.csv(system.file("extdata", "raw", "feeder_index.csv", package = "feedr"))
locs$feeder_id <- paste0(locs$experiment, "-", locs$feeder_name)

chickadees <- merge(chickadees, locs[, c("feeder_id", "lat", "lon")], by = "feeder_id", all.x = TRUE, all.y = FALSE)

write.csv(chickadees, "./data-raw/chickadees.csv", row.names = FALSE)
devtools::use_data(chickadees, overwrite = TRUE)

## Copy Data to Website Data folder
system("cp -r ./inst/extdata/raw/* ./docs/Data/Raw")
system("cp ./inst/extdata/* ./docs/Data/")
