library(dplyr)
library(magrittr)

## Finches
finches_lg <- dl_data(start = "2015-09-01",
               end = "2015-11-01") %>%
  filter(species == "House Finch",
         sex %in% c("M", "F"),
         ! (animal_id %in% c("041868D100", "041868D396"))) %>%
  arrange(animal_id, time, logger_id) %>%
  droplevels() %>%
  as_tibble()

write.csv("./data-raw/finches_lg.csv", row.names = FALSE)
write.csv("./inst/extdata/finches_lg.csv", row.names = FALSE)
devtools::use_data(finches_lg, overwrite = TRUE)

finches <- dl_data(start = "2016-01-28", end = "2016-01-30") %>%
  filter(species == "House Finch") %>%
  droplevels() %>%
  as_tibble()

write.csv(finches, "./data-raw/finches.csv", row.names = FALSE)
write.csv(finches, "./inst/extdata/finches.csv", row.names = FALSE)
devtools::use_data(finches, overwrite = TRUE)


### Chickadees
locs <- read.csv(system.file("extdata", "chickadees_logger_index.csv", package = "feedr")) %>%
  mutate(logger_id = paste0(experiment, "-", logger_name))

chickadees <- load_raw_all(system.file("extdata", "raw", package = "feedr"),
                           extra_pattern = "exp[0-9]{1}",
                           extra_name = "experiment",
                          ) %>%
  mutate(logger_id = paste0(experiment, "-", logger_id)) %>%  ##Make logger id unique
  filter((date > as.Date("2016-01-10") & date <= as.Date("2016-01-25")) |
         (date > as.Date("2016-01-31") & date <= as.Date("2016-02-15"))) %>%
  left_join(locs[, c("logger_id", "lat", "lon")], by = "logger_id") %>%
  check_ids(ids = data.frame(animal_id = "0000000000", species = "error")) %>%
  load_format() %>%
  droplevels() %>%
  as_tibble()

write.csv(chickadees, "./data-raw/chickadees.csv", row.names = FALSE)
write.csv(chickadees, "./inst/extdata/chickadees.csv", row.names = FALSE)
devtools::use_data(chickadees, overwrite = TRUE)

## Copy Data to Website Data folder
system("cp -r ./inst/extdata/raw/* ./docs/Data/Raw")
system("cp ./inst/extdata/* ./docs/Data/")
