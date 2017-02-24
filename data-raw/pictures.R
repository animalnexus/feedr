library(magrittr)
library(RPostgreSQL)

if(file.exists("/usr/local/share/feedr/db_full.R")) {
  source("/usr/local/share/feedr/db_full.R")
} else db <- NULL

pics <- data.frame(img = paste0("http://gaia.tru.ca/birdMOVES/img.kl/", list.files("./data-raw/Pictures - Formatted/"))) %>%
  dplyr::mutate(animal_id_orig = gsub(".*/([^/]+).jpg$", "\\1", img),
                animal_id = substr(animal_id_orig, 1, 10))


con <- dbConnect(dbDriver("PostgreSQL"), host = db$host, port = db$port, dbname = db$name, user = db$user, password = db$pass)
dbGetQuery(con, statement = paste0("SELECT birds.bird_id, species.engl_name FROM birds, species WHERE (species.code = birds.species)")) %>%
  load_format(tz = "") %>%
  dplyr::mutate(author = "TRU",
                citation = NA,
                img = paste0("http://gaia.tru.ca/birdMOVES/img.kl/", animal_id, ".jpg")) %>%
  dplyr::filter(animal_id %in% pics$animal_id) %>%
  write.csv("./inst/extdata/shiny-data/img_index.csv", row.names = FALSE)
dbDisconnect(con)

## Rename picture files:
file.rename(paste0("./data-raw/Pictures - Formatted/", pics$animal_id_orig, ".jpg"),
            paste0("./data-raw/Pictures - Formatted/", pics$animal_id, ".jpg"))

