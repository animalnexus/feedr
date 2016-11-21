library(RPostgreSQL)
library(tidyr)
library(dplyr)

dbname <- 'birdmv-dev'
dbuser <- 'tern'
dbpass <- '627_VBN'
dbport <- 5432
drv <- dbDriver("PostgreSQL")
dbhost <- 'geoteach.tru.ca'
con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)

#dbListTables(con)
#dbExistsTable(con, c("raw", "visits"))
#dbExistsTable(con, "testtz")
#data <- dbReadTable(con, c("ages"))
#feeders <- dbReadTable(con, c("feeders"))
#dbGetQuery(con, statement = paste("SELECT *",
#                                  "FROM feeders",
#                                  "WHERE site_name LIKE 'Kamloops%'"))





# 350 / 800?
microbenchmark::microbenchmark(dbGetQuery(con, statement = paste0("SELECT * FROM birds")), times = 10)

# 500
microbenchmark::microbenchmark(dbGetQuery(con, statement = paste0("SELECT bird_id, species, age, sex FROM birds")), times = 10)


# 132 / 420
microbenchmark::microbenchmark(dbGetQuery(con, statement = paste0("SELECT bird_id, species FROM birds")), times = 10)

# 425
microbenchmark::microbenchmark(
  birds <- dbGetQuery(con, 
                      statement = paste0("SELECT DISTINCT ON (raw.visits.bird_id) 
                                                               birds.bird_id, birds.species, raw.visits.bird_id
                                                               FROM birds, raw.visits
                                                               WHERE birds.bird_id = raw.visits.bird_id")), times = 10)

# 164
microbenchmark::microbenchmark(
  birds <- dbGetQuery(con, 
                      statement = paste0("SELECT DISTINCT raw.visits.bird_id
                                           FROM raw.visits
                                           INNER JOIN birds ON raw.visits.bird_id = birds.bird_id")), times = 10)

# 266
microbenchmark::microbenchmark(
  birds <- dbGetQuery(con, statement = paste0("SELECT DISTINCT raw.visits.bird_id
                                                 FROM raw.visits")) %>%
    left_join(dbGetQuery(con, statement = paste0("SELECT bird_id, species FROM birds")), by = "bird_id"), 
  times = 10)

# 137
microbenchmark::microbenchmark(
  birds <- dbGetQuery(con, 
                      statement = paste0("SELECT DISTINCT ON (raw.visits.bird_id) 
                                                               raw.visits.bird_id
                                                               FROM raw.visits")), times = 10)

dbDisconnect(con)