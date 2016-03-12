library(feedr)
context("Loading files")

r <- read.csv("~/Projects/RFID/Data/"

test_that("The loaded data frame is not empty", {
  expect_is(loadRaw(







    source("fun_visits.R")
    files <- list.files("../Data/", pattern = "[GPR]{2,3}[0-9]{1,2}DATA", full.names = T)
    test <- do.call('rbind', lapply(files, FUN = load))
    test$time <- as.POSIXct(strptime(paste0(test$date,test$time), "%m/%d/%y %H:%M:%S"))
    v <- visits(test)
    v1 <- v[v$bird_id == "0620000418",]
    v1 <- v[v$bird_id == "062000031A",]

    data <- read.table("../Data/OnlineDATA_2.csv", col.names = c("time","feeder_id","bird_id"), sep = ",")

    data$time <- as.POSIXct(strptime(test$time, "%m-%d-%yT%H:%M:%SZ"))





    ## ---- testing1 ----
    source("fun_load.R")
    r <- loadWeb("../Data/Dec01_Dec14_Data.csv")
    bird_ids <- unique(data.frame(bird_id = r[,3], species = as.character("bcch")))
    bird_ids$species <- as.character(bird_ids$species)

    # Test perfect match
    b <- bird_ids
    t <- checkIds(r, bird_ids)

    # Test omit wand:
    b <- bird_ids
    b$species[1:3] <- "wand"
    t <- checkIds(r, b)

    # Test more in index than data
    b <- bird_ids
    t <- checkIds(r[r$bird_id!=b$bird_id[1],], b)

    # Test more in data than in index
    b <- bird_ids[-c(1:5),]
    t <- checkIds(r, b)

    # Test combos:
    b <- bird_ids[-c(1:5),]
    b$species[1:3] <- "wand"
    t <- checkIds(r[r$bird_id!=b$bird_id[1],], b)

    # wand should be omitted
    r[r$bird_id == b[1,1],]
    t[t$bird_id == b[1,1],]
    t[t$bird_id == "041868EF6B",] # in data but not in index

    ## ---- testing2 ----
    problems <- read.csv("../Data/problems_test.csv")

    # No problem
    temp <- r[r$bird_id != as.character(problems$original_id[1]),]
    test <- checkProblems(temp, problems)
    temp[temp$bird_id == as.character(problems$original_id[1]),]

    # Problem
    temp <- r
    temp[temp$bird_id == as.character(problems$original_id[1]),]
    test <- checkProblems(temp, problems = "../Data/problems_test.csv")
    test[test$bird_id == as.character(problems$original_id[1]),]

    ## Levels changed as they should
    levels(temp$bird_id)
    levels(test$bird_id)
