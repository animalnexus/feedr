ids <- sort(unique(finches$animal_id))
locs <- sort(unique(paste0(finches$logger_id, "_", finches$date)))
loggers <- sort(unique(finches$logger_id))

test_that("convert_asnipe converts gmmevents as expected", {
  expect_silent(a <- convert_asnipe(finches))
  expect_s3_class(a, "data.frame")
  expect_equal(nrow(a), 412)
  expect_named(a, c("time", "identity", "location"))
  expect_type(a$time, "double")

  expect_equal(a[1,],
               data.frame(time = 0,
                          identity = factor("0620000514", levels = ids),
                          location = factor("2200_2016-01-28", levels = locs)),
               ignore_attr = TRUE)

  expect_equal(a[nrow(a),],
               data.frame(time = 97247,
                          identity = factor("041868D396", levels = ids),
                          location = factor("2100_2016-01-29", levels = locs)),
               ignore_attr = TRUE)

  expect_silent(a <- convert_asnipe(finches, by_day = FALSE))
  expect_s3_class(a, "data.frame")
  expect_equal(nrow(a), 412)
  expect_named(a, c("time", "identity", "location"))
  expect_type(a$time, "double")

  expect_equal(a[1,],
               data.frame(time = 0,
                          identity = factor("0620000514", levels = ids),
                          location = factor("2200", levels = loggers)),
               ignore_attr = TRUE)

  expect_equal(a[nrow(a),],
               data.frame(time = 97247,
                          identity = factor("041868D396", levels = ids),
                          location = factor("2100", levels = loggers)),
               ignore_attr = TRUE)

  expect_silent(a <- convert_asnipe(finches, time_scale = "hours"))
  expect_s3_class(a, "data.frame")
  expect_equal(nrow(a), 412)
  expect_named(a, c("time", "identity", "location"))
  expect_type(a$time, "double")

  expect_equal(a[1,],
               data.frame(time = 0,
                          identity = factor("0620000514", levels = ids),
                          location = factor("2200_2016-01-28", levels = locs)),
               ignore_attr = TRUE)

  expect_equal(a[nrow(a),],
               data.frame(time = 97247/60/60,
                          identity = factor("041868D396", levels = ids),
                          location = factor("2100_2016-01-29", levels = locs)),
               ignore_attr = TRUE)
})

test_that("convert_asnipe converts get_associations_points_tw as expected", {

  expect_silent(a <- convert_asnipe(finches, fun = "get_associations_points_tw"))
  expect_s3_class(a, "data.frame")
  expect_named(a, c("Date", "Time", "ID", "Location"))
  expect_type(a$Time, "double")
  expect_type(a$Date, "double")

  expect_equal(a[1,],
               data.frame(Date = 1,
                          Time = 0,
                          ID = factor("0620000514", levels = ids),
                          Location = factor("2200", levels = loggers)),
               ignore_attr = TRUE)

  expect_equal(a[nrow(a),],
               data.frame(Date = 2,
                          Time = 97247,
                          ID = factor("041868D396", levels = ids),
                          Location = factor("2100", levels = loggers)),
               ignore_attr = TRUE)

})

test_that("convert_asnipe data runs in asnipe gmmevents function", {
  ## gmmevents
  set.seed(201)
  a <- convert_asnipe(finches)[1:100,]

  temp <- capture.output(b <- asnipe::gmmevents(time = a$time,
                                                identity = a$identity,
                                                location = a$location)) %>%
    suppressWarnings() %>%
    expect_silent()

  expect_type(b, "list")
  expect_length(b, 3)
  expect_named(b, c("gbi", "metadata", "B"))
  expect_true("matrix" %in% class(b$gbi))
  expect_s3_class(b$metadata, "data.frame")
  expect_true("matrix" %in% class(b$B))

  a <- convert_asnipe(chickadees)[1:100,]
  expect_error(suppressWarnings(temp <- capture.output(b <- asnipe::gmmevents(time = a$time,
                                                                              identity = a$identity,
                                                                              location = a$location)), NA))
  expect_type(b, "list")
  expect_length(b, 3)
  expect_named(b, c("gbi", "metadata", "B"))
  expect_true("matrix" %in% class(b$gbi))
  expect_s3_class(b$metadata, "data.frame")
  expect_true("matrix" %in% class(b$B))
})

test_that("convert_asnipe data runs in asnipe get_associations_points_tw function", {
  a <- convert_asnipe(finches, fun = "get_associations_points_tw")[1:100,]
  expect_silent(b <- asnipe::get_associations_points_tw(a))

  expect_type(b, "list")
  expect_named(b, NULL)
  expect_length(b, 4)
  expect_true("matrix" %in% class(b[[1]]))
  expect_type(b[[2]], "double")
  expect_type(b[[3]], "double")
  expect_s3_class(b[[4]], "factor")

  a <- convert_asnipe(chickadees, fun = "get_associations_points_tw")[1:100,]
  expect_silent(b <- asnipe::get_associations_points_tw(a))

  expect_type(b, "list")
  expect_named(b, NULL)
  expect_length(b, 4)
  expect_true("matrix" %in% class(b[[1]]))
  expect_type(b[[2]], "double")
  expect_type(b[[3]], "double")
  expect_s3_class(b[[4]], "factor")
})

test_that("convert_anidom converts as expected", {
  d <- disp(visits(finches_lg), bw = 5)

  expect_silent(a <- convert_anidom(d$displacements))
  expect_silent(a2 <- convert_anidom(d))
  expect_equivalent(a, a2)
  expect_s3_class(a, "data.frame")
  expect_equal(nrow(a), nrow(d$displacements)/2)
  expect_named(a, c("winner", "loser"))
  expect_type(a$winner, "character")
  expect_type(a$loser, "character")
  expect_equivalent(a[1,],
                    data.frame(winner = "0620000500",
                               loser = "06200004F8",
                               stringsAsFactors = FALSE))
  expect_equivalent(a[nrow(a),],
                    data.frame(winner = "06200004F8",
                               loser = "06200003AA", stringsAsFactors = FALSE))
})

test_that("convert_anidom runs aniDom functions as expected", {

  d <- disp(visits(finches_lg), bw = 5)
  i <- convert_anidom(d)

  # Elo_scores
  expect_silent(s <- aniDom::elo_scores(winners = i$winner, losers = i$loser))
  expect_true("matrix" %in% class(s))
  expect_equal(dimnames(s)[[1]],  c("0620000500", "06200004F8",
                                    "0620000477", "06200003AA", "0620000400"))

  # Estimate repeatability
  set.seed(191)
  expect_silent(r1 <- aniDom::estimate_uncertainty_by_repeatability(winners = i$winner, losers = i$loser))
  expect_silent(r2 <- aniDom::estimate_uncertainty_by_splitting(winners = i$winner, losers = i$loser, randomise = TRUE))

  expect_type(r1, "double")
  expect_type(r2, "double")
  expect_named(r2, c("Mean", "2.5%", "97.5%"))
})

test_that("convert_dominance converts as expected", {
  skip_on_os(c("mac", "linux"))
  d <- disp(visits(finches_lg), bw = 5)
  expect_silent(a <- convert_dominance(d$displacements))
  expect_silent(a2 <- convert_dominance(d))
  expect_equivalent(a, a2)
  expect_type(a, "list")
  expect_length(a, 4)

  expect_equal(nrow(a$data_sheet), nrow(d$displacements)/2)
  expect_named(a$data_sheet, c("action.from", "action.to", "kind.of.action"))
  expect_type(a$data_sheet$action.from, "double")
  expect_type(a$data_sheet$action.to, "double")
  expect_type(a$data_sheet$kind.of.action, "double")
  expect_equivalent(a$data_sheet[1,], data.frame(action.from = 5, action.to = 4,
                                                 kind.of.action = 1, stringsAsFactors = FALSE))
  expect_equivalent(a$data_sheet[nrow(a$data_sheet),], data.frame(action.from = 4, action.to = 1,
                                                                  kind.of.action = 1, stringsAsFactors = FALSE))

  expect_equal(nrow(a$items), length(unique(d$displacements$animal_id)))
  expect_named(a$items, c("Name", "item.number"))
  expect_type(a$items$Name, "character")
  expect_type(a$items$item.number, "double")
  expect_equivalent(a$items[1,], data.frame(Name = "06200003AA", item.number = 1, stringsAsFactors = FALSE))
  expect_equivalent(a$items[nrow(a$items),], data.frame(Name = "0620000500", item.number = 5, stringsAsFactors = FALSE))

  expect_equal(nrow(a$actions), 1)
  expect_named(a$actions, c("name.of.action", "action.number", "classification", "weighting"))
  expect_type(a$actions$name.of.action, "character")
  expect_type(a$actions$action.number, "double")
  expect_type(a$actions$classification, "double")
  expect_type(a$actions$weighting, "double")
  expect_equivalent(a$actions[1,], data.frame(name.of.action = "displacement", action.number = 1, classification = 1, weighting = 1, stringsAsFactors = FALSE))

  expect_equal(a$bytes, "1")
})

test_that("convert_dominance runs Dominance ADI functions as expected", {
  skip_on_os(c("mac", "linux"))

  d <- disp(visits(finches_lg), bw = 5)
  i <- convert_dominance(d)

  # ADI
  expect_silent(s <- Dominance::ADI(data_sheet = i$data_sheet, items = i$items,
                                    actions = i$actions, bytes = i$bytes))

  expect_type(s, "list")
  expect_length(s, 3)

  expect_true("matrix" %in% class(s$ADI))
  expect_equal(dim(s$ADI), c(5,8))
  expect_equal(dimnames(s$ADI)[[1]], c("0620000500", "06200004F8", "06200003AA",
                                       "0620000477", "0620000400"))
  expect_equal(dimnames(s$ADI)[[2]], c("0620000500", "06200004F8", "06200003AA",
                                       "0620000477", "0620000400",
                                       "results.ADI", "id", "rank"))

  expect_type(s$Colors, "character")
  expect_equal(s$Colors, "")

  expect_true("matrix" %in% class(s$ADI_count_matrix))
  expect_equal(dim(s$ADI_count_matrix), c(5,5))
  expect_equal(dimnames(s$ADI_count_matrix)[[1]], sort(as.character(unique(d$displacements$animal_id))))
  expect_equal(dimnames(s$ADI_count_matrix)[[2]], sort(as.character(unique(d$displacements$animal_id))))

})

test_that("convert_dominance runs Dominance Sociogram functions as expected", {
  skip_on_os(c("mac", "linux"))

  d <- disp(visits(finches_lg), bw = 5)
  i <- convert_dominance(d)

  # Sociogram
  expect_silent(s <- Dominance::Sociogram(
    data_sheet = i$data_sheet, items = i$items,
    actions = i$actions, bits = i$bytes))
  expect_type(s, "list")
  expect_named(s, c("sociogram", "counts_circles", "count_interactions",
                    "line_size"))
  file.remove("Rplots.pdf")
})

test_that("convert_perc converts as expected", {
  d <- disp(visits(finches_lg), bw = 5)

  expect_silent(a <- convert_perc(d$interactions))
  expect_silent(a2 <- convert_perc(d))
  expect_equivalent(a, a2)
  expect_s3_class(a, "data.frame")
  expect_equal(sum(a$Freq), sum(d$interactions$n))
  expect_named(a, c("Initiator1", "Recipient1", "Freq"))
  expect_type(a$Initiator1, "character")
  expect_type(a$Recipient1, "character")
  expect_type(a$Freq, "double")
  expect_equivalent(a[1,], data.frame(Initiator1 = "0620000500",
                                      Recipient1 = "06200003AA",
                                      Freq = 1))
  expect_equivalent(a[nrow(a),], data.frame(Initiator1 = "0620000477",
                                            Recipient1 = "0620000500",
                                            Freq = 1))
})

test_that("convert_perc runs Perc functions as expected", {

  d <- disp(visits(finches_lg), bw = 5)
  i <- convert_perc(d)

  # as.conflictmat
  expect_silent(s <- Perc::as.conflictmat(i, weighted = TRUE))
  expect_equal(dimnames(s), list(sort(unique(c(i$Initiator1, i$Recipient1))),
                                 sort(unique(c(i$Initiator1, i$Recipient1)))))

  # conductance
  expect_silent(c <- Perc::conductance(s, 2))
  expect_named(c, c("imputed.conf", "p.hat"))
  expect_true("matrix" %in% class(c$imputed.conf))
  expect_true("matrix" %in% class(c$p.hat))

  # simRankOrder
  set.seed(111)
  expect_silent(r <- Perc::simRankOrder(c$p.hat, num = 10, kmax = 100))
  expect_named(r, c("BestSimulatedRankOrder", "Costs", "AllSimulatedRankOrder"))
  expect_s3_class(r$BestSimulatedRankOrder, "data.frame")
  expect_s3_class(r$Cost, "data.frame")
  expect_s3_class(r$AllSimulatedRankOrder, "data.frame")
  expect_equal(r$BestSimulatedRankOrder,
               data.frame(ID = c("0620000500", "06200004F8", "06200003AA",
                                 "0620000477", "0620000400"), ranking = 1:5))
})

test_that("convert_activity converts as expected", {
  r <- finches_lg

  expect_silent(i <- convert_activity(r))
  expect_type(i, "list")
  expect_named(i, as.character(sort(unique(r$animal_id))))
  expect_type(i[[1]], "double")
  expect_equal(i[["062000043E"]], c(2.805326, 2.805471), tolerance = 0.0000001)
})

test_that("convert_activity runs activity functions as expected", {
  r <- finches_lg
  i <- convert_activity(r)

  #a <- lapply(i, activity::fitact, sample = "none")
  #' plot(a[[3]])
  #' plot(a[["06200004F8"]])
  expect_silent(a <- activity::fitact(i[["06200003AA"]], sample = "none"))
  expect_s4_class(a, "actmod")
  expect_silent(activity:::plot.actmod (a))
})
