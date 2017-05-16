context("Convert functions")

test_that("convert_asnipe converts gmmevents as expected", {

  expect_silent(a <- convert_asnipe(finches))
  expect_is(a, "data.frame")
  expect_equal(nrow(a), 412)
  expect_named(a, c("time", "identity", "location"))
  expect_is(a$time, "numeric")
  expect_equivalent(a[1,], data.frame(time = 0, identity = factor("0620000514"), location = factor("2200_2016-01-28")))
  expect_equivalent(a[nrow(a),], data.frame(time = 97247, identity = factor("041868D396"), location = factor("2100_2016-01-29")))

  expect_silent(a <- convert_asnipe(finches, by_day = FALSE))
  expect_is(a, "data.frame")
  expect_equal(nrow(a), 412)
  expect_named(a, c("time", "identity", "location"))
  expect_is(a$time, "numeric")
  expect_equivalent(a[1,], data.frame(time = 0, identity = factor("0620000514"), location = factor("2200")))
  expect_equivalent(a[nrow(a),], data.frame(time = 97247, identity = factor("041868D396"), location = factor("2100")))

  expect_silent(a <- convert_asnipe(finches, time_scale = "hours"))
  expect_is(a, "data.frame")
  expect_equal(nrow(a), 412)
  expect_named(a, c("time", "identity", "location"))
  expect_is(a$time, "numeric")
  expect_equivalent(a[1,], data.frame(time = 0, identity = factor("0620000514"), location = factor("2200_2016-01-28")))
  expect_equivalent(a[nrow(a),], data.frame(time = 97247/60/60, identity = factor("041868D396"), location = factor("2100_2016-01-29")))
})

test_that("convert_asnipe converts get_associations_points_tw as expected", {

  expect_silent(a <- convert_asnipe(finches, fun = "get_associations_points_tw"))
  expect_is(a, "data.frame")
  expect_named(a, c("Date", "Time", "ID", "Location"))
  expect_is(a$Time, "numeric")
  expect_is(a$Date, "numeric")
  expect_equivalent(a[1,], data.frame(Date = 1, Time = 0, ID = "0620000514", Location = "2200"))
  expect_equivalent(a[nrow(a),], data.frame(Date = 2, Time = 97247, ID = "041868D396", Location = "2100"))

})

test_that("convert_asnipe data runs in asnipe gmmevents function", {
  ## gmmevents
  set.seed(201)
  a <- convert_asnipe(finches)[1:100,]

  expect_error(suppressWarnings(temp <- capture.output(b <- asnipe::gmmevents(time = a$time,
                                                                              identity = a$identity,
                                                                              location = a$location), NA)))

  expect_is(b, "list")
  expect_length(b, 3)
  expect_named(b, c("gbi", "metadata", "B"))
  expect_is(b$gbi, "matrix")
  expect_is(b$metadata, "data.frame")
  expect_is(b$B, "matrix")

  a <- convert_asnipe(chickadees)[1:100,]
  expect_error(suppressWarnings(temp <- capture.output(b <- asnipe::gmmevents(time = a$time,
                                                                              identity = a$identity,
                                                                              location = a$location)), NA))
  expect_is(b, "list")
  expect_length(b, 3)
  expect_named(b, c("gbi", "metadata", "B"))
  expect_is(b$gbi, "matrix")
  expect_is(b$metadata, "data.frame")
  expect_is(b$B, "matrix")
})

test_that("convert_asnipe data runs in asnipe get_associations_points_tw function", {
  a <- convert_asnipe(finches, fun = "get_associations_points_tw")[1:100,]
  expect_silent(b <- asnipe::get_associations_points_tw(a))

  expect_is(b, "list")
  expect_named(b, NULL)
  expect_length(b, 3)
  expect_is(b[[1]], "matrix")
  expect_is(b[[2]], "numeric")
  expect_is(b[[3]], "numeric")

  a <- convert_asnipe(chickadees, fun = "get_associations_points_tw")[1:100,]
  expect_silent(b <- asnipe::get_associations_points_tw(a))

  expect_is(b, "list")
  expect_named(b, NULL)
  expect_length(b, 3)
  expect_is(b[[1]], "matrix")
  expect_is(b[[2]], "numeric")
  expect_is(b[[3]], "numeric")
})

test_that("convert_anidom converts as expected", {
  d <- disp(visits(finches_lg))

  expect_silent(a <- convert_anidom(d$displacements))
  expect_silent(a2 <- convert_anidom(d))
  expect_equivalent(a, a2)
  expect_is(a, "data.frame")
  expect_equal(nrow(a), nrow(d$displacements)/2)
  expect_named(a, c("winner", "loser"))
  expect_is(a$winner, "character")
  expect_is(a$loser, "character")
  expect_equivalent(a[1,], data.frame(winner = "0620000500", loser = "06200004F8", stringsAsFactors = FALSE))
  expect_equivalent(a[nrow(a),], data.frame(winner = "06200004F8", loser = "06200003AA", stringsAsFactors = FALSE))
})

test_that("convert_anidom runs aniDom functions as expected", {

  d <- disp(visits(finches_lg))
  i <- convert_anidom(d)

  # Elo_scores
  expect_silent(s <- aniDom::elo_scores(winners = i$winner, losers = i$loser))
  expect_equal(s, matrix(c(383.75568, 59.78409, -219.62800, -177.56012, -46.35165), ncol = 1, dimnames = list(c("0620000500", "06200004F8", "0620000477", "06200003AA", "0620000400"))), tolerance = 0.00001)

  # Estimate repeatability
  set.seed(191)
  expect_silent(r1 <- aniDom::estimate_uncertainty_by_repeatability(winners = i$winner, losers = i$loser))
  expect_silent(r2 <- aniDom::estimate_uncertainty_by_splitting(winners = i$winner, losers = i$loser, randomise = TRUE))

  expect_equal(r1, 0.9585894, tolerance = 0.000001)
  expect_equal(r2, c(Mean = 0.334, "2.5%" = -0.3000, "97.5%" = 1.000))
})

test_that("convert_dominance converts as expected", {
  d <- disp(visits(finches_lg))
  expect_silent(a <- convert_dominance(d$displacements))
  expect_silent(a2 <- convert_dominance(d))
  expect_equivalent(a, a2)
  expect_is(a, "list")
  expect_length(a, 4)

  expect_equal(nrow(a$data_sheet), nrow(d$displacements)/2)
  expect_named(a$data_sheet, c("action.from", "action.to", "kind.of.action"))
  expect_is(a$data_sheet$action.from, "numeric")
  expect_is(a$data_sheet$action.to, "numeric")
  expect_is(a$data_sheet$kind.of.action, "numeric")
  expect_equivalent(a$data_sheet[1,], data.frame(action.from = 5, action.to = 4,
                                                 kind.of.action = 1, stringsAsFactors = FALSE))
  expect_equivalent(a$data_sheet[nrow(a$data_sheet),], data.frame(action.from = 4, action.to = 1,
                                                                  kind.of.action = 1, stringsAsFactors = FALSE))

  expect_equal(nrow(a$items), length(unique(d$displacements$animal_id)))
  expect_named(a$items, c("Name", "item.number"))
  expect_is(a$items$Name, "character")
  expect_is(a$items$item.number, "numeric")
  expect_equivalent(a$items[1,], data.frame(Name = "06200003AA", item.number = 1, stringsAsFactors = FALSE))
  expect_equivalent(a$items[nrow(a$items),], data.frame(Name = "0620000500", item.number = 5, stringsAsFactors = FALSE))

  expect_equal(nrow(a$actions), 1)
  expect_named(a$actions, c("name.of.action", "action.number", "classification", "weighting"))
  expect_is(a$actions$name.of.action, "character")
  expect_is(a$actions$action.number, "numeric")
  expect_is(a$actions$classification, "numeric")
  expect_is(a$actions$weighting, "numeric")
  expect_equivalent(a$actions[1,], data.frame(name.of.action = "displacement", action.number = 1, classification = 1, weighting = 1, stringsAsFactors = FALSE))

  expect_equal(a$bytes, "1")
})

test_that("convert_dominance runs Dominance ADI functions as expected", {
  skip_on_travis()
  skip_on_appveyor()

  d <- disp(visits(finches_lg))
  i <- convert_dominance(d)

  # ADI
  expect_silent(s <- Dominance::ADI(data_sheet = i$data_sheet, items = i$items,
                                    actions = i$actions, bytes = i$bytes))

  expect_is(s, "list")
  expect_length(s, 3)

  expect_is(s$ADI, "matrix")
  expect_equal(dim(s$ADI), c(5,8))
  expect_equal(dimnames(s$ADI)[[1]], c("0620000500", "06200004F8", "06200003AA", "0620000477", "0620000400"))
  expect_equal(dimnames(s$ADI)[[2]], c("0620000500", "06200004F8", "06200003AA",
                                       "0620000477", "0620000400", "results.ADI", "id", "rank"))

  expect_is(s$Colors, "character")
  expect_equal(s$Colors, "")

  expect_is(s$ADI_count_matrix, "matrix")
  expect_equal(dim(s$ADI_count_matrix), c(5,5))
  expect_equal(dimnames(s$ADI_count_matrix)[[1]], sort(as.character(unique(d$displacements$animal_id))))
  expect_equal(dimnames(s$ADI_count_matrix)[[2]], sort(as.character(unique(d$displacements$animal_id))))

})

test_that("convert_dominance runs Dominance Sociogram functions as expected", {
  skip_on_travis()
  skip_on_appveyor()

  d <- disp(visits(finches_lg))
  i <- convert_dominance(d)

  # Sociogram
  expect_silent(s <- Dominance::Sociogram(data_sheet = i$data_sheet, items = i$items,
                                          actions = i$actions, bits = i$bytes))
  expect_is(s, "list")
  expect_named(s, c("sociogram", "counts_circles", "count_interactions", "line_size"))
})
