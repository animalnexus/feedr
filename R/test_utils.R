expect_snapshot_leaflet <- function(map) {
  saveRDS(map, "test.rds")
  testthat::expect_snapshot_file("test.rds")
}


system_os <- function() {
  tolower(Sys.info()[["sysname"]])
}
