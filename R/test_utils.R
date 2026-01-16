expect_snapshot_leaflet <- function(map) {
  saveRDS(map, "test.rds")
  expect_snapshot_file("test.rds")
}
