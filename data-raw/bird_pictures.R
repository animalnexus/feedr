
list.files("./data-raw/Bird Pictures/small/")

index <- birds_all %>%
  mutate(img = NA)

write.csv(index, "./inst/shiny-examples/birdmoves/data/img_index.csv", row.names = FALSE)
