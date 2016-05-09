output$data_current_birdid <- renderText({
  paste0("<strong>Bird Ids:</strong> ", 
         paste0(as.character(unique(values$current$bird_id)), collapse = ", "))
})

output$data_current_species <- renderText({
  paste0("<strong>Species:</strong> ", 
         paste0(as.character(unique(values$current$species)), collapse = ", "))
})

output$data_current_sitename <- renderText({
  paste0("<strong>Site:</strong> ", 
         paste0(as.character(unique(values$current$sitename)), collapse = ", "))
})

