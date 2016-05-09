### Interactive UI

# Data Selection

output$UI_data_sitename <- renderUI({
  req(birds_all)
  radioButtons("data_sitename", "Site:",
             choices = as.character(unique(birds_all$site_name)))
            
})

output$UI_data_species <- renderUI({
  req(birds_all, input$data_sitename)
  checkboxGroupInput("data_species", "Species to include (select all that apply):",
                   choices = as.character(unique(birds_all$species[birds_all$site_name == input$data_sitename])),
                   selected = as.character(unique(birds_all$species[birds_all$site_name == input$data_sitename])))
})

output$UI_data_birdid <- renderUI({
  req(birds_all, input$data_sitename, input$data_species)
  selectInput("data_birdid", "Select bird(s)",
              choices = c("All", as.character(unique(birds_all$bird_id[birds_all$species %in% input$data_species & birds_all$site_name == input$data_sitename]))),
              selected = "All", multiple = TRUE)
})

# Animations
output$UI_time <- renderUI({
  req(input$speed, input$interval)
  sliderInput("time", "Time",
              min = floor_date(min(v()$start), unit = "day"), 
              max = ceiling_date(max(v()$start), unit = "day"),
              value = floor_date(min(v()$start), unit = "day"),
              step = 60*60*input$interval,
              animate = animationOptions(interval = 1000 * (1 - input$speed/100), loop = TRUE),
              #animate = animationOptions(interval = 500, loop = TRUE),
              width = "100%")
})

# Static
output$UI_static_birdid <- renderUI({
  selectInput("static_birdid", "Select bird",
              choices = as.character(unique(m()$bird_id)))
})