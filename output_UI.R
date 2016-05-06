### Interactive UI
output$UI_time <- renderUI({
  req(v, input$speed, input$interval)
  sliderInput("time", "Time",
              min = floor_date(min(v$start), unit = "day"), 
              max = ceiling_date(max(v$start), unit = "day"),
              value = floor_date(min(v$start), unit = "day"),
              step = 60*60*input$interval,
              animate = animationOptions(interval = 1000 * (1 - input$speed/100), loop = TRUE),
              #animate = animationOptions(interval = 500, loop = TRUE),
              width = "100%")
})

output$UI_bird_id <- renderUI({
  req(m)
  selectInput("bird_id", "Select bird",
              choices = as.character(unique(m$bird_id)))
})