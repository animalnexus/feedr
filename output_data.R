## Data and Text Outputs

output$dt_data_current <- DT::renderDataTable(
  DT::datatable(values$current)
)

output$dt_data <- DT::renderDataTable(
  DT::datatable(data(), options = list(pageLength = 100))
)

output$dt_birds <- DT::renderDataTable(
  DT::datatable(birds_sub(), options = list(pageLength = 100), rownames = FALSE, selection = "single"), server = FALSE
)

output$dt_v <- DT::renderDataTable(
  DT::datatable(v(), rownames = FALSE)
)

output$dt_points <- DT::renderDataTable(
  DT::datatable(v_points(), rownames = FALSE)
)

output$dt_paths <- DT::renderDataTable(
  DT::datatable(m_paths(), rownames = FALSE)
)

#output$selection <- renderText({input$birds})
