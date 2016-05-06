## Data and Text Outputs

output$data <- DT::renderDataTable(
  DT::datatable(data, options = list(pageLength = 100))
)

output$birds <- DT::renderDataTable(
  DT::datatable(birds, options = list(pageLength = 10), rownames = FALSE, selection = "single"), server = FALSE
)

output$v <- DT::renderDataTable(
  DT::datatable(v, rownames = FALSE)
)

output$points <- DT::renderDataTable(
  DT::datatable(v_points(), rownames = FALSE)
)

output$paths <- DT::renderDataTable(
  DT::datatable(m_paths(), rownames = FALSE)
)

#output$selection <- renderText({input$birds})
