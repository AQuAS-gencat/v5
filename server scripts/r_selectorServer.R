# SELECTOR SERVER #



# Center selector depends on the geography level
output$r_center_selector <- renderUI({
  req(input$r_geography_level)
  
  # Filter the data based on the selected geography level
  r_centers <- dades_menu_tbl %>%
    filter(Granularitat == !!input$r_geography_level) %>%
    select(`Centre/Territori`, tipus_centre) %>%
    arrange(tipus_centre, `Centre/Territori`) %>%  # Sort by `tipus_centre` and `Centre/Territori`
    collect()  # Bring the data into memory after filtering
  
  # Create the list of choices grouped by `tipus_centre`
  r_centre_choices <- r_centers %>%
    group_by(tipus_centre) %>%
    summarise(choices = list(`Centre/Territori`), .groups = "drop") %>% 
    {setNames(.$choices, .$tipus_centre)}
  
  # Render the selectizeInput UI element
  selectizeInput(
    "r_center", 
    "Tria un territori:", 
    choices = r_centre_choices, 
    #options = list(placeholder = "Tria un centre"),
    selected = NULL
  )
})



observe({
  req(input$r_geography_level, input$r_center)  # Ensure inputs are not NULL
  
  r_selected_values$geography <- input$r_geography_level
  r_selected_values$center <- input$r_center
  
})