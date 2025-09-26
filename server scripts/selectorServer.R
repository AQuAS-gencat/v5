# SELECTOR SERVER #



output$center_selector <- renderUI({
  #req(input$geography_level)
  
  # Filter the data based on the selected geography level
  centers <- dades_menu_tbl %>%
    filter(Granularitat == "Centre (Unitat proveïdora)") %>%
    #filter(Granularitat == !!input$geography_level) %>%
    select(`Centre/Territori`, tipus_centre) %>%
    arrange(tipus_centre, `Centre/Territori`) %>%  # Sort by `tipus_centre` and `Centre/Territori`
    collect()  # Bring the data into memory after filtering
  
  
  print(head(centers))
  
  # Create the list of choices grouped by `tipus_centre`
  centre_choices <- centers %>%
    group_by(tipus_centre) %>%
    summarise(choices = list(`Centre/Territori`), .groups = "drop") %>% 
    {setNames(.$choices, .$tipus_centre)}
  
  # Render the selectizeInput UI element
  selectizeInput(
    "center", 
    #label = NULL,
    "Tria un centre:", 
    choices = centre_choices, 
    options = list(placeholder = "Tria un centre"),
    selected = NULL
  )
})



observe({
  req(#input$geography_level, 
      input$center)  # Ensure inputs are not NULL
  
  #selected_values$geography <- input$geography_level
  selected_values$center <- input$center
  
})
