# Mapa server

# Create color palette reactively based on the data

# Create color palette reactively based on the data
color_palette <- reactive({
  req(map_data())
  
  # Calculate quintile breaks
  values <- map_data()$resultat[!is.na(map_data()$resultat)]
  breaks <- quantile(values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
  
  # Create a binned color palette
  colorBin(
    palette = "YlGnBu",
    domain = map_data()$resultat,
    bins = breaks,
    na.color = "gray"
  )
})

#observe({
#  req(color_palette())
#  print("COLOR PALETTE")
#  print(color_palette())  # Check if data exists
#})

# Create labels reactively
#mapLabelHTML <- reactive({
#  req(map_data())
#  sprintf(
#    "<strong>%s</strong><br/>
#     Resultat: %s %s<br/>
#     Any: %s",
#    map_data()$nom,
#    round(map_data()$resultat, 2),
#    map_data()$mesura,
#    map_data()$any
#  ) %>% lapply(HTML)
#})

# Render the map
# Render the map
output$map <- renderLeaflet({
  # Create base map
  leaflet() %>%
    addProviderTiles(
      providers$CartoDB.Positron,
      options = providerTileOptions(opacity = 1, minZoom = 7, maxZoom = 12)
    ) %>%
    setView(lng = 1.736, lat = 41.698, zoom = 8.25) %>% 
    addResetMapButton() #%>% 
  #addEasyprint(options = easyprintOptions(
  #  title = "Print map",
  #  position = "bottomleft",
  #  exportOnly = TRUE
  #))
})


#debugMap(map)

# Observer to update polygons when data changes

# In your polygon observer, use the same palette
observe({
  req(map_data())
  pal <- color_palette()
  
  leafletProxy("map") %>%
    clearGroup("polygons") %>%     # Clear existing polygons
    clearControls() %>%            # Clear existing controls
    #clearShapes() %>%              # Clear all shapes
    #clearMarkers() %>%             # Clear any markers
    removeControl("legend") %>%     # Remove legend explicitly
    addPolygons(
      data = map_data(),
      group = "polygons",          # Assign to group for easier clearing
      fillColor = ~pal(resultat),
      weight = 1,
      opacity = 1,
      color = "grey",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      #label = ~mapLabelHTML(),
      label = ~ mapLabelHTML(nom, resultat, mesura, any)
      #labelOptions = labelOptions(
      #  style = list("font-weight" = "normal", padding = "3px 8px"),
      #  textsize = "15px",
      #  direction = "auto"
      #)
    ) %>% 
    clearSearchFeatures() %>% 
    addSearchFeatures(targetGroups = "polygons",  
                      options = searchFeaturesOptions(position = "topright", 
                                                      #textPlaceholder = "Ves a una àrea",
                                                      textPlaceholder = ifelse(r_selected_values$geography == "Àrea Bàsica de Salut", "Ves a una ABS",
                                                                               ifelse(r_selected_values$geography == "Àrea de Gestió Assistencial", "Ves a una AGA",
                                                                                      "Ves a una Regió Sanitària")),
                                                      #names(map_layer_choices[match(input$flt_map_layer, map_layer_choices)])
                                                      zoom = 11, firstTipSubmit = TRUE, 
                                                      autoCollapse = TRUE)) 
})



# In your legend observer, use the same breaks
observe({
  req(map_data())
  pal <- color_palette()
  
  # Calculate the same breaks for the legend
  values <- map_data()$resultat[!is.na(map_data()$resultat)]
  mesura_value <- unique(map_data()$mesura[!is.na(map_data()$mesura)])
  breaks <- quantile(values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
  
  leafletProxy("map", data = map_data()) %>%
    clearControls() %>% 
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~resultat,
      bins = 5,  # Explicitly set number of bins
      title = paste0("Resultat (", mesura_value, ")"),
      opacity = 0.7,
      labFormat = labelFormat(digits = 1)
    )
})


output$map_titol = renderUI({
  
  HTML(paste0("<strong> Mapa segons ", r_selected_values$geography, "</strong>"))
  
})

output$map_ambit <- renderUI({
  # Get unique `ambit` values for the selected center
  
  ambit <- dades_r_resum_tbl %>%
    filter(Granularitat == r_selected_values$geography) %>%
    filter(`Centre/Territori` == r_selected_values$center) %>%
    pull(ambit) %>%
    as.character() %>%  # Convert factor to character to ensure correct display
    unique() %>%
    sort()
  
  etiqueta <- dades_r_resum_tbl %>%
    filter(Granularitat == r_selected_values$geography) %>%
    filter(`Centre/Territori` == r_selected_values$center) %>%
    select(tag1, tag2, tag3) %>%
    pivot_longer(cols = everything(), names_to = "tag", values_to = "value") %>%
    filter(!is.na(value)) %>%
    distinct(value) %>%
    collect() %>%
    pull(value)
  
  
  # Combine into a named list for grouping
  grouped_choices_map <- list(
    "Àmbit" = as.list(ambit),
    "Etiqueta" = as.list(etiqueta)
  )
  
  
  div(style = "margin-top: 20px; margin-bottom: 20px;",
      selectInput("map_ambit", shiny::HTML("<p class='step-text'>Pas 1: Tria un àmbit o etiqueta</p>"), 
                  choices = grouped_choices_map, selected = r_selected_values$ambit)
  )
})

observe({
  req(map_data())
  print("MAP DATA")
  print(map_data())
  print("RESULTAT COLUMN")
  print(map_data()$resultat)
})


output$map_indicador <- renderUI({
  # Filter and create a distinct subset
  dades_r_resum_tbl2 <- dades_r_resum_tbl %>%
    distinct(indicador, dimensio, ambit, tag1, tag2, tag3, Granularitat, `Centre/Territori`) %>%
    filter(
      Granularitat == r_selected_values$geography,
      `Centre/Territori` == r_selected_values$center,
      (ambit == r_selected_values$ambit |
         tag1 == r_selected_values$ambit |
         tag2 == r_selected_values$ambit |
         tag3 == r_selected_values$ambit)#,
    ) %>%
    mutate(codi = indicador) %>%
    collect()  # Collect the results
  
  # Generate the choices, grouping by `dimensio`
  ind_choices_map <- dades_r_resum_tbl2 %>%
    split(.$dimensio) %>%
    lapply(function(x) {
      i <- sort(x$indicador)
      names(i) <- i  # Set display names for the indicators
      i
    })
  
  # Render the select input for the indicator
  div(
    selectInput("map_indicador", 
                shiny::HTML("<p class='step-text'>Pas 2: Tria un indicador</p>"), 
                choices = ind_choices_map,
                selected = r_selected_values$indicador)
  )
})

output$map_any <- renderUI({
  req(r_selected_values$geography, 
      r_selected_values$ambit,
      r_selected_values$indicador)
  
  anys_map <- dades_r_resum_tbl %>%
    filter(
      Granularitat == r_selected_values$geography,
      `Centre/Territori` == r_selected_values$center,
      (ambit == r_selected_values$ambit |
         tag1 == r_selected_values$ambit |
         tag2 == r_selected_values$ambit |
         tag3 == r_selected_values$ambit),
      indicador == r_selected_values$indicador
    ) %>%
    pull(any) %>%
    #collect() %>%  # Collect data from DuckDB before processing
    unique() %>%
    sort(decreasing = TRUE)
  
  div(
    style = "margin-top: 20px; margin-bottom: 20px;",
    selectInput("map_any", 
                label = "Pas 3: Tria un any", 
                choices = anys_map,
                selected = anys_map[1])
  )
  
})


# Create a reactive dataset that filters based on selected values
map_data <- reactive({
  req(input$map_ambit, input$map_indicador, input$map_any, r_selected_values$geography)
  
  # Get the data for the selected indicator and ambit
  indicator_data <- dades_r_resum_tbl %>%
    filter(
      Granularitat == r_selected_values$geography,
      (ambit == input$map_ambit |
         tag1 == input$map_ambit |
         tag2 == input$map_ambit |
         tag3 == input$map_ambit),
      indicador == input$map_indicador,
      any == input$map_any
    ) %>%
    collect()
  
  print("INDICATOR DATA")
  print(indicator_data)
  
  
  if (r_selected_values$geography == "Àrea Bàsica de Salut") { 
    map_joined <- map_abs_layer %>%
      left_join(indicator_data, by = c("codi_geo" = "Codi")) %>%
      mutate(
        resultat = as.numeric(resultat)#,  # Ensure resultat is numeric
        #nom = `Centre/Territori`          # Use the proper name field
        #nom = abs          # Use the proper name field
      )
  } else if (r_selected_values$geography == "Àrea de Gestió Assistencial") {
    map_joined <- map_aga_layer %>%
      left_join(indicator_data, by = c("codi_geo" = "Codi")) %>%
      mutate(
        resultat = as.numeric(resultat),  # Ensure resultat is numeric
        #nom = `Centre/Territori`          # Use the proper name field
        nom = aga          # Use the proper name field
      )
  } else if (r_selected_values$geography == "Regió Sanitària") {
    
    if ("Barcelona" %in% indicator_data$`Centre/Territori`) {
      
      map_joined <- map_rs_layer %>%
        left_join(indicator_data, by = c("codi_geo" = "Codi")) %>%
        mutate(
          resultat = as.numeric(resultat),  # Ensure resultat is numeric
          #nom = `Centre/Territori`          # Use the proper name field
          nom = rs          # Use the proper name field
        )
      
    } else {
      
      map_joined <- map_rs2_layer %>%
        left_join(indicator_data, by = c("codi_geo" = "Codi")) %>%
        mutate(
          resultat = as.numeric(resultat),  # Ensure resultat is numeric
          #nom = `Centre/Territori`          # Use the proper name field
          nom = rs          # Use the proper name field
        )
      
    }
    
    
    
    
  }
  
  print("MAP JOINED")
  print(map_joined)
  
  
  return(map_joined)
})





mapLabelHTML = function(name, value, mesura, any) {
  lapply(
    paste0("<strong>", name, "</strong><p>Resultat: ", value, " ", mesura, "<br>Any: ", any, "</p>"),
    htmltools::HTML
  )
}

#mapLabelHTML <- function(name, value, mesura, any) {
#  sprintf(
#    "<strong>%s</strong><br/>
#     Resultat: %s %s<br/>
#     Any: %s",
#    name,
#    round(value, 2),
#    mesura,
#    any
#  ) %>% lapply(HTML)
#}
#
#output$map <- renderLeaflet({
#  leaflet(map_abs_layer) %>%  # Use your loaded GeoJSON data
#    addProviderTiles(providers$CartoDB.Positron, 
#                     options = providerTileOptions(opacity = 1, minZoom = 7, maxZoom = 12), 
#                     group = "Open Street Map") %>%
#    addPolygons(
#      fillColor = "blue",  # Change as needed (or use a dynamic color scale)
#      weight = 1,
#      opacity = 1,
#      color = "black",
#      fillOpacity = 0.5,
#      highlightOptions = highlightOptions(
#        weight = 3, color = "red", fillOpacity = 0.7, bringToFront = TRUE
#      ),
#      label = ~mapLabelHTML(nom, codi_geo)
#    ) %>%
#    setView(lng = 1.736, lat = 41.698, zoom = 8)
#})


#output$map <- renderMaplibre({
#  maplibre(style = carto_style("positron"),
#           center = c(1.736, 41.698),
#           zoom = 8) %>% 
#    fit_bounds(map_abs_layer, animate = FALSE) %>% 
#    add_fill_layer(id = "abs",
#                   source = map_abs_layer,
#                   fill_color = interpolate(
#                     column = "codi_geo",
#                     values = c(min(map_abs_layer$codi_geo), max(map_abs_layer$codi_geo)),
#                     stops = c("lightblue", "darkblue"),
#                     na_color = "lightgrey"
#                   ),
#                   fill_opacity = 0.5,
#                   tooltip = "tooltip_html") %>% 
#    add_legend("Llegenda",
#               values = c(min(map_abs_layer$codi_geo), max(map_abs_layer$codi_geo)),
#               colors = c("lightblue", "darkblue"),
#               type = "continuous",
#               position = "top-left")
#  
#})
