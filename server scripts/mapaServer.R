# Mapa server



observeEvent(input$ajuda_mapa, {
  
  showModal(modalDialog(
    title = "Com funciona?",
    p("Aquesta pestanya permet seleccionar un indicador i veure'n l'evolució temporal per a tots els anys disponibles, 
    combinant múltiples centres i territoris de diferents unitats territorials. 
    Per tant, permet comparar les tendències d'aquest centres o territoris. 
    Aquesta pestanya és independent de la selecció de centre o territori realitzada a l'Inici."),
    p("Seleccionant un àmbit i un indicador al menú de l'esquerra, el gràfic mostra per 
      defecte i en vermell els resultats de Catalunya per tots els anys disponibles, resultats que es poden amagar deseleccionant la casella Mostra l'evolució global de Catalunya"),
    p("Segons l'indicador seleccionat, es mostren tants selectors com unitats territorials hi hagi amb dades disponibles, permetent selecionar múltiples opcions, 
      que s'afegeixen automàticament al gràfic. Si està disponible, també es mostra un selector de sexe on seleccionar el que es prefereixi.
      Els centres seleccionats es poden eliminar prement el botó X o bé, dins el selector, prement el botó Esborra del teclat."),
    p("Passant per sobre els punts del gràfic apareix informació sobre el centre o territori corresponent, l'any, i el resultat de l'indicador."),
    p("Es poden descarregar tant el gràfic de barres que es visualitza en format PNG, 
      clicant la icona de la càmera fotogràfica, com les dades que alimenten el gràfic en format Excel, clicant el botó Descarrega les dades."),
    
    size = "l", easyClose = TRUE, footer = modalButton("Tanca")))
}) 

output$result_switch_mapa_ui <- renderUI({
  
  if(r_selected_values$geography != "Catalunya") {
    # Only show toggle if standardized results exist
    if (has_standardized_results_mapa()) {
      div(
        div(align = "left", "Mostra els resultats estandarditzats:"),
        div(
          align = "left",
          title = "Mostra els resultats crus o estandarditzats (raó Observats/Esperats)",
          prettySwitch(
            inputId = "result_switch_mapa",
            label = NULL,
            fill = TRUE,
            status = "primary",
            value = FALSE  # Default to FALSE (raw results)
          )
        )
      )
    } else {
      NULL
    }
  } else {
    NULL
  }
})


# Create color palette reactively based on the data

# Create color palette reactively based on the data
color_palette <- reactive({
  
  if(r_selected_values$geography != "Catalunya") {
    
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
    
  } else {
    NULL
    
  }

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

# Check if standardized results exist for the selected indicator
has_standardized_results_mapa <- reactive({
  req(r_selected_values$indicador, r_selected_values$ambit)
  
  # Check if any non-NA oe values exist for this indicator
  oe_check <- dades_r_tbl %>%
    filter(
      nom_indicador == r_selected_values$indicador,
      ambit == r_selected_values$ambit |
        etiqueta == r_selected_values$ambit
    ) %>%
    summarise(has_oe = any(!is.na(oe))) %>%
    pull(has_oe)
  
  return(oe_check)
})

# Render the map
output$map <- renderLeaflet({
  
  if(r_selected_values$geography != "Catalunya") {
    
    # Create base map
    leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        #providers$Stadia.AlidadeSmooth,
        #providers$OpenStreetMap.Mapnik,
        #providers$OpenStreetMap.CAT,
        options = providerTileOptions(opacity = 1, minZoom = 7, maxZoom = 12)
      ) %>%
      setView(lng = 1.736, lat = 41.698, zoom = 8.25) %>% 
      addResetMapButton()
    
  } else {
    NULL
  }

})


#debugMap(map)

# Observer to update polygons when data changes

# In your polygon observer, use the same palette
# Update the legend observer in mapaServer.R
observe({
  
  if(r_selected_values$geography != "Catalunya") {
    
    req(map_data())
    pal <- color_palette()
    
    # Calculate the same breaks for the legend
    values <- map_data()$resultat[!is.na(map_data()$resultat)]
    mesura_value <- unique(map_data()$unitats[!is.na(map_data()$unitats)])
    breaks <- quantile(values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
    
    # Adjust the title based on which result we're showing
    legend_title <- if (isTRUE(input$result_switch_mapa)) {
      "Raó O/E"
    } else {
      paste0("Resultat (", mesura_value, ")")
    }
    
    leafletProxy("map", data = map_data()) %>%
      clearControls() %>% 
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~resultat,
        bins = 5,  # Explicitly set number of bins
        title = legend_title,
        opacity = 0.7,
        labFormat = labelFormat(digits = 1)
      ) %>% 
      # Add easyprint after the data is loaded
      leaflet.extras2::addEasyprint(options = easyprintOptions(
        title = "Descarrega el mapa en format PNG",
        position = "topleft",
        sizeModes = list("A4Portrait", "A4Landscape"),
        defaultSizeTitles = list(
          A4Landscape = 'Format A4 Horitzontal', 
          A4Portrait = 'Format A4 Vertical'
        ),
        filename = "Mapa",
        exportOnly = TRUE
      ))
    
  } else {
    
    NULL
  }
})


# Add this to mapaServer.R
# In mapaServer.R file, modify the observe block that adds polygons:

observe({
  
  if(r_selected_values$geography != "Catalunya") {
    
    req(map_data())
    
    # Create a color palette for the polygons
    pal <- color_palette()
    
    # Update the map with the filtered data
    leafletProxy("map", data = map_data()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(resultat),
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3, 
          color = "white", 
          fillOpacity = 0.8, 
          bringToFront = TRUE
        ),
        label = ~{
          # Conditionally include IC 95% based on result_switch_mapa
          if (isTRUE(input$result_switch_mapa)) {
            # When showing standardized results, include confidence interval
            lapply(sprintf(
              "<strong>%s</strong><br/>
               Raó O/E: %.2f<br/>
               IC 95%%: [%.2f - %.2f]<br/>
               Any: %s",
              nom,
              resultat,
              ic_inf,
              ic_sup,
              any
            ), HTML)
          } else {
            # When showing regular results, don't include confidence interval
            lapply(sprintf(
              "<strong>%s</strong><br/>
               Resultat: %.2f %s<br/>
               Any: %s",
              nom,
              resultat,
              unitats,
              any
            ), HTML)
          }
        }
      )
    
  }
})

# Add this observer to initialize the sexe selector when result_switch_mapa changes
observeEvent(input$result_switch_mapa, {
  # Only update UI elements if they exist and we're not showing standardized results
  if (!isTRUE(input$result_switch_mapa)) {
    # The UI will be automatically re-rendered via the renderUI function
    # We just need to make sure the input dependencies are properly tracked
  }
})


# Add these observers to trigger map updates
observeEvent(input$selector_sexe_mapa, {
  # The map_data reactive will automatically re-run when this input changes
  # We just need to ensure the observers that depend on it also re-run
})

# Add these observers to trigger map updates
observeEvent(input$selector_edat_mapa, {
  # The map_data reactive will automatically re-run when this input changes
  # We just need to ensure the observers that depend on it also re-run
})

observeEvent(input$result_switch_mapa, {
  # When switching between standardized and raw results, we need to ensure
  # the map and legend are updated
})

output$map_titol = renderUI({
  
  if(r_selected_values$geography != "Catalunya") {
    h4(paste0("Mapa segons ", r_selected_values$geography))
  } else {
    div(
      style = "display: flex; justify-content: center; align-items: center; height: 400px; text-align: center;",
      div(
        style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; max-width: 500px;",
        #icon("exclamation-circle", style = "color: #dc3545; font-size: 48px; margin-bottom: 15px;"),
        h3("Pestanya no disponible per aquesta selecció", style = "margin-bottom: 15px;"),
        p("Aquesta funcionalitat no està disponible quan s'ha seleccionat 'Catalunya' com a unitat territorial."),
        p("Si us plau, torneu a la pestanya d'Inici i seleccioneu una altra unitat territorial.")
      )
    )
  }
})

output$map_ambit <- renderUI({
  # Get unique `ambit` values for the selected center
  
  if(r_selected_values$geography != "Catalunya") {
    
    ambit <- dades_r_tbl %>%
      filter(
        visio == "CAT" | visio == "P",
        Granularitat == r_selected_values$geography,
        `Centre/Territori` == r_selected_values$center) %>%
      pull(ambit) %>%
      as.character() %>%  # Convert factor to character to ensure correct display
      unique() %>%
      sort()
    
    etiqueta <- dades_r_tbl %>%
      filter(
        visio == "CAT" | visio == "P",
        Granularitat == r_selected_values$geography,
        `Centre/Territori` == r_selected_values$center) %>%
      select(#tag1, tag2, 
             etiqueta) %>%
      pivot_longer(cols = everything(), names_to = "etiqueta", values_to = "value") %>%
      filter(!is.na(value)) %>%
      distinct(value) %>%
      collect() %>%
      pull(value)
    
    
    # Combine into a named list for grouping
    grouped_choices_map <- list(
      "Àmbit" = as.list(ambit),
      "Etiqueta" = as.list(etiqueta)
    )
    
    
    div(#style = "margin-top: 20px; margin-bottom: 20px;",
        selectInput("map_ambit", 
        #label = "Pas 1: Tria un àmbit o etiqueta",
        label = NULL,
        choices = grouped_choices_map, selected = r_selected_values$ambit)
    )
    
    
  } else {
    NULL
  }
})

#observe({
#  req(map_data())
#  print("MAP DATA")
#  print(map_data())
#  print("RESULTAT COLUMN")
#  print(map_data()$resultat)
#})


output$map_indicador <- renderUI({
  
  if(r_selected_values$geography != "Catalunya") {
    
    # Filter and create a distinct subset
    dades_r_rank_tbl2 <- dades_r_tbl %>%
      distinct(visio, nom_indicador, dimensio, ambit, #tag1, tag2, 
               etiqueta, Granularitat, `Centre/Territori`) %>%
      filter(
        visio == "CAT" | visio == "P",
        #grup_edat == "Total",
        Granularitat == r_selected_values$geography,
        `Centre/Territori` == r_selected_values$center,
        (ambit == r_selected_values$ambit |
           #tag1 == r_selected_values$ambit |
           #tag2 == r_selected_values$ambit |
           etiqueta == r_selected_values$ambit)#,
      ) %>%
      mutate(codi = nom_indicador) %>%
      collect()  # Collect the results
    
    print("DADES RANK TBL 2")
    print(dades_r_rank_tbl2)
    # Generate the choices, grouping by `dimensio`
    ind_choices_map <- dades_r_rank_tbl2 %>%
      split(.$dimensio) %>%
      lapply(function(x) {
        i <- sort(x$nom_indicador)
        names(i) <- i  # Set display names for the indicators
        i
      })
    
    # Render the select input for the indicator
    div(
      selectInput("map_indicador", 
                  #label = "Pas 2: Tria un indicador", 
                  label = NULL,
                  choices = ind_choices_map,
                  selected = r_selected_values$indicador)
    )
    
  } else {
    NULL
  }
  

})

output$map_any <- renderUI({
  
  if(r_selected_values$geography != "Catalunya") {
    
    req(r_selected_values$geography, 
        r_selected_values$ambit,
        r_selected_values$indicador)
    
    anys_map <- dades_r_tbl %>%
      filter(
        visio == "CAT" | visio == "P",
        Granularitat == r_selected_values$geography,
        `Centre/Territori` == r_selected_values$center,
        (ambit == r_selected_values$ambit |
           #tag1 == r_selected_values$ambit |
           #tag2 == r_selected_values$ambit |
           etiqueta == r_selected_values$ambit),
        nom_indicador == r_selected_values$indicador
      ) %>%
      pull(any) %>%
      #collect() %>%  # Collect data from DuckDB before processing
      unique() %>%
      sort(decreasing = TRUE)
    
    div(
      #style = "margin-top: 20px; margin-bottom: 20px;",
      selectInput("map_any", 
                  #label = "Pas 3: Tria un any", 
                  label = NULL,
                  choices = anys_map,
                  selected = anys_map[1])
    )
  } else {
    NULL
  }
  
})

# Add this to mapaServer.R
output$selector_sexe_mapa_ui <- renderUI({
  
  if(r_selected_values$geography != "Catalunya") {
    # Only show if no standardized results exist OR toggle is FALSE
    if (!has_standardized_results_mapa() || !isTRUE(input$result_switch_mapa)) {
      
      # Get available sexe categories
      sexe_categories <- dades_r_tbl %>%
        filter(
          visio == "CAT" | visio == "P",
          ambit == input$map_ambit |
            etiqueta == input$map_ambit,
          nom_indicador == input$map_indicador
        ) %>%
        pull(sexe) %>%
        unique() %>%
        sort()
      
      # Show the selector if there is more than one category
      if (length(sexe_categories) > 1) {
        div(
          div(align = "left", "Tria el sexe:"),
          div(align = "left",
              title = "Mostra les dades agregades o bé diferenciades per sexe.",
              awesomeRadio("selector_sexe_mapa", label = NULL, inline = FALSE, 
                           choices = sexe_categories, selected = "Total")
          )
        )
      } else {
        # Even if there's only one category, create a hidden input with the default value
        if (length(sexe_categories) == 1) {
          div(style = "display: none;",
              awesomeRadio("selector_sexe_mapa", label = NULL, inline = FALSE, 
                           choices = sexe_categories, selected = sexe_categories[1])
          )
        }
      }
    }
  } else {
    NULL
  }
})


output$selector_edat_mapa_ui <- renderUI({
  
  if(r_selected_values$geography != "Catalunya") {
    # Only show if no standardized results exist OR toggle is FALSE
    if (!has_standardized_results_mapa() || !isTRUE(input$result_switch_mapa)) {
      
      # Get available edat categories
      edat_categories <- dades_r_tbl %>%
        filter(
          visio == "CAT" | visio == "P",
          ambit == input$map_ambit |
            etiqueta == input$map_ambit,
          nom_indicador == input$map_indicador
        ) %>%
        pull(grup_edat) %>%
        unique() %>%
        sort()
      
      # Show the selector if there is more than one category
      if (length(edat_categories) > 1) {
        div(
          div(align = "left", "Tria el grup d'edat:"),
          div(align = "left",
              title = "Mostra les dades agregades o bé diferenciades segons el grup d'edat.",
              awesomeRadio("selector_edat_mapa", label = NULL, inline = TRUE, 
                           choices = edat_categories, selected = "Total")
          )
        )
      } else {
        # Even if there's only one category, create a hidden input with the default value
        if (length(edat_categories) == 1) {
          div(style = "display: none;",
              awesomeRadio("selector_edat_mapa", label = NULL, inline = TRUE, 
                           choices = edat_categories, selected = edat_categories[1])
          )
        }
      }
    }
  } else {
    NULL
  }
})

# Create a reactive dataset that filters based on selected values
# Update the map_data reactive in mapaServer.R
map_data <- reactive({
  if (r_selected_values$geography != "Catalunya") {
    
    req(input$map_ambit, input$map_indicador, input$map_any, r_selected_values$geography)
    gv <- r_selected_values$geography
    center_sel <- r_selected_values$center
    ambit_sel <- input$map_ambit
    indicador_sel <- input$map_indicador
    any_sel <- input$map_any
    
    indicator_data <- dades_r_tbl %>%
      filter(
        visio == "CAT" | visio == "P",
        Granularitat == gv,
        (ambit == ambit_sel | etiqueta == ambit_sel),
        nom_indicador == indicador_sel,
        any == any_sel
      ) %>%
      mutate(Codi = as.double(Codi)) %>%
      collect()
    
    # Determine which result to use based on standardized results availability
    use_standardized <- has_standardized_results_mapa() && isTRUE(input$result_switch_mapa)
    
    if (use_standardized) {
      indicator_data <- indicator_data %>% 
        filter(sexe == "Total", grup_edat == "Total")
    } else {
      if (!is.null(input$selector_sexe_mapa)) {
        indicator_data <- indicator_data %>% filter(sexe == input$selector_sexe_mapa)
      } else {
        indicator_data <- indicator_data %>% filter(sexe == "Total")
      }
      if (!is.null(input$selector_edat_mapa)) {
        indicator_data <- indicator_data %>% filter(grup_edat == input$selector_edat_mapa)
      } else {
        indicator_data <- indicator_data %>% filter(grup_edat == "Total")
      }
    }
    
    print("INDICATOR DATA")
    print(indicator_data)
    
    result_column <- if (use_standardized) "oe" else "r"
    
    if (gv == "Àrea Bàsica de Salut") {
      map_joined <- map_abs_layer %>%
        left_join(indicator_data, by = c("codi_geo" = "Codi")) %>%
        mutate(resultat = as.numeric(.data[[result_column]]))
    } else if (gv == "Àrea de Gestió Assistencial") {
      map_joined <- map_aga_layer %>%
        left_join(indicator_data, by = c("codi_geo" = "Codi")) %>%
        mutate(resultat = as.numeric(.data[[result_column]]), nom = aga)
    } else if (gv == "Regió Sanitària") {
      if ("Barcelona" %in% indicator_data$`Centre/Territori`) {
        map_joined <- map_rs_layer %>%
          left_join(indicator_data, by = c("codi_geo" = "Codi")) %>%
          mutate(resultat = as.numeric(.data[[result_column]]), nom = rs)
      } 
      
      else if ("Penedès" %in% indicator_data$`Centre/Territori`){
        map_joined <- map_rs2_layer %>%
          left_join(indicator_data, by = c("codi_geo" = "Codi")) %>%
          mutate(resultat = as.numeric(.data[[result_column]]), nom = rs3)
      }
      
      else {
        map_joined <- map_rs2_layer %>%
          left_join(indicator_data, by = c("codi_geo" = "Codi")) %>%
          mutate(resultat = as.numeric(.data[[result_column]]), nom = rs2)
      }
      
      #else {
      #  print("ERROR")

      #}
    }
    
    print("MAP JOINED")
    print(map_joined)
    return(map_joined)
  } else {
    NULL
  }
})




# Update the mapLabelHTML function in mapaServer.R
mapLabelHTML = function(name, value, unitats, any) {
  # Adjust the label if we're showing standardized results
  if (isTRUE(input$result_switch_mapa)) {
    lapply(
      paste0("<strong>", name, "</strong><p>Raó O/E: ", value, 
             "<br>Any: ", any, "</p>"),
      htmltools::HTML
    )
  } else {
    lapply(
      paste0("<strong>", name, "</strong><p>Resultat: ", value, " ", unitats, 
             "<br>Any: ", any, "</p>"),
      htmltools::HTML
    )
  }
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
