# Evolutiu server

# ###############################################.
# ## Server de la visió temporal ----
# ###############################################.

#dades2 = data.frame(ambit = "Selecciona un àmbit:")

#dades_evolutiu_tbl = bind_rows(dades, dades2)





################################################################################
# Pop-up d'ajuda ----
################################################################################

observeEvent(input$ajuda_evolutiu, {
  
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

#observeEvent(input$def_evolutiu, {
#  showModal(
#    modalDialog(
#      title = "Fitxa metodològica",
#      htmlOutput("def_text_evolutiu"),
#      easyClose = TRUE,
#      footer = modalButton("Tanca")
#    )
#  )
#})


output$explicacio_evolutiu <- renderUI({
  div(
    style = "font-size: 14px; padding: 10px; background-color: #FFE7CE; border-left: 5px solid #FFA500; margin-bottom: 15px;",
    "Aquesta pestanya permet seleccionar un indicador i veure'n l'evolució temporal per a tots els anys disponibles, 
     combinant múltiples centres per poder-ne comparar les tendències."
  )
})

################################################################################
# Pop-up definició  ----
################################################################################

#Subsetting by domain and profile. Profile is fiddly as vector uses abbreviations 
# so needs to be converted to the names to match techdoc.
def_data_evolutiu <- reactive({
  
  alfred %>%
    filter(name == selected_values$indicador) %>%
    filter(map_lgl(Àmbit, ~ selected_values$ambit %in% . #|
                   #selected_values$ambit %in% (Etiqueta %||% "")
    )
    )
})



output$def_text_evolutiu <- renderUI({
  def_data <- def_data_evolutiu() # Cache reactive
  
  # Helper function to safely extract and clean text from potentially nested lists
  safe_extract <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return("N/A")
    }
    
    # If it's a list, extract the first element or concatenate all elements
    if (is.list(value)) {
      value <- unlist(value, recursive = TRUE)
    }
    
    # If it's still a vector with multiple elements, paste them together
    if (length(value) > 1) {
      value <- paste(value, collapse = " ")
    }
    
    # Ensure it's a single character string
    value <- as.character(value)[1]
    
    # Return "N/A" if empty or NA
    if (is.na(value) || value == "" || value == "NULL") {
      return("N/A")
    }
    
    return(value)
  }
  
  # Helper function to safely convert to HTML and handle HTML entities
  safe_html <- function(value) {
    cleaned_value <- safe_extract(value)
    if (cleaned_value == "N/A") {
      return("N/A")
    }
    # Use HTML() to properly render HTML content
    return(HTML(cleaned_value))
  }
  
  # Handle missing values with defaults using the safe_html function
  indicador <- safe_extract(unique(def_data$name))
  definicio <- safe_html(unique(def_data$desc))
  num <- safe_html(unique(def_data$num_def))
  den <- safe_html(unique(def_data$den_def))
  formula <- safe_html(unique(def_data$formula))
  unitat <- safe_html(unique(def_data$units))
  interpretacio <- safe_html(unique(def_data$interpretation_criteria_text))
  font <- safe_html(unique(def_data$source))
  justificacio <- safe_html(unique(def_data$reason))
  exclusions <- safe_html(unique(def_data$exclusions))
  limitacions <- safe_html(unique(def_data$limitations))
  criteris_tecnics <- safe_html(unique(def_data$criteria))
  
  # Using bslib components for better formatting
  tagList(
    card(
      card_header(h5(indicador, class = "fw-bold")),
      card_body(
        div(definicio),  # Remove p() wrapper since content might already have <p> tags
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("NUMERADOR"),
            card_body(div(num))
          ),
          card(
            card_header("DENOMINADOR"),
            card_body(div(den))
          )
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("FÓRMULA"),
            card_body(div(formula))
          ),
          card(
            card_header("UNITAT"),
            card_body(div(unitat))
          )
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("INTERPRETACIÓ"),
            card_body(div(interpretacio))
          ),
          card(
            card_header("FONT"),
            card_body(div(font))
          )
        ),
        accordion(
          accordion_panel(
            "Justificació",
            div(justificacio)  # Remove p() wrapper
          ),
          accordion_panel(
            "Exclusions",
            div(exclusions)  # Remove p() wrapper
          ),
          accordion_panel(
            "Limitacions",
            div(limitacions)  # Remove p() wrapper
          ),
          accordion_panel(
            "Criteris tècnics",
            div(criteris_tecnics)  # Remove p() wrapper
          )
        )
      )
    )
  )
})


# Store selection for 'Centre/Territori'
stored_selections_evolutiu <- reactiveValues(
  `Centre/Territori` = NULL
)

stored_centers <- reactiveValues(
  centers = NULL
)


# Update stored selection whenever select_combined_evolutiu changes
#observeEvent(input$select_combined_evolutiu, {
#  stored_selections_evolutiu$`Centre/Territori` <- input$select_combined_evolutiu
#})

# Restore selection when input$select_indicador_evolutiu changes
#observeEvent(input$select_indicador_evolutiu, {
#  # Update select_combined_evolutiu with the stored value when relevant input changes
#  if (!is.null(stored_selections_evolutiu$`Centre/Territori`)) {
#    updateSelectizeInput(session, "select_combined_evolutiu", selected = stored_selections_evolutiu$`Centre/Territori`)
#  }
#})

####

#observeEvent(input$clear_evolutiu, {
#  updateSelectizeInput(session, "select_combined_evolutiu", selected = "NULL")
#  stored_selections_evolutiu$`Centre/Territori` <- NULL  # Clear stored value
#})

#observeEvent(input$def_evolutiu, {
#  
#  showModal(modalDialog(
#    footer = modalButton("Tanca")))
#}) 

################################################################################
# Altres reactivitats ----
################################################################################


# Observadors dels selectors: Pestanya Visió temporal
#observeEvent(input$select_ambit_evolutiu, {
#  updateSelectInput(session, "select_indicador_evolutiu", choices = NULL)  # Reset data selector
#})

# Check if standardized results exist for the selected indicator
has_standardized_results_evolutiu <- reactive({
  req(selected_values$indicador, selected_values$ambit)
  
  # Check if any non-NA oe values exist for this indicator
  oe_check <- dades_tbl %>%
    filter(
      nom_indicador == selected_values$indicador,
      ambit == selected_values$ambit |
        etiqueta == selected_values$ambit
    ) %>%
    summarise(has_oe = any(!is.na(oe))) %>%
    pull(has_oe)
  
  return(oe_check)
})



observe({
  req(input$select_ambit_evolutiu)  # Ensure a value is selected in the first selector
  
  # Filter indicators based on the selected tag (ambit, tag1, tag2, or tag3)
  filtered_data <- dades_tbl %>%
    filter(
      #grup_edat == "Total",
      `Centre/Territori` == selected_values$center,
      ambit == selected_values$ambit |
        #tag1 == selected_values$ambit |
        #tag2 == selected_values$ambit |
        etiqueta == selected_values$ambit
    ) %>%
    distinct(nom_indicador, dimensio) %>%
    collect()  # Bring filtered data to R
  
  # Group indicators by `dimensio` for the select input
  ind_choices <- lapply(unique(filtered_data$dimensio), function(dim) {
    # Extract indicators for the current dimensio
    indicators <- filtered_data$nom_indicador[filtered_data$dimensio == dim]
    # Return as a named list with dimensio as the group header
    setNames(as.list(indicators), indicators)
  })
  
  # Combine into a single named list
  names(ind_choices) <- unique(filtered_data$dimensio)
  
  # Check if the current selected_values$indicador is in the available choices
  all_indicators <- unlist(ind_choices)
  selected_indicator <- if (selected_values$indicador %in% all_indicators) {
    selected_values$indicador
  } else {
    # If not, use the first available indicator
    all_indicators[1]
  }
  
  # Update the second select input
  updateSelectInput(
    session,
    "select_indicador_evolutiu",
    choices = ind_choices,
    selected = selected_indicator
  )
})

# Add this observer to keep selected_values$indicador in sync
observe({
  req(input$select_indicador_evolutiu)
  selected_values$indicador <- input$select_indicador_evolutiu
})



#observe({
#  sub_options <- dades_evolutiu_tbl %>%
#    filter(indicador == input$select_indicador_evolutiu & (ambit == input$select_ambit_evolutiu |
#                                                             tag1 == input$select_ambit_evolutiu |
#                                                             tag2 == input$select_ambit_evolutiu |
#                                                             tag3 == input$select_ambit_evolutiu)) %>%
#    collect() %>%  # Ensure data is materialized before calling unique()
#    pull(subtipologia) %>% unique()

#  updateSelectInput(session, "select_sub_evolutiu", choices = sub_options)
#})

#output$select_sub_evolutiu_ui <- renderUI({
#  
#  # Query the DuckDB table using dplyr and filter based on input values
#  sub <- dades_evolutiu_tbl %>%
#    filter(
#      ambit == input$select_ambit_evolutiu |
#        tag1 == input$select_ambit_evolutiu |
#        tag2 == input$select_ambit_evolutiu |
#        tag3 == input$select_ambit_evolutiu, 
#      indicador == input$select_indicador_evolutiu
#    ) %>%
#    select(subtipologia) %>%
#    distinct() %>%
#    collect() %>%
#    pull(subtipologia)  # Extract the `subtipologia` column
#  
#  # Ensure that the subtipologia list is not empty or contains only "" values
#  if (length(sub) > 0 && !all(sub == "")) {
#    div(
#      br(),
#      div(align = "left", tags$b(class = "step-text", "Pas 2b: Tria una subcategoria:")),
#      br(),
#      div(align = "left",
#          selectizeInput("select_sub_evolutiu", label = NULL,
#                         choices = sub),
#          br()
#      )
#    )
#    
#  } else {
#    # Return NULL if there's no valid subtipologia to display
#    return(NULL)
#  }
#})






output$select_ambit_evolutiu_ui <- renderUI({
  #ambit <- sort(unique(dades_rank_tbl$ambit[dades_rank_tbl$`Centre/Territori` == selected_values$center]))
  
  ambit <- dades_tbl %>%
    filter(`Centre/Territori` == selected_values$center#, grup_edat == "Total"
           ) %>%
    pull(ambit) %>%
    as.character() %>%  # Convert factor to character to ensure correct display
    unique() %>%
    sort()
  
  etiqueta <- dades_tbl %>%
    filter(`Centre/Territori` == selected_values$center#, grup_edat == "Total"
           ) %>%
    select(#tag1, tag2, 
           etiqueta) %>%
    pivot_longer(cols = everything(), names_to = "etiqueta", values_to = "value") %>%
    filter(!is.na(value)) %>%
    distinct(value) %>%
    collect() %>%
    pull(value)
  
  # Combine into a named list for grouping
  grouped_choices_evolutiu <- list(
    "Àmbit" = as.list(ambit),
    "Etiqueta" = as.list(etiqueta)
  )
  
  div(#style = "margin-top: 20px; margin-bottom: 20px;",
      selectInput("select_ambit_evolutiu",
                  label = NULL,
                  #shiny::HTML("<p class='step-text'>Pas 1: Tria un àmbit o etiqueta</p>"), 
                  choices=grouped_choices_evolutiu, selected = selected_values$ambit)# selected = "Alcohol-related hospital admissions")),  
  )
  
  
})




#output$select_combined_evolutiu_ui <- renderUI({
#  # Prepare the data
#  combined_choices <- dades_evolutiu_tbl %>%
#    filter(Granularitat != "Catalunya") %>% 
#    distinct(indicador, dimensio, ambit, tag1, tag2, tag3, Granularitat, `Centre/Territori`) %>%
#    filter(
#      ambit == selected_values$ambit |
#        tag1 == selected_values$ambit |
#        tag2 == selected_values$ambit |
#        tag3 == selected_values$ambit,
#      indicador == selected_values$indicador
#    ) %>%
#    mutate(codi = indicador) %>%
#    collect() # Assuming there's a `dimensio` variable
#  
#  # Generate the choices, grouping by `Granularitat`
#  ind_choices_evolutiu <- combined_choices %>%
#    split(.$Granularitat) %>%
#    lapply(function(x) {
#      i <- sort(x$`Centre/Territori`)
#      names(i) <- i  # Set display names for the indicators
#      i
#    })
#  
#  # Render the selectizeInput UI
#  div(
#    selectizeInput(
#      inputId = "select_combined_evolutiu",
#      label = NULL, 
#      choices = c(
#        "Tria centres i territoris" = "", # Placeholder as the first choice
#        ind_choices_evolutiu             # Hierarchical list of options
#      ),
#      options = list(
#        placeholder = "Tria centres i territoris", # Set input bar placeholder
#        plugins = list('remove_button')           # Add remove button for selected items
#      ),
#      multiple = TRUE
#    )
#  )
#})




#available_sexe <- reactive({
#  dades_evolutiu_tbl %>%
#    filter(
#      ambit == selected_values$ambit |
#        tag1 == selected_values$ambit |
#        tag2 == selected_values$ambit |
#        tag3 == selected_values$ambit,
#      indicador == input$select_indicador_evolutiu
#    ) %>%
#    select(sexe) %>%
#    distinct() %>%
#    collect() %>%
#    pull()
#})


#observe({
#  # Get the available sexe categories
#  sexes <- available_sexe()
#  
#  print(sexes)
#  
#  # If there is only one category ("Total"), hide the selector
#  if (length(sexes) == 1 && "Total" %in% sexes) {
#    updateAwesomeRadio(session, "selector_sexe_evolutiu", selected = "Total")
#    hide("selector_sexe_evolutiu_ui")
#  } else {
#    show("selector_sexe_evolutiu_ui")
#  }
#})
#
output$selector_sexe_evolutiu_ui <- renderUI({
  # Only show if no standardized results exist OR toggle is FALSE
  if (!has_standardized_results_evolutiu() || !isTRUE(input$result_switch)) {
    
    # Get available sexe categories
    sexe_categories <- dades_tbl %>%
      filter(
        ambit == selected_values$ambit |
          etiqueta == selected_values$ambit,
        nom_indicador == selected_values$indicador
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
            awesomeRadio("selector_sexe_evolutiu", label = NULL, inline = FALSE, 
                         choices = sexe_categories, selected = "Total")
        )
      )
    } else {
      # Even if there's only one category, create a hidden input with the default value
      if (length(sexe_categories) == 1) {
        div(style = "display: none;",
            awesomeRadio("selector_sexe_evolutiu", label = NULL, inline = FALSE, 
                         choices = sexe_categories, selected = sexe_categories[1])
        )
      }
    }
  }
})


output$selector_edat_evolutiu_ui <- renderUI({
  # Only show if no standardized results exist OR toggle is FALSE
  if (!has_standardized_results_evolutiu() || !isTRUE(input$result_switch)) {
    
    # Get available edat categories
    edat_categories <- dades_tbl %>%
      filter(
        ambit == selected_values$ambit |
          etiqueta == selected_values$ambit,
        nom_indicador == selected_values$indicador
      ) %>%
      pull(grup_edat) %>%
      unique() %>%
      sort()
    
    # Show the selector if there is more than one category
    if (length(edat_categories) > 1) {
      div(
        div(align = "left", "Tria el grup d'edat:"),
        div(align = "left",
            title = "Mostra les dades agregades o bé diferenciades per grup d'edat",
            awesomeRadio("selector_edat_evolutiu", label = NULL, inline = TRUE, 
                         choices = edat_categories, selected = "Total")
        )
      )
    } else {
      # Even if there's only one category, create a hidden input with the default value
      if (length(edat_categories) == 1) {
        div(style = "display: none;",
            awesomeRadio("selector_edat_evolutiu", label = NULL, inline = TRUE, 
                         choices = edat_categories, selected = edat_categories[1])
        )
      }
    }
  }
})


year_range_reactive <- reactive({
  req(input$select_indicador_evolutiu)  # Ensure the input is available
  dades_tbl %>%
    filter(
      grup_edat == "Total",
      nom_indicador == input$select_indicador_evolutiu) %>%
    summarise(
      min_year = min(any, na.rm = TRUE),
      max_year = max(any, na.rm = TRUE)
    ) %>%
    collect()
})


observeEvent(year_range_reactive(), {
  year_range <- year_range_reactive()
  updateSliderInput(
    session,
    inputId = "year_range_evolutiu",
    min = year_range$min_year,
    max = year_range$max_year,
    value = c(year_range$min_year, year_range$max_year)
  )
})


output$result_switch_ui <- renderUI({
  # Only show toggle if standardized results exist
  if (has_standardized_results_evolutiu()) {
    div(
      div(align = "left", "Mostra els resultats estandarditzats:"),
      div(
        align = "left",
        prettySwitch(
          inputId = "result_switch",
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
})

###############################################.
## Reactive data ----
###############################################.





selected_data <- reactive({
  req(selected_values$geography, selected_values$center, selected_values$ambit, selected_values$indicador)
  
  # Base query
  selected <- dades_tbl %>% 
    filter(
      Granularitat == selected_values$geography,
      `Centre/Territori` == selected_values$center,
      any >= !!input$year_range_evolutiu[1],
      any <= !!input$year_range_evolutiu[2],
      ambit == selected_values$ambit |
        etiqueta == selected_values$ambit,
      nom_indicador == selected_values$indicador
    )
  
  # Determine which result to use based on standardized results availability
  use_standardized <- has_standardized_results_evolutiu() && isTRUE(input$result_switch)
  
  # Add sexe filter based on result_switch with safe defaults
  if (use_standardized) {
    selected <- selected %>% 
      filter(sexe == "Total", grup_edat == "Total") %>% 
      mutate(ic = paste0("[", round(ic_inf, 2), " - ", round(ic_sup, 2), "]"))
  } else {
    # Use safe defaults for selector values
    sexe_value <- if (!is.null(input$selector_sexe_evolutiu) && length(input$selector_sexe_evolutiu) > 0) {
      input$selector_sexe_evolutiu
    } else {
      "Total"
    }
    
    edat_value <- if (!is.null(input$selector_edat_evolutiu) && length(input$selector_edat_evolutiu) > 0) {
      input$selector_edat_evolutiu
    } else {
      "Total"
    }
    
    selected <- selected %>% 
      filter(sexe == sexe_value, grup_edat == edat_value)
  }
  
  selected %>% 
    as.data.frame() %>% 
    unique() %>% 
    collect()
})

#observe({
#  print(table(selected_data()$`Centre/Territori`))
#})


observe({
  req(input$additional_centers)
  stored_centers$centers <- input$additional_centers
})

observeEvent(input$select_ambit_evolutiu, {
  stored_centers$centers <- NULL
})

observeEvent(input$clear_evolutiu, {
  # Clear the stored centers
  stored_centers$centers <- NULL
  
  # Clear the selectize input
  updateSelectizeInput(
    session,
    "additional_centers",
    selected = character(0)  # Clear all selections
  )
})

# Add this observer to initialize the evolutiu checkbox with the global setting
observe({
  updateAwesomeCheckbox(session, "toggle_y_axis_zero_evolutiu", value = chart_settings$y_axis_zero)
})

# Add this observer to update the global setting when the evolutiu checkbox changes
observe({
  if (!is.null(input$toggle_y_axis_zero_evolutiu)) {
    chart_settings$y_axis_zero <- input$toggle_y_axis_zero_evolutiu
  }
})


other_centers_data <- reactive({
  req(selected_values$geography, selected_values$ambit, selected_values$indicador)
  
  dades_tbl %>% 
    filter(
      #grup_edat == "Total",
      Granularitat == selected_values$geography,
      #`Centre/Territori` == selected_values$center,
      ambit == selected_values$ambit |
        #tag1 == selected_values$ambit |
        #tag2 == selected_values$ambit |
        etiqueta == selected_values$ambit,
      nom_indicador == selected_values$indicador
    ) %>% 
    #arrange(`Centre/Territori`, any) %>% 
    distinct(`Centre/Territori`) %>% 
    collect()
  
})

output$additional_centers_ui <- renderUI({
  req(other_centers_data())
  
  other_centers <- other_centers_data()$`Centre/Territori`
  
  # Get the intersection of stored centers and available centers
  selected_centers <- if (!is.null(stored_centers$centers)) {
    intersect(stored_centers$centers, other_centers)
  } else {
    NULL
  }
  
  selectizeInput(
    "additional_centers",
    label = NULL,
    #"Afegeix altres centres",
    choices = other_centers,
    selected = selected_centers,  # Use the stored selection
    multiple = TRUE,
    options = list(
      placeholder = "Tria centres addicionals",
      plugins = list('remove_button')#,
      #openOnFocus = TRUE,     # Open dropdown when input is focused
      #closeAfterSelect = FALSE#, # Don't close after selection
      #dropdownParent = "body" # Attach dropdown to body to prevent closing
    )
  )
})

global_data <- reactive({
  req(selected_values$ambit, selected_values$indicador)
  
  # Determine which result to use based on standardized results availability
  use_standardized <- has_standardized_results_evolutiu() && isTRUE(input$result_switch)
  
  # Only show Catalunya data if we're not showing standardized results
  if (!use_standardized && isTRUE(input$selector_cat_evolutiu)) {
    data <- dades_tbl %>% 
      filter(
        Granularitat == "Catalunya",
        ambit == selected_values$ambit |
          etiqueta == selected_values$ambit,
        nom_indicador == selected_values$indicador,
        any >= !!input$year_range_evolutiu[1],
        any <= !!input$year_range_evolutiu[2]
      )
    
    # Apply sexe and edat filters when using raw results
    # Use safe defaults if inputs are not yet available
    sexe_value <- if (!is.null(input$selector_sexe_evolutiu) && length(input$selector_sexe_evolutiu) > 0) {
      input$selector_sexe_evolutiu
    } else {
      "Total"
    }
    
    edat_value <- if (!is.null(input$selector_edat_evolutiu) && length(input$selector_edat_evolutiu) > 0) {
      input$selector_edat_evolutiu
    } else {
      "Total"
    }
    
    data <- data %>% 
      filter(
        sexe == sexe_value,
        grup_edat == edat_value
      )
    
    data %>% collect()
  } else {
    NULL
  }
})



selected_result <- reactive({
  if (input$result_switch) {
    "oe"
  } else {
    "r"
  }
})


combined_data <- reactive({
  req(selected_data())
  
  data <- selected_data()
  
  # Determine which result to use based on standardized results availability
  use_standardized <- has_standardized_results_evolutiu() && isTRUE(input$result_switch)
  
  # Add sexe filter based on result_switch
  if (use_standardized) {
    data <- data %>% 
      filter(sexe == "Total", grup_edat == "Total")
  }
  
  if (!is.null(input$additional_centers) && length(input$additional_centers) > 0) {
    other_data <- dades_tbl %>% 
      filter(
        Granularitat == selected_values$geography,
        `Centre/Territori` %in% input$additional_centers,
        any >= !!input$year_range_evolutiu[1],
        any <= !!input$year_range_evolutiu[2],
        ambit == selected_values$ambit |
          etiqueta == selected_values$ambit,
        nom_indicador == selected_values$indicador
      )
    
    # Apply the same sexe filter to other_data
    if (use_standardized) {
      other_data <- other_data %>% 
        filter(sexe == "Total", grup_edat == "Total") %>% 
        mutate(ic = paste0("[", round(ic_inf, 2), " - ", round(ic_sup, 2), "]"))
    } else {
      # Use safe defaults for selector values
      sexe_value <- if (!is.null(input$selector_sexe_evolutiu) && length(input$selector_sexe_evolutiu) > 0) {
        input$selector_sexe_evolutiu
      } else {
        "Total"
      }
      
      edat_value <- if (!is.null(input$selector_edat_evolutiu) && length(input$selector_edat_evolutiu) > 0) {
        input$selector_edat_evolutiu
      } else {
        "Total"
      }
      
      other_data <- other_data %>% 
        filter(sexe == sexe_value, grup_edat == edat_value)
    }
    
    other_data <- other_data %>% 
      arrange(`Centre/Territori`, any) %>% 
      collect()
    
    data <- bind_rows(data, other_data)
  }
  
  # Only include Catalunya data if result_switch is FALSE
  if (!is.null(global_data()) && !isTRUE(input$result_switch)) {
    # Use safe defaults for filtering Catalunya data
    sexe_value <- if (!is.null(input$selector_sexe_evolutiu) && length(input$selector_sexe_evolutiu) > 0) {
      input$selector_sexe_evolutiu
    } else {
      "Total"
    }
    
    edat_value <- if (!is.null(input$selector_edat_evolutiu) && length(input$selector_edat_evolutiu) > 0) {
      input$selector_edat_evolutiu
    } else {
      "Total"
    }
    
    data <- bind_rows(data, global_data() %>% 
                        filter(sexe == sexe_value, grup_edat == edat_value))
  }
  
  # Round resultat2 to 2 decimal places
  data$oe <- round(data$oe, 2)
  
  # When using standardized results, remove any "Catalunya" entry that might have slipped in
  if (isTRUE(input$result_switch)) {
    data <- data %>% filter(`Centre/Territori` != "Catalunya")
  }
  
  print("DATA")
  print(data)
  
  data
})


output$selector_cat_evolutiu_ui <- renderUI({
  # Only show the Catalunya checkbox when not showing standardized results
  if (!has_standardized_results_evolutiu() || !isTRUE(input$result_switch)) {
    div(awesomeCheckbox("selector_cat_evolutiu", p("Mostra l'evolució global de Catalunya"), value = TRUE))
  }
})

################################################################################
# Creació del gràfic ----
################################################################################

# Títols

titol_exists <- reactive({
  !is.na(unique(combined_data()$nom_indicador))
})

output$titol_exists <- reactive({
  titol_exists()
})
#
#outputOptions(output, "titol_exists", suspendWhenHidden = FALSE)
#
##output$titol_evolutiu <- renderText(paste0(input$select_indicador_evolutiu))
#
output$titol_evolutiu <- renderUI({
  
  if (titol_exists()) {
    
    if (isTRUE(input$result_switch)) {
      HTML(paste0("<strong>", selected_values$indicador, " (Raó O/E)", "</strong>"))
      
    } else {
      HTML(paste0("<strong>", selected_values$indicador, " (", unique(combined_data()$unitats), ")", "</strong>"))
      
    }
    
    
  } else {
    NULL
  }
})

output$titol_evolutiu_table <- renderUI({
  
  if (titol_exists()) {
    
    if (isTRUE(input$result_switch)) {
      HTML(paste0("<strong>", selected_values$indicador, " (Raó O/E)", "</strong>"))
      
    } else {
      HTML(paste0("<strong>", selected_values$indicador, " (", unique(combined_data()$unitats), ")", "</strong>"))
      
    }
    
    
  } else {
    NULL
  }
})


#subtipologia_exists <- reactive({
#  !is.na(unique(evolutiu_data()$id_subtipologia))
#})

#output$subtipologia_exists <- reactive({
#  subtipologia_exists()
#})

#outputOptions(output, "subtipologia_exists", suspendWhenHidden = FALSE)


#output$subtitol_evolutiu <- renderText({
#  if (subtipologia_exists()) {
#    paste0(unique(evolutiu_data()$dg_extra), ": ", input$select_sub_evolutiu)
#  } else {
#    NULL
#  }
#})

output$subtitol_evolutiu2 <- renderText({
  # Don't show subtitle when result_switch is TRUE
  if (!is.null(input$result_switch) && length(input$result_switch) > 0 && input$result_switch == TRUE) {
    return(NULL)
  }
  
  # Get sexe and edat values with safe defaults
  sexe_value <- if (!is.null(input$selector_sexe_evolutiu) && 
                    length(input$selector_sexe_evolutiu) > 0 && 
                    input$selector_sexe_evolutiu != "") {
    input$selector_sexe_evolutiu
  } else {
    "Total"
  }
  
  edat_value <- if (!is.null(input$selector_edat_evolutiu) && 
                    length(input$selector_edat_evolutiu) > 0 && 
                    input$selector_edat_evolutiu != "") {
    input$selector_edat_evolutiu
  } else {
    "Total"
  }
  
  # Build subtitle based on what should be shown
  if (titol_exists()) {
    # Hide sexe when it's "Total"
    if (sexe_value == "Total") {
      # Also hide edat when it's "Total"
      if (edat_value == "Total") {
        NULL  # Hide entire subtitle when both are "Total"
      } else {
        paste0("Grup d'edat: ", edat_value)
      }
    } else {
      # Hide edat when it's "Total" but show sexe
      if (edat_value == "Total") {
        paste0("Sexe: ", sexe_value)
      } else {
        paste0("Sexe: ", sexe_value, ", Grup d'edat: ", edat_value)
      }
    }
  } else {
    NULL
  }
})

output$subtitol_evolutiu2_table <- renderText({
  # Don't show subtitle when result_switch is TRUE
  if (!is.null(input$result_switch) && length(input$result_switch) > 0 && input$result_switch == TRUE) {
    return(NULL)
  }
  
  # Get sexe and edat values with safe defaults
  sexe_value <- if (!is.null(input$selector_sexe_evolutiu) && 
                    length(input$selector_sexe_evolutiu) > 0 && 
                    input$selector_sexe_evolutiu != "") {
    input$selector_sexe_evolutiu
  } else {
    "Total"
  }
  
  edat_value <- if (!is.null(input$selector_edat_evolutiu) && 
                    length(input$selector_edat_evolutiu) > 0 && 
                    input$selector_edat_evolutiu != "") {
    input$selector_edat_evolutiu
  } else {
    "Total"
  }
  
  # Build subtitle based on what should be shown
  if (titol_exists()) {
    # Hide sexe when it's "Total"
    if (sexe_value == "Total") {
      # Also hide edat when it's "Total"
      if (edat_value == "Total") {
        NULL  # Hide entire subtitle when both are "Total"
      } else {
        paste0("Grup d'edat: ", edat_value)
      }
    } else {
      # Hide edat when it's "Total" but show sexe
      if (edat_value == "Total") {
        paste0("Sexe: ", sexe_value)
      } else {
        paste0("Sexe: ", sexe_value, ", Grup d'edat: ", edat_value)
      }
    }
  } else {
    NULL
  }
})


# Gràfic
plot_trend_chart <- function() {
  # If no data available for that period, then plot a message saying data is missing
  if (is.data.frame(combined_data()) && nrow(combined_data()) == 0) {
    plot_nodata()
  } else { 
    # If data is available, then plot it
    req(combined_data())
    
    # Make a local copy of the data to avoid reference issues
    plotting_data <- as.data.frame(combined_data())
    
    # Determine which column to use for the y-axis with safe default
    selected_y <- if (!is.null(input$result_switch) && length(input$result_switch) > 0 && isTRUE(input$result_switch)) "oe" else "r"
    
    # Safe check for selector_cat_evolutiu
    show_catalunya <- !is.null(input$selector_cat_evolutiu) && 
      length(input$selector_cat_evolutiu) > 0 && 
      isTRUE(input$selector_cat_evolutiu)
    
    # Safe check for result_switch
    use_standardized <- !is.null(input$result_switch) && 
      length(input$result_switch) > 0 && 
      isTRUE(input$result_switch)
    
    # Safe check for additional_centers
    additional_centers_count <- if (!is.null(input$additional_centers) && length(input$additional_centers) > 0) {
      length(input$additional_centers)
    } else {
      0
    }
    
    # Creating palette of colors: colorblind proof
    longitud_evolutiu <- ifelse(
      as.numeric(show_catalunya) + 1 + additional_centers_count > 12, 
      12,
      as.numeric(show_catalunya) + 1 + additional_centers_count
    )
    
    colors_evolutiu <- c("red", "#ccccff", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                         "#a6cee3", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928")
    center_order <- unique(plotting_data$`Centre/Territori`)
    
    # Create a named vector for color assignment
    trend_scale <- setNames(rep(NA, length(center_order)), center_order)
    
    # Ensure the selected center is always black
    if (!is.null(selected_values$center) && selected_values$center %in% center_order) {
      trend_scale[selected_values$center] <- "#ccccff"
    }
    
    # If Catalunya is selected and we're not using standardized results, ensure it's always red
    if (!use_standardized && show_catalunya && "Catalunya" %in% center_order) {
      trend_scale["Catalunya"] <- "red"
    }
    
    # Assign remaining colors from the palette to other centers
    remaining_centers <- setdiff(center_order, c(selected_values$center, "Catalunya"))
    remaining_colors <- colors_evolutiu[!colors_evolutiu %in% c("#ccccff", "red")]
    
    # Assign colors sequentially for remaining centers (if any)
    if (length(remaining_centers) > 0) {
      trend_scale[remaining_centers] <- remaining_colors[1:min(length(remaining_centers), length(remaining_colors))]
    }
    
    # Extract the color vector to pass to plotly
    trend_col <- trend_scale[center_order]
    
    # Modifying standard layout with safe checks
    if (exists("yaxis_plots")) {
      yaxis_plots[["title"]] <- paste0(unique(plotting_data$unitats))
      yaxis_plots[["tickfont"]] <- list(size = 14, family = "sans-serif")
      yaxis_plots[["rangemode"]] <- ifelse(
        !is.null(chart_settings$y_axis_zero) && chart_settings$y_axis_zero, 
        "tozero", 
        "normal"
      )
    } else {
      yaxis_plots <- list(
        title = paste0(unique(plotting_data$unitats)),
        tickfont = list(size = 14, family = "sans-serif"),
        rangemode = "tozero"
      )
    }
    
    if (exists("xaxis_plots")) {
      xaxis_plots[["title"]] <- "Any"
      xaxis_plots[["tickangle"]] <- ifelse(max(nchar(as.character(plotting_data$any))) > 7, -45, 0)
      xaxis_plots[["dtick"]] <- ifelse(length(unique(plotting_data$any)) >= 10, 2, 1)
      xaxis_plots[["tickfont"]] <- list(size = 15, family = "sans-serif")
    } else {
      xaxis_plots <- list(
        title = "Any",
        tickangle = ifelse(max(nchar(as.character(plotting_data$any))) > 7, -45, 0),
        dtick = ifelse(length(unique(plotting_data$any)) >= 10, 2, 1),
        tickfont = list(size = 15, family = "sans-serif")
      )
    }
    
    # Same approach for symbols
    paleta_simbols <- c('circle', 'diamond', 'circle', 'diamond', 'circle', 'diamond',
                        'square', 'triangle-up', 'square', 'triangle-up', 'square', 'triangle-up')
    
    symbols_scale <- setNames(rep(NA, length(center_order)), center_order)
    
    # Ensure the selected center is always diamond
    if (!is.null(selected_values$center) && selected_values$center %in% center_order) {
      symbols_scale[selected_values$center] <- "diamond"
    }
    
    # If Catalunya is selected and we're not using standardized results, ensure it's always circle
    if (!use_standardized && show_catalunya && "Catalunya" %in% center_order) {
      symbols_scale["Catalunya"] <- "circle"
    }
    
    # Assign remaining symbols from the palette to other centers
    remaining_symbols <- paleta_simbols[!paleta_simbols %in% c("diamond", "circle")]
    
    # Assign symbols sequentially for remaining centers (if any)
    if (length(remaining_centers) > 0) {
      symbols_scale[remaining_centers] <- remaining_symbols[1:min(length(remaining_centers), length(remaining_symbols))]
    }
    
    symbols_trend <- symbols_scale[center_order]
    
    # Create line style assignment (similar to colors and symbols)
    line_styles <- setNames(rep("solid", length(center_order)), center_order)
    
    # Make Catalunya line always dashed when it's included
    if (!use_standardized && show_catalunya && "Catalunya" %in% center_order) {
      line_styles["Catalunya"] <- "dash"
    }
    
    # Text for tooltip (dynamically referencing the selected column)
    plotting_data$tooltip <- if (use_standardized) {
      paste0(plotting_data$`Centre/Territori`, "<br>",
             plotting_data$any, "<br>", 
             "Raó O/E: ", 
             "<b>", round(plotting_data[[selected_y]], 2), "</b> ",
             if ("ic" %in% colnames(plotting_data)) plotting_data$ic else "")
    } else {
      paste0(plotting_data$`Centre/Territori`, "<br>",
             plotting_data$any, "<br>", 
             paste0(unique(plotting_data$unitats)), ": ", 
             "<b>", round(plotting_data[[selected_y]], 2), "</b>")
    }
    
    # Ensure the column we're plotting exists and has data
    if (!(selected_y %in% colnames(plotting_data))) {
      stop(paste("Column", selected_y, "not found in data"))
    }
    
    # Start creating the plot
    p <- plot_ly(height = 400)
    
    # Add traces for each center individually to control line styles
    for (center in center_order) {
      center_data <- plotting_data[plotting_data$`Centre/Territori` == center, ]
      
      p <- p %>% add_trace(
        data = center_data,
        x = ~any,
        y = center_data[[selected_y]],
        type = 'scatter',
        mode = 'lines+markers',
        name = center,
        line = list(
          color = trend_col[center],
          width = 3,
          dash = line_styles[center]
        ),
        marker = list(
          size = 6,
          symbol = symbols_trend[center],
          color = trend_col[center]
        ),
        text = center_data$tooltip,
        hoverinfo = "text"
      )
    }
    
    # Create the base plot layout with safe font_plots check
    font_plots_safe <- if (exists("font_plots")) font_plots else list(family = "Arial, sans-serif")
    
    plot_with_layout <- p %>% 
      layout(
        annotations = list(), #It needs this because of a buggy behaviour of Plotly
        separators = ",.",
        yaxis = yaxis_plots, 
        xaxis = xaxis_plots, 
        font = font_plots_safe,
        showlegend = TRUE,
        legend = list(
          orientation = "h", 
          y = 1.2, 
          x = 0.5, 
          xanchor = "center", 
          yanchor = "top"
        ),
        margin = list(t = 5, b = 5)
      )
    
    # Add the reference line depending on result_switch with safe check
    if (use_standardized) {
      # For standardized results, add a horizontal line at value 1
      year_min <- min(plotting_data$any)
      year_max <- max(plotting_data$any)
      
      plot_with_layout <- plot_with_layout %>%
        add_segments(
          x = year_min,
          xend = year_max,
          y = 1,
          yend = 1,
          line = list(color = 'red', width = 2, dash = "dash"),
          showlegend = FALSE,
          hoverinfo = 'text',
          text = "Catalunya: <b>1</b>"
        )
    }
    
    # Configure and return the plot
    plot_with_layout %>%
      config(
        displayModeBar = T, 
        displaylogo = F, # taking out plotly logo button
        modeBarButtonsToRemove = list(
          "sendDataToCloud", "zoomIn2d", "zoomOut2d", 
          "autoScale2d", "resetScale2d", "toggleSpikelines", 
          "resetViews", "toggleHover", "hoverClosestCartesian", 
          "hoverCompareCartesian", "zoom2d", "pan2d", "select2d", 
          "lasso2d", "toggleHover"
        ),
        toImageButtonOptions = list(
          filename = "grafic_evolutiu", 
          format = "png",
          label = "Descarrega el gràfic en format PNG",
          scale = 3
        ),
        locale = "ca"
      )
  }
}


output$evolution_plot <- renderPlotly({plot_trend_chart()})



output$evolution_table <- DT::renderDataTable({
  req(combined_data())
  
  data <- combined_data()
  
  # Determine which column to use with safe default
  result_col <- if (!is.null(input$result_switch) && length(input$result_switch) > 0 && isTRUE(input$result_switch)) "oe" else "r"
  
  # Ensure the column exists in the data
  if (!(result_col %in% colnames(data))) {
    return(NULL)
  }
  
  # Create the table data with proper column selection
  table_data <- data %>% 
    select(any, `Centre/Territori`, all_of(result_col)) %>% 
    pivot_wider(names_from = any, values_from = all_of(result_col)) %>% 
    rename(Territori = `Centre/Territori`) %>% 
    mutate(across(
      where(is.numeric), 
      ~ formatC(., format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
    ))
  
  DT::datatable(
    table_data,
    rownames = FALSE,
    options = list(
      paging = FALSE,
      searching = FALSE,
      scrollX = TRUE,  # Fixed typo: was "srollX"
      dom = 't'
    )
  )
})


################################################################################
# Descàrregues ----
################################################################################


# Downloading data (Excel)
# Downloading data (Excel) for chart
output$descarrega_dades_evolutiu <- downloadHandler(
  filename = function() {
    paste(selected_values$center, "-evolutiu-", paste(input$year_range_evolutiu, collapse = "-"), ".xlsx", sep = "")
  },
  content = function(file) {
    req(combined_data())
    
    # Determine which result column to use
    result_col <- if (!is.null(input$result_switch) && length(input$result_switch) > 0 && isTRUE(input$result_switch)) "oe" else "r"
    
    # Prepare the data for download
    data_to_download_evolutiu <- combined_data() %>% 
      select(
        nom_indicador,
        Granularitat, 
        `Centre/Territori`,
        any, 
        unitats,
        all_of(result_col)
      ) %>%
      rename(
        indicador = nom_indicador,
        granularitat = Granularitat,
        centre_territori = `Centre/Territori`,
        mesura = unitats,
        resultat = all_of(result_col)
      )
    
    # Write to Excel
    writexl::write_xlsx(data_to_download_evolutiu, path = file)
  }
)

# Downloading data (Excel) for table
output$descarrega_dades_evolutiu_taula <- downloadHandler(
  filename = function() {
    paste(selected_values$center, "-evolutiu-taula-", paste(input$year_range_evolutiu, collapse = "-"), ".xlsx", sep = "")
  },
  content = function(file) {
    req(combined_data())
    
    # Determine which result column to use
    result_col <- if (!is.null(input$result_switch) && length(input$result_switch) > 0 && isTRUE(input$result_switch)) "oe" else "r"
    
    # Prepare the data for download (same as chart data)
    data_to_download_evolutiu <- combined_data() %>% 
      select(
        nom_indicador,
        Granularitat, 
        `Centre/Territori`,
        any, 
        unitats,
        all_of(result_col)
      ) %>%
      rename(
        indicador = nom_indicador,
        granularitat = Granularitat,
        centre_territori = `Centre/Territori`,
        mesura = unitats,
        resultat = all_of(result_col)
      )
    
    # Write to Excel
    writexl::write_xlsx(data_to_download_evolutiu, path = file)
  }
)