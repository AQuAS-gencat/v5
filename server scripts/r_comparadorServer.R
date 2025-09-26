# ###############################################.
# ## Server del comparador ----
# ###############################################.


################################################################################
# Pop-up d'ajuda ----
################################################################################

observeEvent(input$r_ajuda_comparador, {
  showModal(modalDialog(
    title = "Com funciona?",
    p("Per cada indicador, el gràfic ordena els resultats dels centres o territoris que pertanyen a la unitat geogràfica seleccionada a la pestanya d'Inici, 
    i els mostra en relació al centre o territori seleccionat, en color lavanda, segons l'any i, en cas que estigui disponible, subcategoria i sexe seleccionats. 
    La línia vermella mostra el valor de Catalunya."), 
    p("En indicadors amb interpretació, el color de les barres mostra quins centres o territoris assoleixen resultats millors (blau) o pitjors (taronja) que el seleccionat.
    En indicadors sense interpretació, les barres són de color verd oliva."),
    p("Quan cal mostrar més de 80 centres, el gràfic gira 90º per facilitar la identificació de la posició relativa."),
    p("Per facilitar la comparabilitat adequada, en indicadors hospitalaris només mostra aquells centres que pertanyen al mateix nivell hospitalari que el seleccionat."),
    p("Passant el ratolí per sobre de cada barra, s'obté informació sobre el centre o territori pertinent i el seu resultat en l'indicador."),
    p("Clicant el botó Fitxa de l'indicador, s'obre un requadre amb la fitxa metodològica de l'indicador."),
    p("Es poden descarregar tant el gràfic de barres que es visualitza en format PNG, 
      clicant la icona de la càmera fotogràfica, com les dades que alimenten el gràfic en format Excel, clicant el botó Descarrega les dades."),
    size = "l", easyClose = TRUE, footer = modalButton("Tanca")))
}) 

#observeEvent(input$def_ranquing, {
#  showModal(
#    modalDialog(
#      title = "Fitxa metodològica",
#      htmlOutput("def_text_ranquing"),
#      easyClose = TRUE,
#      footer = modalButton("Tanca")
#    )
#  )
#})

################################################################################
# Pop-up definició  ----
################################################################################

#docu_r_comparador = docu %>% 
#  select(-id_resum) %>% 
#  unique()

#Subsetting by domain and profile. Profile is fiddly as vector uses abbreviations 
# so needs to be converted to the names to match techdoc.
r_def_data_ranquing <- reactive({
  alfred %>%
    filter(name == r_selected_values$indicador) %>%
    filter(map_lgl(Àmbit, ~ r_selected_values$ambit %in% . #|
                   #selected_values$ambit %in% (Etiqueta %||% "")
    )
    ) %>% 
    unique()
})



#output$r_def_text_ranquing <- renderUI({
  
#  r_subtitles$def_text_ranquing
  
#}) 


output$r_explicacio_comparador <- renderUI({
  
  tagList(
    div(
      style = "font-size: 14px; padding: 10px; background-color: #FFE7CE; border-left: 5px solid #FFA500; margin-bottom: 15px;",
      "Per cada indicador, el gràfic ordena els resultats dels centres o territoris que pertanyen a la unitat geogràfica seleccionada a la pestanya d'Inici, 
    i els mostra en relació al centre o territori seleccionat. Quan cal mostrar més de 80 centres, el gràfic gira 90º per facilitar la identificació de la posició relativa."
    )
  )
})

output$r_explicacio_comparador2 <- renderUI({
  # Read the HTML file
  html_content <- includeHTML("www/llegenda_variabilitat.html")
  
  # Return the HTML content
  tagList(
    div(
      style = "font-size: 14px; padding: 10px; background-color: #FFE7CE; border-left: 5px solid #FFA500; margin-bottom: 15px;",
      html_content
    )
  )
})


# Check if Catalunya is selected for geography
is_catalunya_selected <- reactive({
  return(r_selected_values$geography == "Catalunya")
})

# Create a message when Catalunya is selected
output$r_catalunya_message <- renderUI({
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
})

# Modify each tab's content to check if Catalunya is selected
output$r_ranquing_conditional <- renderUI({
  if(is_catalunya_selected()) {
    uiOutput("r_catalunya_message")
  } else {
    plotlyOutput("r_ranquing")
  }
})

output$r_rank_table_conditional <- renderUI({
  if(is_catalunya_selected()) {
    uiOutput("r_catalunya_message")
  } else {
    DT::dataTableOutput("r_rank_table")
  }
})

output$r_def_text_ranquing_conditional <- renderUI({
  if(is_catalunya_selected()) {
    uiOutput("r_catalunya_message")
  } else {
    htmlOutput("r_def_text_ranquing")
  }
})


###############################################.
## Reactive data ----
###############################################.


# Comparator data rank plot. 
# Reactive for rank_compar (dplyr version)
r_rank_compar <- reactive({
  req(input$r_select_any_ranquing, r_selected_values$indicador, r_selected_values$ambit)
  
  data_chosen <- input$r_select_any_ranquing
  
  # Base query
  rank_compar <- dades_r_tbl %>%
    filter(
      #grup_edat == "Total",
      `any` == data_chosen,
      Granularitat %in% r_selected_values$geography,
      `Centre/Territori` == r_selected_values$center,
      nom_indicador == r_selected_values$indicador,
      ambit == r_selected_values$ambit |
        #tag1 == r_selected_values$ambit |
        #tag2 == r_selected_values$ambit |
        etiqueta == r_selected_values$ambit
    )
  
  # Determine which result to use based on standardized results availability
  use_standardized <- r_has_standardized_results() && isTRUE(input$r_result_toggle)
  
  # Add sexe filter based on result_toggle
  if (use_standardized) {
    rank_compar <- rank_compar %>% 
      filter(sexe == "Total", grup_edat == "Total")
  } else {
    rank_compar <- rank_compar %>% 
      filter(sexe == input$r_selector_sexe_rank) %>%
      filter(grup_edat == if (!is.null(input$r_selector_edat_rank)) input$r_selector_edat_rank else "Total")
  }
  
  # Collect data
  rank_compar <- rank_compar %>% collect()
  
  print("RANK COMPAR")
  print(rank_compar)
  
  return(rank_compar)
})


r_rank_bar_data <- reactive({
  req(r_rank_compar())
  
  # Base query
  rank_bar <- dades_r_tbl %>%
    filter(
      #grup_edat == "Total",
      Granularitat == r_selected_values$geography,
      `any` == input$r_select_any_ranquing,
      ambit == r_selected_values$ambit |
        #tag1 == r_selected_values$ambit |
        #tag2 == r_selected_values$ambit |
        etiqueta == r_selected_values$ambit,
      nom_indicador == r_selected_values$indicador
    )
  
  # Determine which result to use based on standardized results availability
  use_standardized <- r_has_standardized_results() && isTRUE(input$r_result_toggle)
  
  # Add sexe and grup_edat filter based on result_toggle
  if (use_standardized) {
    rank_bar <- rank_bar %>% 
      filter(sexe == "Total", grup_edat == "Total")
  } else {
    rank_bar <- rank_bar %>% 
      filter(sexe == input$r_selector_sexe_rank) %>%
      filter(grup_edat == if (!is.null(input$r_selector_edat_rank)) input$r_selector_edat_rank else "Total")
  }
  
  # Conditional filtering for `nivell`
  # if (r_selected_values$geography == "Centre (Unitat proveïdora)" && 
  #      r_selected_values$ambit == "Atenció Hospitalària") {
  #    rank_bar <- rank_bar %>%
  #      filter(nivell == r_rank_compar()$nivell)
  #  }
  
  # Collect data before further processing
  rank_bar <- rank_bar %>% collect()
  
  # Dynamically select result column based on availability
  result_column <- if (use_standardized) "oe" else "r"
  
  rank_bar <- rank_bar %>%
    mutate(
      resultat = .data[[result_column]],
      comp_value = if (use_standardized) r_rank_compar()$oe else r_rank_compar()$r,
      comp_name = r_rank_compar()$`Centre/Territori`,
      
      # Confidence interval for each center in rank_bar
      ic = paste0("[", round(ic_inf, 1), "-", round(ic_sup, 1), "]"),
      
      # Comparator confidence interval (only shown if using standardized results)
      comp_ic = if (use_standardized) {
        paste0("[", round(r_rank_compar()$ic_inf, 1), "-", round(r_rank_compar()$ic_sup, 1), "]")
      } else {
        NA_character_
      }
    ) %>%
    arrange(desc(resultat))
  
  print(rank_bar)
  
  return(rank_bar)
})


# Check if standardized results exist for the selected indicator
r_has_standardized_results <- reactive({
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
  

###############################################.
## Reactive controls ----
###############################################.

# Observadors dels selectors



## Remember the selected samples
# creates reactive values to remember user selection of the geography level
# so it only changes when the user changes it on purpose
#georank_chosen <- reactiveValues(value_geo = "Regió Sanitària")

#observeEvent(input$select_geography_type_ranquing, 
#             isolate({ georank_chosen$value_geo <- input$select_geography_type_ranquing})
#)


output$r_select_ambit_ranquing_ui <- renderUI({
  #ambit <- sort(unique(dades_rank_tbl$ambit[dades_rank_tbl$`Centre/Territori` == selected_values$center]))
  
  ambit <- dades_r_tbl %>%
    filter(
      grup_edat == "Total",
      `Centre/Territori` == r_selected_values$center) %>%
    pull(ambit) %>%
    as.character() %>%  # Convert factor to character to ensure correct display
    unique() %>%
    sort()
  
  etiqueta <- dades_r_tbl %>%
    filter(
      grup_edat == "Total",
      `Centre/Territori` == r_selected_values$center) %>%
    select(etiqueta) %>% 
    #select(tag1, tag2, tag3) %>%
    pivot_longer(cols = everything(), names_to = "tag", values_to = "value") %>%
    filter(!is.na(value)) %>%
    distinct(value) %>%
    collect() %>%
    pull(value)
  
  # Combine into a named list for grouping
  grouped_choices_rank <- list(
    "Àmbit" = as.list(ambit),
    "Etiqueta" = as.list(etiqueta)
  )
  
  div(#style = "margin-top: 20px; margin-bottom: 20px;",
      selectInput("r_select_ambit_ranquing", 
                  label = NULL,
                  #shiny::HTML("<p class='step-text'>Pas 1: Tria un àmbit o etiqueta</p>"), 
                  choices=grouped_choices_rank, selected = r_selected_values$ambit)# selected = "Alcohol-related hospital admissions")),  
  )
  
  
})

output$r_select_indicador_ranquing_ui <- renderUI({
  req(input$r_select_ambit_ranquing)  # Ensure ambit is selected
  
  # Update r_selected_values$ambit first
  #r_selected_values$ambit <- input$r_select_ambit_ranquing
  
  # Filter the data and create a distinct subset
  dades_comparador2 <- dades_r_tbl %>%
    filter(grup_edat == "Total") %>% 
    distinct(nom_indicador, dimensio, ambit, #tag1, tag2, 
             etiqueta, Granularitat, `Centre/Territori`) %>%
    filter(
      Granularitat == r_selected_values$geography,
      `Centre/Territori` == r_selected_values$center,
      (ambit == input$r_select_ambit_ranquing |
         #tag1 == input$r_select_ambit_ranquing |
         #tag2 == input$r_select_ambit_ranquing |
         etiqueta == input$r_select_ambit_ranquing)
    ) %>%
    mutate(codi = nom_indicador) %>% collect() # Assuming there's a `dimensio` variable

    ind_choices_rank <- dades_comparador2 %>%
      split(.$dimensio) %>%
      lapply(function(x) {
        i <- sort(x$nom_indicador)
        names(i) <- i  # Set display names for the indicators
        i
      })
  
  # Render the select input for the indicator
  div(
    selectInput(
      "r_select_indicador_ranquing",
      label = NULL,
      #shiny::HTML("<p class='step-text'>Pas 2: Tria un indicador</p>"), 
      choices = ind_choices_rank,
      selected = r_selected_values$indicador
    )
  )
})

# Add an observer to keep r_selected_values$ambit updated
observe({
  req(input$r_select_ambit_ranquing)
  r_selected_values$ambit <- input$r_select_ambit_ranquing
})

# Add an observer to keep r_selected_values$indicador updated
observe({
  req(input$r_select_indicador_ranquing)
  r_selected_values$indicador <- input$r_select_indicador_ranquing
})



output$r_select_any_ranquing_ui <- renderUI({
  req(r_selected_values$geography, 
      r_selected_values$center, 
      r_selected_values$ambit,
      r_selected_values$indicador)
  
  any <- dades_r_tbl %>%
    filter(
      grup_edat == "Total",
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
  
  if (length(any) > 0) {
    div(
      #style = "margin-top: 20px; margin-bottom: 20px;",
      selectInput("r_select_any_ranquing", 
                  label = NULL,
                  #label = "Pas 3: Tria un any", 
                  #shiny::HTML("<p class='step-text'>Pas 3: Tria un any</p>"), 
                  choices = any, 
                  selected = first(any))
    )
  } else {
    return(NULL)
  }
})


output$r_selector_sexe_rank_ui <- renderUI({
  # Only show if no standardized results exist OR toggle is FALSE
  if (!r_has_standardized_results() || !isTRUE(input$r_result_toggle)) {
    
    # Filter to get `sexe` categories
    sexe_categories <- dades_r_tbl %>%
      filter(
        grup_edat == "Total",
        ambit == r_selected_values$ambit |
          #tag1 == r_selected_values$ambit |
          #tag2 == r_selected_values$ambit |
          etiqueta == r_selected_values$ambit,
        nom_indicador == r_selected_values$indicador
      ) %>%
      pull(sexe) %>%
      unique() %>%
      sort()
    
    # Show the selector if there is more than one category and result_toggle is FALSE
    if (length(sexe_categories) > 1) {
      div(
        div(align = "left", "Tria el sexe:"),
        div(align = "left", #style = "margin-top: 20px; margin-bottom: 20px;",
            title = "Mostra les dades agregades o bé diferenciades per sexe.",
            awesomeRadio("r_selector_sexe_rank", label = NULL, inline = FALSE, 
                         choices = sexe_categories, selected = "Total")
        )
      )
    }
    
    
  }
    

})


output$r_selector_edat_rank_ui <- renderUI({
  # Only show if no standardized results exist OR toggle is FALSE
  if (!r_has_standardized_results() || !isTRUE(input$r_result_toggle)) {
    
    # Filter to get `grup_edat` categories
    edat_categories <- dades_r_tbl %>%
      filter(
        ambit == r_selected_values$ambit |
          #tag1 == r_selected_values$ambit |
          #tag2 == r_selected_values$ambit |
          etiqueta == r_selected_values$ambit,
        nom_indicador == r_selected_values$indicador
      ) %>%
      pull(grup_edat) %>%
      unique() %>%
      sort()
    
    # Show the selector if there is more than one category
    if (length(edat_categories) > 1) {
      div(
        div(align = "left", "Tria el grup d'edat:"),
        div(align = "left",
            title = "Mostra les dades agregades o bé diferenciades per grup d'edat.",
            #          selectInput("selector_edat_rank", 
            #                      #label = "Pas 3: Tria un any", 
            #                      label = NULL,
            #                      choices = edat_categories, selected = "Total")
            #shiny::HTML("<p class='step-text'>Pas 3: Tria un any</p>"),
            #choices = any, selected = first(any))
            awesomeRadio("r_selector_edat_rank", label = NULL, inline = TRUE, 
                         choices = edat_categories, selected = "Total")
        )
      )
    }
    
  }

})



# Add this to your UI where you define selectors
output$r_result_toggle_ui <- renderUI({
  
  # Only show toggle if standardized results exist
  if (r_has_standardized_results()) {
    div(
      div(align = "left", "Mostra els resultats estandarditzats:"),
      div(
        align = "left",
        prettySwitch(
          inputId = "r_result_toggle",
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


#output$selector_edat_rank_ui <- renderUI({
#  # Filter to get `sexe` categories
#  edat_categories <- dades_rank_tbl %>%
#    filter(
#      ambit == input$select_ambit_ranquing |
#        tag1 == input$select_ambit_ranquing |
#        tag2 == input$select_ambit_ranquing |
#        tag3 == input$select_ambit_ranquing,
#      indicador == input$select_indicador_ranquing
#    ) %>%
#    pull(grup_edat) %>%
#    unique()
#  
#  # Show the selector if there is more than one category
#  if (length(edat_categories) > 1) {
#    div(
#      div(align = "left", tags$b(class = "step-text", "Tria un grup d'edat:")),
#      div(align = "left", style = "margin-top: 20px; margin-bottom: 20px;",
#          title = "Mostra les dades agregades o bé diferenciades per grup d'edat.",
#          awesomeRadio("selector_edat_rank", label = NULL, inline = TRUE, 
#                       choices = edat_categories, selected = "Total")
#      )
#    )
#  } else {
#    return(NULL)
#  }
#})




#output$selector_subtipologia_rank_ui <- renderUI({
#  subtipologia_categories <- dades_rank_tbl %>%
#    filter(
#      ambit == selected_values$ambit |
#        tag1 == selected_values$ambit |
#        tag2 == selected_values$ambit |
#        tag3 == selected_values$ambit,
#      indicador == selected_values$indicador
#    ) %>%
#    pull(subtipologia) %>%
#    na.omit() %>%
#    unique()
#  
#  if (length(subtipologia_categories) > 1) {
#    div(
#      selectInput("selector_subtipologia_rank", 
#                  label = "Tria una subtipologia:", 
#                  choices = subtipologia_categories)
#    )
#  } else {
#    return(NULL)
#  }
#})



################################################################################
# Creació del gràfic ----
################################################################################

# Títols

#subtipologia_rank_exists <- reactive({
#  !is.na(unique(rank_bar_data()$id_subtipologia))
#})

#output$subtipologia_rank_exists <- reactive({
#  subtipologia_rank_exists()
#})
#outputOptions(output, "subtipologia_rank_exists", suspendWhenHidden = FALSE)

#r_nivell_rank_exists <- reactive({
#  !is.na(unique(r_rank_bar_data()$nivell))
#})

#output$r_nivell_rank_exists <- reactive({
#  r_nivell_rank_exists()
#})
#outputOptions(output, "r_nivell_rank_exists", suspendWhenHidden = FALSE)


r_sexe_rank_exists <- reactive({
  # Show sexe selector when using raw results AND standardized results don't exist OR toggle is off
  !r_has_standardized_results() || !isTRUE(input$r_result_toggle)})

output$r_sexe_rank_exists <- reactive({
  r_sexe_rank_exists()
})
outputOptions(output, "r_sexe_rank_exists", suspendWhenHidden = FALSE)



r_edat_rank_exists <- reactive({
  # Same logic as sexe_rank_exists
  !r_has_standardized_results() || !isTRUE(input$r_result_toggle)
})

output$r_edat_rank_exists <- reactive({
  r_edat_rank_exists()
})
outputOptions(output, "r_edat_rank_exists", suspendWhenHidden = FALSE)


# Create reactive values to store the subtitle states, això permet que al canviar les seleccions de l'esquerra NOMÉS s'actualitzin definicions, títols i subtítols quan es prem el botó.
#subtitles <- reactiveValues(def_text_ranquing = NULL, titol_comparador = NULL, subtitol_comparador = NULL, subtitol_comparador2 = NULL, nivell_comparador = NULL)

# Update subtitles when the refresh_button is clicked
#observeEvent(input$refresh_button, {

# Update def_text_ranquing
output$r_def_text_ranquing <- renderUI({
  def_data <- r_def_data_ranquing() # Cache reactive
  
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



output$r_titol_comparador <- renderUI({
  if (isTRUE(input$r_result_toggle)) {
    HTML(paste0("<strong>", input$r_select_indicador_ranquing, " (Raó O/E)", "</strong>"))
    
  } else {
    HTML(paste0("<strong>", input$r_select_indicador_ranquing, " (", unique(r_rank_bar_data()$unitats), ")", "</strong>"))
    
  }
  
})

output$r_titol_comparador_taula <- renderUI({
  
  if (isTRUE(input$r_result_toggle)) {
    HTML(paste0("<strong>", input$r_select_indicador_ranquing, " (Raó O/E)", "</strong>"))
    
  } else {
    HTML(paste0("<strong>", input$r_select_indicador_ranquing, " (", unique(r_rank_bar_data()$unitats), ")", "</strong>"))
    
  }
  
})

#output$subtitol_comparador <- renderText({
#  
#  if (subtipologia_rank_exists()) {
#    
#    paste0(unique(rank_bar_data()$dg_extra), ": ", input$selector_subtipologia_rank)
#    
#  } else {
#    
#    NULL
#    
#  }
#  
#})

output$r_subtitol_comparador2 <- renderText({ 
  
  # Safety checks to prevent zero-length argument errors
  has_sexe <- !is.null(input$r_selector_sexe_rank) && length(input$r_selector_sexe_rank) > 0
  has_edat <- !is.null(input$r_selector_edat_rank) && length(input$r_selector_edat_rank) > 0
  
  if (r_sexe_rank_exists() && has_sexe) {
    
    if (r_edat_rank_exists() && has_edat) {
      
      # Hide sexe when it's "Total"
      if (input$r_selector_sexe_rank == "Total") {
        # Also hide edat when it's "Total"
        if (input$r_selector_edat_rank == "Total") {
          NULL  # Hide entire subtitle when both are "Total"
        } else {
          HTML(paste0("Grup d'edat: ", input$r_selector_edat_rank))
        }
      } else {
        # Hide edat when it's "Total" but show sexe
        if (input$r_selector_edat_rank == "Total") {
          HTML(paste0("Sexe: ", input$r_selector_sexe_rank))
        } else {
          HTML(paste0("Sexe: ", input$r_selector_sexe_rank, ", Grup d'edat: ", input$r_selector_edat_rank))
        }
      }
      
    } else {
      
      # Hide the entire subtitle when sexe is "Total" and there's no age group
      if (input$r_selector_sexe_rank == "Total") {
        NULL
      } else {
        HTML(paste0("Sexe: ", input$r_selector_sexe_rank))
      }
      
    }
    
  } else {
    
    if (r_edat_rank_exists() && has_edat) {
      
      # Hide edat when it's "Total"
      if (input$r_selector_edat_rank == "Total") {
        NULL
      } else {
        HTML(paste0("Grup d'edat: ", input$r_selector_edat_rank))
      }
      
    } else {
      
      NULL
      
    }
    
  }
  
})


output$r_subtitol_comparador2_taula <- renderText({
  
  # Safety checks to prevent zero-length argument errors
  has_sexe <- !is.null(input$r_selector_sexe_rank) && length(input$r_selector_sexe_rank) > 0
  has_edat <- !is.null(input$r_selector_edat_rank) && length(input$r_selector_edat_rank) > 0
  
  if (r_sexe_rank_exists() && has_sexe) {
    
    if (r_edat_rank_exists() && has_edat) {
      
      # Hide sexe when it's "Total"
      if (input$r_selector_sexe_rank == "Total") {
        # Also hide edat when it's "Total"
        if (input$r_selector_edat_rank == "Total") {
          NULL  # Hide entire subtitle when both are "Total"
        } else {
          HTML(paste0("Grup d'edat: ", input$r_selector_edat_rank))
        }
      } else {
        # Hide edat when it's "Total" but show sexe
        if (input$r_selector_edat_rank == "Total") {
          HTML(paste0("Sexe: ", input$r_selector_sexe_rank))
        } else {
          HTML(paste0("Sexe: ", input$r_selector_sexe_rank, ", Grup d'edat: ", input$r_selector_edat_rank))
        }
      }
      
    } else {
      
      # Hide the entire subtitle when sexe is "Total" and there's no age group
      if (input$r_selector_sexe_rank == "Total") {
        NULL
      } else {
        HTML(paste0("Sexe: ", input$r_selector_sexe_rank))
      }
      
    }
    
  } else {
    
    if (r_edat_rank_exists() && has_edat) {
      
      # Hide edat when it's "Total"
      if (input$r_selector_edat_rank == "Total") {
        NULL
      } else {
        HTML(paste0("Grup d'edat: ", input$r_selector_edat_rank))
      }
      
    } else {
      
      NULL
      
    }
    
  }
  
})

#output$r_subtitol_comparador3 <- renderText({
#  
#  HTML(paste0(
#    "Resultats ordenats segons <b>", r_selected_values$geography, "</b>", 
#    " i comparats amb <b>", r_selected_values$center, "</b>", 
#    " (", input$r_select_any_ranquing, ")"
#  ))
#  
#})

#output$r_subtitol_comparador3_taula <- renderText({
#  
#  HTML(paste0(
#    "Resultats ordenats segons <b>", r_selected_values$geography, "</b>", 
#    #" i comparats amb <b>", r_selected_values$center, "</b>", 
#    " (", input$r_select_any_ranquing, ")"
#  ))
#  
#})

# Update nivell_comparador

#output$r_nivell_comparador <- renderText({ 
#  
#  if (r_nivell_rank_exists()) {
#    
#    HTML(paste0("Nivell hospitalari: ", unique(r_rank_bar_data()$nivell)))
#    
#  } else {
#    
#    NULL
#    
#  }
#  
#})

#output$r_nivell_comparador_taula <- renderText({ 
#  
#  if (r_nivell_rank_exists()) {
#    
#    HTML(paste0("Nivell hospitalari: ", unique(r_rank_bar_data()$nivell)))
#    
#  } else {
#    
#    NULL
#    
#  }
#  
#})

output$r_cat_checkbox_ui <- renderUI({
  # Only show the checkbox when result_toggle is FALSE
  if (!isTRUE(input$r_result_toggle)) {
    awesomeCheckbox("r_selector_cat_ranquing", 
                    p("Mostra el resultat global de Catalunya"), 
                    value = TRUE)
  }
})

# Render the titol_comparador
#output$titol_comparador <- renderText({ 
#  subtitles$titol_comparador # Only updates when refresh_button is clicked
#})
#
## Render the subtitol_comparador
#output$subtitol_comparador <- renderText({
#  subtitles$subtitol_comparador  # Only updates when refresh_button is clicked
#})
#
## Render the subtitol_comparador2
#output$subtitol_comparador2 <- renderText({
#  subtitles$subtitol_comparador2  # Only updates when refresh_button is clicked
#})
#
## Render the nivell_comparador
#output$nivell_comparador <- renderText({
#  subtitles$nivell_comparador  # Only updates when refresh_button is clicked
#})


# Gràfic

r_comparador_chart <- function() {
  # If no data is available for that period, plot a message saying plot is not available
  if (is.data.frame(r_rank_bar_data()) && nrow(r_rank_bar_data()) == 0) {
    plot_nodata()
  } else { # If data is available, then plot it
    
    # Coloring based on if significantly different from comparator
    r_rank_bar_data <- r_rank_bar_data() %>% 
      mutate(color_pal = case_when(invers == 2 & r != comp_value ~ '#999966',
                                   is.na(comp_value) | is.na(r) | r == 0 ~ '#999966',
                                   r > comp_value & invers == 0 ~ '#91BFDB',
                                   r > comp_value & invers == 1 ~ '#FC8D59',
                                   r < comp_value & invers == 1 ~ '#91BFDB',
                                   r < comp_value & invers == 0 ~ '#FC8D59', 
                                   r == comp_value  ~ '#ccccff',
                                   TRUE ~ 'pink'),
             int_hover = case_when(color_pal == '#999966' ~ "Indicador sense interpretació",
                                   color_pal == '#91BFDB' ~ "Resultat relativament millor que la referència",
                                   color_pal == '#FC8D59' ~ "Resultat relativament pitjor que la referència",
                                   color_pal == '#ccccff' ~ "Resultat igual al de la referència",
                                   TRUE ~ "NA")
      )
    
    
    
    # Text for tooltip
    r_tooltip_bar <- if (isTRUE(input$r_result_toggle)) {
      paste0(r_rank_bar_data()$`Centre/Territori`, ": ", "<b>", round(r_rank_bar_data()$r, 2), "</b> ",  r_rank_bar_data()$ic, " (Raó O/E)",
             "<br>", r_selected_values$center, ": ", "<b>", round(r_rank_bar_data()$comp_value, 2), "</b> ",  r_rank_bar_data()$comp_ic, " (Raó O/E)",
             "<br>", r_rank_bar_data$int_hover)
      
      
    } else {
      paste0(#r_rank_bar_data()$Granularitat, ": ", 
        r_rank_bar_data()$`Centre/Territori`, ": ", "<b>", round(r_rank_bar_data()$r, 2), "</b> (", r_rank_bar_data()$unitats, ")",
        "<br>", r_selected_values$center, ": ", "<b>", round(r_rank_bar_data()$comp_value, 2), "</b> (", r_rank_bar_data()$unitats, ")",
        "<br>", r_rank_bar_data$int_hover)
      
    }
    
    
    # Creating a vector with the area names in the order they are going to be plotted
    r_order_areas <- as.vector(r_rank_bar_data()$`Centre/Territori`)
    
    # Determine the type of chart based on the number of bars
    if (nrow(r_rank_bar_data()) > 40) {
      # Vertical bar chart
      r_rank_plot <- plot_ly(data = r_rank_bar_data, height = 400) %>%
        add_trace(x = ~`Centre/Territori`, 
                  y = ~comp_value, 
                  #name = paste0("Selecció: ", unique(r_rank_bar_data()$comp_name)), 
                  type = 'scatter', 
                  mode = 'lines+markers',
                  text = ~paste0(r_selected_values$center, ": <b>", round(comp_value, 2), "</b> (", unitats, ")"),
                  #text = ~paste0("Referència: ", "<b>", round(comp_value, 2), "</b> (", mesura, ")"),
                  line = list(color = '#ccccff',
                              width = 3),
                  marker = list(color = '#ccccff',
                                size = 3),
                  showlegend = F, 
                  hoverinfo = "text") %>%
        add_bars(x = ~`Centre/Territori`, y = ~resultat, text = r_tooltip_bar, textposition = "none", hoverinfo = "text",
                 marker = list(color = ~color_pal)) %>%
        layout(
          yaxis = list(title = list(text = unique(r_rank_bar_data()$unitats), standoff = 10), 
                       titlefont = list(size = 14), 
                       tickfont = list(size = 14, family = "sans-serif"), fixedrange = TRUE),
          xaxis = list(title = list(text = unique(r_rank_bar_data()$Granularitat)), tickvals = list(), ticktext = list(), fixedrange = TRUE,
                       tickfont = list(size = 12, family = "sans-serif"), 
                       categoryorder = "array", 
                       categoryarray = r_order_areas),
          font = font_plots,
          #margin = list(b = 180, t = 5, l = 0, r = 0),
          hovermode = 'false',
          legend = list(orientation = "h", y = 1.2, x = 0.5, xanchor = "center", yanchor = "top")
        )
      
    } else {
      # Horizontal bar chart with dynamic height
      base_height <- 500
      r_additional_height_per_bar <- 20
      r_plot_height <- base_height + (nrow(r_rank_bar_data()) - 20) * r_additional_height_per_bar
      
      r_rank_plot <- plot_ly(data = r_rank_bar_data, height = r_plot_height) %>%
        add_trace(y = ~`Centre/Territori`, 
                  x = ~comp_value, 
                  #name = paste0("Selecció: ", unique(r_rank_bar_data()$comp_name)), 
                  type = 'scatter', 
                  mode = 'lines+markers',
                  text = ~paste0(r_selected_values$center, ": <b>", round(comp_value, 2), "</b> (", unitats, ")"),
                  #text = ~paste0("Referència: ", "<b>", round(comp_value, 2), "</b> (", mesura, ")"),
                  line = list(color = '#ccccff',
                              width = 3),
                  marker = list(color = '#ccccff',
                                size = 3),
                  showlegend = F, 
                  hoverinfo = "text") %>%
        add_bars(y = ~`Centre/Territori`, x = ~r, text = r_tooltip_bar, textposition = "none", hoverinfo = "text",
                 marker = list(color = ~color_pal)) %>%
        layout(
          separators = ",.",     
          xaxis = list(title = list(text = unique(r_rank_bar_data()$unitats), standoff = 10), 
                       titlefont = list(size = 14), 
                       tickfont = list(size = 14, family = "sans-serif"), fixedrange = TRUE),
          yaxis = list(title = "", tickangle = 0, fixedrange = TRUE,
                       tickfont = list(size = 12, family = "sans-serif"), 
                       categoryorder = "array", 
                       categoryarray = r_order_areas),
          font = font_plots,
          margin = list(pad = 10),
          hovermode = 'false',
          legend = list(orientation = "h", y = 1.2, x = 0.5, xanchor = "center", yanchor = "top")
        )
    }
    
    # Different behavior based on result_toggle
    if (isTRUE(input$r_result_toggle)) {
      # For standardized results (r_result_toggle = TRUE)
      # Always add a reference line at value 1
      if (nrow(r_rank_bar_data()) <= 40) {
        r_rank_plot <- r_rank_plot %>%
          add_trace(y = ~`Centre/Territori`, 
                    x = ~comp_value, 
                    type = 'scatter', 
                    mode = 'lines+markers',
                    text = ~paste0(r_selected_values$center, ": <b>", round(comp_value, 2), "</b> (Raó O/E)"),
                    line = list(color = '#ccccff',
                                width = 3),
                    marker = list(color = '#ccccff',
                                  size = 3),
                    showlegend = F, 
                    hoverinfo = "text") %>%
          add_trace(y = ~`Centre/Territori`,
                    x = rep(1, length(r_order_areas)),
                    type = 'scatter',
                    mode = 'lines',
                    text = "Catalunya: <b>1</b>",
                    hoverinfo = "text",
                    line = list(color = 'red', width = 3),
                    showlegend = FALSE)
      } else {
        r_rank_plot <- r_rank_plot %>%
          add_trace(x = ~`Centre/Territori`, 
                    y = ~comp_value, 
                    type = 'scatter', 
                    mode = 'lines+markers',
                    text = ~paste0(r_selected_values$center, ": <b>", round(comp_value, 2), "</b> (Raó O/E)"),
                    line = list(color = '#ccccff',
                                width = 3),
                    marker = list(color = '#ccccff',
                                  size = 3),
                    showlegend = F, 
                    hoverinfo = "text") %>%
          add_trace(x = ~`Centre/Territori`,
                    y = rep(1, length(r_order_areas)),
                    type = 'scatter',
                    mode = 'lines',
                    text = "Catalunya: <b>1</b>",
                    hoverinfo = "text",
                    line = list(color = 'red', width = 3),
                    showlegend = FALSE)
      }
    } else {
      # For raw results (result_toggle = FALSE)
      # Add the red line for mitjana only if checked
      if (input$r_selector_cat_ranquing == TRUE & (nrow(r_rank_bar_data()) <= 40)) {
        r_rank_plot <- r_rank_plot %>%
          add_trace(y = ~`Centre/Territori`,
                    x = ~mitjana,
                    type = 'scatter',
                    mode = 'lines+markers',
                    text = ~paste0("Catalunya: ", "<b>", round(mitjana, 2), "</b> (", unitats, ")"),
                    hoverinfo = "text",
                    line = list(color = 'red', width = 3),
                    marker = list(color = 'red', size = 3),
                    showlegend = FALSE)
        
      } else if (input$r_selector_cat_ranquing == TRUE & (nrow(r_rank_bar_data()) > 40)) {
        r_rank_plot <- r_rank_plot %>%
          add_trace(x = ~`Centre/Territori`,
                    y = ~mitjana,
                    type = 'scatter',
                    mode = 'lines+markers',
                    text = ~paste0("Catalunya: ", "<b>", round(mitjana, 2), "</b> (", unitats, ")"),
                    hoverinfo = "text",
                    line = list(color = 'red', width = 3),
                    marker = list(color = 'red', size = 3),
                    showlegend = FALSE)
      }
    }
    
    # Configure plot settings
    r_rank_plot %>% config(displayModeBar = TRUE, displaylogo = FALSE,
                         modeBarButtonsToRemove = list("sendDataToCloud", "zoomIn2d", "zoomOut2d", 
                                                       "autoScale2d", "resetScale2d", "toggleSpikelines", 
                                                       "resetViews", "toggleHover", "hoverClosestCartesian", 
                                                       "hoverCompareCartesian", "zoom2d", "pan2d", "select2d", 
                                                       "lasso2d", "toggleHover"),
                         toImageButtonOptions = list(filename = "grafic_variabilitat", 
                                                     format = "png",
                                                     label = "Descarrega el gràfic en format PNG",
                                                     scale = 3),
                         locale = "ca")
  }
}




# Calling the renderPlotly object
output$r_ranquing <- renderPlotly({   r_comparador_chart() }) 


output$r_rank_table <- DT::renderDataTable({
  req(r_rank_bar_data())
  
  data <- r_rank_bar_data()
  result_col <- if (isTRUE(input$r_result_toggle)) "oe" else "r"
  
  # Base columns
  cols_to_select <- c("Centre/Territori", result_col)
  
  
  # Add confidence interval when showing standardized results
  if (isTRUE(input$r_result_toggle)) {
    # Include ic_inferior and ic_superior columns to create a formatted IC column
    cols_to_select <- c(cols_to_select, "ic_inf", "ic_sup")
  }
  
  # For raw results (result_toggle = FALSE), conditionally include mitjana
  if (!isTRUE(input$r_result_toggle) && isTRUE(input$r_selector_cat_ranquing)) {
    cols_to_select <- c(cols_to_select, "mitjana")
  }
  
  # Select the columns and rename them for display
  table_data <- data %>% 
    select(all_of(cols_to_select)) %>% 
    rename(Centre = `Centre/Territori`)
  
  # Format numeric columns with 2 decimal places and comma as separator
  numeric_cols <- names(table_data)[sapply(table_data, is.numeric)]
  for (col in numeric_cols) {
    table_data[[col]] <- format(round(table_data[[col]], 2), decimal.mark=",", nsmall=2)
  }
  
  # Rename the result column for display
  if (result_col == "oe") {
    table_data <- table_data %>% 
      rename("Resultat" = "oe")
    
    # Create a formatted IC column with proper decimal separator
    if ("ic_inf" %in% colnames(table_data) && "ic_sup" %in% colnames(table_data)) {
      table_data <- table_data %>%
        mutate(IC = paste0("[", ic_inf, " - ", ic_sup, "]")) %>%
        select(-ic_inf, -ic_sup)
    }
  } else {
    table_data <- table_data %>% 
      rename("Resultat" = "r")
  }
  
  # If mitjana is included, rename it to "Catalunya"
  if ("mitjana" %in% colnames(table_data)) {
    table_data <- table_data %>% 
      rename("Catalunya" = "mitjana")
  }
  
  
  DT::datatable(
    table_data,
    options = list(
      paging = FALSE,
      searching = FALSE,
      srollX = TRUE,
      dom = 't'
    )
  )
})


################################################################################
# Descàrregues ----
################################################################################

#rank_csv <- reactive({ format_csv(rank_bar_data(), extra_vars = c("comp_value", "comp_name")) %>%
#    rename("resultat_comparador" = "comp_value", "nom_comparador" = "comp_name") })

#output$descarrega_dades_comparador <- downloadHandler(filename =  'dades_variabilitat.csv',
#                                        content = function(file) {write.csv(rank_csv(), file, row.names=FALSE) })

# Download as Excel
output$r_descarrega_dades_comparador <- downloadHandler(
  filename = function() { 
    paste(r_selected_values$center, "-variabilitat-", input$r_select_any_ranquing, ".xlsx", sep = "")
  },
  content = function(file) {
    # Prepare the data for download
    r_data_to_download_rank <- r_rank_bar_data() %>% 
      select(
        indicador, 
        granularitat = Granularitat, 
        centre_territori = `Centre/Territori`,
        nom_comparador = comp_name,
        any, 
        mesura, 
        resultat,
        resultat_comparador = comp_value
      )
    
    # Write to Excel
    writexl::write_xlsx(r_data_to_download_rank, path = file)
  }
)
