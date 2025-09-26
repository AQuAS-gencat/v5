# ###############################################.
# ## Server del comparador ----
# ###############################################.


################################################################################
# Pop-up d'ajuda ----
################################################################################

observeEvent(input$ajuda_comparador, {
  
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

docu_comparador = docu %>% 
  select(-id_resum) %>% 
  unique()

#Subsetting by domain and profile. Profile is fiddly as vector uses abbreviations 
# so needs to be converted to the names to match techdoc.
def_data_ranquing <- reactive({docu_comparador %>% 
    subset(selected_values$indicador == indicador & (selected_values$ambit == ambit | selected_values$ambit == tag1 | 
                                                       selected_values$ambit == tag2 | selected_values$ambit == tag3))
  
})



output$def_text_ranquing <- renderUI({
  
  subtitles$def_text_ranquing
  
}) 


output$explicacio_comparador <- renderUI({
  
  tagList(
    div(
      style = "font-size: 14px; padding: 10px; background-color: #FFE7CE; border-left: 5px solid #FFA500; margin-bottom: 15px;",
      "Per cada indicador, el gràfic ordena els resultats dels centres o territoris que pertanyen a la unitat geogràfica seleccionada a la pestanya d'Inici, 
    i els mostra en relació al centre o territori seleccionat. Quan cal mostrar més de 80 centres, el gràfic gira 90º per facilitar la identificació de la posició relativa."
    )
  )
})

output$explicacio_comparador2 <- renderUI({
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




###############################################.
## Reactive data ----
###############################################.


# Comparator data rank plot. 
# Reactive for rank_compar (dplyr version)
rank_compar <- reactive({
  req(input$select_any_ranquing, selected_values$indicador, selected_values$ambit)
  
  data_chosen <- input$select_any_ranquing
  
  # Base query
  rank_compar <- dades_c_tbl %>%
    filter(
      `any` == data_chosen,
      Granularitat %in% selected_values$geography,
      `Centre/Territori` == selected_values$center,
      indicador == selected_values$indicador,
      ambit == selected_values$ambit |
        tag1 == selected_values$ambit |
        tag2 == selected_values$ambit |
        tag3 == selected_values$ambit
    )
  
  # Add sexe filter based on result_toggle
  if (isTRUE(input$result_toggle)) {
    rank_compar <- rank_compar %>% 
      filter(sexe == "Total", grup_edat == "Total")
  } else {
    rank_compar <- rank_compar %>% 
      filter(sexe == input$selector_sexe_rank) %>%
      # Add grup_edat filter if selector exists
      filter(grup_edat == if (!is.null(input$selector_edat_rank)) input$selector_edat_rank else "Total")
  }
  
  # Collect data and handle subtipologia
  rank_compar <- rank_compar %>% collect()
  
  return(rank_compar)
})

rank_bar_data <- reactive({
  req(rank_compar())
  
  # Base query
  rank_bar <- dades_c_tbl %>%
    filter(
      Granularitat == selected_values$geography,
      `any` == input$select_any_ranquing,
      ambit == selected_values$ambit |
        tag1 == selected_values$ambit |
        tag2 == selected_values$ambit |
        tag3 == selected_values$ambit,
      indicador == selected_values$indicador
    )
  
  # Add sexe and grup_edat filter based on result_toggle
  if (isTRUE(input$result_toggle)) {
    rank_bar <- rank_bar %>% 
      filter(sexe == "Total", grup_edat == "Total")
  } else {
    rank_bar <- rank_bar %>% 
      filter(sexe == input$selector_sexe_rank) %>%
      # Add grup_edat filter if selector exists
      filter(grup_edat == if (!is.null(input$selector_edat_rank)) input$selector_edat_rank else "Total")
  }
  
  # Collect data before further processing
  rank_bar <- rank_bar %>% collect()
  
  # Rest of the function remains the same...
  # [existing code]
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


output$select_ambit_ranquing_ui <- renderUI({
  #ambit <- sort(unique(dades_c_tbl$ambit[dades_c_tbl$`Centre/Territori` == selected_values$center]))
  
  ambit <- dades_c_tbl %>%
    filter(`Centre/Territori` == selected_values$center) %>%
    pull(ambit) %>%
    as.character() %>%  # Convert factor to character to ensure correct display
    unique() %>%
    sort()
  
  etiqueta <- dades_c_tbl %>%
    filter(`Centre/Territori` == selected_values$center) %>%
    select(tag1, tag2, tag3) %>%
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
    selectInput("select_ambit_ranquing",# shiny::HTML("<p class='step-text'>Pas 1: Tria un àmbit o etiqueta</p>"), 
                label = NULL,
                choices=grouped_choices_rank, selected = selected_values$ambit)# selected = "Alcohol-related hospital admissions")),  
  )
  
  
})

output$select_indicador_ranquing_ui <- renderUI({
  req(input$select_ambit_ranquing)
  
  # Filter the data and create a distinct subset
  dades_comparador2 <- dades_c_tbl %>%
    distinct(indicador, dimensio, ambit, tag1, tag2, tag3, Granularitat, `Centre/Territori`) %>%
    filter(
      Granularitat == selected_values$geography,
      `Centre/Territori` == selected_values$center,
      ambit == selected_values$ambit |
        tag1 == selected_values$ambit |
        tag2 == selected_values$ambit |
        tag3 == selected_values$ambit
    ) %>%
    mutate(codi = indicador) %>% collect() # Assuming there's a `dimensio` variable
  
  # Generate the choices, grouping by `dimensio`
  ind_choices_rank <- dades_comparador2 %>%
    split(.$dimensio) %>%
    lapply(function(x) {
      i <- sort(x$indicador)
      names(i) <- i  # Set display names for the indicators
      i
    })
  
  # Render the select input for the indicator
  div(
    selectInput("select_indicador_ranquing", 
                #shiny::HTML("<p class='step-text'>Pas 2: Tria un indicador</p>"), 
                label = NULL,
                choices = ind_choices_rank,
                selected = selected_values$indicador)
  )
})

output$select_any_ranquing_ui <- renderUI({
  any <- dades_c_tbl %>%
    filter(
      Granularitat == selected_values$geography,
      `Centre/Territori` == selected_values$center,
      ambit == selected_values$ambit |
        tag1 == selected_values$ambit |
        tag2 == selected_values$ambit |
        tag3 == selected_values$ambit,
      indicador == selected_values$indicador
    ) %>%
    pull(any) %>%
    unique() %>%
    sort(decreasing = TRUE)
  
  if (length(any) > 0) {
    div(
      #style = "margin-top: 20px; margin-bottom: 20px;",
      selectInput("select_any_ranquing", 
                  #label = "Pas 3: Tria un any", 
                  label = NULL,
                  #shiny::HTML("<p class='step-text'>Pas 3: Tria un any</p>"),
                  choices = any, selected = first(any))
    )
  } else {
    return(NULL)
  }
})


output$selector_sexe_rank_ui <- renderUI({
  req(!isTRUE(input$result_toggle))  # Only show if result_toggle is FALSE
  
  # Filter to get `sexe` categories
  sexe_categories <- dades_c_tbl %>%
    filter(
      ambit == selected_values$ambit |
        tag1 == selected_values$ambit |
        tag2 == selected_values$ambit |
        tag3 == selected_values$ambit,
      indicador == selected_values$indicador
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
          awesomeRadio("selector_sexe_rank", label = NULL, inline = FALSE, 
                       choices = sexe_categories, selected = "Total")
      )
    )
  }
})



output$selector_edat_rank_ui <- renderUI({
  req(!isTRUE(input$result_toggle))  # Only show if result_toggle is FALSE (same logic as sexe selector)
  
  # Filter to get `grup_edat` categories
  edat_categories <- dades_c_tbl %>%
    filter(
      ambit == selected_values$ambit |
        tag1 == selected_values$ambit |
        tag2 == selected_values$ambit |
        tag3 == selected_values$ambit,
      indicador == selected_values$indicador
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
          awesomeRadio("selector_edat_rank", label = NULL, inline = FALSE, 
                       choices = edat_categories, selected = "Total")
      )
    )
  }
})



# Add this to your UI where you define selectors
output$result_toggle_ui <- renderUI({
  div(
    div(align = "left", "Mostra els resultats estandarditzats:"),
    div(
      align = "left",
      #style = "margin-top: 20px; margin-bottom: 20px;",
      prettySwitch(
        inputId = "result_toggle",
        label = NULL,
        fill = TRUE,
        status = "primary",
        value = TRUE
      )
    )
  )
})


#output$selector_edat_rank_ui <- renderUI({
#  # Filter to get `sexe` categories
#  edat_categories <- dades_c_tbl %>%
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
#  subtipologia_categories <- dades_c_tbl %>%
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

nivell_rank_exists <- reactive({
  !is.na(unique(rank_bar_data()$nivell))
})

output$nivell_rank_exists <- reactive({
  nivell_rank_exists()
})
outputOptions(output, "nivell_rank_exists", suspendWhenHidden = FALSE)



sexe_rank_exists <- reactive({
  !isTRUE(input$result_toggle) # Si el toggle és T les dades són estandarditzades ergo no existeix sexe, per això el !
})

output$sexe_rank_exists <- reactive({
  sexe_rank_exists()
})
outputOptions(output, "sexe_rank_exists", suspendWhenHidden = FALSE)

edat_rank_exists <- reactive({
  !isTRUE(input$result_toggle) # Same logic as sexe_rank_exists
})

output$edat_rank_exists <- reactive({
  edat_rank_exists()
})
outputOptions(output, "edat_rank_exists", suspendWhenHidden = FALSE)

# Create reactive values to store the subtitle states, això permet que al canviar les seleccions de l'esquerra NOMÉS s'actualitzin definicions, títols i subtítols quan es prem el botó.
#subtitles <- reactiveValues(def_text_ranquing = NULL, titol_comparador = NULL, subtitol_comparador = NULL, subtitol_comparador2 = NULL, nivell_comparador = NULL)

# Update subtitles when the refresh_button is clicked
#observeEvent(input$refresh_button, {

# Update def_text_ranquing
output$def_text_ranquing <- renderText({
  def_data <- def_data_ranquing() # Cache reactive
  
  # Handle missing values with defaults
  indicador <- unique(def_data$indicador) %||% "N/A"
  definicio <- unique(def_data$definicio) %||% "No definition available"
  num <- unique(def_data$num) %||% "Not specified"
  den <- unique(def_data$den) %||% "Not specified"
  formula <- unique(def_data$formula) %||% "Not specified"
  unitat <- unique(def_data$unitat) %||% "Not specified"
  interpretacio <- unique(def_data$int) %||% "Not specified"
  font <- unique(def_data$font) %||% "Not specified"
  justificacio <- unique(def_data$justificacio) %||% "Not specified"
  exclusions <- unique(def_data$exclusions) %||% "Not specified"
  limitacions <- unique(def_data$limitacions) %||% "Not specified"
  criteris_tecnics <- unique(def_data$criteris_tecnics) %||% "Not specified"
  
  HTML(paste(
    sprintf(
      "<div style='text-align: left;'>
      <b>%s</b> <br> %s <br> <br> <br>
      <b>Numerador:</b> %s <br> <br>
      <b>Denominador:</b> %s <br> <br>
      <b>Fórmula:</b> %s <br> <br>
      <b>Unitat:</b> %s <br> <br>
      <b>Interpretació:</b> %s <br> <br> 
      <b>Font:</b> %s <br> <br>
      <b>Justificació:</b> %s <br> <br> 
      <b>Exclusions:</b> %s <br> <br>
      <b>Limitacions:</b> %s <br> <br>
      <b>Criteris tècnics:</b> <br> %s
      </div>",
      indicador, definicio, num, den, formula, unitat, interpretacio, font, justificacio, exclusions, limitacions, criteris_tecnics
    ),
    collapse = "<br><br>"
  ))
})

output$titol_comparador <- renderUI({
  if (isTRUE(input$result_toggle)) {
    HTML(paste0("<strong>", input$select_indicador_ranquing, " (Raó O/E)", "</strong>"))
    
  } else {
    HTML(paste0("<strong>", input$select_indicador_ranquing, " (", unique(rank_bar_data()$mesura), ")", "</strong>"))
    
  }
  
})

output$titol_comparador_taula <- renderUI({
  
  if (isTRUE(input$result_toggle)) {
    HTML(paste0("<strong>", input$select_indicador_ranquing, " (Raó O/E)", "</strong>"))
    
  } else {
    HTML(paste0("<strong>", input$select_indicador_ranquing, " (", unique(rank_bar_data()$mesura), ")", "</strong>"))
    
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

# Update subtitol_comparador2


output$subtitol_comparador2 <- renderText({ 
  
  if (sexe_rank_exists()) {
    
    HTML(paste0("Sexe: ", input$selector_sexe_rank))
    
  } else {
    
    NULL
    
  }
  
})

output$subtitol_comparador2_taula <- renderText({
  
  if (sexe_rank_exists()) {
    
    HTML(paste0("Sexe: ", input$selector_sexe_rank))
    
  } else {
    
    NULL
    
  }
  
})

output$subtitol_comparador_edat <- renderText({ 
  if (edat_rank_exists() && !is.null(input$selector_edat_rank)) {
    HTML(paste0("Grup d'edat: ", input$selector_edat_rank))
  } else {
    NULL
  }
})

output$subtitol_comparador_edat_taula <- renderText({ 
  if (edat_rank_exists() && !is.null(input$selector_edat_rank)) {
    HTML(paste0("Grup d'edat: ", input$selector_edat_rank))
  } else {
    NULL
  }
})

#output$subtitol_comparador3 <- renderText({
#  
#  HTML(paste0(
#    "Resultats ordenats segons <b>", selected_values$geography, "</b>", 
#    " i comparats amb <b>", selected_values$center, "</b>", 
#    " (", input$select_any_ranquing, ")"
#  ))
#  
#})

#output$subtitol_comparador3_taula <- renderText({
#  
#  HTML(paste0(
#    "Resultats ordenats segons <b>", selected_values$geography, "</b>", 
#    #" i comparats amb <b>", selected_values$center, "</b>", 
#    " (", input$select_any_ranquing, ")"
#  ))
#  
#})

# Update nivell_comparador

output$nivell_comparador <- renderText({ 
  
  if (nivell_rank_exists()) {
    
    HTML(paste0("Nivell hospitalari: ", unique(rank_bar_data()$nivell)))
    
  } else {
    
    NULL
    
  }
  
})

output$nivell_comparador_taula <- renderText({ 
  
  if (nivell_rank_exists()) {
    
    HTML(paste0("Nivell hospitalari: ", unique(rank_bar_data()$nivell)))
    
  } else {
    
    NULL
    
  }
  
})


output$cat_checkbox_ui <- renderUI({
  # Only show the checkbox when result_toggle is FALSE
  if (!isTRUE(input$result_toggle)) {
    awesomeCheckbox("selector_cat_ranquing", 
                    p("Mostra el resultat global de Catalunya"), 
                    value = TRUE)
  }
})

# Add this observer to handle the PNG download button click
#observeEvent(input$download_png_chart, {
#  # Define the download options
#  download_options <- list(
#    selector = '.js-plotly-plot',
#    format = 'png',
#    filename = 'grafic_variabilitat',
#    scale = 3
#  )
#  
#  # Send the message directly to the client
#  session$sendCustomMessage(type = "clickPlotlyDownload", message = download_options)
#})

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

# Modify the comparador_chart function
comparador_chart <- function() {
  # If no data is available for that period, plot a message saying plot is not available
  if (is.data.frame(rank_bar_data()) && nrow(rank_bar_data()) == 0) {
    plot_nodata()
  } else { # If data is available, then plot it
    
    # Coloring based on if significantly different from comparator
    rank_bar_data <- rank_bar_data() %>% 
      mutate(color_pal = case_when(invers == 2 & resultat != comp_value ~ '#999966',
                                   is.na(comp_value) | is.na(resultat) | resultat == 0 ~ '#999966',
                                   resultat > comp_value & invers == 0 ~ '#91BFDB',
                                   resultat > comp_value & invers == 1 ~ '#FC8D59',
                                   resultat < comp_value & invers == 1 ~ '#91BFDB',
                                   resultat < comp_value & invers == 0 ~ '#FC8D59', 
                                   resultat == comp_value  ~ '#ccccff',
                                   TRUE ~ 'pink'),
             int_hover = case_when(color_pal == '#999966' ~ "Indicador sense interpretació",
                                   color_pal == '#91BFDB' ~ "Resultat relativament millor que la referència",
                                   color_pal == '#FC8D59' ~ "Resultat relativament pitjor que la referència",
                                   color_pal == '#ccccff' ~ "Resultat igual al de la referència",
                                   TRUE ~ "NA")
      )
    
    # Text for tooltip
    tooltip_bar <- if (isTRUE(input$result_toggle)) {
      paste0(rank_bar_data()$`Centre/Territori`, ": ", "<b>", round(rank_bar_data()$resultat, 2), "</b> ",  rank_bar_data()$ic, " (Raó O/E)",
             "<br>", selected_values$center, ": ", "<b>", round(rank_bar_data()$comp_value, 2), "</b> ",  rank_bar_data()$comp_ic, " (Raó O/E)",
             "<br>", rank_bar_data$int_hover)
      
      
    } else {
      paste0(#r_rank_bar_data()$Granularitat, ": ", 
        rank_bar_data()$`Centre/Territori`, ": ", "<b>", round(rank_bar_data()$resultat, 2), "</b> (", rank_bar_data()$mesura, ")",
        "<br>", selected_values$center, ": ", "<b>", round(rank_bar_data()$comp_value, 2), "</b> (", rank_bar_data()$mesura, ")",
        "<br>", rank_bar_data$int_hover)
      
    }
    
    
    
    
    # Creating a vector with the area names in the order they are going to be plotted
    order_areas <- as.vector(rank_bar_data()$`Centre/Territori`)
    
    # Determine the type of chart based on the number of bars
    if (nrow(rank_bar_data()) > 80) {
      # Vertical bar chart
      rank_plot <- plot_ly(data = rank_bar_data, height = 400) %>%
        add_trace(x = ~`Centre/Territori`, 
                  y = ~comp_value, 
                  type = 'scatter', 
                  mode = 'lines+markers',
                  text = ~paste0(selected_values$center, ": <b>", round(comp_value, 2), "</b> (", mesura, ")"),
                  line = list(color = '#ccccff',
                              width = 5),
                  marker = list(color = '#ccccff',
                                size = 5),
                  showlegend = F, 
                  hoverinfo = "text") %>%
        add_bars(x = ~`Centre/Territori`, y = ~resultat, text = tooltip_bar, textposition = "none", hoverinfo = "text",
                 marker = list(color = ~color_pal)) %>%
        layout(
          yaxis = list(title = list(text = unique(rank_bar_data()$mesura), standoff = 10), 
                       titlefont = list(size = 14), 
                       tickfont = list(size = 14, family = "sans-serif"), fixedrange = TRUE),
          xaxis = list(title = list(text = unique(rank_bar_data()$Granularitat)), tickvals = list(), ticktext = list(), fixedrange = TRUE,
                       tickfont = list(size = 12, family = "sans-serif"), 
                       categoryorder = "array", 
                       categoryarray = order_areas),
          font = font_plots,
          hovermode = 'false',
          legend = list(orientation = "h", y = 1.2, x = 0.5, xanchor = "center", yanchor = "top")
        )
      
    } else {
      # Horizontal bar chart with dynamic height
      base_height <- 500
      additional_height_per_bar <- 20
      plot_height <- base_height + (nrow(rank_bar_data()) - 20) * additional_height_per_bar
      
      rank_plot <- plot_ly(data = rank_bar_data, height = plot_height) %>%
        add_trace(y = ~`Centre/Territori`, 
                  x = ~comp_value, 
                  type = 'scatter', 
                  mode = 'lines+markers',
                  text = ~paste0(selected_values$center, ": <b>", round(comp_value, 2), "</b> (", mesura, ")"),
                  line = list(color = '#ccccff',
                              width = 3),
                  marker = list(color = '#ccccff',
                                size = 3),
                  showlegend = F, 
                  hoverinfo = "text") %>%
        add_bars(y = ~`Centre/Territori`, x = ~resultat, text = tooltip_bar, textposition = "none", hoverinfo = "text",
                 marker = list(color = ~color_pal)) %>%
        layout(
          separators = ",.",     
          xaxis = list(title = list(text = unique(rank_bar_data()$mesura), standoff = 10), 
                       titlefont = list(size = 14), 
                       tickfont = list(size = 14, family = "sans-serif"), fixedrange = TRUE),
          yaxis = list(title = "", tickangle = 0, fixedrange = TRUE,
                       tickfont = list(size = 12, family = "sans-serif"), 
                       categoryorder = "array", 
                       categoryarray = order_areas),
          font = font_plots,
          margin = list(pad = 10),
          hovermode = 'false',
          legend = list(orientation = "h", y = 1.2, x = 0.5, xanchor = "center", yanchor = "top")
        )
    }
    
    # Different behavior based on result_toggle
    if (isTRUE(input$result_toggle)) {
      # For standardized results (result_toggle = TRUE)
      # Always add a reference line at value 1
      if (nrow(rank_bar_data()) <= 80) {
        rank_plot <- rank_plot %>%
          add_trace(y = ~`Centre/Territori`, 
                    x = ~comp_value, 
                    type = 'scatter', 
                    mode = 'lines+markers',
                    text = ~paste0(selected_values$center, ": <b>", round(comp_value, 2), "</b> (Raó O/E)"),
                    line = list(color = '#ccccff',
                                width = 3),
                    marker = list(color = '#ccccff',
                                  size = 3),
                    showlegend = F, 
                    hoverinfo = "text") %>%
          add_trace(y = ~`Centre/Territori`,
                    x = rep(1, length(order_areas)),
                    type = 'scatter',
                    mode = 'lines',
                    text = "Catalunya: <b>1</b>",
                    hoverinfo = "text",
                    line = list(color = 'red', width = 3),
                    showlegend = FALSE)
      } else {
        rank_plot <- rank_plot %>%
          add_trace(x = ~`Centre/Territori`, 
                    y = ~comp_value, 
                    type = 'scatter', 
                    mode = 'lines+markers',
                    text = ~paste0(selected_values$center, ": <b>", round(comp_value, 2), "</b> (Raó O/E)"),
                    line = list(color = '#ccccff',
                                width = 5),
                    marker = list(color = '#ccccff',
                                  size = 5),
                    showlegend = F, 
                    hoverinfo = "text") %>%
          add_trace(x = ~`Centre/Territori`,
                    y = rep(1, length(order_areas)),
                    type = 'scatter',
                    mode = 'lines',
                    text = "Catalunya: <b>1</b>",
                    hoverinfo = "text",
                    line = list(color = 'red', width = 5),
                    showlegend = FALSE)
      }
    } else {
      # For raw results (result_toggle = FALSE)
      # Add the red line for mitjana only if checked
      if (input$selector_cat_ranquing == TRUE & (nrow(rank_bar_data()) <= 80)) {
        rank_plot <- rank_plot %>%
          add_trace(y = ~`Centre/Territori`,
                    x = ~mitjana,
                    type = 'scatter',
                    mode = 'lines+markers',
                    text = ~paste0("Catalunya: ", "<b>", round(mitjana, 2), "</b> (", mesura, ")"),
                    hoverinfo = "text",
                    line = list(color = 'red', width = 3),
                    marker = list(color = 'red', size = 3),
                    showlegend = FALSE)
        
      } else if (input$selector_cat_ranquing == TRUE & (nrow(rank_bar_data()) > 80)) {
        rank_plot <- rank_plot %>%
          add_trace(x = ~`Centre/Territori`,
                    y = ~mitjana,
                    type = 'scatter',
                    mode = 'lines+markers',
                    text = ~paste0("Catalunya: ", "<b>", round(mitjana, 2), "</b> (", mesura, ")"),
                    hoverinfo = "text",
                    line = list(color = 'red', width = 5),
                    marker = list(color = 'red', size = 5),
                    showlegend = FALSE)
      }
    }
    
    # Configure plot settings
    rank_plot %>% config(displayModeBar = TRUE, displaylogo = FALSE,
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
output$ranquing <- renderPlotly({   comparador_chart() }) 


output$rank_table <- DT::renderDataTable({
  req(rank_bar_data())
  
  data <- rank_bar_data()
  result_col <- if (isTRUE(input$result_toggle)) "resultat2" else "resultat"
  
  # Base columns to show
  cols_to_select <- c("Centre/Territori", result_col)
  
  # Add confidence interval when showing standardized results
  if (isTRUE(input$result_toggle)) {
    # Include ic_inferior and ic_superior columns to create a formatted IC column
    cols_to_select <- c(cols_to_select, "ic_inferior", "ic_superior")
  }
  
  # For raw results (result_toggle = FALSE), conditionally include mitjana
  if (!isTRUE(input$result_toggle) && isTRUE(input$selector_cat_ranquing)) {
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
  if (result_col == "resultat2") {
    table_data <- table_data %>% 
      rename("Resultat" = "resultat2")
    
    # Create a formatted IC column with proper decimal separator
    if ("ic_inferior" %in% colnames(table_data) && "ic_superior" %in% colnames(table_data)) {
      table_data <- table_data %>%
        mutate(IC = paste0("[", ic_inferior, " - ", ic_superior, "]")) %>%
        select(-ic_inferior, -ic_superior)
    }
  } else {
    table_data <- table_data %>% 
      rename("Resultat" = "resultat")
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
      scrollX = TRUE,
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
output$descarrega_dades_comparador <- downloadHandler(
  filename = function() { 
    paste(selected_values$center, "-variabilitat-", input$select_any_ranquing, ".xlsx", sep = "")
  },
  content = function(file) {
    # Prepare the data for download
    data_to_download_rank <- rank_bar_data() %>% 
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
    writexl::write_xlsx(data_to_download_rank, path = file)
  }
)


# Download as Excel
output$descarrega_dades_comparador_taula <- downloadHandler(
  filename = function() { 
    paste(selected_values$center, "-variabilitat-", input$select_any_ranquing, ".xlsx", sep = "")
  },
  content = function(file) {
    # Prepare the data for download
    data_to_download_rank <- rank_bar_data() %>% 
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
    writexl::write_xlsx(data_to_download_rank, path = file)
  }
)
