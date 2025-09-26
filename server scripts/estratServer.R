# Estratificació server

observeEvent(input$ajuda_estrat, {
  
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



# Reactive: Filtered Data for Estratificació
filtered_sexe_data <- reactive({
  
  # Check if subtipologia filter is needed
  #is_subtipologia_filtered <- length(unique(dades_tbl$id_subtipologia[dades_tbl$indicador == input$select_indicador_estrat & dades_tbl$ambit == input$select_ambit_estrat])) > 1
  
  #is_subtipologia_filtered <- length(unique(dades_tbl %>%
  #                                            filter(indicador == input$select_indicador_estrat & (ambit == input$select_ambit_estrat |
  #                                                                                                   tag1 == input$select_ambit_estrat |
  #                                                                                                   tag2 == input$select_ambit_estrat |
  #                                                                                                   tag3 == input$select_ambit_estrat)) %>%
  #                                            pull(id_subtipologia))) > 1
  
  #print(is_subtipologia_filtered)
  
  data <- dades_tbl %>% filter(grup_edat == "Total")  # Your main dataset
  
  # Debugging: Check initial data
  #print("Initial Data:")
  #print(head(data))
  
  # Filtering by Granularitat
  data <- data %>% filter(Granularitat == selected_values$geography)
  
  # Filtering by Centre/Territori
  data <- data %>% filter(`Centre/Territori` == input$center)
  
  # Filter by Ambit
  data <- data %>% filter((ambit == selected_values$ambit |
                             #tag1 == selected_values$ambit |
                             #tag2 == selected_values$ambit |
                             etiqueta == selected_values$ambit),
                          any >= !!input$year_range_estrat[1],  # Inject the first year as a scalar
                          any <= !!input$year_range_estrat[2],
                          nom_indicador == input$select_indicador_estrat)
  #print(paste("Filtered by Indicador:", input$select_indicador_estrat))
  
  # Debugging: Check filtered data
  #print("Filtered Data:")
  #print(head(data))
  
  # Collect the data into a local data frame
  data <- data %>% collect()
  
  return(data)
})



filtered_edat_data <- reactive({
  
  # Check if subtipologia filter is needed
  #is_subtipologia_filtered <- length(unique(dades_tbl$id_subtipologia[dades_tbl$indicador == input$select_indicador_estrat & dades_tbl$ambit == input$select_ambit_estrat])) > 1
  
  #is_subtipologia_filtered <- length(unique(dades_tbl %>%
  #                                            filter(indicador == input$select_indicador_estrat & (ambit == input$select_ambit_estrat |
  #                                                                                                   tag1 == input$select_ambit_estrat |
  #                                                                                                   tag2 == input$select_ambit_estrat |
  #                                                                                                   tag3 == input$select_ambit_estrat)) %>%
  #                                            pull(id_subtipologia))) > 1
  
  #print(is_subtipologia_filtered)
  
  data <- dades_tbl %>% filter(sexe == "Total") # Your main dataset
  
  # Debugging: Check initial data
  #print("Initial Data:")
  #print(head(data))
  
  # Filtering by Granularitat
  data <- data %>% filter(Granularitat == selected_values$geography)
  
  # Filtering by Centre/Territori
  data <- data %>% filter(`Centre/Territori` == input$center)
  
  # Filter by Ambit
  data <- data %>% filter((ambit == selected_values$ambit |
                             #tag1 == selected_values$ambit |
                             #tag2 == selected_values$ambit |
                             etiqueta == selected_values$ambit),
                          any >= !!input$year_range_estrat[1],  # Inject the first year as a scalar
                          any <= !!input$year_range_estrat[2],
                          nom_indicador == input$select_indicador_estrat)
  
  # Debugging: Check filtered data
  #print("Filtered Data:")
  #print(head(data))
  
  # Collect the data into a local data frame
  data <- data %>% collect()
  
  return(data)
})

output$select_ambit_estrat_ui <- renderUI({
  # Get unique `ambit` values for the selected center
  
  ambit <- dades_tbl %>%
    filter(`Centre/Territori` == selected_values$center) %>%
    pull(ambit) %>%
    as.character() %>%  # Convert factor to character to ensure correct display
    unique() %>%
    sort()
  
  etiqueta <- dades_tbl %>%
    filter(`Centre/Territori` == selected_values$center) %>%
    select(etiqueta) %>% 
    #select(tag1, tag2, tag3) %>%
    pivot_longer(cols = everything(), names_to = "tag", values_to = "value") %>%
    filter(!is.na(value)) %>%
    distinct(value) %>%
    collect() %>%
    pull(value)
  
  
  # Combine into a named list for grouping
  grouped_choices_estrat <- list(
    "Àmbit" = as.list(ambit),
    "Etiqueta" = as.list(etiqueta)
  )
  
  
  div(#style = "margin-top: 20px; margin-bottom: 20px;",
      selectInput("select_ambit_estrat", 
                  label = NULL,
                  #shiny::HTML("<p class='step-text'>Pas 1: Tria un àmbit o etiqueta</p>"), 
                  choices = grouped_choices_estrat, selected = selected_values$ambit)
  )
})


output$select_indicador_estrat_ui <- renderUI({
  # Filter and create a distinct subset
  dades_tbl2 <- dades_tbl %>%
    distinct(nom_indicador, dimensio, ambit, 
             #tag1, tag2, 
             etiqueta, Granularitat, `Centre/Territori`) %>%
    filter(
      Granularitat == selected_values$geography,
      `Centre/Territori` == selected_values$center,
      (ambit == selected_values$ambit |
         #tag1 == selected_values$ambit |
         #tag2 == selected_values$ambit |
         etiqueta == selected_values$ambit)#,
      #any >= !!input$year_range_estrat[1],  # Inject the first year as a scalar
      #any <= !!input$year_range_estrat[2],  # Inject the second year as a scalar
    ) %>%
    mutate(codi = nom_indicador) %>%
    collect()  # Collect the results
  
  # Generate the choices, grouping by `dimensio`
  ind_choices_estrat <- dades_tbl2 %>%
    split(.$dimensio) %>%
    lapply(function(x) {
      i <- sort(x$nom_indicador)
      names(i) <- i  # Set display names for the indicators
      i
    })
  
  # Render the select input for the indicator
  div(
    selectInput("select_indicador_estrat", 
                label = NULL,
                #shiny::HTML("<p class='step-text'>Pas 2: Tria un indicador</p>"), 
                choices = ind_choices_estrat,
                selected = selected_values$indicador)
  )
})


year_range_reactive_estrat <- reactive({
  req(input$select_indicador_estrat)  # Ensure the input is available
  dades_tbl %>%
    filter(nom_indicador == input$select_indicador_estrat) %>%
    summarise(
      min_year = min(any, na.rm = TRUE),
      max_year = max(any, na.rm = TRUE)
    ) %>%
    collect()
})


observeEvent(year_range_reactive_estrat(), {
  year_range <- year_range_reactive_estrat()
  updateSliderInput(
    session,
    inputId = "year_range_estrat",
    min = year_range$min_year,
    max = year_range$max_year,
    value = c(year_range$min_year, year_range$max_year)
  )
})


# Replace the current observers with this code
observe({
  # When checkbox changes in sexe tab, update both the global setting and edat tab's checkbox
  if (!is.null(input$toggle_y_axis_zero_sexe)) {
    chart_settings$y_axis_zero <- input$toggle_y_axis_zero_sexe
    updateAwesomeCheckbox(session, "toggle_y_axis_zero_edat", value = input$toggle_y_axis_zero_sexe)
  }
})

observe({
  # When checkbox changes in edat tab, update both the global setting and sexe tab's checkbox
  if (!is.null(input$toggle_y_axis_zero_edat)) {
    chart_settings$y_axis_zero <- input$toggle_y_axis_zero_edat
    updateAwesomeCheckbox(session, "toggle_y_axis_zero_sexe", value = input$toggle_y_axis_zero_edat)
  }
})

# Add this observer to initialize the checkboxes with the global setting when the server starts
observe({
  updateAwesomeCheckbox(session, "toggle_y_axis_zero_sexe", value = chart_settings$y_axis_zero)
  updateAwesomeCheckbox(session, "toggle_y_axis_zero_edat", value = chart_settings$y_axis_zero)
})

#output$select_subtipologia_estrat_ui <- renderUI({
#  # Filter to get `subtipologia` categories
#  subtipologia_categories <- dades_tbl %>%
#    filter(
#      (ambit == selected_values$ambit |
#         tag1 == selected_values$ambit |
#         tag2 == selected_values$ambit |
#         tag3 == selected_values$ambit),
#      indicador == selected_values$indicador,
#      `Centre/Territori` == selected_values$center
#    ) %>%
#    pull(subtipologia) %>%
#    unique()
#  
#  # Show the selector if there are valid subcategories
#  if (all(subtipologia_categories != "" & !is.na(subtipologia_categories))) {
#    div(
#      br(),
#      div(align = "left", tags$b(class = "step-text", "Pas 2b: Tria una subcategoria")),
#      div(align = "left",
#          selectizeInput("select_subtipologia_estrat", label = NULL,
#                         choices = subtipologia_categories, 
#                         selected = first(subtipologia_categories))
#      )
#    )
#  } else {
#    return(NULL)
#  }
#})


def_data_estrat <- reactive({
  req(input$select_indicador_estrat)
  docu_ambit = as.data.table(alfred)
  filtered_docu = subset(docu_ambit, input$select_indicador_estrat == name & (Àmbit == input$select_ambit_estrat #|
                                                                                         #tag1 == input$select_ambit_estrat |
                                                                                         #tag2 == input$select_ambit_estrat |
                                                                                         #Etiqueta == input$select_ambit_estrat
                                                                              ))
  return(filtered_docu)
})



output$def_text_estrat <- renderUI({
  def_data <- def_data_estrat() # Cache reactive
  
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


# Add these reactive functions to check if stratification variables exist
has_sexe_stratification <- reactive({
  req(selected_values$indicador, selected_values$ambit)
  
  # Check if any non-"Total" sexe values exist for this indicator
  sexe_check <- dades_tbl %>%
    filter(
      nom_indicador == selected_values$indicador,
      ambit == selected_values$ambit |
        etiqueta == selected_values$ambit
    ) %>%
    filter(sexe != "Total") %>%
    summarise(has_sexe = n() > 0) %>%
    pull(has_sexe)
  
  return(sexe_check)
})

has_edat_stratification <- reactive({
  req(selected_values$indicador, selected_values$ambit)
  
  # Check if any non-"Total" grup_edat values exist for this indicator
  edat_check <- dades_tbl %>%
    filter(
      nom_indicador == selected_values$indicador,
      ambit == selected_values$ambit |
        etiqueta == selected_values$ambit
    ) %>%
    filter(grup_edat != "Total") %>%
    summarise(has_edat = n() > 0) %>%
    pull(has_edat)
  
  return(edat_check)
})

# Make these available to the UI
output$has_sexe_stratification <- reactive({
  has_sexe_stratification()
})
outputOptions(output, "has_sexe_stratification", suspendWhenHidden = FALSE)

output$has_edat_stratification <- reactive({
  has_edat_stratification()
})
outputOptions(output, "has_edat_stratification", suspendWhenHidden = FALSE)


# Add a combined check for both stratifications
#output$has_any_stratification <- reactive({
#  has_sexe_stratification() || has_edat_stratification()
#})
#outputOptions(output, "has_any_stratification", suspendWhenHidden = FALSE)



################################################################################
# Creació del gràfic ----
################################################################################

# Títols

titol_exists_sexe <- reactive({
  !is.na(unique(filtered_sexe_data()$nom_indicador))
})

output$titol_exists_sexe <- reactive({
  titol_exists_sexe()
})
#
#outputOptions(output, "titol_exists", suspendWhenHidden = FALSE)
#
##output$titol_evolutiu <- renderText(paste0(input$select_indicador_evolutiu))
#
output$titol_sexe <- renderUI({
  
  if (titol_exists_sexe()) {
    HTML(paste0("<strong>", selected_values$indicador, " (", unique(filtered_sexe_data()$unitats), ")", "</strong>"))
    
  } else {
    NULL
  }
})

output$titol_sexe_taula <- renderUI({
  
  if (titol_exists_sexe()) {
    HTML(paste0("<strong>", selected_values$indicador, " (", unique(filtered_sexe_data()$unitats), ")", "</strong>"))
    
  } else {
    NULL
  }
})

output$subtitol_sexe <- renderText({
  
  if (titol_exists_sexe()) {
    selected_values$center
  } else
    NULL
})

output$subtitol_sexe_taula <- renderText({
  
  if (titol_exists_sexe()) {
    selected_values$center
  } else
    NULL
})

output$subtitol_sexe2 <- renderText({
  #mesura_text <- unique(filtered_sexe_data()$mesura)
  sexe_text <- "Estratificació segons sexe"

  if (titol_exists_sexe()) {
    paste0(#mesura_text, 
           sexe_text
    )
  } else
    NULL
})

output$subtitol_sexe2_taula <- renderText({
  #mesura_text <- unique(filtered_sexe_data()$mesura)
  sexe_text <- "Estratificació segons sexe"
  
  if (titol_exists_sexe()) {
    paste0(#mesura_text, 
           sexe_text
    )
  } else
    NULL
})


# Add this observer to handle the PNG download button click
#observeEvent(input$download_png_sexe, {
#  # Define the download options
#  download_options <- list(
#    selector = '.js-plotly-plot',
#    format = 'png',
#    filename = 'grafic_sexe',
#    scale = 3
#  )
#  
#  # Send the message directly to the client
#  session$sendCustomMessage(type = "clickPlotlyDownload", message = download_options)
#})



# Function: Plot the Estratificació Chart
plot_sexe_chart <- function() {
  
  # If no data available for that period, then plot a message saying data is missing
  if (is.data.frame(filtered_sexe_data()) && nrow(filtered_sexe_data()) == 0) {
    plot_nodata()
  } else { 
    # If data is available, then plot it
    
    req(filtered_sexe_data())
  
    # Modifying standard layout
    yaxis_plots[["title"]] <- paste0(unique(filtered_sexe_data()$unitats))
    yaxis_plots[["tickfont"]] <- list(size = 14, family = "sans-serif")
    yaxis_plots[["rangemode"]] <- ifelse(chart_settings$y_axis_zero, "tozero", "normal")
    xaxis_plots[["title"]] <- "Any"
    xaxis_plots[["tickangle"]] <- ifelse(max(nchar(as.character(filtered_sexe_data()$any))) > 7, -45, 0)
    xaxis_plots[["dtick"]] <- ifelse(length(unique(filtered_sexe_data()$any)) >= 10, 2, 1)
    xaxis_plots[["tickfont"]] <- list(size = 15, family = "sans-serif")
    
    
    tooltip_sexe <- c(paste0(#filtered_sexe_data()$`Centre/Territori`, 
                             filtered_sexe_data()$any, "<br>", 
                             paste0(unique(filtered_sexe_data()$unitats)), ": ", 
                             "<b>", round(filtered_sexe_data()$r, 2), "</b>", "<br>",
                             "Sexe: <b>", filtered_sexe_data()$sexe, "</b>"))
    
    
    
    evolutiu_sexe <- plot_ly(
      data=as.data.frame(filtered_sexe_data()),
      x = ~any,
      y = ~r,
      color = ~sexe,
      text = tooltip_sexe,
      #colors = trend_col,
      hoverinfo = "text",
      height = 400) %>% 
      add_trace(
        type = 'scatter', mode = 'lines+markers', 
        line = list(width = 3),  # Increase line thickness
        marker = list(size = 6)#,  # Increase marker size
        #symbol = ~`Centre/Territori`, symbols = symbols_trend
      ) %>% 
      layout(annotations = list(), #It needs this because of a buggy behaviour of Plotly
             separators = ",.",
             #margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots, font = font_plots,
             showlegend = TRUE,
             legend = list(orientation = 'h',
                           xanchor = 'center',
                           x = 0.5,
                           y = 1.1),
             margin = list(t = 5, b = 5))  %>%
      config(displayModeBar = T, displaylogo = F, # taking out plotly logo button
             modeBarButtonsToRemove = list("sendDataToCloud", "zoomIn2d", "zoomOut2d", 
                                           "autoScale2d", "resetScale2d", "toggleSpikelines", 
                                           "resetViews", "toggleHover", "hoverClosestCartesian", 
                                           "hoverCompareCartesian", "zoom2d", "pan2d", "select2d", 
                                           "lasso2d", "toggleHover"),# "toImage"),
             toImageButtonOptions = list(filename = "grafic_sexe", 
                                         format = "png",
                                         label = "Descarrega el gràfic en format PNG",
                                         scale = 3),
             locale = "ca"
      ) 
    
    
  }
}

output$sexe_plot <- renderPlotly({plot_sexe_chart()})  



titol_exists_edat <- reactive({
  !is.na(unique(filtered_edat_data()$nom_indicador))
})

output$titol_exists_edat <- reactive({
  titol_exists_edat()
})
#
#outputOptions(output, "titol_exists", suspendWhenHidden = FALSE)
#
##output$titol_evolutiu <- renderText(paste0(input$select_indicador_evolutiu))
#
output$titol_edat <- renderUI({
  
  if (titol_exists_edat()) {
    HTML(paste0("<strong>", selected_values$indicador, " (", unique(filtered_edat_data()$unitats), ")", "</strong>"))
    
  } else {
    NULL
  }
})

output$titol_edat_taula <- renderUI({
  
  if (titol_exists_edat()) {
    HTML(paste0("<strong>", selected_values$indicador, " (", unique(filtered_edat_data()$unitats), ")", "</strong>"))
    
  } else {
    NULL
  }
})

output$subtitol_edat <- renderText({
  
  if (titol_exists_edat()) {
    selected_values$center
  } else
    NULL
})

output$subtitol_edat_taula <- renderText({
  
  if (titol_exists_edat()) {
    selected_values$center
  } else
    NULL
})

output$subtitol_edat2 <- renderText({
  #mesura_text <- unique(filtered_edat_data()$mesura)
  edat_text <- " Estratificació segons grup d'edat"
  
  if (titol_exists_edat()) {
    paste0(#mesura_text, 
           edat_text
    )
  } else
    NULL
})

output$subtitol_edat2_taula <- renderText({
  #mesura_text <- unique(filtered_edat_data()$mesura)
  edat_text <- "Estratificació segons grup d'edat"
  
  if (titol_exists_edat()) {
    paste0(#mesura_text, 
           edat_text
    )
  } else
    NULL
})


plot_edat_chart <- function() {
  
  # If no data available for that period, then plot a message saying data is missing
  if (is.data.frame(filtered_edat_data()) && nrow(filtered_edat_data()) == 0) {
    plot_nodata()
  } else { 
    # If data is available, then plot it
    
    req(filtered_edat_data())
    
    # Modifying standard layout
    yaxis_plots[["title"]] <- paste0(unique(filtered_edat_data()$unitats))
    yaxis_plots[["tickfont"]] <- list(size = 14, family = "sans-serif")
    yaxis_plots[["rangemode"]] <- ifelse(chart_settings$y_axis_zero, "tozero", "normal")
    xaxis_plots[["title"]] <- "Any"
    xaxis_plots[["tickangle"]] <- ifelse(max(nchar(as.character(filtered_edat_data()$any))) > 7, -45, 0)
    xaxis_plots[["dtick"]] <- ifelse(length(unique(filtered_edat_data()$any)) >= 10, 2, 1)
    xaxis_plots[["tickfont"]] <- list(size = 15, family = "sans-serif")
    
    
    tooltip_edat <- c(paste0(#filtered_edat_data()$`Centre/Territori`, 
                             filtered_edat_data()$any, "<br>", 
                             paste0(unique(filtered_edat_data()$unitats)), ": ", 
                             "<b>", round(filtered_edat_data()$r, 2), "</b>", "<br>",
                             paste0("Grup d'edat: ", 
                             "<b>", filtered_edat_data()$grup_edat, "</b>")))
    
    
    
    evolutiu_edat <- plot_ly(
      data=as.data.frame(filtered_edat_data()),
      x = ~any,
      y = ~r,
      color = ~grup_edat,
      text = tooltip_edat,
      #colors = trend_col,
      hoverinfo = "text",
      height = 400) %>% 
      add_trace(
        type = 'scatter', mode = 'lines+markers', 
        line = list(width = 3),  # Increase line thickness
        marker = list(size = 6)#,  # Increase marker size
        #symbol = ~`Centre/Territori`, symbols = symbols_trend
      ) %>% 
      layout(annotations = list(), #It needs this because of a buggy behaviour of Plotly
             separators = ",.",
             #margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots, font = font_plots,
             showlegend = TRUE,
             legend = list(orientation = 'h',
                           xanchor = 'center',
                           x = 0.5,
                           y = 1.1),
             margin = list(t = 5, b = 5))  %>%
      config(displayModeBar = T, displaylogo = F, # taking out plotly logo button
             modeBarButtonsToRemove = list("sendDataToCloud", "zoomIn2d", "zoomOut2d", 
                                           "autoScale2d", "resetScale2d", "toggleSpikelines", 
                                           "resetViews", "toggleHover", "hoverClosestCartesian", 
                                           "hoverCompareCartesian", "zoom2d", "pan2d", "select2d", 
                                           "lasso2d", "toggleHover"),
             toImageButtonOptions = list(filename = "grafic_edat", 
                                         format = "png",
                                         label = "Descarrega el gràfic en format PNG",
                                         scale = 3),
             locale = "ca"
      ) 
    
    
  }
}

output$edat_plot <- renderPlotly({plot_edat_chart()})  


output$sexe_table <- DT::renderDataTable({
  req(filtered_sexe_data())
  
  data <- filtered_sexe_data()
  
  table_data <- data %>% 
    select(c(any, sexe, r)) %>% 
    pivot_wider(names_from = "sexe", values_from = "r") %>% 
    arrange(any) %>% 
    mutate(
      any = as.character(any),
      across(
        where(is.numeric), 
        ~ formatC(., format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
    ))
  
  DT::datatable(
    table_data,
    rownames = FALSE,
    options = list(
      paging = FALSE,
      searching = FALSE,
      srollX = TRUE,
      dom = 't'
    )
  )
})


output$edat_table <- DT::renderDataTable({
  req(filtered_sexe_data())
  
  data <- filtered_edat_data()
  
  table_data <- data %>% 
    select(c(any, grup_edat, r)) %>% 
    pivot_wider(names_from = "grup_edat", values_from = "r") %>% 
    mutate(
      any = as.character(any),
      across(
        where(is.numeric), 
        ~ formatC(., format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
      ))
  
  DT::datatable(
    table_data,
    rownames = FALSE,
    #extensions = "Buttons",
    options = list(
      paging = FALSE,
      searching = FALSE,
      srollX = TRUE,
      dom = 't'
      #dom = 'Bfrtip',
      #buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
})



output$descarrega_dades_sexe <- downloadHandler(
  filename = function() { 
    paste(selected_values$center, "-sexe-", input$select_any_ranquing, ".xlsx", sep = "")
  },
  content = function(file) {
    # Prepare the data for download
    data_to_download_sexe <- filtered_sexe_data() %>% 
      select(
        indicador, 
        centre_territori = `Centre/Territori`,
        any, 
        sexe,
        mesura, 
        resultat
      )
    
    # Write to Excel
    writexl::write_xlsx(data_to_download_sexe, path = file)
  }
)



output$descarrega_dades_sexe_taula <- downloadHandler(
  filename = function() { 
    paste(selected_values$center, "-sexe-", input$select_any_ranquing, ".xlsx", sep = "")
  },
  content = function(file) {
    # Prepare the data for download
    data_to_download_sexe <- filtered_sexe_data() %>% 
      select(
        indicador, 
        centre_territori = `Centre/Territori`,
        any, 
        sexe,
        mesura, 
        resultat
      )
    
    # Write to Excel
    writexl::write_xlsx(data_to_download_sexe, path = file)
  }
)



output$descarrega_dades_edat <- downloadHandler(
  filename = function() { 
    paste(selected_values$center, "-edat-", input$select_any_ranquing, ".xlsx", sep = "")
  },
  content = function(file) {
    # Prepare the data for download
    data_to_download_edat <- filtered_edat_data() %>% 
      select(
        indicador, 
        centre_territori = `Centre/Territori`,
        any, 
        grup_edat,
        mesura, 
        resultat
      )
    
    # Write to Excel
    writexl::write_xlsx(data_to_download_edat, path = file)
  }
)



output$descarrega_dades_edat_taula <- downloadHandler(
  filename = function() { 
    paste(selected_values$center, "-edat-", input$select_any_ranquing, ".xlsx", sep = "")
  },
  content = function(file) {
    # Prepare the data for download
    data_to_download_edat <- filtered_edat_data() %>% 
      select(
        indicador, 
        centre_territori = `Centre/Territori`,
        any, 
        grup_edat,
        mesura, 
        resultat
      )
    
    # Write to Excel
    writexl::write_xlsx(data_to_download_edat, path = file)
  }
)