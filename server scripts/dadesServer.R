###############################################
#
# Server logic for data tab
#
###############################################



#####################################.      
#### Reactive data ----
#####################################. 

# Define reactive values for min and max years
#min_year <- reactive({ min(filter_table()$any) })  # Replace with your actual reactive value
#max_year <- reactive({ max(filter_table()$any) })  # Replace with your actual reactive value


# Render a placeholder for the slider in the UI
output$date_slider_ui <- renderUI({
  sliderInput("date_from", label = NULL, min = min_year, 
              max = max_year, value = c(min_year, max_year), 
              step = 1, sep = "", round = TRUE, ticks = TRUE, dragRange = FALSE)
})

###############################################.
## Reactive filters ----
###############################################.

dades_download = dades_total
#to clear choices when boxes are unticked/radio button is changed

observeEvent(input$rs=="FALSE", {#for RSs
  updateSelectizeInput(session, "rs_true", label = NULL, 
                       choices = rs, selected = character(0), 
                       options = list(placeholder = "Tria o escriu una o més regions sanitàries")) 
  # Also uncheck the select all checkbox
  updateAwesomeCheckbox(session, "select_all_rs", value = FALSE)
})

observeEvent(input$aga=="FALSE", { #for AGAs
  updateSelectizeInput(session, "aga_true", label = NULL,
                       choices = aga, selected = character(0),
                       options = list(placeholder = "Tria o escriu una o més AGAs")) 
  # Also uncheck the select all checkbox
  updateAwesomeCheckbox(session, "select_all_aga", value = FALSE)
})

observeEvent(input$abs=="FALSE", { #for ABSs
  updateSelectizeInput(session, "abs_true", label = NULL,
                       choices = abs, selected = character(0),
                       options = list(placeholder = "Tria o escriu una o més ABSs")) 
  # Also uncheck the select all checkbox
  updateAwesomeCheckbox(session, "select_all_abs", value = FALSE)
})

observeEvent(input$centre=="FALSE", { #for centres
  updateSelectizeInput(session, "centre_true", label = NULL,
                       choices = centre, selected = character(0), options = 
                         list(placeholder = "Tria o escriu un o més centres"))
  # Also uncheck the select all checkbox
  updateAwesomeCheckbox(session, "select_all_centre", value = FALSE)
})


observeEvent(input$product_filter, { # for indicator/topic/profile filters
  updateSelectizeInput(session,"indicator_filter", label = NULL,
                       choices = indicadors, selected = character(0), options = 
                         list(maxOptions = 1000,
                              placeholder = "Escriu o selecciona els indicadors que vulguis mostrar"))
  
  updateSelectizeInput(session,"ambit_filter", label = NULL, 
                       choices = ambits, selected = NULL, options = 
                         list(maxOptions = 1000, 
                              placeholder = "Escriu o selecciona els àmbits que vulguis mostrar"))
  
  updateSelectizeInput(session,"dimensio_filter", label = NULL, 
                       choices = dimensions, selected = NULL, options = 
                         list(maxOptions = 1000, 
                              placeholder = "Escriu o selecciona les dimensions que vulguis mostrar"))
})

#Clearing all user inputs to default
observeEvent(input$clear, {
  
  updateCheckboxInput(session, "rs", label = NULL, value = FALSE)
  updateSelectInput(session, "rs_true", label = NULL,
                    choices = rs, selected = character(0))
  updateAwesomeCheckbox(session, "select_all_rs", value = FALSE)
  
  updateCheckboxInput(session, "aga", label = NULL, value = FALSE)
  updateSelectInput(session, "aga_true", label = NULL,
                    choices = aga, selected = character(0))
  updateAwesomeCheckbox(session, "select_all_aga", value = FALSE)
  
  updateCheckboxInput(session, "abs", label = NULL, value = FALSE)
  updateSelectInput(session, "abs_true", label = NULL,
                    choices = abs, selected = character(0))
  updateAwesomeCheckbox(session, "select_all_abs", value = FALSE)
  
  updateCheckboxInput(session, "centre", label = NULL, value = FALSE)
  updateSelectInput(session, "centre_true", label = NULL,
                    choices = centre, selected = character(0))
  updateAwesomeCheckbox(session, "select_all_centre", value = FALSE)
  
  updateCheckboxInput(session, "catalunya", label = NULL, value = FALSE)
  updateCheckboxInput(session, "all_geo", label = NULL, value = FALSE)
  updateSliderInput(session, "date_from", label = NULL, value = c(min_year,max_year),
                    min = min_year, max = max_year, step = 1)
  updateSelectizeInput(session,"indicator_filter", label = NULL,
                       choices = indicadors, selected = character(0),
                       options = list(maxOptions = 1000, placeholder = "Escriu o selecciona els indicadors que vulguis mostrar"))
  updateSelectizeInput(session,"ambit_filter", label = NULL, choices = ambits, selected = NULL,
                       options = list(maxOptions = 1000, placeholder = "Escriu o selecciona els àmbits que vulguis mostrar"))
  updateSelectizeInput(session,"dimensio_filter", label = NULL, choices = dimensions, selected = NULL,
                       options = list(maxOptions = 1000, placeholder = "Escriu o selecciona les dimensions que vulguis mostrar"))
  updateAwesomeRadio(session,"product_filter", label=NULL, choices = c("Indicador", "Àmbit", "Dimensió"), selected = NULL, inline = FALSE,
                     status = "primary", checkbox = TRUE)
  
})


###############################################.
## Select All Checkboxes ----
###############################################.

# Select all RS checkbox
observeEvent(input$select_all_rs, {
  if (input$select_all_rs) {
    # If checked, select all RS
    updateSelectizeInput(session, "rs_true", 
                         selected = rs,
                         choices = rs)
  } else {
    # If unchecked, clear selections
    updateSelectizeInput(session, "rs_true", 
                         selected = character(0),
                         choices = rs)
  }
})

# Select all AGA checkbox
observeEvent(input$select_all_aga, {
  if (input$select_all_aga) {
    updateSelectizeInput(session, "aga_true", 
                         selected = aga,
                         choices = aga)
  } else {
    updateSelectizeInput(session, "aga_true", 
                         selected = character(0),
                         choices = aga)
  }
})

# Select all ABS checkbox
observeEvent(input$select_all_abs, {
  if (input$select_all_abs) {
    updateSelectizeInput(session, "abs_true", 
                         selected = abs,
                         choices = abs)
  } else {
    updateSelectizeInput(session, "abs_true", 
                         selected = character(0),
                         choices = abs)
  }
})

# Select all centres checkbox
observeEvent(input$select_all_centre, {
  if (input$select_all_centre) {
    updateSelectizeInput(session, "centre_true", 
                         selected = centre,
                         choices = centre)
  } else {
    updateSelectizeInput(session, "centre_true", 
                         selected = character(0),
                         choices = centre)
  }
})



###############################################.
## Reactive data ----
###############################################.

filter_table <- reactive ({
  
  # Convert input values to variables that DuckDB can understand
  date_start <- as.numeric(input$date_from[1])
  date_end <- as.numeric(input$date_from[2])
  
  if (is.null(input$indicator_filter) & is.null(input$ambit_filter) & is.null(input$dimensio_filter)) {
    # if no data selected create empty dataset to avoid app crashing
    table <- data.frame(ambit = factor(), dimensio = factor(), `Centre/Territori` = factor(), 
                        Granularitat = factor(), nom_indicador = factor(), 
                        any = double(), 
                        sexe = factor(), grup_edat = factor(), 
                        r = double(), mitjana=double(), unitats = character())
    
    #if list of indicators selected
  } else {
    if (!is.null(input$indicator_filter)) { #if indicator selected
      if (input$all_geo == TRUE) {
        
        filtered_geos <- dades_total %>%  
          filter(any >= !!date_start & any <= !!date_end & 
                   nom_indicador %in% !!input$indicator_filter)
        
      } else {
        
        filtered_geo <- dades_total %>% 
          filter(
            (`Centre/Territori` %in% !!input$rs_true & Granularitat == "Regió Sanitària")|
              (`Centre/Territori` %in% !!input$aga_true & Granularitat == "Àrea de Gestió Assistencial")|
              (`Centre/Territori` %in% !!input$abs_true & Granularitat == "Àrea Bàsica de Salut")|
              (`Centre/Territori` %in% !!input$centre_true & Granularitat == "Centre (Unitat proveïdora)")) %>% 
          filter(any >= !!date_start & any <= !!date_end & 
                   nom_indicador %in% !!input$indicator_filter)
        
        filtered_geo2 <- if (input$catalunya == TRUE) {
          dades_total %>% filter(`Centre/Territori` == "Catalunya" &
                                   any >= !!date_start & any <= !!date_end &
                                   nom_indicador %in% !!input$indicator_filter)
        } else {
          NULL
        }
        
        if (!is.null(filtered_geo2)) {
          filtered_geos <- union_all(filtered_geo, filtered_geo2)
        } else {
          filtered_geos <- filtered_geo
        }
      }
      
      #if list of ambits selected
    } else if (!is.null(input$ambit_filter)) { 
      
      if (input$all_geo == TRUE) {
        
        filtered_geos <- dades_total %>%  
          filter(any >= !!date_start & any <= !!date_end & 
                   ambit %in% !!input$ambit_filter)
        
      } else {
        
        filtered_geo <- dades_total %>% 
          filter(
            (`Centre/Territori` %in% !!input$rs_true & Granularitat == "Regió Sanitària")|
              (`Centre/Territori` %in% !!input$aga_true & Granularitat == "Àrea de Gestió Assistencial")|
              (`Centre/Territori` %in% !!input$abs_true & Granularitat == "Àrea Bàsica de Salut")|
              (`Centre/Territori` %in% !!input$centre_true & Granularitat == "Centre (Unitat proveïdora)")) %>% 
          filter(any >= !!date_start & any <= !!date_end &
                   ambit %in% !!input$ambit_filter)
        
        filtered_geo2 <- if (input$catalunya == TRUE) {
          dades_total %>% 
            filter(`Centre/Territori` == "Catalunya" &
                     any >= !!date_start & any <= !!date_end &
                     ambit %in% !!input$ambit_filter)
        } else {
          NULL
        }
        
        if (!is.null(filtered_geo2)) {
          filtered_geos <- union_all(filtered_geo, filtered_geo2)
        } else {
          filtered_geos <- filtered_geo
        }
      }
      
    } else { #dimensio filter
      
      if (input$all_geo == TRUE) {
        
        filtered_geos <- dades_total %>%  
          filter(any >= !!date_start & any <= !!date_end &
                   dimensio %in% !!input$dimensio_filter)
        
      } else {
        
        filtered_geo <- dades_total %>% 
          filter((`Centre/Territori` %in% !!input$rs_true & Granularitat == "Regió Sanitària")|
                   (`Centre/Territori` %in% !!input$aga_true & Granularitat == "Àrea de Gestió Assistencial")|
                   (`Centre/Territori` %in% !!input$abs_true & Granularitat == "Àrea Bàsica de Salut")|
                   (`Centre/Territori` %in% !!input$centre_true & Granularitat == "Centre (Unitat proveïdora)")) %>% 
          filter(any >= !!date_start & any <= !!date_end &
                   dimensio %in% !!input$dimensio_filter)
        
        filtered_geo2 <- if (input$catalunya == TRUE) {
          dades_total %>% 
            filter(`Centre/Territori` == "Catalunya" &
                     any >= !!date_start & any <= !!date_end &
                     dimensio %in% !!input$dimensio_filter)
        } else {
          NULL
        }
        
        if (!is.null(filtered_geo2)) {
          filtered_geos <- union_all(filtered_geo, filtered_geo2)
        } else {
          filtered_geos <- filtered_geo
        }
      } 
    }
    
    # Convert to data frame with collect() and select the columns
    table <- filtered_geos %>% 
      select(ambit, dimensio, `Centre/Territori`, Granularitat, nom_indicador, 
             sexe, grup_edat, any, r, mitjana, unitats) %>%
      collect()
    
  }
  
  return(table)
})



###############################################.
## Table ----
###############################################.

###############################################.
## Table ----
###############################################.

# Display table using DT instead of reactable
# Display table using DT instead of reactable
###############################################.
## Table ----
###############################################.

# Display table using DT with column visibility controls
output$table_filtered <- renderDT({
  
  datatable(
    filter_table(),
    filter = 'top',  # Enables column-specific filtering at the top
    rownames = FALSE,
    extensions = 'Buttons',  # Add Buttons extension
    options = list(
      pageLength = 25,  # Number of rows per page
      scrollX = TRUE,   # Enable horizontal scrolling
      autoWidth = TRUE,
      # Configure buttons including column visibility
      dom = 'Bfrtip',  # B = buttons, f = filter, r = processing, t = table, i = info, p = pagination
      buttons = list(
        list(
          extend = 'colvis',
          text = 'Mostra/amaga columnes',
          columns = ':visible',  # Apply to all visible columns
          className = 'btn-link dt-button-link'  # Add custom CSS class
        )
      ),
      language = list(
        info = "Mostrant _START_ a _END_ de _TOTAL_ files",
        paginate = list(previous = "Anterior", `next` = "Següent"),
        search = "Cerca:",
        lengthMenu = "Mostra _MENU_ files",
        zeroRecords = "La selecció actual no permet mostrar cap dada. Recorda seleccionar quelcom al Pas 1 i Pas 2 per poder veure la taula.",
        buttons = list(
          colvis = "Columnes",
          colvisRestore = "Restaura columnes"
        )
      ),
      # Add initialization callback to apply custom styling
      initComplete = JS(
        "function(settings, json) {",
        "  // Style the column visibility button to look like actionLink",
        "  $('.dt-button-link').css({",
        "    'background': 'none',",
        "    'border': 'none',",
        "    'color': '#007bff',", # Bootstrap primary color
        "    'text-decoration': 'underline',",
        "    'cursor': 'pointer',",
        "    'padding': '0',",
        "    'font-size': 'inherit',",
        "    'font-family': 'inherit'",
        "  });",
        "  // Add hover effect",
        "  $('.dt-button-link').hover(",
        "    function() { $(this).css('color', '#0056b3'); },", # Darker on hover
        "    function() { $(this).css('color', '#007bff'); }",   # Original color
        "  );",
        "}"
      )
    ),
    colnames = c(
      "Àmbit", 
      "Dimensió", 
      "Centre/Territori", 
      "Granularitat", 
      "Indicador", 
      "Sexe", 
      "Grup d'edat", 
      "Any", 
      "Resultat", 
      "Catalunya", 
      "Mesura"
    ),
    class = 'cell-border stripe hover'  # Styling classes
  )
  
})

#print(filter_table())

# Downloading data in csv format
table_csv <- reactive({ format_csv(filter_table()) })
table_csv_total <- reactive({ format_csv(dades_download) })


###############################################.
## Downloads ----
###############################################.
# 1. Download filtered data as Excel
output$download_table_excel <- downloadHandler(
  filename = "dades_CentralDeResultats.xlsx",
  content = function(file) {
    data_to_download_seleccio <- table_csv() %>%
      select(
        ambit,
        dimensio,
        `Centre/Territori`,
        Granularitat,
        nom_indicador,
        sexe,
        grup_edat,
        any,
        r,
        mitjana,
        unitats
      )
    
    
    # Write to Excel
    writexl::write_xlsx(data_to_download_seleccio, path = file)
  }
)

# 2. Descàrrega totes les dades en CSV
output$download_all_csv <- downloadHandler(
  filename <- "dades_CentralDeResultats.csv",
  content <- function(file) {
    file.copy("datasets/dades_CentralDeResultats.csv", file)
  },
  contentType = "text/csv"
)


##END