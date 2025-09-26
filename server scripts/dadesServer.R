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


#  # Render a placeholder for the slider in the UI
#  output$date_slider_ui <- renderUI({
#    sliderInput("date_from", label = NULL, min = min_year, 
#                max = max_year, value = c(min_year, max_year), 
#                step = 1, sep = "", round = TRUE, ticks = TRUE, dragRange = FALSE)
#  })
#
################################################.
### Reactive filters ----
################################################.
#
#dades_download = dades
##to clear choices when boxes are unticked/radio button is changed
#
#observeEvent(input$rs=="FALSE", {#for RSs
#  updateSelectizeInput(session, "rs_true", label = NULL, 
#                       choices = rs, selected = character(0), 
#                       options = list(placeholder = "Tria o escriu una o més regions sanitàries")) 
#})
#
#observeEvent(input$aga=="FALSE", { #for AGAs
#  updateSelectizeInput(session, "aga_true", label = NULL,
#                       choices = aga, selected = character(0),
#                       options = list(placeholder = "Tria o escriu una o més AGAs")) 
#})
#
#observeEvent(input$abs=="FALSE", { #for ABSs
#  updateSelectizeInput(session, "abs_true", label = NULL,
#                       choices = abs, selected = character(0),
#                       options = list(placeholder = "Tria o escriu una o més ABSs")) 
#})
#
#observeEvent(input$centre=="FALSE", { #for centres
#  updateSelectizeInput(session, "centre_true", label = NULL,
#                       choices = centre, selected = character(0), options = 
#                         list(placeholder = "Tria o escriu un o més centres"))
#})
#
#
#observeEvent(input$centre=="FALSE", { #for UTs
#  updateSelectizeInput(session, "ut_true", label = NULL,
#                       choices = ut, selected = character(0), options = 
#                         list(placeholder = "Tria o escriu una o més UT"))
#})
#
#observeEvent(input$centre=="FALSE", { #for UTs
#  updateSelectizeInput(session, "usl_true", label = NULL,
#                       choices = usl, selected = character(0), options = 
#                         list(placeholder = "Tria o escriu una o més USL"))
#})
#
#
#observeEvent(input$product_filter, { # for indicator/topic/profile filters
#  updateSelectizeInput(session,"indicator_filter", label = NULL,
#                       choices = indicadors, selected = character(0), options = 
#                         list(maxOptions = 1000,
#                              placeholder = "Escriu o selecciona els indicadors que vulguis mostrar"))
#  
#  updateSelectizeInput(session,"ambit_filter", label = NULL, 
#                       choices = ambits, selected = NULL, options = 
#                         list(maxOptions = 1000, 
#                              placeholder = "Escriu o selecciona els àmbits que vulguis mostrar"))
#  
#  updateSelectizeInput(session,"dimensio_filter", label = NULL, 
#                       choices = dimensions, selected = NULL, options = 
#                         list(maxOptions = 1000, 
#                              placeholder = "Escriu o selecciona les dimensions que vulguis mostrar"))
#})
#
##Clearing all user inputs to default
#observeEvent(input$clear, {
#  
#  updateCheckboxInput(session, "rs", label = NULL, value = FALSE)
#  updateSelectInput(session, "rs_true", label = NULL,
#                    choices = rs, selected = character(0))
#  updateCheckboxInput(session, "aga", label = NULL, value = FALSE)
#  updateSelectInput(session, "aga_true", label = NULL, #"Type in the box to search",
#                    choices = aga, selected = character(0))
#  updateCheckboxInput(session, "abs", label = NULL, value = FALSE)
#  updateSelectInput(session, "abs_true", label = NULL, #"Type in the box to search",
#                    choices = abs, selected = character(0))
#  updateCheckboxInput(session, "centre", label = NULL, value = FALSE)
#  updateSelectInput(session, "centre_true", label = NULL, #"Type in the box to search",
#                    choices = centre, selected = character(0))
#  updateCheckboxInput(session, "ut", label = NULL, value = FALSE)
#  updateSelectInput(session, "ut_true", label = NULL, #"Type in the box to search",
#                    choices = ut, selected = character(0))
#  updateCheckboxInput(session, "usl", label = NULL, value = FALSE)
#  updateSelectInput(session, "usl_true", label = NULL, #"Type in the box to search",
#                    choices = usl, selected = character(0))
#  updateCheckboxInput(session, "catalunya", label = NULL, value = FALSE)
#  updateCheckboxInput(session, "all_geo", label = NULL, value = FALSE)
#  updateSliderInput(session, "date_from", label = NULL, value = c(min_year,max_year),
#                    min = min_year, max = max_year, step = 1)
#  updateSelectizeInput(session,"indicator_filter", label = NULL,
#                       choices = indicadors, selected = character(0),
#                       options = list(maxOptions = 1000, placeholder = "Escriu o selecciona els indicadors que vulguis mostrar"))
#  updateSelectizeInput(session,"ambit_filter", label = NULL, choices = ambits, selected = NULL,
#                       options = list(maxOptions = 1000, placeholder = "Escriu o selecciona els àmbits que vulguis mostrar"))
#  updateSelectizeInput(session,"dimensio_filter", label = NULL, choices = dimensions, selected = NULL,
#                       options = list(maxOptions = 1000, placeholder = "Escriu o selecciona les dimensions que vulguis mostrar"))
#  updateAwesomeRadio(session,"product_filter", label=NULL, choices = c("Indicador", "Àmbit", "Dimensió"), selected = NULL, inline = FALSE,
#                     status = "primary", checkbox = TRUE)
#  
#})
#
################################################.
### Reactive data ----
################################################.
#filter_table <- reactive ({
#  if (is.null(input$indicator_filter) & is.null(input$ambit_filter) & is.null(input$dimensio_filter)) {
#    # if no data selected create empty dataset to avoid app crashing
#    table <- data.frame(ambit = factor(), dimensio = factor(), id_indicador = factor(), `Centre/Territori` = factor(), 
#                        Granularitat = factor(), indicador = factor(), 
#                        any = double(), 
#                        #n =double(), d =double(), 
#                        resultat =double(), mitjana=double(), mesura = double(), invers = double())
#    
#    #if list of indicators selected
#  } else {
#    if (!is.null(input$indicator_filter)) { #if indicator selected
#      if (input$all_geo == TRUE) {
#        
#        
#        filtered_geos <- dades %>%  
#          filter(any>=input$date_from[1] & any<=input$date_from[2] & 
#                   indicador %in% input$indicator_filter)
#        
#        
#      } else {
#        
#        filtered_geo <- dades %>% 
#          filter(
#            (`Centre/Territori` %in% input$rs_true & Granularitat == "Regió Sanitària")|
#              (`Centre/Territori` %in% input$aga_true & Granularitat == "Àrea de Gestió Sanitària")|
#              (`Centre/Territori` %in% input$abs_true & Granularitat == "Àrea Bàsica de Salut")|
#              (`Centre/Territori` %in% input$centre_true & Granularitat == "Centre (Unitat proveïdora)")|
#              (`Centre/Territori` %in% input$ut_true & Granularitat == "Unitat territorial (protecció de la salut)")|
#              (`Centre/Territori` %in% input$usl_true & Granularitat == "Unitat de Salut Laboral")) %>% 
#          filter(any>=input$date_from[1] & any<=input$date_from[2] & 
#                   indicador %in% input$indicator_filter)
#        
#        filtered_geo2 <- if (input$catalunya == TRUE) {
#          dades %>% filter(`Centre/Territori` == "Catalunya" &
#                             (any>=input$date_from[1] & any<=input$date_from[2]) &
#                             indicador %in% input$indicator_filter)
#          
#        }      
#        
#        filtered_geos <- rbind(filtered_geo, filtered_geo2)
#        
#      }
#      
#      #if list of ambits selected
#      
#    } else if (!is.null(input$ambit_filter)) { 
#      
#      if (input$all_geo == TRUE) {
#        
#        filtered_geos <- dades %>%  
#          filter(any>=input$date_from[1] & any<=input$date_from[2] & 
#                   ambit %in% input$ambit_filter)
#        
#      } else {
#        
#        filtered_geo <- dades %>% 
#          filter(
#            (`Centre/Territori` %in% input$rs_true & Granularitat == "Regió Sanitària")|
#              (`Centre/Territori` %in% input$aga_true & Granularitat == "Àrea de Gestió Sanitària")|
#              (`Centre/Territori` %in% input$abs_true & Granularitat == "Àrea Bàsica de Salut")|
#              (`Centre/Territori` %in% input$centre_true & Granularitat == "Centre (Unitat proveïdora)")|
#              (`Centre/Territori` %in% input$ut_true & Granularitat == "Unitat territorial (protecció de la salut)")|
#              (`Centre/Territori` %in% input$usl_true & Granularitat == "Unitat de Salut Laboral")) %>% 
#          filter(any>=input$date_from[1] & any<=input$date_from[2] &
#                   ambit %in% input$ambit_filter)
#        
#        filtered_geo2 <- if (input$catalunya == TRUE) {
#          dades %>% 
#            filter(`Centre/Territori` == "Catalunya" &
#                     any>=input$date_from[1] & any<=input$date_from[2] &
#                     ambit %in% input$ambit_filter)
#          
#        }
#        # Merging together Scotland and other areas selected
#        filtered_geos <- rbind(filtered_geo,filtered_geo2)
#        
#      }
#      
#      
#      
#    } else { #ending the profile selection bit
#      
#      # if all available geographies checkbox checked
#      if (input$all_geo == TRUE) {
#        
#        filtered_geos <- dades %>%  
#          filter(any>=input$date_from[1] & any<=input$date_from[2])
#        
#      } else {
#        
#        filtered_geo <- dades %>% 
#          filter((`Centre/Territori` %in% input$rs_true & Granularitat == "Regió Sanitària")|
#                   (`Centre/Territori` %in% input$aga_true & Granularitat == "Àrea de Gestió Sanitària")|
#                   (`Centre/Territori` %in% input$abs_true & Granularitat == "Àrea Bàsica de Salut")|
#                   (`Centre/Territori` %in% input$centre_true & Granularitat == "Centre (Unitat proveïdora)")|
#                   (`Centre/Territori` %in% input$ut_true & Granularitat == "Unitat territorial (protecció de la salut)")|
#                   (`Centre/Territori` %in% input$usl_true & Granularitat == "Unitat de Salut Laboral")) %>% 
#          filter(any>=input$date_from[1] & any<=input$date_from[2])
#        
#        filtered_geo2 <- if (input$catalunya == TRUE) {
#          dades %>% 
#            filter(`Centre/Territori` == "Catalunya" &
#                     any>=input$date_from[1] & any<=input$date_from[2])
#          
#        }
#        # Merging together Scotland and other areas selected
#        filtered_geos <- rbind(filtered_geo,filtered_geo2)
#        
#      } 
#      
#    } #end of the else if statement for all available geographies
#    
#    table <- filtered_geos %>% select(ambit, dimensio, `Centre/Territori`, Granularitat, indicador, subtipologia, sexe, any, 
#                                      #n, 
#                                      #d, 
#                                      resultat, mitjana, mesura
#                                      #, invers
#                                      )
#  } #end of the whole if statement (if users have selected any data)
#  
#  
#  #  print(table)
#  
#})
#
#
#
################################################.
### Table ----
################################################.
#
##display table based on selection made by user on indicator tab
#output$table_filtered <- renderReactable({
#  
#  reactable(
#    filter_table(),
#    #searchable = TRUE,  # Enables global search
#    filterable = TRUE,  # Enables column-specific filtering
#    defaultPageSize = 25,  # Number of rows per page
#    striped = TRUE,             # Adds row stripes
#    bordered = TRUE,            # Adds borders to cells
#    highlight = TRUE,           # Highlights rows when hovered
#    columns = list(
#      "ambit" = colDef(name = "Àmbit", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      "dimensio" = colDef(name = "Dimensió", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      "Centre/Territori" = colDef(name = "Centre/Territori", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      "Granularitat" = colDef(name = "Granularitat", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      "indicador" = colDef(name = "Indicador", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      "subtipologia" = colDef(name = "Subcategoria", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      "sexe" = colDef(name = "Sexe", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      "any" = colDef(name = "Any", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      #"n" = colDef(name = "Numerador", filterable = TRUE),
#      #"d" = colDef(name = "Denominador", filterable = TRUE),
#      "resultat" = colDef(name = "Resultat", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      "mitjana" = colDef(name = "Catalunya", filterable = TRUE, headerStyle = list(textAlign = "center")),
#      "mesura" = colDef(name = "Mesura", filterable = TRUE, headerStyle = list(textAlign = "center"))#,
#      #"invers" = colDef(name = "Invers", filterable = TRUE, headerStyle = list(textAlign = "center"))
#    ),
#    language = reactableLang(
#      noData = "La selecció actual no permet mostrar cap dada. Recorda seleccionar quelcom al Pas 1 i Pas 2 per poder veure la taula.",
#      pageInfo = "{rowStart}\u2013{rowEnd} de {rows} files", # text to display in table footer
#      pagePrevious = "Anterior",
#      pageNext = "Següent",
#    )
#  )
#})
#
##print(filter_table())
#
## Downloading data in csv format
#table_csv <- reactive({ format_csv(filter_table()) })
#table_csv_total <- reactive({ format_csv(dades_download) })
#
#
################################################.
### Downloads ----
################################################.
## 1. Download filtered data as Excel
#output$download_table_excel <- downloadHandler(
#  filename = "dades_CentralDeResultats.xlsx",
#  content = function(file) {
#    data_to_download_seleccio <- table_csv() %>%
#      select(
#        ambit,
#        dimensio,
#        centre_territori,
#        granularitat,
#        indicador,
#        subcategoria,
#        sexe,
#        any,
#        resultat,
#        catalunya,
#        unitat
#      )
#    
#    # Write to Excel
#    writexl::write_xlsx(data_to_download_seleccio, path = file)
#  }
#)
#
## 2. Descàrrega totes les dades en CSV
#output$download_all_csv <- downloadHandler(
#  filename <- "dades_CentralDeResultats.csv",
#  content <- function(file) {
#    file.copy("datasets/dades_CentralDeResultats.csv", file)
#  },
#  contentType = "text/csv"
#)
#
#
###END#