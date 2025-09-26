# Catalunya server

docu_reactive_cat <- reactive({
  # If docu needs to be filtered based on input, add filtering conditions here
  as.data.table(docu) %>% 
    mutate(data_act = format(as.Date(data_act), "%b-%y"))
})


#Dropdown any based on centre/territori selection  
output$select_any_cat_ui <- renderUI({
  
  any <- sort(unique(dades_r_resum$any[dades_r_resum$Granularitat == "Catalunya"]), 
              decreasing = TRUE)
  
  print("ANY")
  print(any)
  
  div(
    align = "left", "Tria un any:",
    style = "margin-top: 10px; margin-bottom: 20px;",
    selectInput("select_any_cat", 
                label = NULL,
                #shiny::HTML("<p>Tria un any</p>"), 
                #choices=unique(selected_data$any))# selected = "Alcohol-related hospital admissions")),
                choices=unique(any))# selected = "Alcohol-related hospital admissions")),
  )
  

  
})

# Update subtitles when the refresh_button is clicked
output$titol_taula_cat <- renderUI({ 
  tags$h4(paste0("Resum: Catalunya"),# input$select_any_cat),
          style = "font-weight: bold;")
})


summary_data_cat <- reactive({
  dt <- as.data.table(dades_r_resum)
  
  # Filter for Catalunya first
  chosen_geo_type <- subset(dt, Granularitat == "Catalunya")
  
  # Create sparkline data before filtering by year
  sparkline_data <- chosen_geo_type %>%
    group_by(id_resum) %>%
    arrange(any) %>%  # Make sure data is ordered by year
    summarise(sparkline_values = list(resultat)) %>%
    mutate(sparkline_values = lapply(sparkline_values, function(x) x[!is.na(x)])) # Remove NA values
  
  # Now filter by selected year
  chosen_any <- subset(chosen_geo_type, any %in% input$select_any_cat)
  chosen_any <- chosen_any %>% 
    filter(!is.na(any))
  
  # Select columns and round resultat
  final <- chosen_any %>%
    select(c(ambit, ambit_curt, dimensio, id_resum, 
             indicador, any, resultat, mitjana, mesura, 
             trend_icona, Granularitat, `Centre/Territori`, invers))
  
  final$resultat <- round(final$resultat, 2)
  
  # Join with documentation
  final <- final %>%
    left_join(docu_reactive(), by = c("id_resum", "indicador", "ambit", "dimensio"))
  
  # Add sparkline data
  final <- final %>%
    left_join(sparkline_data, by = "id_resum")
  
  final
})

observe({
  print("Sparkline Values:")
  print(summary_data_cat()[, .(id_resum, sparkline_values)])
})

observeEvent(input$cat_show_indicator_modal, {
  req(input$cat_show_indicator_modal)
  
  # Get the row index
  index <- input$cat_show_indicator_modal$index
  
  # Get the data for this row
  row <- summary_data_cat()[index + 1, ]
  
  if(length(row) == 0) {
    showModal(modalDialog(
      title = "Informació no disponible",
      "No s'ha trobat informació per aquest indicador.",
      easyClose = TRUE
    ))
    return()
  }
  
  # Create modal with the indicator details
  showModal(modalDialog(
    title = paste0("Fitxa de l'indicador: ", row$indicador),
    size = "l",
    easyClose = TRUE,
    
    # Responsive container for the modal content
    div(class = 'details-container', style = 'display: flex; flex-direction: column;',
        div(style = 'display: flex; justify-content: space-between;',
            div(style = 'font-size: medium;', tags$strong("Darrera actualització:"), " ", row$data_act %||% "N/A"),
            div(style = 'font-size: medium;', tags$strong("Propera actualització:"), " ", row$propera_act %||% "N/A")
        ),
        div(style = 'margin-top: 20px;', 
            h4("Definició"), 
            div(HTML(row$definicio %||% "N/A"))
        ),
        div(style = 'margin-top: 20px; display: flex;',
            div(style = 'flex: 1;', h4("Numerador"), div(HTML(row$num %||% "N/A"))),
            div(style = 'flex: 1;', h4("Denominador"), div(HTML(row$den %||% "N/A")))
        ),
        div(style = 'margin-top: 20px; display: flex;',
            div(style = 'flex: 1;', h4("Fórmula"), div(HTML(row$formula %||% "N/A"))),
            div(style = 'flex: 1;', h4("Unitat"), div(HTML(row$unitat %||% "N/A")))
        ),
        div(style = 'margin-top: 20px; display: flex;',
            div(style = 'flex: 1;', h4("Interpretació"), div(HTML(row$int %||% "N/A"))),
            div(style = 'flex: 1;', h4("Font"), div(HTML(row$font %||% "N/A")))
        ),
        div(style = 'margin-top: 20px;', h4("Justificació"), div(HTML(row$justificacio %||% "N/A"))),
        div(style = 'margin-top: 20px;', h4("Exclusions"), div(HTML(row$exclusions %||% "N/A"))),
        div(style = 'margin-top: 20px;', h4("Limitacions"), div(HTML(row$limitacions %||% "N/A"))),
        div(style = 'margin-top: 20px;', h4("Criteris tècnics"), div(HTML(row$criteris_tecnics %||% "N/A"))),
        div(style = 'margin-top: 20px; display: flex;',
            div(style = 'flex: 1;',
                h4("Enllaç a l'agrupació de l'indicador a l'aplicatiu PowerBI"),
                div(style = 'display: flex; gap: 10px;',
                    if(!is.null(row$pbi) && row$pbi != "N/A") {
                      tags$a(class = 'qia-down-pbi', href = row$pbi, target = '_blank', role = 'button',
                             "Aplicatiu PowerBI")
                    }
                )
            ),
            if(!is.null(row$fitxa)) {
              div(style = 'flex: 1;',
                  h4("Enllaç a la fitxa d'anàlisi (visió residència del pacient)"),
                  div(style = 'display: flex; gap: 10px;',
                      tags$a(class = 'qia-down-pbi', href = row$fitxa, target = '_blank', role = 'button',
                             "Fitxa d'anàlisi")
                  )
              )
            }
        )
    ),
    footer = modalButton("Tanca")
  ))
})

  # Create modal with the indicator details
  # Add this to catServer.R

output$taula_resum_cat <- renderUI({
  
  summary_table <- widgetTableCat(data = summary_data_cat(), 
                               
                               columns = list(
                                 
                                 # # Domain column ---------
                                 #ambit = colDef(
                                #   name = "Àmbit",
                                #   maxWidth = 120,
                                #   
                                #   # this JS function hides ambit name from appearing on every row
                                #   # i.e. gives appearance of 'merged' cells
                                #   style = JS("function(rowInfo, column, state) {

                                #         const prevRow = state.pageRows[rowInfo.viewIndex - 1]

                                 #        if (prevRow && rowInfo.values['ambit'] === prevRow['ambit']) {

                                  #         return {visibility: 'hidden'}
                                  #       }
                                  #     }
                                  #   "),
                                  # cell = function(value){
                                  #   div(style = "margin-top: 10px;", value)
                                  # }),
                                 
                                ambit_curt = colDef(
                                  name = "Àmbit",
                                  filterable = TRUE,
                                  maxWidth = 80,
                                  
                                  # Hide repeated ambit names for a merged-cell effect
                                  style = JS("function(rowInfo, column, state) {
    const prevRow = state.pageRows[rowInfo.viewIndex - 1];
    if (prevRow && rowInfo.values['ambit_curt'] === prevRow['ambit_curt']) {
      return { visibility: 'hidden' };
    }
  }"),
                                  
                                  cell = function(value) {
                                    div(
                                      style = "margin-top: 5px; margin-bottom: 5px; text-align: center;",
                                      ambit_icon(value)  # Use our new function here
                                    )
                                  }
                                ),
                                
                                 # dimensio column --------
                                 dimensio = colDef(
                                   name = "Dimensió",
                                   filterable = T,
                                   maxWidth = 120,
                                   # this JS function hides ambit name from appearing on every row
                                   # i.e. gives appearance of 'merged' cells
                                   style = JS("function(rowInfo, column, state) {

                                         const prevRow = state.pageRows[rowInfo.viewIndex - 1]

                                         if (prevRow && rowInfo.values['dimensio'] === prevRow['dimensio']) {

                                           return {visibility: 'hidden'}
                                         }
                                       }
                                     "),
                                   cell = function(value){
                                     div(style = "margin-top: 10px;", value)
                                   }),
                                 
                                 
                                # indicator column --------
                                indicador = colDef(
                                  minWidth = 320,
                                  show = TRUE,
                                  html = TRUE,
                                  filterable = TRUE,
                                  name = "Indicadors",
                                  cell = JS("function(rowInfo) {
    // Get the indicator value
    const indicatorValue = rowInfo.values['indicador'];

    // Return formatted HTML with info icon and indicator
    // Added the title attribute to the info icon span for the tooltip
    return `<div style='word-wrap: break-word; white-space: normal; overflow: visible; margin-top: 10px; margin-bottom: 5px;'>
              <span class='expand-row' data-row-index='${rowInfo.index}' 
                    style='color: #0078D4; cursor: pointer; margin-right: 8px;'
                    title='Obre la fitxa metodològica'>
                <i class='fa fa-info-circle'></i>
              </span>
              <span style='font-weight: bold;'>${indicatorValue}</span>
            </div>`;
  }")
                                ),
                                 
                                 
                                 # indicator column --------
                                 #indicador = colDef(
                                 #   name = "Indicador",
                                 #   minWidth = 320,
                                 #   cell = function(value, index) {
                                 #     id_resum <- summary_data()$id_resum[index + 1]
                                 #     print(id_resum)
                                 #     tooltip_content <- get_html_content(id_resum)
                                 #     print(tooltip_content)
                                 #     with_tooltip(
                                 #       div(style = "margin-top: 25px;font-size:1.5rem;", value),
                                 #       HTML(tooltip_content)
                                 #     )
                                 #   }),
                                 
                                 
                                 # Chosen area column -------
                                 resultat = colDef(
                                   maxWidth = 200,
                                   filterable = T,
                                   align = "center",
                                   name = "Resultat",
                                   cell = function(value){
                                     div(style = "margin-top: 25px;", value)
                                   }),
                                 
                                 
                                 
                                 # Catalunya column -------
                                 #mitjana = colDef(
                                #   maxWidth = 90,
                                #   align = "center",
                                #   name = "Catalunya",
                                #   cell = function(value){
                                #     div(style = "margin-top: 25px;", value)
                                #   }),
                                 
                                 
                                 
                                 # Mesura column -------
                                 mesura = colDef(
                                   maxWidth = 150,
                                   filterable = T,
                                   align = "center",
                                   name = "Unitat",
                                   cell = function(value){
                                     div(style = "margin-top: 25px;", value)
                                   }),
                                 
                                 # Variació column -------
                                 trend_icona = colDef(
                                   maxWidth = 80,
                                   filterable = F,
                                   align = "center",
                                   name = "Variació",
                                   cell = function(value){
                                     div(style = "margin-top: 15px;", trend_indicator(value))
                                   }),
                                

                                # Add sparkline column
                                sparkline_values = colDef(
                                  minWidth = 180,
                                  filterable = F,
                                  align = "center",
                                  name = "Tendència",
                                  cell = react_sparkline(
                                    summary_data_cat(),
                                    height = 60,
                                    decimals = 2,
                                    show_area = T,
                                    highlight_points = highlight_points(min = "red", max = "red"),
                                    line_color = "blue",
                                    statline = "mean",
                                    statline_color = "black",
                                    tooltip_type = 1,
                                    margin = c(5, 40, 5, 30) # Adjust the margins (top, right, bottom, left)


                                    )

                                ),
                                



                                
                                #sparkline = colDef(cell = function(value, index) {
                                #  sparkline(data$sparkline_values[[index]])
                                #}),
                                
                                #sparkline_values = colDef(
                                #  name = "Tendència",
                                #  cell = function(value, index) {
                                #    dui_sparkline(
                                #      data = value[[1]],
                                #      height = 80,
                                #      # make some room for our statistics
                                #      margin = list(right = 40),
                                #      components = list(
                                #        dui_sparklineseries(
                                #          stroke = colpal[index],
                                #          showArea = TRUE,
                                #          fill = colpal[index]
                                #        )
#
                                #      )
                                #    )
                                #  }
                                #),


                                 # hide some columns
                                 # note these columns are hidden but are used within various functions above for those columns that are displayed
                                 #data = colDef(show = FALSE), # required for indicator col

                                 Granularitat = colDef(show = FALSE), # required for chart
                                 `Centre/Territori` = colDef(show = FALSE), # required for chart
                                 invers = colDef(show = FALSE), # required for chart
                                 any = colDef(show = FALSE),
                                 id_resum = colDef(show = FALSE),
                                 id_indicador = colDef(show = FALSE),
                                 pbi = colDef(show = FALSE),
                                 fitxa = colDef(show = FALSE),
                                 ambit = colDef(show = FALSE),

                                 
                                 tag1 = colDef(show = FALSE),
                                 tag2 = colDef(show = FALSE),
                                 tag3 = colDef(show = FALSE),
                                 
                                 definicio = colDef(show = FALSE),
                                 data_act = colDef(show = FALSE),
                                 propera_act = colDef(show = FALSE),
                                 font = colDef(show = FALSE),
                                 num = colDef(show = FALSE),
                                 den = colDef(show = FALSE),
                                 formula = colDef(show = FALSE),
                                 unitat = colDef(show = FALSE),
                                 justificacio = colDef(show = FALSE),
                                 int = colDef(show = FALSE),
                                 limitacions = colDef(show = FALSE),
                                 exclusions = colDef(show = FALSE),
                                 criteris_tecnics = colDef(show = FALSE),
                                 mitjana = colDef(show = FALSE)
                                 #pub_relacionades = colDef(show = FALSE),
                                 #notes = colDef(show = FALSE)
                                 
                                 
                                 
                                 
                               ), # close columns list 
                               
                               
                               # explicitly set the column order
                               #                               order = c("ambit", "indicador", "any", "resultat", "mitjana", "mesura", "spine_chart"), # Per algun motiu ni indicant l'ordre es canvia l'ordre de les columnes a la taula
                               
                               # include highchart dependencies otherwise charts won't render
                               #deps = htmlwidgets::getDependency("highchart" , "highcharter"),
                               session$sendCustomMessage("adjustRowHeight", NULL) # Altura de les files ajustable segons longitud del text
                               
  )
  
  
  
  # Return the modified summary_table
  summary_table
  
}) #%>% 
