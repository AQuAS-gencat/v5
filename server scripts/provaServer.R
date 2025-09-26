# Prova server

# ###############################################.
# ## Server de la visió resum ----
# ###############################################.

################################################################################
# Pop-up d'ajuda ----
################################################################################

observeEvent(input$ajuda_resum, {
  
  showModal(modalDialog(
    title = "Com funciona aquesta pestanya?",
    p("Després d'haver seleccionat una unitat territorial i un centre o territori 
      a la pestanya d'Inici del Visor de centres i territoris, a la pestanya Resum s'hi mostra una taula.
      Per defecte és la del darrer any disponible, modificable al selector Tria un any."),
    p("La taula resum mostra tots els indicadors disponibles pel centre o territori 
    seleccionat, ordenats segons àmbit i dimensió, mostrant-ne el resultat, acompanyat 
      del resultat global de Catalunya, una icona de tendència (variació) i un gràfic d'espina (comparació amb Catalunya)."),
    p("La icona de tendència compara el resultat del centre o territori seleccionat 
    de l'any seleccionat amb l'anterior (o darrer disponible). La direcció de la fletxa 
    indica si el resultat ha augmentat o disminuït respecte el darrer any, mentre que el 
    color indica la interpretació d'aquest resultat (blau si implica millora, taronja si 
    implica empitjorament o negre si té interpretació neutral). Si la variació és inferior 
      a un 1% o si l'indicador no té dades anteriors no es mostra cap fletxa."),
    p("Per cada indicador, el gràfic resum ordena els resultats de tots els centres o 
    territoris de la unitat territorial seleccionada,  de manera que el 50% d'aquests se 
    situen a la franja gris fosc i els dos quartils restants a la franja gris clar. A 
    l'esquerra sempre hi trobarem els resultats més baixos, a la dreta els més alts i 
    a la línia vertical central el valor de Catalunya. La bola correspon al valor del 
      centre o territori seleccionat, i és blava si el resultat s'interpreta com a millor 
      que la mitjana de Catalunya, taronja si pitjor i blanca si té una interpretació neutral."),
    size = "l", easyClose = TRUE, footer = modalButton("Tanca")))
  
  
}) 

observeEvent(input$int_resum, {
  
  showModal(modalDialog(
    title = "Interpretació de la icona de variació i el gràfic d'espina",
    #p("Després d'haver seleccionat una unitat territorial i un centre o territori 
    #  a la pestanya d'Inici del Visor de centres i territoris, a la pestanya Resum s'hi mostra una taula.
    #  Per defecte és la del darrer any disponible, modificable a l'apartat Tria un any."),
    div(
      style = "margin-top: 20px;",
      htmltools::includeHTML("www/llegenda_variacio.html") # Adjust path as necessary
    ),
    br(),
    div(
      style = "margin-top: 20px;",
      htmltools::includeHTML("www/llegenda_resum.html") # Adjust path as necessary
    ),
    #div(style = "display: flex; justify-content: center; margin: 15px;", img(src = "fletxes.png", width = "100%")),
    #div(style = "display: flex; justify-content: center; margin: 15px;", img(src = "llegenda_resum.png", width = "100%")),
    
    size = "l", easyClose = TRUE, footer = modalButton("Tanca")))
  
  
}) 

#with_tooltip <- function(value, tooltip, ...) {
#  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
#      tippy(value, tooltip, ...))
#}




###############################################.
## Reactive data ----
###############################################.

docu_reactive <- reactive({
  # If docu needs to be filtered based on input, add filtering conditions here
  as.data.table(docu) %>% 
    mutate(data_act = format(as.Date(data_act), "%b-%y"))
})

summary_prova <- reactive ({
  
  #  req(input$render_table)  # Ensure the table is only rendered after the button is clicked
  
  # convert to data.table format to make run faster 
  dt <- as.data.table(dades_resum)
  
  chosen_geo_type <- subset(dt, Granularitat %in% selected_values$geography)
  chosen_geo <- subset(chosen_geo_type, `Centre/Territori` %in% selected_values$center)
  chosen_any <- subset(chosen_geo, any %in% input$select_any_prova)
  
  chosen_any = chosen_any %>% 
    filter(!is.na(any))
  
  print(chosen_any)
  
  # calculate quantiles for each indicator within chosen geography level for spine chart 
  altres <- dt[Granularitat == selected_values$geography][chosen_any, on = .(id_resum, indicador, any), nomatch = 0][,
                                                                                                                     .(Q0 = quantile(resultat, probs = 0, na.rm = TRUE),
                                                                                                                       Q100 = quantile(resultat, probs = 1, na.rm = TRUE),
                                                                                                                       Q25 = quantile(resultat, probs = 0.25, na.rm = TRUE),
                                                                                                                       Q75 = quantile(resultat, probs = 0.75, na.rm = TRUE)),
                                                                                                                     by = .(id_resum, indicador, any)]
  print("ALTRES")
  print(altres)
  
  # add quantile values for each indicator to table
  chosen_any <- chosen_any[altres, on = c("id_resum", "indicador", "any"),
                           c("Q0", "Q100", "Q25", "Q75") := .(altres$Q0, 
                                                              altres$Q100,
                                                              altres$Q25,
                                                              altres$Q75)]
  print("CHOSEN ANY")
  print(chosen_any)
  
  # assign colours to values depending on statistical significance
  final <- chosen_any %>%
    mutate(marker_colour = case_when(
      #      lower_ci <= mitjana & upper_ci >= mitjana & invers %in% c(0, 1) ~'#6A6C6D',
      resultat > mitjana & invers == 0 ~ '#0073E6',
      resultat > mitjana & invers == 1 ~ 'orange',
      resultat < mitjana & invers == 1 ~ '#0073E6',
      resultat < mitjana & invers == 0 ~ 'orange',
      invers == 2 ~ '#FFFFFF', 
      TRUE ~ '#FFFFFF'))
  
  
  
  # No need to invert values, just ensure all values are scaled correctly
  # conditionally calculating worst to best
  final <- final %>%
    mutate(chosen_value = resultat) %>%
    mutate(scale_min = pmin(Q0, mitjana - (Q100 - mitjana)),
           scale_max = pmax(Q100, mitjana + (mitjana - Q0)),
           q0 = Q0, # dupliquem aquestes columnes per poder mostrar els valors reals al tooltip del gràfic d'espina
           q25 = Q25,
           q75 = Q75,
           q100 = Q100) %>%
    mutate(across(c(chosen_value, q0, q25, q75, q100), ~ (. - scale_min) / (scale_max - scale_min)))
  
  
  
  final$row_number <- 1:nrow(final) # assign each row an 'id' to be used when constructing highchart
  final$spine_chart <- NA # create empty column to populate with in-line highcharts
  
  # selecting columns required for table
  final <- final %>%
    select(c(ambit, ambit_curt, dimensio, id_resum, 
             #info_icon, 
             indicador, dg_extra, subtipologia, any, resultat, mitjana, mesura, trend_icona, Granularitat, `Centre/Territori`,
             row_number, spine_chart, Q100, Q75, Q25, Q0, q100, q75, q25, q0, chosen_value, marker_colour, invers))
  
  final$resultat = round(final$resultat, 2)
  
  
  final <- final %>%
    left_join(docu_reactive(), by = c("id_resum", "indicador", "ambit", "dimensio")) 
  
  
  print("FINAL")
  print(final)
  
}) 

###############################################.
## Reactive controls ----
###############################################.



#Dropdown any based on centre/territori selection  
output$select_any_prova_ui <- renderUI({
  
  req(selected_values$geography, selected_values$center)  # Ensure the required values are available
  
  selected_data <- dades_resum[dades_resum$Granularitat == selected_values$geography & 
                                 dades_resum$`Centre/Territori` == selected_values$center, ]
  
  any <- sort(unique(dades_resum$any[dades_resum$Granularitat == selected_values$geography & 
                                       dades_resum$`Centre/Territori` == selected_values$center]), 
              decreasing = TRUE)
  
  selectInput("select_any_prova", shiny::HTML("<p>Tria un any</p>"), 
              #choices=unique(selected_data$any))# selected = "Alcohol-related hospital admissions")),
              choices=unique(any))# selected = "Alcohol-related hospital admissions")),
  
})

# 3. dynamic table title  --------


output$explicacio_resum <- renderUI({  
  #tagList(
  div(
    style = "font-size: 14px; padding: 10px; background-color: #FFE7CE; border-left: 5px solid #FFA500; margin-bottom: 15px;",
    "La taula resum mostra tots els indicadors disponibles pel centre o territori 
    seleccionat, ordenats segons àmbit i dimensió, mostrant-ne el resultat, acompanyat 
      del resultat global de Catalunya, una icona de tendència (variació) i un gràfic d'espina (comparació amb Catalunya). Desplegant cada fila es mostra la fitxa metodològica de l'indicador."
  )
  
})




# Update subtitles when the refresh_button is clicked
output$titol_taula <- renderUI({ 
  tags$h3(paste0("Resum: ", selected_values$center, " (", selected_values$geography, "), ", input$select_any_prova),
          style = "font-weight: bold;")
})




# 4. downloads --------


# 5. summary table of results  ------------

# 5. summary table of results  ------------
output$taula_prova <- renderUI({
  
  summary_table <- widgetTableProva(data = summary_prova(), 
                               
                               columns = list(
                                 
                                 # # Domain column ---------
                                 ambit_curt = colDef(
                                   name = "Àmbit"
                                   ),
                                 
                                 # dimensio column --------
                                 dimensio = colDef(
                                   name = "Dimensió"),
                                 
                                 
                                 # indicator column --------
                                 indicador = colDef(
                                   name = "Indicadors"),
                                 
                                 # Chosen area column -------
                                 resultat = colDef(
                                   name = "Resultat"),
                                 
                                 
                                 
                                 # Catalunya column -------
                                 mitjana = colDef(
                                   name = "Catalunya"),
                                 

                                 # Mesura column -------
                                 mesura = colDef(
                                   name = "Unitat"),
                                 
                                 # Variació column -------
#                                 trend_icona = colDef(
#                                   maxWidth = 80,
#                                   align = "center",
#                                   name = "Variació",
#                                   cell = function(value){
#                                     div(style = "margin-top: 15px;", trend_indicator(value))
#                                   }),                                
#                                 
#                                 # in-line chart -------
#                                 spine_chart = colDef(
#                                   html = TRUE,
#                                   minWidth = 200,
#                                   header = div(
#                                     style = "display: flex; justify-content: space-between; margin-left: 25px; margin-right: 25px;",
#                                     div("<<< Inferior a la mitjana"), 
#                                     div("Superior a la mitjana >>>")
#                                   ),
#                                   cell = JS("function(rowInfo) {
#    const chartId = `spine-chart-${rowInfo.values['row_number']}`;
#    const html = `<div id='${chartId}' style='width:100%;height:70px;margin-top:9px;'></div>`;
#    
#    setTimeout(() => {
#      const chart = echarts.init(document.getElementById(chartId));
#      
#      const option = {
#        grid: {
#          top: '5%',
#          bottom: '5%',
#          left: '3%',
#          right: '3%',
#          containLabel: true
#        },
#        xAxis: {
#          type: 'value',
#          min: 0,
#          max: 1,
#          show: false
#        },
#        yAxis: {
#          type: 'category',
#          show: false,
#          data: ['']
#        },
#tooltip: {
#  trigger: 'item',
#  position: function(point, params, dom, rect, size) {
#    const [x, y] = point;
#    const { contentSize, viewSize } = size;
#    const tooltipWidth = contentSize[0];
#    const viewWidth = viewSize[0];
#    return x + tooltipWidth + 10 <= viewWidth ? [x + 10, y] : [x - tooltipWidth - 10, y];
#  },
#  formatter: function(params) {
#    const valuesMap = {
#      'Q0': rowInfo.values['Q0'],
#      'Q25': rowInfo.values['Q25'],
#      'Q75': rowInfo.values['Q75'],
#      'Q100': rowInfo.values['Q100'],
#      'Resultat': rowInfo.values['resultat']
#    };
#    const roundedValue = valuesMap[params.seriesName] !== undefined 
#      ? valuesMap[params.seriesName].toFixed(2) 
#      : params.value[0].toFixed(2);
#    
#    return `<strong>${params.seriesName}:</strong> ${roundedValue}`;
#  }
#},
#        series: [
#          // Circle for chosen value
#          {
#            name: 'Resultat',
#            type: 'scatter',
#            data: [[rowInfo.values['chosen_value'], 0]],
#            itemStyle: {
#              color: rowInfo.values['marker_colour'],
#              borderColor: 'black',
#              borderWidth: 1
#            },
#            symbolSize: 16,
#            z: 5
#          },
#          // Circle for Q25
#          {
#            name: 'Q25',
#            type: 'scatter',
#            data: [[rowInfo.values['q25'], 0]],
#            itemStyle: {
#              color: 'darkgray',
#              borderColor: 'darkgray',
#              borderWidth: 1
#            },
#            symbolSize: 10,
#            z: 3
#          },
#          // Circle for Q75
#          {
#            name: 'Q75',
#            type: 'scatter',
#            data: [[rowInfo.values['q75'], 0]],
#            itemStyle: {
#              color: 'darkgray',
#              borderColor: 'darkgray',
#              borderWidth: 1
#            },
#            symbolSize: 10,
#            z: 3
#          },
#          // Circle for Q0
#          {
#            name: 'Q0',
#            type: 'scatter',
#            data: [[rowInfo.values['q0'], 0]],
#            itemStyle: {
#              color: 'lightgray',
#              borderColor: 'lightgray',
#              borderWidth: 1
#            },
#            symbolSize: 10,
#            z: 3
#          },
#          // Circle for Q100
#          {
#            name: 'Q100',
#            type: 'scatter',
#            data: [[rowInfo.values['q100'], 0]],
#            itemStyle: {
#              color: 'lightgray',
#              borderColor: 'lightgray',
#              borderWidth: 1
#            },
#            symbolSize: 10,
#            z: 3
#          },
#          // Vertical red line for the mean
#          {
#            name: 'Mitjana',
#            type: 'scatter',
#            markLine: {
#              symbol: 'none',
#              lineStyle: {
#                color: 'red',
#                width: 3,
#                type: 'solid'
#              },
#              data: [{ xAxis: 0.5 }]
#            },
#            animation: false
#          },
#           // Horizontal black line from Q0 to Q100
#          {
#            name: 'Q0-Q100 Line',
#            type: 'line',
#            data: [[rowInfo.values['q0'], 0], [rowInfo.values['q100'], 0]],
#            lineStyle: {
#              color: 'lightgray',
#              width: 10,
#              type: 'solid',
#              opacity: 0.7
#            },
#            z: 1
#          },
#            // Green line connecting Q25 and Q75
#  {
#    name: 'Q25-Q75 Line',
#    type: 'line',
#    data: [[rowInfo.values['q25'], 0], [rowInfo.values['q75'], 0]],
#    lineStyle: {
#      color: 'darkgray',  // Green color
#      width: 10,        // Slightly thicker
#      type: 'solid',    // Solid line
#      opacity: 0.7
#    },
#    z: 2  // Ensure it appears above the background but below points
#  }
#        ],
#        animation: true
#      };
#      
#      chart.setOption(option);
#    }, 0);
#    
#    return html;
#  }")
#                                 ),
                                 
                                 
                                 # hide some columns
                                 # note these columns are hidden but are used within various functions above for those columns that are displayed
                                 #data = colDef(show = FALSE), # required for indicator col
                                 Q100 = colDef(show = FALSE),
                                 Q75 = colDef(show = FALSE),
                                 Q25 = colDef(show = FALSE),
                                 Q0 = colDef(show = FALSE),
                                 q100 = colDef(show = FALSE),
                                 q75 = colDef(show = FALSE),
                                 q25 = colDef(show = FALSE),
                                 q0 = colDef(show = FALSE),
                                 row_number = colDef(show = FALSE), # required for chart
                                 chosen_value = colDef(show = FALSE), # required for chart
                                 marker_colour = colDef(show = FALSE), # required for chart
                                 Granularitat = colDef(show = FALSE), # required for chart
                                 `Centre/Territori` = colDef(show = FALSE), # required for chart
                                 invers = colDef(show = FALSE), # required for chart
                                 any = colDef(show = FALSE),
                                 id_resum = colDef(show = FALSE),
                                 id_indicador = colDef(show = FALSE),
                                 pbi = colDef(show = FALSE),
                                 fitxa = colDef(show = FALSE),
                                 #mesura = colDef(show = FALSE) # required for chart
                                 subtipologia = colDef(show = FALSE),
                                 dg_extra = colDef(show = FALSE),
                                 ambit_curt = colDef(show = FALSE),
                                 tag1 = colDef(show = FALSE),
                                 tag2 = colDef(show = FALSE),
                                 tag3 = colDef(show = FALSE),
ambit = colDef(show = FALSE),
trend_icona = colDef(show = FALSE),
spine_chart = colDef(show = FALSE),
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
                                 criteris_tecnics = colDef(show = FALSE)
                                 #pub_relacionades = colDef(show = FALSE),
                                 #notes = colDef(show = FALSE)
                                 
                                 
                                 
                                 
                               ), # close columns list 
                               
                               
                               # explicitly set the column order
                               #                               order = c("ambit", "indicador", "any", "resultat", "mitjana", "mesura", "spine_chart"), # Per algun motiu ni indicant l'ordre es canvia l'ordre de les columnes a la taula
                               
                               # include highchart dependencies otherwise charts won't render
                               deps = htmlwidgets::getDependency("echarts4r"),
                               session$sendCustomMessage("adjustRowHeight", NULL) # Altura de les files ajustable segons longitud del text
                               
  )
  
  
  
  # Return the modified summary_table
  summary_table
  
})



#output$taula_prova <- renderUI({
#  
#  summary_table <- summary_prova()
#  
#  datatable(
#    summary_table,
#    rownames = FALSE,
#    escape = FALSE,
#    options = list(
#      #dom = 't', # Remove search bar and pagination
#      paging = FALSE,
#      ordering = TRUE,
#      columnDefs = list(
#        list(
#          targets = "ambit",
#          title = "Àmbit"),
#        list(
#          targets = "dimensio",
#          title = "Dimensió"
#        ),
#        list(
#          targets = "indicador",
#          title = "Indicador"
#        ),
#        list(targets = "resultat",
#             title = "Resultat"),
#        list(targets = "spine_chart",
#             title = "Spinechart"),
#        list(
#          targets = c("ambit_curt", "id_resum", "dg_extra", "subtipologia", "any", "mitjana", "mesura", "trend_icona", "Granularitat", "Centre/Territori", "row_number",
#                      "Q100", "Q75", "Q25", "Q0", "q100", "q75", "q25", "q0", "chosen_value", "marker_colour", "invers", "id_indicador", "definicio",
#                      "data_act", "propera_act", "font", "num", "den", "formula", "unitat", "justificacio", "exclusions", "limitacions", "criteris_tecnics", "int", "pbi",
#                      "fitxa", "tag1", "tag2", "tag3"),
#          visible = FALSE
#        )
#      )
#    )
#  )
#      
#
#})











