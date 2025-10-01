# ###############################################.
# ## Server de la visió resum ----
# ###############################################.

################################################################################
# Pop-up d'ajuda ----
################################################################################

#observeEvent(input$ajuda_resum, {
#  
#  showModal(modalDialog(
#    title = "Com funciona aquesta pestanya?",
#    p("Després d'haver seleccionat una unitat territorial i un centre o territori 
#      a la pestanya d'Inici del Visor de centres i territoris, a la pestanya Resum s'hi mostra una taula.
#      Per defecte és la del darrer any disponible, modificable al selector Tria un any."),
#    p("La taula resum mostra tots els indicadors disponibles pel centre o territori 
#    seleccionat, ordenats segons àmbit i dimensió, mostrant-ne el resultat, acompanyat 
#      del resultat global de Catalunya, una icona de tendència (variació) i un gràfic d'espina (comparació amb Catalunya)."),
#    p("La icona de tendència compara el resultat del centre o territori seleccionat 
#    de l'any seleccionat amb l'anterior (o darrer disponible). La direcció de la fletxa 
#    indica si el resultat ha augmentat o disminuït respecte el darrer any, mentre que el 
#    color indica la interpretació d'aquest resultat (blau si implica millora, taronja si 
#    implica empitjorament o negre si té interpretació neutral). Si la variació és inferior 
#      a un 1% o si l'indicador no té dades anteriors no es mostra cap fletxa."),
#    p("Per cada indicador, el gràfic resum ordena els resultats de tots els centres o 
#    territoris de la unitat territorial seleccionada,  de manera que el 50% d'aquests se 
#    situen a la franja gris fosc i els dos quartils restants a la franja gris clar. A 
#    l'esquerra sempre hi trobarem els resultats més baixos, a la dreta els més alts i 
#    a la línia vertical central el valor de Catalunya. La bola correspon al valor del 
#      centre o territori seleccionat, i és blava si el resultat s'interpreta com a millor 
#      que la mitjana de Catalunya, taronja si pitjor i blanca si té una interpretació neutral."),
#    size = "l", easyClose = TRUE, footer = modalButton("Tanca")))
  
  
#}) 

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
  as.data.table(alfred) #%>% 
    #mutate(data_act = format(as.Date(data_act), "%b-%y"))
})

summary_data <- reactive({
  
  req(input$select_any_global)
  req(selected_values$geography, selected_values$center)
  
  # Determine which result column to use
  result_column <- if (isTRUE(input$taula_switch_resum)) "oe" else "r"
  
  dades_resum_tbl <- dades_tbl %>% filter(grup_edat == "Total",
                                          sexe == "Total")  # Your main dataset
  
  # Step 1: filter geography only, not center/year
  dt_geo <- dades_resum_tbl %>%
    filter(Granularitat %in% selected_values$geography)
  
  # Step 2: collect into R
  dt_geo <- collect(dt_geo)
  
  # Step 3: filter chosen_any
  chosen_any <- dt_geo %>%
    filter(`Centre/Territori` %in% selected_values$center,
           any %in% input$select_any_global,
           !is.na(.data[[result_column]]))
  
  # Step 4: compute quantiles per indicator over all units in that geography
  altres <- dt_geo %>%
    filter(!is.na(.data[[result_column]])) %>%
    group_by(codi_indicador, nom_indicador, any) %>%
    summarise(
      Q0   = quantile(.data[[result_column]], 0, na.rm = TRUE),
      Q25  = quantile(.data[[result_column]], 0.25, na.rm = TRUE),
      Q75  = quantile(.data[[result_column]], 0.75, na.rm = TRUE),
      Q100 = quantile(.data[[result_column]], 1, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Step 5: join quantiles back to chosen_any
  chosen_any <- left_join(chosen_any, altres, by = c("codi_indicador", "nom_indicador", "any"))
  
  
  # Step 6: add marker colors and scaling
  final <- chosen_any %>%
    mutate(
      marker_colour = case_when(
        r > mitjana & invers == 0 ~ '#91BFDB',
        r > mitjana & invers == 1 ~ '#FC8D59',
        r < mitjana & invers == 1 ~ '#91BFDB',
        r < mitjana & invers == 0 ~ '#FC8D59',
        invers == 2 ~ '#FFFFFF',
        TRUE ~ '#FFFFFF'
      ),
      chosen_value = r,
      # keep natural quartiles
      q0 = Q0,
      q25 = Q25,
      q75 = Q75,
      q100 = Q100,
      # compute scale min/max per row like original script
      scale_min = pmin(Q0, mitjana - (Q100 - mitjana)),
      scale_max = pmax(Q100, mitjana + (mitjana - Q0))
    ) %>%
    mutate(
      # normalise based on scale_min/scale_max
      across(c(chosen_value, q0, q25, q75, q100), ~ (. - scale_min) / (scale_max - scale_min)),
      ic = paste0("[", ic_inf, " - ", ic_sup, "]"),
      row_number = row_number(),
      spine_chart = NA,
      display_type = ifelse(result_column == "oe", "Estandarditzat", "Cru"),
      r = round(r, 2)
    )
  
  print(final)
  #print(names(final))
  

  
  # Step 8: select final columns for reactable
  if(result_column == "oe") {
    final <- final %>%
      select(
        ambit, ambit_curt, dimensio, codi_indicador, 
        nom_indicador, any, oe, ic, trend_icona, Granularitat, `Centre/Territori`,
        row_number, spine_chart, Q100, Q75, Q25, Q0, q100, q75, q25, q0, chosen_value, marker_colour, invers, display_type
      )
  } else {
    final <- final %>%
      select(
        ambit, ambit_curt, dimensio, codi_indicador, 
        nom_indicador, any, r, mitjana, 
        unitats, trend_icona, Granularitat, `Centre/Territori`,
        row_number, spine_chart, Q100, Q75, Q25, Q0, q100, q75, q25, q0, chosen_value, marker_colour, invers, display_type
      )
  }
  
  
  # Step 7: join documentation table, selecting only needed columns
  final <- final %>%
    left_join(
      docu_reactive() %>% 
        mutate(code = as.character(code),
               name = as.character(name),
               Àmbit = as.character(Àmbit),
               Dimensió = as.character(Dimensió)),
        by = c("codi_indicador" = "code", "nom_indicador" = "name", "ambit" = "Àmbit", "dimensio" = "Dimensió")
    )
  
  #print("FINAL")
  #print(names(final))
  
  #print("FINAL")
  #print(final)
  

  
  #print(names(final))
  #print(final[17:24])

  
  return(final)
})



###############################################.
## Reactive controls ----
###############################################.



#Dropdown any based on centre/territori selection  
output$select_any_global_ui <- renderUI({
  req(selected_values$geography, selected_values$center)
  
  dades_resum_tbl <- dades_tbl %>% filter(grup_edat == "Total",
                                          sexe == "Total")  # Your main dataset
  
  # Filter in DuckDB, then collect into R
  selected_data <- dades_resum_tbl %>%
    filter(
      Granularitat == !!selected_values$geography,
      `Centre/Territori` == !!selected_values$center
    ) %>%
    collect()
  
  # Extract available years
  any_choices <- selected_data %>%
    filter(!is.na(any)) %>%
    distinct(any) %>%
    arrange(desc(any)) %>%
    pull(any)
  
  div(
    align = "left", "Tria un any:",
    style = "margin-top: 10px; margin-bottom: 20px;",
    selectInput(
      "select_any_global",
      label = NULL,
      choices = any_choices
    )
  )
})


# 3. dynamic table title  --------


#output$explicacio_resum <- renderUI({  
#  #tagList(
#  div(
#    style = "font-size: 14px; padding: 10px; background-color: #FFE7CE; border-left: 5px solid #FFA500; margin-bottom: 15px;",
#    "La taula resum mostra tots els indicadors disponibles pel centre o territori 
#    seleccionat, ordenats segons àmbit i dimensió, mostrant-ne el resultat, acompanyat 
#      del resultat global de Catalunya, una icona de tendència (variació) i un gràfic d'espina (comparació amb Catalunya). Desplegant cada fila es mostra la fitxa metodològica de l'indicador."
#  )
#  
#})


output$taula_switch_resum_ui <- renderUI({
  div(
      align = "left",
      #"Mostra els resultats estandarditzats:",
      #tags$b(class = "step-text", "Mostra els resultats estandarditzats:"),
      style = "margin-top: 10px; margin-bottom: 20px; margin-left: 20px",
      prettySwitch(
        inputId = "taula_switch_resum",
        label = "Resultats estandarditzats",
        fill = TRUE,
        status = "primary",
        value = FALSE
      )
    )
})

# Update subtitles when the refresh_button is clicked
# Update subtitles when the refresh_button is clicked
output$titol_taula <- renderUI({ 
  # Get the first row to determine display type
  display_type <- if(nrow(summary_data()) > 0) {
    if(isTRUE(input$taula_switch_resum)) "estandarditzats" else "crus" 
  } else {
    ""
  }
  
  tagList(
    tags$h4(paste0("Resum: ", selected_values$center), style = "font-weight: bold;"),
    #tags$h5(paste0(input$select_any_global, 
    #               " (Resultats ", display_type, ")"))
  )
})




# 4. downloads --------

# download as PDF
# Add this to your existing download handlers
#output$download_summary_pdf <- downloadHandler(
#  filename = function() {
#    paste(selected_values$center, "-resum-", input$select_any_global, ".pdf", sep = "")
#  },
#  content = function(file) {
#    # Create temporary directory
#    tempDir <- tempdir()
#    tempReport <- file.path(tempDir, "spinecharts.Rmd")
#    
#    # Copy the report file to the temp directory
#    file.copy("spinecharts.Rmd", tempReport, overwrite = TRUE)
#    
#    # Set up parameters to pass to Rmd document
#    params <- list(
#      reactive_df = summary_data(),
#      chosen_geo_type = selected_values$geography,
#      chosen_geo = selected_values$center,
#      chosen_any = input$select_any_global
#    )
#    
#    # Render the report
#    tryCatch({
#      rmarkdown::render(
#        tempReport,
#        output_file = file,
#        params = params,
#        envir = new.env(parent = globalenv()),
#        quiet = TRUE
#      )
#    }, error = function(e) {
#      message("Error in PDF generation: ", e$message)
#    })
#  }
#)

# download as csv 
#output$download_summary_csv <- downloadHandler(
#  filename = function() { 
#    paste(selected_values$center, "-resum-", input$select_any_global, ".csv", sep="")
#  },
#  content = function(file) {
#    
#    
#    data_to_download_resum <- summary_data() %>%
#      mutate(variacio = ifelse(trend_icona == "up", "augmenta",
#                               ifelse(trend_icona == "down", "disminueix",
#                                      ifelse(trend_icona == "betterup" | trend_icona == "betterdown", "millora",
#                                             ifelse(trend_icona == "worseup" | trend_icona == "worsedown", "empitjora",
#                                                    ifelse(trend_icona == "new", "sense dades prèvies",
#                                                           ifelse(trend_icona == "unchanged", "sense canvi", ""))))))) %>% 
#      select(ambit, dimensio, indicador, 
#             subcategoria = subtipologia, 
#             granularitat = Granularitat, 
#             centre_territori = `Centre/Territori`, 
#             resultat, 
#             catalunya = mitjana, 
#             unitat = mesura, 
#             variacio,
#             any)
#    
#    # Export with UTF-8 encoding for accents
#    write.csv2(data_to_download_resum, file, row.names = FALSE, 
#               #fileEncoding = "UTF-8"
#               fileEncoding = "ISO-8859-1"
#               )
#    
#  })



# Download as Excel
output$download_summary_excel <- downloadHandler(
  filename = function() { 
    paste(selected_values$center, "-resum-", input$select_any_global, ".xlsx", sep = "")
  },
  content = function(file) {
    # Prepare the data for download
    data_to_download_resum <- summary_data() %>%
      mutate(variacio = case_when(
        trend_icona == "up" ~ "augmenta",
        trend_icona == "down" ~ "disminueix",
        trend_icona %in% c("betterup", "betterdown") ~ "millora",
        trend_icona %in% c("worseup", "worsedown") ~ "empitjora",
        trend_icona == "new" ~ "sense dades prèvies",
        trend_icona == "unchanged" ~ "sense canvi",
        TRUE ~ ""
      )) %>% 
      select(
        ambit, 
        ambit_curt,
        dimensio, 
        indicador, 
        subcategoria = subtipologia, 
        granularitat = Granularitat, 
        centre_territori = `Centre/Territori`, 
        resultat, 
        catalunya = mitjana, 
        unitat = mesura, 
        variacio,
        any
      )
    
    # Write to Excel
    writexl::write_xlsx(data_to_download_resum, path = file)
  }
)

# Add this to resumServer.R
observeEvent(input$show_indicator_modal, {
  req(input$show_indicator_modal)
  
  # Get the row index
  index <- input$show_indicator_modal$index
  
  # Get the data for this row
  row <- summary_data()[index + 1, ]
  
  if(length(row) == 0) {
    showModal(modalDialog(
      title = "Informació no disponible",
      "No s'ha trobat informació per aquest indicador.",
      easyClose = TRUE
    ))
    return()
  }
  
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
  
  # Create modal with the indicator details using bslib components
  showModal(modalDialog(
    title = paste0("Fitxa de l'indicador: ", row$nom_indicador),
    size = "l",
    easyClose = TRUE,
    
    # Using bslib components for better formatting
    tagList(
      card(
        card_header("DEFINICIÓ"),
        card_body(div(safe_html(row$desc)))
      ),
      
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("NUMERADOR"),
          card_body(div(safe_html(row$num_def)))
        ),
        card(
          card_header("DENOMINADOR"),
          card_body(div(safe_html(row$den_def)))
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("FÓRMULA"),
          card_body(div(safe_html(row$formula)))
        ),
        card(
          card_header("UNITAT"),
          card_body(div(safe_html(row$units)))
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("INTERPRETACIÓ"),
          card_body(div(safe_html(row$interpretation_criteria_text)))
        ),
        card(
          card_header("FONT"),
          card_body(div(safe_html(row$source)))
        )
      ),
      br(),
      accordion(
        accordion_panel(
          "Justificació",
          div(safe_html(row$reason))
        ),
        accordion_panel(
          "Exclusions",
          div(safe_html(row$exclusions))
        ),
        accordion_panel(
          "Limitacions",
          div(safe_html(row$limitations))
        ),
        accordion_panel(
          "Criteris tècnics",
          div(safe_html(row$criteria))
        )
      )
    ),
    footer = modalButton("Tanca")
  ))
})

# 5. summary table of results  ------------
output$taula_resum <- renderUI({
  
  data = summary_data()
  is_standardized <- isTRUE(input$taula_switch_resum)
  column_list = list(
                                 
                                 # # Domain column ---------
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
                                       style = "margin-top: 10px; margin-bottom: 5px; text-align: center;",
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
                                     div(style = "margin-top: 10px; margin-bottom: 5px; font-size: 0.85rem;", value)
                                   }),
                                 
                                 
                                 # indicator column --------
                                 nom_indicador = colDef(
                                   minWidth = 320,
                                   show = TRUE,
                                   html = TRUE,
                                   filterable = TRUE,
                                   name = "Indicadors",
                                   cell = JS("function(rowInfo) {
    // Get the indicator value
    const indicatorValue = rowInfo.values['nom_indicador'];

    // Return formatted HTML with info icon and indicator
    // Added the title attribute to the info icon span for the tooltip
    return `<div style='word-wrap: break-word; white-space: normal; overflow: visible; margin-top: 5px; margin-bottom: 5px;'>
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
                                 
                                 
                                 # Resultat cru column column -------
                                 r = if(!is_standardized) colDef(
                                   maxWidth = 100,
                                   filterable = T,
                                   align = "center",
                                   name = "Resultat",
                                   cell = function(value){
                                     div(style = "margin-top: 10px; margin-bottom: 5px;", value)
                                   }) else NULL,
                                 
                                 # OE column -------
                                 oe = if(is_standardized) colDef(
                                   maxWidth = 100,
                                   filterable = T,
                                   align = "center",
                                   name = "Raó O/E",
                                   cell = function(value){
                                     div(style = "margin-top: 10px; margin-bottom: 5px;", value)
                                   }) else NULL,
                                 
                                 # Mesura column - conditionally include
                                 ic = if(is_standardized) colDef(
                                   maxWidth = 150,
                                   filterable = T,
                                   #align = "center",
                                   name = "IC (95%)",
                                   cell = function(value){
                                     div(style = "margin-top: 10px; margin-bottom: 5px;", value)
                                   }) else NULL,
                                 
                                 # Catalunya column -------
                                 #mitjana = colDef(
                                #   maxWidth = 100,
                                #   filterable = T,
                                #   align = "center",
                                #   name = "Catalunya",
                                #   cell = function(value){
                                #     div(style = "margin-top: 10px; margin-bottom: 5px;", value)
                                #   }),
                                 
                                 # Catalunya column - conditionally include
                                 mitjana = if(!is_standardized) colDef(
                                   maxWidth = 100,
                                   filterable = T,
                                   align = "center",
                                   name = "Catalunya",
                                   cell = function(value){
                                     div(style = "margin-top: 10px; margin-bottom: 5px;", value)
                                   }) else NULL,
                                 
                                 
                                 # Mesura column -------
                                 #mesura = colDef(
                                #   maxWidth = 150,
                                #   filterable = T,
                                #   align = "center",
                                #   name = "Unitat",
                                #   cell = function(value){
                                #     div(style = "margin-top: 10px; margin-bottom: 5px;", value)
                                #   }),
                                 
                                # Mesura column - conditionally include
                                unitats = if(!is_standardized) colDef(
                                  maxWidth = 150,
                                  filterable = T,
                                  #align = "center",
                                  name = "Mesura",
                                  cell = function(value){
                                    div(style = "margin-top: 10px; margin-bottom: 5px;", value)
                                  }) else NULL,
                                

                               
                                 # Variació column -------
                                 trend_icona = colDef(
                                   maxWidth = 80,
                                   align = "center",
                                   name = "Variació",
                                   cell = function(value){
                                     div(style = "margin-top: 0px; margin-bottom: 0px;", trend_indicator(value))
                                   }),                                
                                 
                                # in-line chart -------
                                spine_chart = colDef(
                                  html = TRUE,
                                  minWidth = 200,
                                  header = htmltools::tags$div(
                                    style = "display: flex; justify-content: space-between; width: 100%;",
                                    htmltools::tags$div(style = "text-align: right; flex: 1;", "< mitjana"),
                                    htmltools::tags$div(style = "text-align: center; flex: 1;", "|"),
                                    htmltools::tags$div(style = "text-align: left; flex: 1;", "> mitjana")
                                  ),
                                  cell = JS("function(rowInfo) {
    const chartId = `spine-chart-${rowInfo.values['row_number']}`;
    const html = `<div id='${chartId}' style='width:100%;height:50px; margin-top: 8px; margin-bottom: 0px;'></div>`;
    
    setTimeout(() => {
      const chart = echarts.init(document.getElementById(chartId));
      
      // Determine if using standardized results
      const isStandardized = rowInfo.values['display_type'] === 'Estandarditzat';
      
      // Get the result value based on display type
      const resultValue = isStandardized ? rowInfo.values['oe'] : rowInfo.values['r'];
      
      // For standardized results, we need to calculate proper ranges
      let xMin, xMax, linePosition;
      let q0Val, q25Val, q75Val, q100Val, resultPos;
      
      if (isStandardized) {
        // Use raw quartile values for standardized results
        q0Val = rowInfo.values['Q0'];
        q25Val = rowInfo.values['Q25'];
        q75Val = rowInfo.values['Q75'];
        q100Val = rowInfo.values['Q100'];
        resultPos = resultValue;
        
        // Calculate range with some padding
        const minVal = Math.min(q0Val, resultValue, 1); // 1 is the reference value
        const maxVal = Math.max(q100Val, resultValue, 1);
        const padding = (maxVal - minVal) * 0.1;
        xMin = Math.max(0, minVal - padding);
        xMax = maxVal + padding;
        linePosition = 1; // Reference line at 1 for O/E ratio
      } else {
        // Use normalized values for non-standardized results
        q0Val = rowInfo.values['q0'];
        q25Val = rowInfo.values['q25'];
        q75Val = rowInfo.values['q75'];
        q100Val = rowInfo.values['q100'];
        resultPos = rowInfo.values['chosen_value'];
        
        xMin = 0;
        xMax = 1;
        linePosition = 0.5; // Reference line at midpoint
      }
      
      // Set line color based on display_type
      const lineColor = isStandardized ? 'black' : 'red';
      
      // Calculate tooltip values based on display type
      const valuesMap = {
        'Q0': rowInfo.values['Q0'],
        'Q25': rowInfo.values['Q25'],
        'Q75': rowInfo.values['Q75'],
        'Q100': rowInfo.values['Q100'],
        'Resultat': resultValue
      };
      
      const option = {
        grid: {
          top: '1%',
          bottom: '1%',
          left: '3%',
          right: '8%',
          containLabel: true
        },
        xAxis: {
          type: 'value',
          min: xMin,
          max: xMax,
          show: false
        },
        yAxis: {
          type: 'category',
          show: false,
          data: ['']
        },
        tooltip: {
          trigger: 'item',
          position: function(point, params, dom, rect, size) {
            const [x, y] = point;
            const { contentSize, viewSize } = size;
            const tooltipWidth = contentSize[0];
            const viewWidth = viewSize[0];
            return x + tooltipWidth + 10 <= viewWidth ? [x + 10, y] : [x - tooltipWidth - 10, y];
          },
          formatter: function(params) {
            const value = valuesMap[params.seriesName] !== undefined 
              ? valuesMap[params.seriesName].toFixed(2) 
              : params.value[0].toFixed(2);
            
            return `<strong>${params.seriesName}:</strong> ${value}`;
          }
        },
        series: [
          // Circle for chosen value
          {
            name: 'Resultat',
            type: 'scatter',
            data: [[resultPos, 0]],
            itemStyle: {
              color: rowInfo.values['marker_colour'],
              borderColor: 'black',
              borderWidth: 1
            },
            symbolSize: 16,
            z: 5
          },
          // Circle for Q25
          {
            name: 'Q25',
            type: 'scatter',
            data: [[q25Val, 0]],
            itemStyle: {
              color: 'darkgray',
              borderColor: 'darkgray',
              borderWidth: 1
            },
            symbolSize: 10,
            z: 3
          },
          // Circle for Q75
          {
            name: 'Q75',
            type: 'scatter',
            data: [[q75Val, 0]],
            itemStyle: {
              color: 'darkgray',
              borderColor: 'darkgray',
              borderWidth: 1
            },
            symbolSize: 10,
            z: 3
          },
          // Circle for Q0
          {
            name: 'Q0',
            type: 'scatter',
            data: [[q0Val, 0]],
            itemStyle: {
              color: 'lightgray',
              borderColor: 'lightgray',
              borderWidth: 1
            },
            symbolSize: 10,
            z: 3
          },
          // Circle for Q100
          {
            name: 'Q100',
            type: 'scatter',
            data: [[q100Val, 0]],
            itemStyle: {
              color: 'lightgray',
              borderColor: 'lightgray',
              borderWidth: 1
            },
            symbolSize: 10,
            z: 3
          },
          // Vertical line for the mean or reference value
          {
            name: isStandardized ? 'Referència' : 'Mitjana',
            type: 'scatter',
            markLine: {
              symbol: 'none',
              lineStyle: {
                color: lineColor,
                width: 3,
                type: 'solid'
              },
              data: [{ xAxis: linePosition }]
            },
            animation: false
          },
          // Horizontal line from Q0 to Q100
          {
            name: 'Q0-Q100 Line',
            type: 'line',
            data: [[q0Val, 0], [q100Val, 0]],
            lineStyle: {
              color: 'lightgray',
              width: 10,
              type: 'solid',
              opacity: 0.7
            },
            z: 1
          },
          // Line connecting Q25 and Q75
          {
            name: 'Q25-Q75 Line',
            type: 'line',
            data: [[q25Val, 0], [q75Val, 0]],
            lineStyle: {
              color: 'darkgray',
              width: 10,
              type: 'solid',
              opacity: 0.7
            },
            z: 2
          }
        ],
        animation: true
      };
      
      chart.setOption(option);
    }, 0);
    
    return html;
  }")
                                ),
                                 
                                 
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
                                 codi_indicador = colDef(show = FALSE),
                                 #id_indicador = colDef(show = FALSE),
                                 #pbi = colDef(show = FALSE),
                                 #fitxa = colDef(show = FALSE),
                                 #mesura = colDef(show = FALSE) # required for chart
                                 #subtipologia = colDef(show = FALSE),
                                 #dg_extra = colDef(show = FALSE),
                                 ambit = colDef(show = FALSE),
                                 display_type = colDef(show = FALSE),
                                 
                                 #tag1 = colDef(show = FALSE),
                                 #tag2 = colDef(show = FALSE),
                                 #tag3 = colDef(show = FALSE),
                                 
                                 desc = colDef(show = FALSE),
                                 #data_act = colDef(show = FALSE),
                                 #propera_act = colDef(show = FALSE),
                                 source = colDef(show = FALSE),
                                 num_def = colDef(show = FALSE),
                                 den_def = colDef(show = FALSE),
                                 formula = colDef(show = FALSE),
                                 units = colDef(show = FALSE),
                                 reason = colDef(show = FALSE),
                                 interpretation_criteria_text = colDef(show = FALSE),
                                 limitations = colDef(show = FALSE),
                                 exclusions = colDef(show = FALSE),
                                 criteria = colDef(show = FALSE),
                                 #pub_relacionades = colDef(show = FALSE),
                                 #notes = colDef(show = FALSE)
                                 
                                 provider = colDef(show = FALSE),
                                 mult = colDef(show = FALSE),
                                 standardized = colDef(show = FALSE),
                                 interpretation_criteria = colDef(show = FALSE),
                                 refs = colDef(show = FALSE),
                                 `Grup Desigualtats` = colDef(show = FALSE),
                                 `NA` = colDef(show = FALSE)
                                 
                                 
                               ) # close columns list 
                               
                               column_list <- column_list[!sapply(column_list, is.null)]
                               
                               summary_table <- widgetTable(
                                 data = data, columns = column_list,
                               # explicitly set the column order
                               #                               order = c("ambit", "indicador", "any", "resultat", "mitjana", "mesura", "spine_chart"), # Per algun motiu ni indicant l'ordre es canvia l'ordre de les columnes a la taula
                               
                               # include highchart dependencies otherwise charts won't render
                               deps = htmlwidgets::getDependency("echarts4r"),
                               session$sendCustomMessage("adjustRowHeight", NULL) # Altura de les files ajustable segons longitud del text
                               
  )
  
  
  
  # Return the modified summary_table
  summary_table
  
}) #%>% 
#bindEvent(input$refresh_button_resum, ignoreNULL = FALSE)












