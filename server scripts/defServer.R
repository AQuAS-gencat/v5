###############################################
#
# Server logic for definitions tab
#
###############################################


## Reactive data to be used in table
## Filter data by a) profile and b) geography levels selected by user
#tech_info <- reactive({
#  # Ensure input$ambit_search has a valid value before proceeding
#  #req(input$ambit_search)
#  
#  # Convert docu to a data table if not already one
#  docu_ambit <- as.data.table(docu) %>% 
#    select(-id_resum) %>% 
#    mutate(#propera_act = format(as.Date(propera_act), "%b-%y"),
#           data_act = format(as.Date(data_act), "%b-%y")) %>% 
#    unique()
#  
#  ## Return the data including the URL for each indicator
#  #return(docu_ambit)
#  
#  # Debug: Print the current selection and available ambits
#  #print(paste("Selected Ambit:", input$ambit_search))
#  #print("Available Ambits in Data:")
#  #print(unique(docu_ambit$ambit))
#  
#  # Filtering logic
#  #if (input$ambit_search == "all") {
#  #  filtered_ambit <- docu_ambit  # Return all rows if "Select all" is chosen
#  #} else {
#  #  filtered_ambit <- subset(docu_ambit, ambit == input$ambit_search)
#  #}
#  
#  # Debug: Print the filtered result to verify correct filtering
#  #print("Filtered Result:")
#  #print(filtered_ambit)
#  
#  #return(filtered_ambit)
#})
#
#
## Download technical info data as Excel when button clicked
#output$docu_download <- downloadHandler(
#  filename = "CentralDeResultats-Fitxes_metodologiques.xlsx",
#  content = function(file) {
#    
#    data_to_download_defs <- docu %>%
#      select(-c(id_indicador, id_resum, pbi, fitxa)) %>%
#      unique()
#    
#    # Write to Excel
#    writexl::write_xlsx(data_to_download_defs, path = file)
#  }
#)
#
#observeEvent(input$docu_meto, {
#  showModal(modalDialog(
#    title = "Document metodològic",
#    div(
#      style = "margin-top: 20px; height: 500px;", # Adjust height as needed
#      tags$iframe(
#        src = "prova.html",            # Path to the pre-rendered HTML file
#        style = "width: 100%; height: 100%; border: none;" # Ensure full modal coverage
#      )
#    ),
#    size = "l", easyClose = TRUE, footer = modalButton("Tanca")
#  ))
#})
#
#
#
######### Selector d'àmbit ######################################################
#
##output$select_ambit_defs_ui <- renderUI({
#  # Define choices as a named vector
#  #ambit_choices <- c("Select all" = "all",
#  #                   "Atenció Primària" = "Atenció Primària",
#  #                   "Atenció Hospitalària" = "Atenció Hospitalària",
#  #                   "Atenció Intermèdia" = "Atenció Intermèdia",
#  #                   "Salut Mental i Addiccions" = "Salut Mental i Addiccions",
#  #                   "Emergències Mèdiques" = "Emergències Mèdiques",
#  #                   "Salut Pública" = "Salut Pública")
#  
##  selectizeInput(
# #   "ambit_search", 
##    label = "Filtra per àmbit",
##    choices = ambits_docu#,
#    #selected = "all"
# # )
##})
#
#
## Display indicator search results in a table with expandable rows
## Using reactive data from above
#output$ind_search_results <- renderReactable({
#  
#  reactable(
#    tech_info(), # reactive data created above
#    searchable = TRUE, # include a global search bar
#    defaultPageSize = 25, # set max number of rows per page
#    theme = table_theme_defs(), # table_theme() can be found in global script
#    rowStyle = list(cursor = "pointer"),
#    onClick = "expand", # expand rows when clicked on
#    highlight = TRUE, # changes row colour when user hovers over it
#    language = reactableLang(
#      searchPlaceholder = "Escriu per cercar un indicador", # placeholder text for global search bar
#      noData = "Sense resultats", # text to display in table when no results to display
#      pageInfo = "{rowStart}\u2013{rowEnd} de {rows} indicadors", # text to display in table footer
#      pagePrevious = "Anterior",
#      pageNext = "Següent",
#      
#    ),
#    
#    # Customising all the columns in the table:
#    columns = list(
#      # Column 1: Indicator name 
#      indicador = colDef(
#        minWidth = 400,
#        show = T, 
#        html = T, 
#        name = "Indicadors",
#        cell = function(value) {
#          # Wrap the value in HTML <strong> tags for bold text
#          HTML(sprintf("<strong>%s</strong>", value))
#        },
#        #style = list(fontWeight = "bold", fontSize = "1.2rem"),
#        # display profile(s) under each indicator name
#        #cell = function(value, index) {
#        #  ambit <- tech_info()$ambit[index]
#        #  dimensio <- tech_info()$dimensio[index]
#        #  combined_text <- paste(ambit, dimensio, sep = " - ")
#        #  div(
#        #    div(style = list(fontWeight = "Medium"), value),
#        #    div(style = list(fontSize = "0.9rem"), combined_text)
#        #  )
#        #},
#        # Technical information to display when row expanded
#        # ${rowInfo.values['column name'] returns the value in that column for the particular row of data the user has expanded
#        details = JS("function(rowInfo) {
#    if (!rowInfo.values) return 'No details available';
#    
#      const data_act = rowInfo.values['data_act'] || 'N/A';
#      const propera_act = rowInfo.values['propera_act'] || 'N/A';
#      const definicio = rowInfo.values['definicio'] || 'N/A';
#      const num = rowInfo.values['num'] || 'N/A';
#      const den = rowInfo.values['den'] || 'N/A';
#      const formula = rowInfo.values['formula'] || 'N/A';
#      const unitat = rowInfo.values['unitat'] || 'N/A';
#      const int = rowInfo.values['int'] || 'N/A';
#      const justificacio = rowInfo.values['justificacio'] || 'N/A';
#      const font = rowInfo.values['font'] || 'N/A';
#      const exclusions = rowInfo.values['exclusions'] || 'N/A';
#      const limitacions = rowInfo.values['limitacions'] || 'N/A';
#      const criteris_tecnics = rowInfo.values['criteris_tecnics'] || 'N/A';
#      const pbi = rowInfo.values['pbi'] || 'N/A';  // Extract pbi link
#      const fitxa = rowInfo.values['fitxa'] || null;  // Extract fitxa link (null if NA or empty)
#      
#          // Conditionally include the Fitxa d'anàlisi button if fitxa exists
#    const fitxaButton = fitxa ? `
#        <div style='flex: 1;'>
#            <h4>Enllaç a la fitxa d'anàlisi (visió residència del pacient)</h4>
#                                                  <div style='display: flex; gap: 10px;'>
#                                                  <a class='qia-down-pbi' 
#                                                href='${fitxa}'
#                                                target='_blank'
#                                                role='button'>
#                                                  Fitxa d'anàlisi
#                </a>
#            </div>
#        </div>` : '';
#
#
#    
#    return `
#        <div class='details-container' style='display: flex; flex-direction: column;'>
#            <div style='display: flex; justify-content: space-between;'>
#                <div style='font-size: medium;'>
#                    <strong>Darrera actualització:</strong> ${data_act}
#                </div>
#                <div style='font-size: medium;'>
#                    <strong>Propera actualització:</strong> ${propera_act}
#                </div>
#            </div>
#            <div style='margin-top: 20px;'>
#                <h4>Definició</h4>
#                <div>${definicio}</div>
#            </div>
#            <div style='margin-top: 20px; display: flex;'>
#                <div style='flex: 1;'>
#                    <h4>Numerador</h4>
#                    <div>${num}</div>
#                </div>
#                <div style='flex: 1;'>
#                    <h4>Denominador</h4>
#                    <div>${den}</div>
#                </div>
#            </div>
#            <div style='margin-top: 20px; display: flex;'>
#                <div style='flex: 1;'>
#                    <h4>Fórmula</h4>
#                    <div>${formula}</div>
#                </div>
#                <div style='flex: 1;'>
#                    <h4>Unitat</h4>
#                    <div>${unitat}</div>
#                </div>
#            </div>
#              <div style='margin-top: 20px; display: flex;'>
#                  <div style='flex: 1;'>
#                      <h4>Interpretació</h4>
#                      <div>${int}</div>
#                  </div>
#                  <div style='flex: 1;'>
#                      <h4>Font</h4>
#                      <div>${font}</div>
#                  </div>
#              </div>
#            <div style='margin-top: 20px;'>
#                <h4>Justificació</h4>
#                <div>${justificacio}</div>
#            </div>
#              <div style='margin-top: 20px;'>
#                  <h4>Exclusions</h4>
#                  <div>${exclusions}</div>
#              </div>
#              <div style='margin-top: 20px;'>
#                  <h4>Limitacions</h4>
#                  <div>${limitacions}</div>
#              </div>              
#              <div style='margin-top: 20px;'>
#                  <h4>Criteris tècnics</h4>
#                  <div>${criteris_tecnics}</div>
#              </div>
#            <div style='margin-top: 20px; display: flex;'>
#                <div style='flex: 1;'>
#                    <h4>Enllaç a l'agrupació de l'indicador a l'aplicatiu PowerBI</h4>
#                                                  <div style='display: flex; gap: 10px;'>
#                                                  <a class='qia-down-pbi' 
#                                                href='${pbi}'
#                                                target='_blank'
#                                                role='button'>
#                                                  Aplicatiu PowerBI
#                                                </a>
#                                                  </div>
#                                                  </div>
#                                                  ${fitxaButton}
#                                                </div>
#                                                  </div>`;
#
#}")        ),
#      
#      ambit = colDef(sortable = TRUE, name = "Àmbit"),
#      
#      dimensio = colDef(sortable = TRUE, name = "Dimensió"),
#      
#      
#      # 2. format date column
#      data_act = colDef(defaultSortOrder = "desc", sortable = TRUE, name = "Darrera actualització"),
#      
#      tag1 = colDef(show = FALSE),
#      tag2 = colDef(show = FALSE),
#      tag3 = colDef(show = FALSE),
#      
#      # 3. hide all other columns in the table. 
#      id_indicador = colDef(show = F),
#      #ambit = colDef(show = F),
#      #data_act = colDef(show = F),
#      propera_act = colDef(show = F),
#      #dimensio = colDef(show = F),
#      definicio = colDef(show = F),
#      font = colDef(show = F),
#      num = colDef(show = F),
#      den = colDef(show = F),
#      formula = colDef(show = F),
#      unitat = colDef(show = F),
#      justificacio = colDef(show = F),
#      int = colDef(show = FALSE),
#      limitacions = colDef(show = FALSE),
#      exclusions = colDef(show = FALSE),
#      criteris_tecnics = colDef(show = FALSE),
#      #pub_relacionades = colDef(show = F),
#      #notes = colDef(show = F),
#      #id_resum = colDef(show = F),
#      pbi = colDef(show = F),
#      fitxa = colDef(show = F)
#    )
#  )
#
#})
#


### END 