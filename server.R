server = function(input, output, session) {
  
  # VISIÓ CENTRE ----
  
  # Reactive values for geography and center
  selected_values <- reactiveValues(
    geography = "Centre (Unitat proveïdora)", 
    #geography = NULL, 
    center = NULL, 
    ambit = NULL,
    indicador = NULL)
  
#  # Update selected geography reactively based on input
#  observe({
#    selected_values$geography <- input$geography_level
#  })
  
  # Update selected center reactively based on center selection
  observe({
    selected_values$center <- input$center
  })
  
  # Add observers to update the values when inputs change
#  observe({
#    if (!is.null(input$geography_level)) {
#      selected_values$geography <- input$geography_level
#    }
#  })
  
  observe({
    if (!is.null(input$center)) {
      selected_values$center <- input$center
    }
  })
  
  # Add validation to prevent NULL values from propagating
  observe({
    req(input$select_ambit_evolutiu)
    selected_values$ambit <- input$select_ambit_evolutiu
  })
  
  observe({
    req(input$select_ambit_ranquing)
    selected_values$ambit <- input$select_ambit_ranquing
  })
  
  observe({
    req(input$select_ambit_estrat)
    selected_values$ambit <- input$select_ambit_estrat
  })
  
  observe({
    req(input$select_indicador_evolutiu)
    selected_values$indicador <- input$select_indicador_evolutiu
  })
  
  observe({
    req(input$select_indicador_ranquing)
    selected_values$indicador <- input$select_indicador_ranquing
  })
  
  observe({
    req(input$select_indicador_estrat)
    selected_values$indicador <- input$select_indicador_estrat
  })
  
  
  
  # Center selector depends on the geography level
#  output$center_selector <- renderUI({
#    #req(input$geography_level)
#    
#    # Filter centers based on selected geography level
#    centers <- dades_simplificades %>%
#      filter(Granularitat == selected_values$geography)
#    
#    # Generate choices based on filtered data
#    centre_choices <- lapply(split(centers, centers$tipus_centre), function(x) {
#      i <- sort(x$`Centre/Territori`)
#      names(i) <- sort(x$`Centre/Territori`)
#      i
#    })
#    
#    selectizeInput("center", "Selecciona un centre:", choices = centre_choices)
#  })
  
  
  
  # Update selected ambit reactively based on input
  observe({
    selected_values$ambit <- input$select_ambit_ranquing
  })
  
  observe({
    selected_values$ambit <- input$select_ambit_estrat
  })
  
  observe({
    selected_values$ambit <- input$select_ambit_evolutiu
  })
  
  observe({
    req(selected_values$ambit)
    
    # Update all ambit selectors in other tabs
    updateSelectInput(session, "select_ambit_ranquing", selected = selected_values$ambit)
    updateSelectInput(session, "select_ambit_estrat", selected = selected_values$ambit)
    updateSelectInput(session, "select_ambit_evolutiu", selected = selected_values$ambit)
  })
  
  
  
  # Update selected indicador reactively based on input
  observe({
    selected_values$indicador <- input$select_indicador_ranquing
  })
  
  observe({
    selected_values$indicador <- input$select_indicador_estrat
  })
  
  observe({
    selected_values$indicador <- input$select_indicador_evolutiu
  })
  
  observe({
    req(selected_values$indicador)
    
    # Update all ambit selectors in other tabs
    updateSelectInput(session, "select_indicador_ranquing", selected = selected_values$indicador)
    updateSelectInput(session, "select_indicador_estrat", selected = selected_values$indicador)
    updateSelectInput(session, "select_indicador_evolutiu", selected = selected_values$indicador)
  })
  
  
  # VISIÓ RESIDÈNCIA PACIENT ----
  
  # Reactive values for geography and center
  r_selected_values <- reactiveValues(
    geography = NULL, 
    center = NULL, 
    ambit = NULL,
    indicador = NULL)
  
  # Update selected geography reactively based on input
  observe({
    r_selected_values$geography <- input$r_geography_level
  })
  
  # Update selected center reactively based on center selection
  observe({
    r_selected_values$center <- input$r_center
  })
  
  
  # Add observers to update the values when inputs change
  observe({
    if (!is.null(input$r_geography_level)) {
      r_selected_values$geography <- input$r_geography_level
    }
  })
  
  observe({
    if (!is.null(input$r_center)) {
      r_selected_values$center <- input$r_center
    }
  })
  
  # Add validation to prevent NULL values from propagating
  observe({
    req(input$r_select_ambit_evolutiu)
    r_selected_values$ambit <- input$r_select_ambit_evolutiu
  })
  
  observe({
    req(input$r_select_ambit_ranquing)
    r_selected_values$ambit <- input$r_select_ambit_ranquing
  })
  
  observe({
    req(input$r_select_ambit_estrat)
    r_selected_values$ambit <- input$r_select_ambit_estrat
  })
  
#  observe({
#    req(input$select_ambit_analisi)
#    r_selected_values$ambit <- input$select_ambit_analisi
#  })
  
  observe({
    req(input$map_ambit)
    r_selected_values$ambit <- input$map_ambit
  })
  
  observe({
    req(input$r_select_indicador_evolutiu)
    r_selected_values$indicador <- input$r_select_indicador_evolutiu
  })
  
  observe({
    req(input$r_select_indicador_ranquing)
    r_selected_values$indicador <- input$r_select_indicador_ranquing
  })
  
  observe({
    req(input$r_select_indicador_estrat)
    r_selected_values$indicador <- input$r_select_indicador_estrat
  })
  
#  observe({
#    req(input$select_indicador_analisi)
#    r_selected_values$indicador <- input$select_indicador_analisi
#  })
  
  observe({
    req(input$map_indicador)
    r_selected_values$indicador <- input$map_indicador
  })
  
  
#  # Center selector depends on the geography level
#  output$r_center_selector <- renderUI({
#    req(input$r_geography_level)
#    
#    # Filter centers based on selected geography level
#    centers <- dades_simplificades %>%
#      filter(Granularitat == r_selected_values$geography)
#    
#    # Generate choices based on filtered data
#    centre_choices <- lapply(split(centers, centers$tipus_centre), function(x) {
#      i <- sort(x$`Centre/Territori`)
#      names(i) <- sort(x$`Centre/Territori`)
#      i
#    })
#    
#    selectizeInput("r_center", "Selecciona un territori:", choices = centre_choices)
#  })
  
  
  # Update selected ambit reactively based on input
  observe({
    r_selected_values$ambit <- input$r_select_ambit_ranquing
  })
  
  observe({
    r_selected_values$ambit <- input$r_select_ambit_estrat
  })
  
  observe({
    r_selected_values$ambit <- input$r_select_ambit_evolutiu
  })
  
  observe({
    r_selected_values$ambit <- input$select_ambit_analisi
  })
  
#  observe({
#    r_selected_values$ambit <- input$map_ambit
#  })
  
  observe({
    req(r_selected_values$ambit)
    
    # Update all ambit selectors in other tabs
    updateSelectInput(session, "r_select_ambit_ranquing", selected = r_selected_values$ambit)
    updateSelectInput(session, "r_select_ambit_estrat", selected = r_selected_values$ambit)
    updateSelectInput(session, "r_select_ambit_evolutiu", selected = r_selected_values$ambit)
    updateSelectInput(session, "select_ambit_analisi", selected = r_selected_values$ambit)
    updateSelectInput(session, "map_ambit", selected = r_selected_values$ambit)
  })
  
  
  
  # Update selected indicador reactively based on input
  observe({
    r_selected_values$indicador <- input$r_select_indicador_ranquing
  })
  
  observe({
    r_selected_values$indicador <- input$r_select_indicador_estrat
  })
  
  observe({
    r_selected_values$indicador <- input$r_select_indicador_evolutiu
  })
  
#  observe({
#    r_selected_values$indicador <- input$select_indicador_analisi
#  })
  
  #observe({
  #  r_selected_values$indicador <- input$map_indicador
  #})
  
  observe({
    req(r_selected_values$indicador)
    
    # Update all ambit selectors in other tabs
    updateSelectInput(session, "r_select_indicador_ranquing", selected = r_selected_values$indicador)
    updateSelectInput(session, "r_select_indicador_estrat", selected = r_selected_values$indicador)
    updateSelectInput(session, "r_select_indicador_evolutiu", selected = r_selected_values$indicador)
    updateSelectInput(session, "select_indicador_analisi", selected = r_selected_values$indicador)
    updateSelectInput(session, "map_indicador", selected = r_selected_values$indicador)
  })
  
  
  
  
  
  # Add this after the existing reactiveValues declarations in server.R
  chart_settings <- reactiveValues(
    y_axis_zero = TRUE  # Default setting for all charts
  )
  
  
  
  
  
  
  
  
  
  observeEvent(input$go_to_defs, {
    
    # a. navigate to the profiles tab 
    bslib::nav_select(id = "nav", 
                      selected = "defs",
                      session = session)
  
    
    #print("Navigating to defs")
    #updateTabsetPanel(session, "nav", selected = "defs")
    #updateNavbarPage(session, "nav", selected = "defs")
  })
  
  
  # Source files with server code for each tab -----------------------------------------
  source(file.path("server scripts/iniciServer.R"), local = TRUE)$value # Pestanya d'inici
  source(file.path("server scripts/selectorServer.R"), local = TRUE)$value # Pestanya selector
  source(file.path("server scripts/r_selectorServer.R"), local = TRUE)$value # Pestanya selector
  source(file.path("server scripts/resumServer.R"), local = TRUE)$value # Pestanya resum
  source(file.path("server scripts/r_resumServer.R"), local = TRUE)$value # Pestanya resum
  source(file.path("server scripts/comparadorServer.R"), local = TRUE)$value # Pestanya comparador    
  source(file.path("server scripts/r_comparadorServer.R"), local = TRUE)$value # Pestanya comparador   
  source(file.path("server scripts/evolutiuServer.R"), local = TRUE)$value # Pestanya evolutiu
  source(file.path("server scripts/r_evolutiuServer.R"), local = TRUE)$value # Pestanya evolutiu
  source(file.path("server scripts/estratServer.R"), local = TRUE)$value # Pestanya estratificació
  source(file.path("server scripts/r_estratServer.R"), local = TRUE)$value # Pestanya estratificació
#  source(file.path("server scripts/procServer.R"), local = TRUE)$value # Pestanya proc
  source(file.path("server scripts/mapaServer.R"), local = TRUE)$value # Pestanya mapa
  source(file.path("server scripts/analisiServer.R"), local = TRUE)$value # Pestanya anàlisi
  source(file.path("server scripts/dadesServer.R"), local = TRUE)$value # Pestanya dades
#  source(file.path("server scripts/defServer.R"), local = TRUE)$value # Pestanya def
#  source(file.path("server scripts/aboutServer.R"), local = TRUE)$value # Pestanya about
  
  
  # Keeps the shiny app from timing out quickly 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
}

if (env_prod) {
  auth0_server(server)
} else {
  server
}