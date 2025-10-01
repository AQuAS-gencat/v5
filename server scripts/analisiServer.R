# Anàlisi server

observeEvent(input$ajuda_analisi, {
  
  showModal(modalDialog(
    title = "Com funciona?",
    p("Aquesta pestanya permet seleccionar un indicador i veure'n l'evolució temporal per a tots els anys disponibles, 
    combinant múltiples centres i territoris de diferents unitats territorials. 
    Per tant, permet comparar les tendències d'aquest centres o territoris. 
    Aquesta pestanya és independent de la selecció de centre o territori realitzada a l'Inici."),
    p("Seleccionant un àmbit i un indicador al menú de l'esquerra, el gràfic mostra per 
      defecte i en vermell els resultats de Catalunya per tots els anys disponibles, resultats que es poden amagar deseleccionant la casella Mostra l'evolució global de Catalunya"),
    p("Segons l'indicador seleccionat, es mostren tants selectors com unitats territorials hi hagi amb dades disponibles, permetent seleccionar múltiples opcions, 
      que s'afegeixen automàticament al gràfic. Si està disponible, també es mostra un selector de sexe on seleccionar el que es prefereixi.
      Els centres seleccionats es poden eliminar prement el botó X o bé, dins el selector, prement el botó Esborra del teclat."),
    p("Passant per sobre els punts del gràfic apareix informació sobre el centre o territori corresponent, l'any, i el resultat de l'indicador."),
    p("Es poden descarregar tant el gràfic de barres que es visualitza en format PNG, 
      clicant la icona de la càmera fotogràfica, com les dades que alimenten el gràfic en format Excel, clicant el botó Descarrega les dades."),
    
    size = "l", easyClose = TRUE, footer = modalButton("Tanca")))
}) 

output$select_ambit_analisi_ui <- renderUI({
  # Your existing code for ambit selection
  ambit <- dades_r_tbl %>%
    filter(visio == "P") %>% 
    pull(ambit) %>%
    as.character() %>%
    unique() %>%
    sort()
  
  etiqueta <- dades_r_tbl %>%
    filter(visio == "P") %>% 
    select(#tag1, tag2, 
           etiqueta) %>%
    pivot_longer(cols = everything(), names_to = "etiqueta", values_to = "value") %>%
    filter(!is.na(value)) %>%
    distinct(value) %>%
    collect() %>%
    pull(value)
  
  # Combine into a named list for grouping
  grouped_choices_analisi <- list(
    "Àmbit" = as.list(ambit),
    "Etiqueta" = as.list(etiqueta)
  )
  
  div(style = "margin-top: 20px; margin-bottom: 20px;",
      selectInput("select_ambit_analisi", 
                  label = "Pas 1: Tria un àmbit o etiqueta", 
                  choices=grouped_choices_analisi, 
                  selected = r_selected_values$ambit)
  )
})

output$select_indicador_analisi_ui <- renderUI({
  # Your existing code for indicator selection
  filtered_data <- dades_r_tbl %>%
    distinct(codi_indicador, nom_indicador, dimensio, ambit, #tag1, tag2, 
             etiqueta) %>%
    filter(
      (ambit == r_selected_values$ambit |
         #tag1 == r_selected_values$ambit |
         #tag2 == r_selected_values$ambit |
         etiqueta == r_selected_values$ambit)
    ) %>%
    mutate(codi = nom_indicador) %>%
    collect()
  
  ind_choices_analisi <- filtered_data %>%
    split(.$dimensio) %>%
    lapply(function(x) {
      i <- sort(x$nom_indicador)
      names(i) <- i
      i
    })
  
  # Add the special HTML file as an additional option
  #ind_choices_analisi[["Mostra fitxes noves"]] <- list(
  #  "Atenció malaltia cardiovascular bon control 4" = "fitxa_especial_cardiovascular"
  #)
  
  div(
    selectInput("select_indicador_analisi", 
                label = "Pas 2: Tria un indicador", 
                choices = ind_choices_analisi,
                selected = r_selected_values$indicador)
  )
})

# Add this observer to keep selected_values$indicador in sync
observe({
  req(input$select_indicador_analisi)
  r_selected_values$indicador <- input$select_indicador_analisi
})

# This reactive expression gets the ID for the selected indicator
indicador_id_reactive <- reactive({
  req(input$select_indicador_analisi)
  
  # Check if it's the special cardiovascular file
#  if(input$select_indicador_analisi == "fitxa_especial_cardiovascular") {
#    return("fitxa_especial_cardiovascular")
#  }
  
  # Get the id_indicador for the selected indicator
  id_data <- dades_r_tbl %>%
    filter(nom_indicador == r_selected_values$indicador) %>%
    distinct(codi_indicador) %>%
    collect()
  
  
  print("id_data")
  print(id_data)
  
  if(nrow(id_data) > 0 && !is.na(id_data$codi_indicador[1])) {
    return(id_data$codi_indicador[1])
  } else {
    return(NULL)
  }
})

# Improved HTML rendering function
output$analisi_html <- renderUI({
  # Get the indicator ID using the reactive expression
  id_indicador <- indicador_id_reactive()
  
  # Add debugging
  message("Selected indicator: ", input$select_indicador_analisi)
  message("ID: ", id_indicador)
  
  if(is.null(id_indicador)) {
    return(div("No analysis available for this indicator."))
  }
  
  # Check if it's the special cardiovascular file
#  if(id_indicador == "fitxa_especial_cardiovascular") {
#    file_path <- "fitxes/Fitxa Atenció Primària_Atenció malaltia cardiovascular bon control 4.html"
#    
#    # Return the iframe with the special file
#    return(tags$iframe(
#      src = file_path,
#      width = "100%",
#      height = "800px",
#      frameborder = "0"
#    ))
#  }
  
  # List all HTML files in the fitxes directory
  all_files <- list.files("www/fitxes", pattern = "\\.html$", full.names = FALSE)
  
  # Look for files that start with "CdR_" followed by the indicator ID
#  pattern <- paste0("^CdR_", id_indicador, "_")
  pattern <- paste0(id_indicador)
  matching_files <- all_files[grep(pattern, all_files)]
  
  # Add debugging
  message("Pattern: ", pattern)
  message("Matching files: ", paste(matching_files, collapse = ", "))
  
  if(length(matching_files) > 0) {
    # Use the first matching file
    file_path <- paste0("fitxes/", matching_files[1])
    
    # Return the iframe with the correct file
    tags$iframe(
      src = file_path,
      width = "100%",
      height = "800px",
      frameborder = "0"
    )
  } else {
    div("Fitxa d'anàlisi no disponible per a l'indicador seleccionat.")
  }
})

#output$descarrega_fitxa_analisi <- downloadHandler(
#  filename = function() {
#    # Get the indicator ID
#    id_indicador <- indicador_id_reactive()
#    
#    # Check if it's the special cardiovascular file
#    if(id_indicador == "fitxa_especial_cardiovascular") {
#      return("Fitxa Atenció Primària_Atenció malaltia cardiovascular bon control 4.html")
#    }
#    
#    # List all available HTML files
#    all_files <- list.files("www/fitxes", pattern = "\\.html$", full.names = FALSE)
#    
#    # Find the matching file based on the indicator ID
#    pattern <- paste0("^CdR_", id_indicador, "_")
#    matching_files <- all_files[grep(pattern, all_files)]
#    
#    # If a matching file exists, use it; otherwise, return a default name
#    if (length(matching_files) > 0) {
#      return(matching_files[1])  # Keep the original filename
#    } else {
#      return("fitxa_no_disponible.html")  # Fallback case
#    }
#  },
#  content = function(file) {
#    id_indicador <- indicador_id_reactive()
#    
#    # Check if it's the special cardiovascular file
#    if(id_indicador == "fitxa_especial_cardiovascular") {
#      special_file_path <- "www/fitxes/Fitxa Atenció Primària_Atenció malaltia cardiovascular bon control 4.html"
#      if(file.exists(special_file_path)) {
#        file.copy(special_file_path, file)
#      } else {
#        writeLines("<html><body><h1>Fitxa especial no trobada</h1></body></html>", file)
#      }
#      return()
#    }
#    
#    # Get the file path
#    all_files <- list.files("www/fitxes", pattern = "\\.html$", full.names = TRUE)
#    pattern <- paste0("^CdR_", id_indicador, "_")
#    matching_files <- all_files[grep(pattern, all_files)]
#    
#    if (length(matching_files) > 0) {
#      # Copy the file to the temporary location
#      file.copy(matching_files[1], file)
#    } else {
#      # If no file is found, create a placeholder file
#      writeLines("<html><body><h1>Fitxa no disponible</h1></body></html>", file)
#    }
#  }
#)