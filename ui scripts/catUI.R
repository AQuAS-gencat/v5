catTab <- nav_panel(
  title = "Catalunya",  # Simplified title without icon
  value = "cat",
  
  div(style = "margin:10px;",
      #h2("Visualitza un resum global de tots els indicadors", 
      #   style = "font-weight: bold;"),
      
      fluidRow(
        
        column(
          width = 2,
          
          div(
            title = "Selecciona l'any del qual vols veure dades.",
            uiOutput("select_any_cat_ui")
          )
          
        ),
        #column(
        #  width = 6
        #),
        column(
          width = 3,
          aligh = "left",
          div(style = "margin:10px;",
              title = "Descarrega la selecció en format Excel",
              downloadButton("download_summary_cat", "Descarrega les dades", class = "qia-down")
          )
        )
        

        
      ),
      
      # Main panel for table display
      page_fillable(
        class = "tableContainer",
        style = "margin-left:10px; margin-right:10px;",
        #shiny::hr(),
        # Title area
        div(
          style = "display: flex; justify-content: space-between; padding: 10px;",
          div(
            style = "color: black; margin-left: 10px;", 
            uiOutput("titol_taula_cat")
          )
        ),
        # Table output with spinner
        withSpinner(uiOutput("taula_resum_cat"), color = "orange"),
        br(),
        hr()
      )
      
  )
)