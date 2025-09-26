resumTab <- nav_panel(
  title = "Resum",  # Simplified title without icon
  value = "resum",
  icon = img(src="icon_resum_cr.svg", role="presentation", width="20", height="20"),

      
      # Wrapper to style the entire wellPanel section
#      wellPanel(
        #style = "background: white;",
        
        div(style = "margin:10px;",
            #h2("Visualitza un resum dels resultats d'un centre o territori en tots els indicadors disponibles", 
            #   style = "font-weight: bold;"),
            
        # Upper part: selectors, explanation, and buttons
        fluidRow(
          # Left column for selectors
#          column(
#            width = 3, 
#            br(),
#            tags$div(
#              #class = "card-button",
#              actionButton(
#                inputId = "change_center_button",
#                title = "Torna a la pestanya Inici del Visor per modificar el centre o territori seleccionat",
#                label = tagList(
#                  #tags$i(class = "fa-solid fa-laptop-medical"),
#                  tags$span("Canvia el centre")
#                ),
#                class = "torna_inici",
#                onclick = "
#    // First, click on the Visor tab
#    $('#nav li:contains(\"Visor\") a').click();
#    // Use a timeout to ensure Visor loads, then click on the Inici subtab
#    setTimeout(function() {
#      $('#nav_visor li:contains(\"Inici\") a').click();
#    }, 50);  // Adjust the delay if needed
#  "
#              )
#            )
#            #br(),
#            #tags$a("Go to home page", onclick="customHref('inici')"),
#
#          ),
          
          # Middle column for explanation
          column(
            width = 2,
            div(title = "Selecciona l'any del qual vols veure dades.",
                uiOutput("select_any_global_ui"))
          ),
          
          column(
            width = 7,
            div(
              style = "text-align: center; margin:30px;", 
              title = "Mostra els resultats crus o estandarditzats",
                uiOutput("taula_switch_resum_ui")
            )
          ),
          
          column(
            width = 3,
            align = "right",
              div(#style = "margin:30px;",
                actionLink(
                  inputId = "change_center_button", 
                  label = HTML('<i class="fas fa-rotate-left"></i> Canvia el centre'),
                  title = "Torna a la pestanya Inici del Visor per modificar el centre seleccionat",
                  onclick = "
                       // First, click on the Visor tab
                       $('#nav li:contains(\"Visor\") a').click();
                       // Use a timeout to ensure Visor loads, then click on the Inici subtab
                       setTimeout(function() {
                         $('#nav_visor li:contains(\"Inici\") a').click();
                       }, 50);  // Adjust the delay if needed
                     "
                )
                #title = "Descarrega la selecció en format Excel",
                #downloadButton("download_summary_excel", "Descarrega les dades", class = "qia-down")
              ),
            br(),
            div(
              actionLink(
                inputId = "int_resum", 
                label = HTML('<i class="fas fa-info-circle"></i> Ajuda')
              )
            ),
            div(
              actionLink(
                inputId = "download_summary_excel", 
                label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                title = "Descarrega la taula en format Excel"
              )
            )
          )
          
          # Right column for help and download buttons
          #column(
          #  width = 3, align = "right",
          #  #div(
          #  #  title = "Com funciona aquesta pestanya?",
          #  #  actionButton("ajuda_resum", label = "Com funciona?", icon = icon('question-circle'), class = "qia-down"),
          #  #  br()
          #  #),
          #  #div(
          #  #  title = "Com s'interpreten la icona de variació i el gràfic resum?",
          #  #  actionButton("int_resum", label = "Interpretació", icon = icon('question-circle'), class = "qia-down"),
          #  #  br()
          #  #),
          #  div(
          #    title = "Descarrega la selecció en format Excel",
          #    downloadButton("download_summary_excel", "Descarrega les dades", class = "qia-down")
          #  )#,
          #  #div(
          #  #  title = "Descarrega la taula en format PDF",
          #  #  downloadButton("download_table_pdf", "Descarrega en PDF", class = "qia-down")
          #  #)
          #)
        ),
        #fluidRow(
        #  div(
        #    br(),
        #    uiOutput("explicacio_resum"))
        #),
        
        # Lower part: images in two columns
        #fluidRow(
        #  column(width = 6,  # Left column for images
        #         div(style = "display: flex; justify-content: right; margin: 15px;", img(src = "llegenda_resum.png", width = "80%"))
        #  ),
        #  column(width = 6,  # Right column for images
        #         div(style = "display: flex; justify-content: left; margin: 15px;", img(src = "fletxes.png", width = "60%"))
        #  )
        #),
        
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
              uiOutput("titol_taula")
            )
          ),
          # Table output with spinner
          withSpinner(uiOutput("taula_resum"), color = "#FC8D59"),
          br(),
          hr()
        )
      ),
      br()
#  ) # well panel
)
