analisiTab <- nav_panel(
  title = "Anàlisi detallada",
  value = "analisi",
  icon = icon("magnifying-glass-chart"),
  
  div(style = "margin:10px;",
      #h2("Fitxa d'anàlisi", style = "font-weight: bold;"),
      
      # Header with Title, Explanation, and Buttons
#      page_fillable(
#        layout_columns(
#          div(
#            div(
#              actionButton(
#                inputId = "r_change_center_button_evolutiu",
#                title = "Torna a la pestanya Inici del Visor per canviar el territori seleccionat",
#                label = tagList(
#                  #tags$i(class = "fa-solid fa-laptop-medical"),
#                  tags$span("Canvia el territori")
#                ),
#                class = "torna_inici",
#                onclick = "
#    // First, click on the Visor tab
#    $('#nav li:contains(\"Visió territorial\") a').click();
#    // Use a timeout to ensure Visor loads, then click on the Inici subtab
#    setTimeout(function() {
#      $('#nav_residencia li:contains(\"Inici\") a').click();
#    }, 50);  // Adjust the delay if needed
#  "
#              )
#            ),
            #div(style = "display: flex; margin: 10px;",
#           #     uiOutput("r_explicacio_evolutiu")
            #)
#          ),
#          div(
#            style = "text-align: right; margin: 10px;",
#            #div(title = "Com funciona",
#            #    actionButton("r_ajuda_evolutiu", label = "Com funciona?", icon = icon('question-circle'), class = "qia-down")),
#            #div(title = "Mostra la fitxa metodològica de l'indicador",
#            #     actionButton("def_evolutiu", label = "Fitxa de l'indicador", icon = icon('info'), class = "qia-down")),
#            div(title = "Descarrega la fitxa d'anàlisi en format HTML",
#                downloadButton('descarrega_fitxa_analisi', 'Descarrega la fitxa', class = "qia-down")),
#            #div(title = "Esborra els centres o territoris seleccionats",
#            #    actionButton("r_clear_evolutiu", label = "Esborra la selecció",  icon ("eraser"), class = "qia-down"))
#          ),
#          col_widths = c(9, 3)
#        )
#      ),
      
      

        layout_sidebar(
          fillable = F,
          sidebar = sidebar(
            width = '25%',
            
            div(title = "Selecciona l'àmbit o etiqueta amb el qual ordenar els indicadors.",
                uiOutput("select_ambit_analisi_ui")),
            
            div(title = "Selecciona un indicador per poder-ne comparar els resultats.",
                uiOutput("select_indicador_analisi_ui")),
            
            actionLink(
              inputId = "ajuda_analisi", 
              label = HTML('<i class="fas fa-info-circle"></i> Ajuda')
            )#,
            
            #downloadLink(
            #  outputId = "descarrega_fitxa_analisi", 
            #  label = HTML('<i class="fas fa-download"></i> Descarrega la fitxa'),
            #  title = "Descarrega la fitxa d'anàlisi en format HTML"
            #)
          ),
          
          #navset_card_underline(
            #textOutput("value")
            htmlOutput("analisi_html")  # This will dynamically load the HTML file
          #)
        )

  )
)
