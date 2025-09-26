comparadorTab <- nav_panel(
  title = "Variabilitat",
  value = "variabilitat",
  icon = img(src="icon_variabilitat_cr.svg", role="presentation", width="20", height="20"),
  
  
  
  #  div(style = "margin:10px;",
  #      h2("Variabilitat", 
  #         style = "font-weight: bold;"),
  #      
  #      # Upper part: selectors, explanation, and buttons
  #      fluidRow(
  #        # Left column for selectors
  #      ),
  #      
  #      # Main panel for table display
  #      mainPanel(
  #        width = 12,
  #        class = "tableContainer",
  #        style = "margin-left:10px; margin-right:10px;",
  #        
  #      )
  #  ),
  #  br()
  #  #  ) # well panel
  #)
  
  
  
  div(style = "margin:10px;",
      #h2("Compara la variabilitat geogràfica d'un indicador en un cert moment del temps", 
      #   style = "font-weight: bold;"),
      #br(),
      
      
      # Header with Title and Explanation
      #  page_fillable(
      #    layout_columns(
      #      div(
      #        #div(style = "display: flex; margin:10px;",
      #        #    #h2("Compara la variabilitat geogràfica d'un indicador en un cert moment del temps", style = "font-weight: bold;"),
      #        #    uiOutput("explicacio_comparador")
      #        #),
      #        #div(
      #          actionButton(
      #            inputId = "change_center_button_comparador",
      #            title = "Torna a la pestanya Inici del Visor per modificar el centre seleccionat",
      #            label = tagList(
      #              #tags$i(class = "fa-solid fa-laptop-medical"),
      #              tags$span("Canvia el centre")
      #            ),
      #            class = "torna_inici",
      #            onclick = "
      #    // First, click on the Visor tab
      #    $('#nav li:contains(\"Visor\") a').click();
      #    // Use a timeout to ensure Visor loads, then click on the Inici subtab
      #    setTimeout(function() {
      #      $('#nav_visor li:contains(\"Inici\") a').click();
      #    }, 50);  // Adjust the delay if needed
      #  "
      #          )
      #        #)
      #      ),
      #div(
      #  style = "display: flex; margin: 10px;", 
      #  uiOutput("explicacio_comparador2")
      #),
      #      div(
      #        style = "text-align: right; margin: 10px;",
      #        #div(title = "Com funciona?",
      #        #    actionButton("ajuda_comparador", label = "Com funciona?", icon = icon('question-circle'), class = "qia-down")),
      #        #div(title = "Mostra la fitxa metodològica de l'indicador",
      #        #     actionButton("def_ranquing", label = "Fitxa de l'indicador", icon = icon('info'), class = "qia-down")),
      #        div(title = "Descarrega la selecció en format Excel",
      #            downloadButton('descarrega_dades_comparador', 'Descarrega les dades', class = "qia-down"))
      #      )#,
      #      col_widths = c(3, 6, 3)
      #    )
      
      #  ),
      
      # Main Layout: Sidebar on Left, Main Content on Right
      
      layout_sidebar(
        fillable = F,
        sidebar = sidebar(
          width = '30%',
          
          accordion(
            multiple = TRUE,  # allows multiple panels to stay open
            open = c("ambit_panel_rank", "indicador_panel_rank", "any_panel_rank", "opcions_panel_rank"),
            
            accordion_panel(
              value = "ambit_panel_rank",
              "Pas 1: Tria un àmbit o etiqueta",
              uiOutput("select_ambit_ranquing_ui")
            ),
            
            accordion_panel(
              value = "indicador_panel_rank",
              "Pas 2: Tria un indicador",
              uiOutput("select_indicador_ranquing_ui")
            ),
            
            accordion_panel(
              value = "any_panel_rank",
              "Pas 3: Tria un any",
              uiOutput("select_any_ranquing_ui")
            ),
            
            accordion_panel(
              value = "opcions_panel_rank",
              "Opcions",
              uiOutput("result_toggle_ui"),
              #br(),
              uiOutput("selector_sexe_rank_ui"),
              #br(),
              uiOutput("selector_edat_rank_ui"),
              #br(),
              uiOutput("cat_checkbox_ui")
            ),
            
            #          actionButton(
            #            inputId = "change_center_button_comparador",
            #            title = "Torna a la pestanya Inici del Visor per modificar el centre seleccionat",
            #            label = tagList(
            #              #tags$i(class = "fa-solid fa-laptop-medical"),
            #              tags$span("Canvia el centre")
            #            ),
            #            class = "torna_inici",
            #            onclick = "
            #    // First, click on the Visor tab
            #    $('#nav li:contains(\"Visor\") a').click();
            #    // Use a timeout to ensure Visor loads, then click on the Inici subtab
            #    setTimeout(function() {
            #      $('#nav_visor li:contains(\"Inici\") a').click();
            #    }, 50);  // Adjust the delay if needed
            #  "
            #          )
            
            
            
            #accordion_panel(
            #   value = "ajuda_panel_rank",
            #   "Ajuda", icon = icon("info-circle"),
            #actionButton("ajuda_comparador", label = "Com funciona?", icon = icon('question-circle'), class = "qia-down")
            #)
          ),
          
          
          actionLink(
            inputId = "change_center_button_comparador", 
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
          ),
          actionLink(
            inputId = "ajuda_comparador", 
            label = HTML('<i class="fas fa-info-circle"></i> Ajuda')
          )
          
          
          
        ),
        
        navset_card_underline(
          full_screen = TRUE, # Optional: Allows full-screen for the card
          height = NULL,   # Set the height of the card
          
          nav_panel(
            title = "Gràfic",
            div(style = "text-align: left;",
                h5(uiOutput("titol_comparador"), style = "color: black; text-align: left"),
                conditionalPanel(
                  condition = "output.sexe_rank_exists | output.edat_rank_exists",
                  h6(htmlOutput("subtitol_comparador2"), style = "color: black; text-align: left")
                ),
                #conditionalPanel(
                #  condition = "output.edat_rank_exists",
                #  h6(htmlOutput("subtitol_comparador_edat"), style = "color: black; text-align: left")
                #),
                conditionalPanel(
                  condition = "output.nivell_rank_exists",
                  h6(htmlOutput("nivell_comparador"), style = "color: black; text-align: left")
                )
                #conditionalPanel(
                #  condition = "output.subtipologia_rank_exists",
                #  h5(textOutput("subtitol_comparador"), style = "color: black; text-align: left")
                #),
                
                #h6(htmlOutput("subtitol_comparador3"), style = "color: black; text-align: left")
            ),
            withSpinner(plotlyOutput("ranquing"), color = "#FC8D59"),
            bslib::card_footer(
              downloadLink(
                outputId = "descarrega_dades_comparador", 
                label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                title = "Descarrega la taula en format Excel"
              )
              #div(
              #   #style = "display: flex; gap: 10px;",
              #   #div(
              #     title = "Descarrega la selecció en format Excel",
              #       downloadButton('descarrega_dades_comparador', 'Descarrega les dades', class = "qia-down")
              #  #   ),
              #   #div(title = "Descarrega el gràfic en format PNG",
              #  #     actionButton('download_png_chart', 'Descarrega el gràfic', 
              #  #                  icon = icon('file-image'), class = "qia-down"))
              # )
            )
          ),
          
          nav_panel(
            title = "Taula",
            bslib::card_body(
              style = "overflow-y: auto; text-align: left;", # Allow vertical scrolling for tables if needed
              div(style = "text-align: left;",
                  h5(uiOutput("titol_comparador_taula"), style = "color: black; text-align: left"),
                  conditionalPanel(
                    condition = "output.sexe_rank_exists | output.edat_rank_exists",
                    h6(htmlOutput("subtitol_comparador2_taula"), style = "color: black; text-align: left")
                  ),
                  #conditionalPanel(
                  #  condition = "output.edat_rank_exists",
                  #  h6(htmlOutput("subtitol_comparador_edat_taula"), style = "color: black; text-align: left")
                  #),
                  conditionalPanel(
                    condition = "output.nivell_rank_exists",
                    h6(htmlOutput("nivell_comparador_taula"), style = "color: black; text-align: left")
                  )
                  #h6(htmlOutput("subtitol_comparador2_taula"), style = "color: black; text-align: left"),
                  #h6(htmlOutput("subtitol_comparador3_taula"), style = "color: black; text-align: left")
              ),
              withSpinner(DT::dataTableOutput("rank_table"), color = "#FC8D59")  # Add the DataTable output
            ),
            bslib::card_footer(
              downloadLink(
                outputId = "descarrega_dades_comparador_taula", 
                label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                title = "Descarrega la taula en format Excel"
              )
              #div(title = "Descarrega la selecció en format Excel",
              #     downloadButton('descarrega_dades_comparador', 'Descarrega les dades', class = "qia-down"))
            )
          ),
          
          nav_panel(
            title = "Fitxa",
            bslib::card_body(
              style = "overflow-y: auto;", # Allow vertical scrolling for tables if needed
              withSpinner(htmlOutput("def_text_ranquing"), color = "#FC8D59")
            )
          ),
          
          
          
          
        )
      )
      
  ) # div
)#