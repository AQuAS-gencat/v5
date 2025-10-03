evolutiuTab <- nav_panel(
  title = "Evolució comparada",
  value = "evolutiu",
  icon = img(src="icon_evolucio_cr.svg", role="presentation", width="20", height="20"),
  
  
#  div(style = "margin:10px;",
#      h2("Evolució temporal", 
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
      #h2("Compara l'evolució dels resultats d'un indicador entre diferents centres", 
      #   style = "font-weight: bold;"),
      #br(),
      
      
      # Header with Title, Explanation, and Buttons
#      page_fillable(
#        layout_columns(
#          div(
#            div(
#              actionButton(
#                inputId = "change_center_button_evolutiu",
#                title = "Torna a la pestanya Inici del Visor per modificar el centre seleccionat",
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
#            ),
            #div(style = "display: flex; margin: 10px;",
            #    uiOutput("explicacio_evolutiu")
            #)
#          ),
#          div(
#            style = "text-align: right; margin: 10px;",
#                div(title = "Com funciona",
#                    actionButton("ajuda_evolutiu", label = "Com funciona?", icon = icon('question-circle'), class = "qia-down")),
#                #div(title = "Mostra la fitxa metodològica de l'indicador",
#                #     actionButton("def_evolutiu", label = "Fitxa de l'indicador", icon = icon('info'), class = "qia-down")),
#                div(title = "Descarrega la selecció en format Excel",
#                    downloadButton('descarrega_dades_evolutiu', 'Descarrega les dades', class = "qia-down")),
#                div(title = "Esborra els centres o territoris seleccionats",
#                    actionButton("clear_evolutiu", label = "Esborra la selecció",  icon ("eraser"), class = "qia-down"))
#          ),
#          col_widths = c(9, 3)
#        )
        
#        ),
      
      # Main Layout: Sidebar on Left, Main Content on Right

        layout_sidebar(
          fillable = F,
          
          sidebar = sidebar(
            width = '30%',
            
            accordion(
              multiple = TRUE,  # allows multiple panels to stay open
              open = c("ambit_panel_evolutiu", "indicador_panel_evolutiu", "interval_panel_evolutiu", "additional_centers_panel", "opcions_panel_evolutiu"),
              
              accordion_panel(
                value = "ambit_panel_evolutiu",
                "Pas 1: Tria un àmbit o etiqueta",
                uiOutput("select_ambit_evolutiu_ui")
              ),
              
              accordion_panel(
                value = "indicador_panel_evolutiu",
                "Pas 2: Tria un indicador",
                selectInput("select_indicador_evolutiu",
                            label = NULL,
                            choices = NULL)
              ),
              
              accordion_panel(
                value = "interval_panel_evolutiu",
                "Tria un interval",
                sliderInput(
                  inputId = "year_range_evolutiu",
                  #label = "Tria un interval:",
                  label = NULL,
                  min = 2016,  # Placeholder value
                  max = 2024,  # Placeholder value
                  value = c(2016, 2024),  # Placeholder value
                  step = 1,
                  ticks = FALSE,
                  sep = ""
                ),
              ),
              
              accordion_panel(
                value = "additional_centers_panel",
                "Afegeix altres centres",
                uiOutput("additional_centers_ui")
              ),
              
              accordion_panel(
                value = "opcions_panel_evolutiu",
                "Opcions",
                div(title = "Mostra els resultats crus o estandarditzats",
                    uiOutput("result_switch_ui")),
                #br(),
                uiOutput("selector_sexe_evolutiu_ui"),
                #br(),
                uiOutput("selector_edat_evolutiu_ui"),
                #br(),

                #br(),
                uiOutput("selector_cat_evolutiu_ui")
                #div(awesomeCheckbox("selector_cat_evolutiu", p("Mostra l'evolució global de Catalunya"), value = TRUE))#,

              )
              
            ),
            
            actionLink(
              inputId = "change_center_button_evolutiu", 
              label = HTML('<i class="fas fa-rotate-left"></i> Canvia el centre'),
              title = "Torna a la pestanya Inici per modificar el centre seleccionat",
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
              inputId = "ajuda_evolutiu", 
              label = HTML('<i class="fas fa-info-circle"></i> Ajuda')
            )

            
          ),
          
          # Main content
          navset_card_underline(
            full_screen = TRUE, # Optional: Allows full-screen for the card
            height = NULL,   # Set the height of the card
            
            # First Tab: Gràfic
            nav_panel(
              title = "Gràfic",
              bslib::card_body(
                style = "overflow-y: hidden;", # Avoid vertical scrollbars
                div(style = "text-align: left;",
                    h5(uiOutput("titol_evolutiu"), style = "color: black; text-align: left"),
                    #    #conditionalPanel(
                    #    #  condition = "output.subtipologia_exists",
                    #    #  h5(textOutput("subtitol_evolutiu"), style = "color: black; text-align: left")
                    #    #),
                    h6(textOutput("subtitol_evolutiu2"), style = "color: black; text-align: left")
                ),
                
                # Add the gear icon
                div(
                  style = "position: absolute; top: 10px; right: 10px; z-index: 100;",
                  bslib::popover(
                    id = "evolutiu_chart_controls_popover",
                    title = "Configuració del gràfic",
                    icon("gear"),
                    awesomeCheckbox(
                      inputId = "toggle_y_axis_zero_evolutiu",
                      label = "Inicia l'eix de les Y a zero",
                      value = TRUE
                    ),
                    placement = "left",
                    container = "body"
                  )
                ),
                
                withSpinner(
                  plotlyOutput("evolution_plot", height = "650px"), # Adjust graph height
                  color = "#FC8D59"
                ),
                bslib::card_footer(
                  downloadLink(
                    outputId = "descarrega_dades_evolutiu", 
                    label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                    title = "Descarrega les dades en format Excel"
                  )
                )
              )
            ),
            
            # Second Tab: Taula
            nav_panel(
              title = "Taula",
              bslib::card_body(
                style = "overflow-y: auto; text-align: left;", # Allow vertical scrolling for tables if needed
                div(style = "text-align: left;",
                    h5(uiOutput("titol_evolutiu_table"), style = "color: black; text-align: left"),
                    #    #conditionalPanel(
                    #    #  condition = "output.subtipologia_exists",
                    #    #  h5(textOutput("subtitol_evolutiu"), style = "color: black; text-align: left")
                    #    #),
                    h6(textOutput("subtitol_evolutiu2_table"), style = "color: black; text-align: left")
                ),
                withSpinner(
                  DT::dataTableOutput("evolution_table"),  # Add the DataTable output
                  color = "#FC8D59"
                ),
                bslib::card_footer(
                  downloadLink(
                    outputId = "descarrega_dades_evolutiu_taula", 
                    label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                    title = "Descarrega la taula en format Excel"
                  )
                )
              )
            ),
            
            # Third Tab: Fitxa
            nav_panel(
              title = "Fitxa",
              bslib::card_body(
                style = "overflow-y: auto;", # Allow vertical scrolling for tables if needed
                withSpinner(
                  htmlOutput("def_text_evolutiu"),
                  color = "#FC8D59"
                )
              )
            )
          )
          
        )



  ) # div
)