estratTab <- nav_panel(
  title = "Evolució estratificada",  # Simplified title without icon
  value = "estrat",
  icon = icon("layer-group"),
  

  div(style = "margin:10px;",
      #h2("Estratificació per edat i sexe", 
      #   style = "font-weight: bold;"),
      
      # Header with Title and Explanation
#      page_fillable(
#        layout_columns(
#            div(
#              actionButton(
#                inputId = "change_center_button_estrat",
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
#
#
#            div(#style = "display: flex; margin: 10px;" 
#                #uiOutput("explicacio_comparador2")
#            ),
#
#
#            div(style = "text-align: right; margin: 10px;",
#                div(title = "Com funciona?",
#                    actionButton("ajuda_estrat", label = "Com funciona?", icon = icon('question-circle'), class = "qia-down"))#,
#                # div(title = "Mostra la fitxa metodològica de l'indicador",
#                #     actionButton("def_estrat", label = "Fitxa de l'indicador", icon = icon('info'), class = "qia-down")),
#                # div(title = "Descarrega la selecció en format Excel",
#                #     downloadButton('descarrega_dades_estrat', 'Descarrega les dades', class = "qia-down")),
#                #br()
#            ),
#
#          col_widths = c(4, 5, 3)
#        )
#
#
#      ),
      
      
      # Main Layout: Sidebar on Left, Main Content on Right
        layout_sidebar(
          fillable = F,
          
          sidebar = sidebar(
            width = '30%',
            
            accordion(
              
              multiple = TRUE,  # allows multiple panels to stay open
              open = c("ambit_panel_estrat", "indicador_panel_estrat", "interval_panel_estrat"),
              
              accordion_panel(
                value = "ambit_panel_estrat",
                "Pas 1: Tria un àmbit o etiqueta",
                uiOutput("select_ambit_estrat_ui")
              ),
              
              accordion_panel(
                value = "indicador_panel_estrat",
                "Pas 2: Tria un indicador",
                uiOutput("select_indicador_estrat_ui")
              ),
              
              accordion_panel(
                value = "interval_panel_estrat",
                "Tria un interval",
                sliderInput(
                  inputId = "year_range_estrat",
                  label = NULL,
                  #label = "Tria un interval:",
                  #shiny::HTML("<p class='step-text'>Tria un interval</p>"), 
                  min = 2016,  # Placeholder value
                  max = 2024,  # Placeholder value
                  value = c(2016, 2024),  # Placeholder value
                  step = 1,
                  ticks = F,
                  sep = ""
                )
              )
              
            ),
            
            
            actionLink(
              inputId = "change_center_button_estrat", 
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
              inputId = "ajuda_estrat", 
              label = HTML('<i class="fas fa-info-circle"></i> Ajuda')
            )
              

            


            #br(),

            ),
          
          navset_card_underline(
            height = NULL,
            
            nav_panel(
              title = "Sexe",
              
              # Show content when sexe stratification exists
              conditionalPanel(
                condition = "output.has_sexe_stratification",
                
                bslib::card(
                  full_screen = T,
                  height = "650px",
                  div(style = "text-align: left;",
                      h5(uiOutput("titol_sexe"), style = "color: black; text-align: left"),
                      h6(textOutput("subtitol_sexe"), style = "color: black; text-align: left"),
                      h6(textOutput("subtitol_sexe2"), style = "color: black; text-align: left"),
                      
                      div(
                        style = "position: absolute; top: 10px; right: 10px; z-index: 100;",
                        bslib::popover(
                          id = "chart_controls_popover_sexe",
                          title = "Configuració del gràfic",
                          icon("gear"),
                          awesomeCheckbox(
                            inputId = "toggle_y_axis_zero_sexe",
                            label = "Inicia l'eix de les Y a zero",
                            value = TRUE
                          ),
                          placement = "left",
                          container = "body"
                        )
                      )
                  ),
                  withSpinner(
                    plotlyOutput("sexe_plot"),
                    color = "#FC8D59"
                  ),
                  bslib::card_footer(
                    downloadLink(
                      outputId = "descarrega_dades_sexe", 
                      label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                      title = "Descarrega les dades en format Excel"
                    )
                  )
                ),
                
                bslib::card(
                  full_screen = T,
                  height = "650px",
                  div(style = "text-align: left;",
                      h5(uiOutput("titol_sexe_taula"), style = "color: black; text-align: left"),
                      h6(textOutput("subtitol_sexe_taula"), style = "color: black; text-align: left"),
                      h6(textOutput("subtitol_sexe2_taula"), style = "color: black; text-align: left")
                  ),
                  withSpinner(
                    DT::dataTableOutput("sexe_table"),
                    color = "#FC8D59"
                  ),
                  bslib::card_footer(
                    downloadLink(
                      outputId = "descarrega_dades_sexe_taula", 
                      label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                      title = "Descarrega la taula en format Excel"
                    )
                  )
                )
              ),
              
              # Show message when sexe stratification doesn't exist
              conditionalPanel(
                condition = "!output.has_sexe_stratification",
                div(
                  style = "padding: 20px; text-align: center;",
                  h4("Contingut no disponible"),
                  p("Aquest indicador no té estratificació per sexe disponible.")
                )
              )
            ),
            
            nav_panel(
              title = "Edat",
              
              # Show content when edat stratification exists
              conditionalPanel(
                condition = "output.has_edat_stratification",
                
                bslib::card(
                  full_screen = T,
                  height = "650px",
                  div(style = "text-align: left;",
                      h5(uiOutput("titol_edat"), style = "color: black; text-align: left"),
                      h6(textOutput("subtitol_edat"), style = "color: black; text-align: left"),
                      h6(textOutput("subtitol_edat2"), style = "color: black; text-align: left"),
                      
                      div(
                        style = "position: absolute; top: 10px; right: 10px; z-index: 100;",
                        bslib::popover(
                          id = "edat_chart_controls_popover",
                          title = "Configuració del gràfic",
                          icon("gear"),
                          awesomeCheckbox(
                            inputId = "toggle_y_axis_zero_edat",
                            label = "Inicia l'eix de les Y a zero",
                            value = TRUE
                          ),
                          placement = "left",
                          container = "body"
                        )
                      )
                  ),
                  withSpinner(
                    plotlyOutput("edat_plot"),
                    color = "#FC8D59"
                  ),
                  bslib::card_footer(
                    downloadLink(
                      outputId = "descarrega_dades_edat", 
                      label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                      title = "Descarrega la taula en format Excel"
                    )
                  )
                ),
                
                bslib::card(
                  full_screen = T,
                  height = "650px",
                  div(style = "text-align: left;",
                      h5(uiOutput("titol_edat_taula"), style = "color: black; text-align: left"),
                      h6(textOutput("subtitol_edat_taula"), style = "color: black; text-align: left"),
                      h6(textOutput("subtitol_edat2_taula"), style = "color: black; text-align: left")
                  ),
                  withSpinner(
                    DT::dataTableOutput("edat_table"),
                    color = "#FC8D59"
                  ),
                  bslib::card_footer(
                    downloadLink(
                      outputId = "descarrega_dades_edat_taula", 
                      label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                      title = "Descarrega la taula en format Excel"
                    )
                  )
                )
              ),
              
              # Show message when edat stratification doesn't exist
              conditionalPanel(
                condition = "!output.has_edat_stratification",
                div(
                  style = "padding: 20px; text-align: center;",
                  h4("Contingut no disponible"),
                  p("Aquest indicador no té estratificació per grup d'edat disponible.")
                )
              )
            ),
            
            nav_panel(
              title = "Fitxa",
              
              bslib::card_body(
                style = "overflow-y: auto;",
                withSpinner(htmlOutput("def_text_estrat"), color = "#FC8D59")
              )
            )
          )
                 

               )

  )
)

        