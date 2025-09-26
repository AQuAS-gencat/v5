restratTab <- nav_panel(
  title = "Evolució estratificada",  # Simplified title without icon
  value = "estrat-residencia",
  icon = icon("layer-group"),
  
  
  div(style = "margin:10px;",
      #h2("Estratificació per edat i sexe", 
      #   style = "font-weight: bold;"),
      
      # Header with Title and Explanation
#      page_fillable(
#        layout_columns(
#               div(
#                 actionButton(
#                   inputId = "r_change_center_button_estrat",
#                   title = "Torna a la pestanya Inici del Visor per canviar el territori seleccionat",
#                   label = tagList(
#                     #tags$i(class = "fa-solid fa-laptop-medical"),
#                     tags$span("Canvia el territori")
#                   ),
#                   class = "torna_inici",
#                   onclick = "
#    // First, click on the Visor tab
#    $('#nav li:contains(\"Visió territorial\") a').click();
#    // Use a timeout to ensure Visor loads, then click on the Inici subtab
#    setTimeout(function() {
#      $('#nav_residencia li:contains(\"Inici\") a').click();
#    }, 50);  // Adjust the delay if needed
#  "
#                 )
#               ),
#               
#               div(),
#               
#               div(
#                 style = "text-align: right; margin: 10px;",
#                 div(title = "Com funciona?",
#                     actionButton("r_ajuda_estrat", label = "Com funciona?", icon = icon('question-circle'), class = "qia-down"))
#               ),
#               col_widths = c(4, 5, 3)
#        ),
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
                   open = c("r_ambit_panel_estrat", "r_indicador_panel_estrat", "r_interval_panel_estrat"),
                   
                   accordion_panel(
                     value = "r_ambit_panel_estrat",
                     "Pas 1: Tria un àmbit o etiqueta",
                     uiOutput("r_select_ambit_estrat_ui")
                   ),
                   
                   accordion_panel(
                     value = "r_indicador_panel_estrat",
                     "Pas 2: Tria un indicador",
                     uiOutput("r_select_indicador_estrat_ui")
                   ),
                   
                   accordion_panel(
                     value = "r_interval_panel_estrat",
                     "Tria un interval",
                     sliderInput(
                       inputId = "r_year_range_estrat",
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
                   )#,
                   
                   #accordion_panel(
                  #   value = "r_ajuda_panel_estrat",
                  #   "Ajuda", icon = icon("info-circle"),
                  #   div(title = "Com funciona?",
                  #       actionButton("r_ajuda_estrat", label = "Com funciona?", icon = icon('question-circle'), class = "qia-down"))
                  # )
                   
                   
                 ),
                 
                 actionLink(
                   inputId = "r_change_center_button_estrat", 
                   label = HTML('<i class="fas fa-rotate-left"></i> Canvia el territori'),
                   title = "Torna a la pestanya Inici del Visor per modificar el territori seleccionat",
                   onclick = "
                       // First, click on the Visor tab
                       $('#nav li:contains(\"Visió territorial\") a').click();
                       // Use a timeout to ensure Visor loads, then click on the Inici subtab
                       setTimeout(function() {
                         $('#nav_residencia li:contains(\"Inici\") a').click();
                       }, 50);  // Adjust the delay if needed
                     "
                 ),
                 actionLink(
                   inputId = "r_ajuda_estrat", 
                   label = HTML('<i class="fas fa-info-circle"></i> Ajuda')
                 )
                 
               ),
               
               navset_card_underline(
                 height = NULL,
                 
                 nav_panel(
                   title = "Sexe",
                   
                   conditionalPanel(
                     condition = "output.r_has_sexe_stratification",
                     bslib::card(
                       full_screen = T,
                       height = "650px",
                       # In estratUI.R, modify the div containing the chart controls:
                       
                       div(style = "text-align: left;",
                           h5(uiOutput("r_titol_sexe"), style = "color: black; text-align: left"),
                           h6(textOutput("r_subtitol_sexe"), style = "color: black; text-align: left"),
                           h6(textOutput("r_subtitol_sexe2"), style = "color: black; text-align: left"),
                           
                           # Replace this div with a div using absolute positioning
                           div(
                             style = "position: absolute; top: 10px; right: 10px; z-index: 100;",
                             bslib::popover(
                               id = "r_chart_controls_popover_sexe",
                               title = "Configuració del gràfic",
                               icon("gear"),  # Gear icon for settings
                               # y-axis toggle
                               awesomeCheckbox(
                                 inputId = "r_toggle_y_axis_zero_sexe",
                                 label = "Inicia l'eix de les Y a zero",
                                 value = TRUE
                               ),
                               # Future settings can be added here
                               placement = "left",  # Position the popover to the left
                               container = "body"   # Attach to body for better positioning
                             )
                           )
                       ),
                       withSpinner(
                         plotlyOutput("r_sexe_plot"), # Adjust graph height
                         color = "#FC8D59"
                       ),
                       bslib::card_footer(
                         downloadLink(
                           outputId = "r_descarrega_dades_sexe", 
                           label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                           title = "Descarrega les dades en format Excel"
                         )
                       ),
                       
                     ),
                     
                     bslib::card(
                       full_screen = T,
                       height = "650px",
                       style = "position: relative;",  # Add this line
                       div(style = "text-align: left;",
                           h5(uiOutput("r_titol_sexe_taula"), style = "color: black; text-align: left"),
                           h6(textOutput("r_subtitol_sexe_taula"), style = "color: black; text-align: left"),
                           h6(textOutput("r_subtitol_sexe2_taula"), style = "color: black; text-align: left")
                       ),
                       withSpinner(
                         DT::dataTableOutput("r_sexe_table"), # Adjust graph height
                         color = "#FC8D59"
                       ),
                       bslib::card_footer(
                         downloadLink(
                           outputId = "r_descarrega_dades_sexe_taula", 
                           label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                           title = "Descarrega la taula en format Excel"
                         )
                       )
                     )
                   ), # Conditional panel
                   
                   # Show message when sexe stratification doesn't exist
                   conditionalPanel(
                     condition = "!output.r_has_sexe_stratification",
                     div(
                       style = "padding: 20px; text-align: center;",
                       h4("Contingut no disponible"),
                       p("Aquest indicador no té estratificació per sexe disponible.")
                     )
                   )
                   
                 ),
                 
                 
                 nav_panel(
                   title = "Edat",
                   
                   conditionalPanel(
                     condition = "output.r_has_edat_stratification",
                     bslib::card(
                       full_screen = T,
                       height = "700px",
                       div(style = "text-align: left;",
                           h5(uiOutput("r_titol_edat"), style = "color: black; text-align: left"),
                           h6(textOutput("r_subtitol_edat"), style = "color: black; text-align: left"),
                           h6(textOutput("r_subtitol_edat2"), style = "color: black; text-align: left"),
                           
                           # Add the gear icon with absolute positioning
                           div(
                             style = "position: absolute; top: 10px; right: 10px; z-index: 100;",
                             bslib::popover(
                               id = "r_edat_chart_controls_popover",
                               title = "Configuració del gràfic",
                               icon("gear"),  # Gear icon for settings
                               # y-axis toggle
                               awesomeCheckbox(
                                 inputId = "r_toggle_y_axis_zero_edat",
                                 label = "Inicia l'eix de les Y a zero",
                                 value = TRUE
                               ),
                               # Future settings can be added here
                               placement = "left",  # Position the popover to the left
                               container = "body"   # Attach to body for better positioning
                             )
                           )
                           
                       ),
                       withSpinner(
                         plotlyOutput("r_edat_plot"), # Adjust graph height
                         color = "#FC8D59"
                       ),
                       bslib::card_footer(
                         downloadLink(
                           outputId = "r_descarrega_dades_edat", 
                           label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                           title = "Descarrega les dades en format Excel"
                         )
                       )
                     ),
                     bslib::card(
                       full_screen = T,
                       height = "700px",
                       div(style = "text-align: left;",
                           h5(uiOutput("r_titol_edat_taula"), style = "color: black; text-align: left"),
                           h6(textOutput("r_subtitol_edat_taula"), style = "color: black; text-align: left"),
                           h6(textOutput("r_subtitol_edat2_taula"), style = "color: black; text-align: left")
                       ),
                       withSpinner(
                         DT::dataTableOutput("r_edat_table"), # Adjust graph height
                         color = "#FC8D59"
                       ),
                       bslib::card_footer(
                         downloadLink(
                           outputId = "r_descarrega_dades_edat_taula", 
                           label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                           title = "Descarrega la taula en format Excel"
                         )
                       )
                     )
                   ),
                   
                   # Show message when edat stratification doesn't exist
                   conditionalPanel(
                     condition = "!output.r_has_edat_stratification",
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
                     style = "overflow-y: auto;", # Allow vertical scrolling for tables if needed
                     withSpinner(htmlOutput("r_def_text_estrat"), color = "#FC8D59")
                   )
                 )
                 
               )
        )

  )
)
