revolutiuTab <- nav_panel(
  title = "Evolució comparada",
  value = "evolutiu-residencia",
  icon = img(src="icon_evolucio_cr.svg", role="presentation", width="20", height="20"),
  
  div(style = "margin:10px;",
      layout_sidebar(
        fillable = F,
        sidebar = sidebar(
          width = '30%',
          
          accordion(
            multiple = TRUE,
            open = c("r_ambit_panel_evolutiu", "r_indicador_panel_evolutiu", "r_interval_panel_evolutiu", "r_additional_centers_panel", "r_opcions_panel_evolutiu"),
            
            # Your existing accordion panels...
            accordion_panel(
              value = "r_ambit_panel_evolutiu",
              "Pas 1: Tria un àmbit o etiqueta",
              uiOutput("r_select_ambit_evolutiu_ui")
            ),
            
            accordion_panel(
              value = "r_indicador_panel_evolutiu",
              "Pas 2: Tria un indicador",
              selectInput("r_select_indicador_evolutiu",
                          label = NULL,
                          choices = NULL)
            ),
            
            accordion_panel(
              value = "r_interval_panel_evolutiu",
              "Tria un interval",
              sliderInput(
                inputId = "r_year_range_evolutiu",
                label = NULL,
                min = 2016,
                max = 2024,
                value = c(2016, 2024),
                step = 1,
                ticks = T,
                sep = ""
              ),
            ),
            
            accordion_panel(
              value = "r_additional_centers_panel",
              "Afegeix altres territoris",
              uiOutput("r_additional_centers_ui")
            ),
            
            accordion_panel(
              value = "r_opcions_panel_evolutiu",
              "Opcions",
              div(title = "Mostra els resultats crus o estandarditzats",
                  uiOutput("r_result_switch_ui")),
              uiOutput("r_selector_sexe_evolutiu_ui"),
              uiOutput("r_selector_edat_evolutiu_ui"),
              uiOutput("r_selector_cat_evolutiu_ui")
            )
          ),
          
          actionLink(
            inputId = "r_change_center_button_evolutiu", 
            label = HTML('<i class="fas fa-rotate-left"></i> Canvia el territori'),
            title = "Torna a la pestanya Inici per modificar el territori seleccionat",
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
            inputId = "r_ajuda_evolutiu", 
            label = HTML('<i class="fas fa-info-circle"></i> Ajuda')
          )
        ),
        
        navset_card_underline(
          full_screen = TRUE,
          height = NULL,
          
          # First Tab: Gràfic - now with conditional content
          nav_panel(
            title = "Gràfic",
            bslib::card_body(
              style = "overflow-y: hidden;",
              div(style = "text-align: left;",
                  h5(uiOutput("r_titol_evolutiu"), style = "color: black; text-align: left"),
                  h6(textOutput("r_subtitol_evolutiu2"), style = "color: black; text-align: left")
              ),
              
              div(
                style = "position: absolute; top: 10px; right: 10px; z-index: 100;",
                bslib::popover(
                  id = "r_evolutiu_chart_controls_popover",
                  title = "Configuració del gràfic",
                  icon("gear"),
                  awesomeCheckbox(
                    inputId = "r_toggle_y_axis_zero_evolutiu",
                    label = "Inicia l'eix de les Y a zero",
                    value = TRUE
                  ),
                  placement = "left",
                  container = "body"
                )
              ),
              
              withSpinner(
                uiOutput("r_evolution_plot_conditional"), # Use conditional UI
                color = "#FC8D59"
              ),
              bslib::card_footer(
                downloadLink(
                  outputId = "r_descarrega_dades_evolutiu", 
                  label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                  title = "Descarrega les dades en format Excel"
                )
              )
            )
          ),
          
          # Second Tab: Taula - now with conditional content
          nav_panel(
            title = "Taula",
            bslib::card_body(
              style = "overflow-y: auto; text-align: left;",
              div(style = "text-align: left;",
                  h5(uiOutput("r_titol_evolutiu_table"), style = "color: black; text-align: left"),
                  h6(textOutput("r_subtitol_evolutiu2_table"), style = "color: black; text-align: left")
              ),
              withSpinner(
                uiOutput("r_evolution_table_conditional"), # Use conditional UI
                color = "#FC8D59"
              ),
              bslib::card_footer(
                downloadLink(
                  outputId = "r_descarrega_dades_evolutiu_taula", 
                  label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                  title = "Descarrega la taula en format Excel"
                )
              )
            )
          ),
          
          # Third Tab: Fitxa - now with conditional content
          nav_panel(
            title = "Fitxa",
            bslib::card_body(
              style = "overflow-y: auto;",
              withSpinner(
                uiOutput("r_def_text_evolutiu_conditional"), # Use conditional UI
                color = "#FC8D59"
              )
            )
          )
        )
      )
  )
)