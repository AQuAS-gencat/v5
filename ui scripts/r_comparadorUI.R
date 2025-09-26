rcomparadorTab <- nav_panel(
  title = "Variabilitat",
  value = "variabilitat-residencia",
  icon = img(src="icon_variabilitat_cr.svg", role="presentation", width="20", height="20"),
  
  div(style = "margin:10px;",
      layout_sidebar(
        fillable = F,
        sidebar = sidebar(
          width = '30%',
          
          accordion(
            multiple = TRUE,
            open = c("r_ambit_panel_rank", "r_indicador_panel_rank", "r_any_panel_rank", "r_opcions_panel_rank"),
            
            accordion_panel(
              value = "r_ambit_panel_rank",
              "Pas 1: Tria un àmbit o etiqueta",
              uiOutput("r_select_ambit_ranquing_ui")
            ),
            
            accordion_panel(
              value = "r_indicador_panel_rank",
              "Pas 2: Tria un indicador",
              uiOutput("r_select_indicador_ranquing_ui")
            ),
            
            accordion_panel(
              value = "r_any_panel_rank",
              "Pas 3: Tria un any",
              uiOutput("r_select_any_ranquing_ui")
            ),
            
            accordion_panel(
              value = "r_opcions_panel_rank",
              "Opcions",
              uiOutput("r_result_toggle_ui"),
              #br(),
              uiOutput("r_selector_sexe_rank_ui"),
              #br(),
              uiOutput("r_selector_edat_rank_ui"),
              #br(),
              uiOutput("r_cat_checkbox_ui")
            )
          ),
          
          actionLink(
            inputId = "r_change_center_button_comparador", 
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
            inputId = "r_ajuda_comparador", 
            label = HTML('<i class="fas fa-info-circle"></i> Ajuda')
          )
        ),
        
        navset_card_underline(
          full_screen = TRUE,
          height = NULL,
          id = "card-r",
          
          nav_panel(
            title = "Gràfic",
            bslib::card_body(
              style = "overflow-y: hidden;",
              div(style = "text-align: left;",
                  h5(uiOutput("r_titol_comparador"), style = "color: black; text-align: left"),
                  conditionalPanel(
                    condition = "output.r_sexe_rank_exists | output.r_edat_rank_exists",
                    h6(htmlOutput("r_subtitol_comparador2"), style = "color: black; text-align: left")
                  ),
                  conditionalPanel(
                    condition = "output.r_nivell_rank_exists",
                    h6(htmlOutput("r_nivell_comparador"), style = "color: black; text-align: left")
                  )
              ),
              withSpinner(
                uiOutput("r_ranquing_conditional"), # Use conditional UI here
                color = "#FC8D59"
              ),
              bslib::card_footer(
                downloadLink(
                  outputId = "r_descarrega_dades_comparador", 
                  label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                  title = "Descarrega la taula en format Excel"
                )
              )
            )
          ),
          
          nav_panel(
            title = "Taula",
            bslib::card_body(
              style = "overflow-y: auto; text-align: left;",
              div(style = "text-align: left;",
                  h5(uiOutput("r_titol_comparador_taula"), style = "color: black; text-align: left"),
                  conditionalPanel(
                    condition = "output.r_sexe_rank_exists",
                    h6(htmlOutput("r_subtitol_comparador2_taula"), style = "color: black; text-align: left")
                  ),
                  conditionalPanel(
                    condition = "output.nivell_rank_exists",
                    h6(htmlOutput("r_nivell_comparador_taula"))
                  )
              ),
              withSpinner(
                uiOutput("r_rank_table_conditional"), # Use conditional UI here
                color = "#FC8D59"
              )
            ),
            bslib::card_footer(
              downloadLink(
                outputId = "r_descarrega_dades_comparador", 
                label = HTML('<i class="fas fa-download"></i> Descarrega les dades'),
                title = "Descarrega la taula en format Excel"
              )
            )
          ),
          
          nav_panel(
            title = "Fitxa",
            bslib::card_body(
              style = "overflow-y: auto;",
              withSpinner(
                uiOutput("r_def_text_ranquing_conditional"), # Use conditional UI here
                color = "#FC8D59"
              )
            )
          )
        )
      )
  )
)