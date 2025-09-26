mapaTab <- nav_panel(
  title = "Mapa",  
  value = "mapa",
  #icon = icon("map"),
  icon = HTML('<svg width="20" height="20"><use href="#catalonia" /></svg>'),
  
  div(
    style = "display: flex; flex-direction: column; height: calc(100vh - 100px); margin: 0; padding: 0;", # Remove outer margins
    
    layout_sidebar(
      fillable = TRUE,
      height = "100%",
      padding = 0, # Remove padding from layout_sidebar
      gap = 0, # Remove gap between sidebar and content
      sidebar = sidebar(
        width = '30%',
        
        uiOutput("map_titol"),
        
        accordion(
          multiple = TRUE,
          open = c("ambit_panel_mapa", "indicador_panel_mapa", "any_panel_mapa", "opcions_panel_mapa"),
          
          accordion_panel(
            value = "ambit_panel_mapa",
            "Pas 1: tria un àmbit o etiqueta",
            uiOutput("map_ambit")
          ),
          
          accordion_panel(
            value = "indicador_panel_mapa",
            "Pas 2: tria un indicador",
            uiOutput("map_indicador")
          ),
          
          accordion_panel(
            value = "any_panel_mapa",
            "Pas 3: tria un any",
            uiOutput("map_any")
          ),
          
          accordion_panel(
            value = "opcions_panel_mapa",
            "Opcions",
            uiOutput("result_switch_mapa_ui"),
            uiOutput("selector_sexe_mapa_ui"),
            uiOutput("selector_edat_mapa_ui")
          )
        ),
        
        actionLink(
          inputId = "change_center_button_mapa", 
          label = HTML('<i class="fas fa-rotate-left"></i> Canvia el centre'),
          title = "Torna a la pestanya Inici per modificar el centre seleccionat",
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
          inputId = "ajuda_mapa", 
          label = HTML('<i class="fas fa-info-circle"></i> Ajuda')
        )
        
      ),
      
      div(
        style = "height: 100%; width: 100%; padding: 0; margin: 0;", # Remove all margins and padding
        leafletOutput("map", width = "100%", height = "100%")
      )
    )
  )
)



#mapaTab <- nav_panel(
#  title = "Mapa",  
#  value = "mapa",
#  div(style = "height: 100vh;",  # Full viewport height
#      #leafletOutput("map", width = "100%", height = "100%")
#      maplibreOutput("map", width = "100%", height = "100%")
#  )
#)
#