options(encoding = 'UTF-8')
options(shiny.port = 8080)

source("ui scripts/iniciUI.R", local = TRUE)
source("ui scripts/selectorUI.R", local = TRUE)
source("ui scripts/r_selectorUI.R", local = TRUE)
source("ui scripts/resumUI.R", local = TRUE)
source("ui scripts/r_resumUI.R", local = TRUE)
source("ui scripts/comparadorUI.R", local = TRUE)
source("ui scripts/r_comparadorUI.R", local = TRUE)
source("ui scripts/evolutiuUI.R", local = TRUE)
source("ui scripts/r_evolutiuUI.R", local = TRUE)
source("ui scripts/estratUI.R", local = TRUE)
source("ui scripts/r_estratUI.R", local = TRUE)
#source("ui scripts/procUI.R", local = TRUE)
source("ui scripts/mapaUI.R", local = TRUE)
source("ui scripts/analisiUI.R", local = TRUE)
source("ui scripts/dadesUI.R", local = TRUE)
#source("ui scripts/defUI.R", local = TRUE)
source("ui scripts/aboutUI.R", local = TRUE)


# UI definition
ui <- tagList(
  
  useShinyjs(),
  tags$head(
    tags$title("Central de Resultats - AQuAS"), 
    tags$meta(charset = 'UTF-8'),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Open+Sans"),
    tags$link(rel = "icon", type = "image/png", href = "favicon_logo_salut.png"),
    tags$script(HTML("
  Shiny.addCustomMessageHandler('adjustRowHeight', function(message) {
    var rows = document.querySelectorAll('.reactable-row'); // Select all rows
    rows.forEach(function(row) {
      var text = row.querySelector('.reactable-cell[data-column=\"nom_indicador\"]')?.innerText || '';
      var textLength = text.length;
      if (textLength > 100) {
        row.style.height = '75px'; // Reduced from 100px
      } else if (textLength > 50) {
        row.style.height = '45px'; // Reduced from 50px
      } else {
        row.style.height = '25px'; // Reduced from 30px
      }
    });
  });
")),


tags$script(HTML("$(document).on('click', '.expand-row, .cat-expand-row', function(e) {
                 e.stopPropagation(); // Prevent row click event from firing\
                 
                 // Get the row index
                 const rowIndex = $(this).data('row-index');
                 
                 // More robust detection of which tab we're in
                 const url = window.location.hash;
                 let modalToUse = 'show_indicator_modal'; // Default modal
                 
                 // Check for 'cat' tab
                 if (url.includes('cat') || 
                 $(this).closest('[id*=\"cat\"]').length > 0 || 
                 $(this).hasClass('cat-expand-row')) {
                 modalToUse = 'cat_show_indicator_modal';
                 }
                 // Check for 'residencia' tab
                 else if (url.includes('residencia') || 
                 $(this).closest('[id^=\"r_\"]').length > 0 ||
                 $(this).closest('tr').find('[id^=\"r-spine-chart\"]').length > 0) {
                 modalToUse = 'r_show_indicator_modal';
                 }
                 
                 // Set the appropriate input value
                 Shiny.setInputValue(modalToUse, { 
                 index: rowIndex, 
                 timestamp: new Date().getTime() 
                 });
                 });
                 ")),


  ),


# Add this to your UI file, for example in a tags$script() element
#tags$head(
#  tags$script(HTML("
#    Shiny.addCustomMessageHandler('clickPlotlyDownload', function(message) {
#      // Get the plotly graph element
#      var gd = document.querySelector('.js-plotly-plot');
#      
#      if (gd) {
#        // Use Plotly's downloadImage function directly
#        Plotly.downloadImage(gd, {
#          format: 'png',
#          filename: 'grafic_variabilitat',
#          scale: 3
#        });
#      } else {
#        console.error('Gràfic no trobat');
#      }
#    });
#  "))
#),
#
## Add this to your UI file, for example in a tags$script() element
#tags$head(
#  tags$script(HTML("
#Shiny.addCustomMessageHandler('clickPlotlyDownload', function(message) {
#  // Get the active tab's plotly chart by finding the visible one
#  var visiblePanel = document.querySelector('.tab-pane.active');
#  var selector = message.selector || '.js-plotly-plot';
#  var gd;
#  
#  if (visiblePanel) {
#    // First look for a plotly chart within the active tab
#    gd = visiblePanel.querySelector(selector);
#  } 
#  
#  // If not found in the active tab, fall back to the specified selector or first one
#  if (!gd) {
#    gd = document.querySelector(selector);
#  }
#  
#  if (gd) {
#    // Use Plotly's downloadImage function directly with custom options
#    Plotly.downloadImage(gd, {
#      format: message.format || 'png',
#      filename: message.filename || 'grafic',
#      scale: message.scale || 3
#    });
#  } else {
#    console.error('Gràfic no trobat: ' + selector);
#  }
#});
#  "))
#),


  includeCSS("css/styles.css"),
  includeScript("www/js/piwik.js"),
  use_gotop(),
  
  navbarPage(
    
    id = "nav",
    theme = aquas_theme,
    title = div(
      tags$a(
        img(src = "aquas.jpg", width = 300, alt = "enllaç a la web de l'AQuAS"),
        #img(src = "logo-aquas-2h.jpg", width = 300, alt = "enllaç a la web de l'AQuAS"),
        href = "https://aquas.gencat.cat/ca/inici/",
        target = "_blank"
      )
    ),
    
    collapsible = TRUE,
    
    #input_dark_mode(id = "mode"), PENDENT ADAPTAR SELECTORS, BOTONS I GRÀFICS AL MODE FOSC
    
    
    
    # Define the top-level tabs
    iniciTab,
    
    # Visor tab with its subtabs
    nav_panel(
      title = "Visor de centres",
      #icon = icon("laptop-medical"),
      
      navset_tab(
        id = "nav_visor",
        # Modificacions icones custom
        selectorTab,
        resumTab,
        comparadorTab,
        estratTab,
        evolutiuTab
      )
      
    ),
    
    # Visor tab with its subtabs
    nav_panel(
      title = "Visió territorial",
      #icon = icon("laptop-medical"),
      #icon = HTML('<svg width="1em" height="1em" viewBox="0 0 260 260"><use xlink:href="#catalonia-small"></use></svg>'),
      
      navset_tab(
        id = "nav_residencia",

        rselectorTab,
        rresumTab,
        rcomparadorTab,
        restratTab,
        revolutiuTab,
        mapaTab,
        analisiTab#,
        #catTab
      )
      
    ),
    
    
    # Independent tabs
    #procTab,
    descarreguesTab,
    #defTab#,
    aboutTab
    

  ) # navbarPage
) # tagList



if (env_prod) {
  auth0_ui(ui)
} else {
  ui
}
