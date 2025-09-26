################################################################################
#
## Funcions per la creació de la taula resum
#
################################################################################


# 1. reactable theme 
# purpose: style to use for tables built using the reactable package
table_theme <- function() {
  search_icon <- function(fill = "none") {
    # Icon from https://boxicons.com
    svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', fill)
    sprintf("url('data:image/svg+xml;charset=utf-8,%s')", URLencode(svg))
  }
  reactableTheme(
    backgroundColor = 'white',
    borderWidth = '1px',
    borderColor = 'lightgrey',
    headerStyle = list(backgroundColor = "#00356A",
                       color = "white"),
    
    searchInputStyle = list(
      borderColor = '#cccccc',
      paddingLeft = "3.5rem",
      width = "100%",
      backgroundSize = "2rem" #,
      #backgroundPosition = "left 1rem center",
      #backgroundRepeat = "no-repeat",
      #backgroundImage = search_icon("black")
      ),
    
    tableBodyStyle = list(flex = "auto"),
    rowStyle = list(minHeight = "50px") # Base minimum height
    #rowStyle=list(height="100px")
    
    
  )
  
}


# 3. reactable theme 
# purpose: style to use for tables built using the reactable package
#table_theme_defs <- function() {
#  search_icon <- function(fill = "none") {
#    # Icon from https://boxicons.com
#    svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', fill)
#    sprintf("url('data:image/svg+xml;charset=utf-8,%s')", URLencode(svg))
#  }
#  reactableTheme(
#    backgroundColor = 'white',
#    borderWidth = '1px',
#    borderColor = 'lightgrey',
#    headerStyle = list(backgroundColor = "#00356A",
#                       color = "white"),
#    
#    searchInputStyle = list(
#      borderColor = '#cccccc',
#      paddingLeft = "3.5rem",
#      width = "70%",
#      backgroundSize = "2rem" #,
#      #backgroundPosition = "left 1rem center",
#      #backgroundRepeat = "no-repeat",
#      #backgroundImage = search_icon("black")
#    ),
#    
#    tableBodyStyle = list(flex = "auto"),
#    rowStyle=list(height="60px")
#    
#    
#  )
#  
#}

# Add this function to funcions_taula.R
ambit_icon <- function(value) {
  # Map of full names for tooltips
  ambit_full_names <- list(
    "APiC" = "Atenció Primària i Comunitària", 
    "AH" = "Atenció Hospitalària", 
    "AI" = "Atenció Intermèdia", 
    "SMiA" = "Salut Mental i Addiccions", 
    "EU" = "Emergències i Urgències", 
    "SP" = "Salut Pública"
  )
  
  # Get the full name for the tooltip
  full_name <- ambit_full_names[[value]]
  
  # Define colors
  colors <- list(
    "APiC" = "#3CA2E6", 
    "AH" = "#E83F69", 
    "AI" = "#74CF51", 
    "SMiA" = "#00A3A3", 
    "EU" = "#F0AB84", 
    "SP" = "#de78d5"
  )
  color <- colors[[value]]
  
  # Return the span with tooltip
  span(
    title = full_name,  # This creates the tooltip
    style = paste0(
      "background-color:", color, "; ",
      "color: white; ",
      "padding: 4px 8px; ",
      "border-radius: 6px; ",
      "display: inline-block; "
    ),
    value
  )
}

#3. Funció que crea les icones de tendència
# Propòsit: triar el tipus d'icona, el color i l'etiqueta a mostrar segons la magnitud del canvi 
# en el resultat de l'indicador respecte el darrer any disponible i la interpretació de l'indicador

trend_indicator <- function(value = c("unchanged", "up", "down", "betterup", "betterdown", "worseup", "worsedown", "new")) {
  value <- match.arg(value)
  label <- switch(value,
                  unchanged = "Es manté igual que el darrer any o amb un canvi inferior a l'1%", up = "Augmenta respecte el darrer any",
                  down = "Disminueix respecte el darrer any", betterup = "Millora respecte el darrer any", betterdown = "Millora respecte el darrer any", 
                  worseup = "Empitjora respecte el darrer any", worsedown = "Empitjora respecte al darrer any", new = "Indicador sense dades prèvies")
  
  # Add img role and tooltip/label for accessibility
  args <- list(role = "img", title = label)
  
  if (value == "unchanged") {
    args <- c(args, list("–", style = "color: #666; font-size: 2rem; font-weight: 700; vertical-align: middle;"))
  } else if (value == "up") {
    args <- c(args, list(shiny::icon("caret-up"), style = "color: #000000; font-size: 2rem;vertical-align: middle;"))
  } else if (value == "down") {
    args <- c(args, list(shiny::icon("caret-down"), style = "color: #000000; font-size: 2rem;vertical-align: middle;"))
  } else if (value == "betterup") {
    args <- c(args, list(shiny::icon("caret-up"), style = "color: #91BFDB; font-size: 2rem;vertical-align: middle;"))
  } else if (value == "betterdown") {
    args <- c(args, list(shiny::icon("caret-down"), style = "color: #91BFDB; font-size: 2rem;vertical-align: middle;"))
  } else if (value == "worseup") {
    args <- c(args, list(shiny::icon("caret-up"), style = "color: #FC8D59; font-size: 2rem;vertical-align: middle;"))
  } else if (value == "worsedown") {
    args <- c(args, list(shiny::icon("caret-down"), style = "color: #FC8D59; font-size: 2rem;vertical-align: middle;"))
  } else {
    args <- c(args, list(shiny::icon("circle"), style = "color: #000000; font-size: 1rem; line-height: 3rem; vertical-align: middle;"))
  }
  do.call(span, args)
}

# Add this function to funcions_taula.R
info_icon <- function() {
  span(
    icon("info-circle"), 
    class = "info-icon",
    style = "color: #0078D4; margin-right: 8px; cursor: pointer;"
  )
}

#3. reactable widget
# purpose: builds a table using reactable. Use for quicker rendering of inline charts 
widgetTable <- function(data, options = list(), columns = list(), deps = list(), ...){
  
  dt <- reactable(data, 
                  columns = columns,
                  sortable = TRUE, 
                  filterable = FALSE,
                  defaultPageSize = nrow(data),
                  theme = table_theme(), # see function above
                  highlight = TRUE,
                  language = reactableLang(
                    filterPlaceholder = "Filtra",
                    filterLabel = "Filtra {name}",
                    noData = "No s'han trobat files"
                  )
                  # Removed the details parameter
  )
  
  dt %>% tagList(deps) %>% browsable
}

widgetTableCat <- function(data, options = list(), columns = list(), ...){
  
  dt <- reactable(data, 
                  #                  groupBy = c("ambit", "dimensio"),
                  columns,
                  compact = TRUE,
                  sortable = T, 
                  filterable = F,
                  #searchable = T,
                  defaultPageSize = nrow(data),
                  theme = table_theme(), # see function above
                  highlight = TRUE,
                  language = reactableLang(
                    filterPlaceholder = "Filtra",
                    filterLabel = "Filtra {name}",
                    noData = "No s'han trobat files"
                  ))
  
  
  dt 
  
  
}