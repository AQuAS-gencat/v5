################################################################################
#
## Funcions per la descàrrega de les dades en csv
#
################################################################################


# 1. formatting data downloads
# select and rename columns to ensure downloaded data is userfriendly
format_csv <- function(reactive_dataset, extra_vars = NULL) {
  
  reactive_dataset <- reactive_dataset %>%
    select(c("ambit", 
             "dimensio",
             "centre_territori" = "Centre/Territori", 
             "granularitat" = "Granularitat",
             "indicador",
             "subcategoria" = "subtipologia",
             "sexe",
             "any",
             #"numerador" = "n", 
             #"denominador" = "d", 
             "resultat", 
             "catalunya" = "mitjana", 
             "unitat" = "mesura", 
             #"invers",
             extra_vars
))
  
}



