###############################################################################
#
# Global script ---- 
#
###############################################################################

# contains :- 

# 1. required packages
# 2. required datafiles
# 3. lists for dashboard filters
# 4. common chart themes
# 5. extra UI components that are not reactive (cookie box/guided tours/updates modal)
# 6. sourcing functions created for app (see functions folder) 


# 1. required packages ----------------------------------------------------------

library(shiny)
library(arrow) # per llegir fitxers parquet
library(dplyr)
library(plotly) # gràfics interactius
library(data.table)
library(kableExtra) # per fer les taules
library(purrr) # funcions per source files
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders) # per afegir animació durant càrrega
library(rintrojs)
library(reactable) # per la taula resum
library(htmltools) # per la taula resum
library(htmlwidgets) # per gràfics de la taula resum
#library(highcharter) # per gràfics de la taula resum
library(webshot) # to download plotly charts
library(auth0) # pel login amb contrassenya
library(readr)
library(gotop) # pel botó de tornar a dalt
library(readxl)
library(stringr) # per str_to_title
library(bslib)
library(writexl)
library(duckdb)
library(dbplyr)
library(tidyr)
library(DT)
library(rlang)
library(sf)
library(leaflet)
library(leaflet.extras) # Pel cercador del mapa
library(leaflet.extras2) # Pel botó de descàrrega del mapa
#library(leafgl)
#library(mapgl)
#library(RColorBrewer)
library(dataui) # Pels sparklines del resum de Catalunya
library(reactablefmtr) # Pels sparklines del resum de Catalunya
library(echarts4r) # Per renderitzar els gràfics d'espina
#library(rmarkdown)
#library(bsicons) # Per les icones de configuració del gràfic (millor integració amb bslib, bootstrap 5, tooltips i svg més personalitzable)


# As well as webshot, phantomjs is needed to download Plotly charts
# https://github.com/rstudio/shinyapps-package-dependencies/pull/180
if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}

options(OutDec=",") # Decimals en comes en comptes de punts

useShinyjs()

# Environment (que només exigeixi usuari i contrasenya quan l'app està deployed)
env_prod = serverInfo()$shinyServer
print(paste("Production env:", env_prod))

## 2. required datafiles ------------------------------------------------------------

## main datasets 

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Read the datasets into DuckDB tables
#dbWriteTable(con, "codebook", read_parquet("codebook5.parquet"))
#dbWriteTable(con, "catalegs", read_excel("catalegs.xlsx", sheet = "dg_extra") %>% 
#               select(dg_extra, codi_cat, cat))


# Query parquet directly
#dades_simplificades_tbl <- tbl(con, sql("SELECT * FROM 'datasets/df_dades_simplificades.parquet'"))

dades_menu_tbl <- open_dataset("datasets/dades_menu", format = "parquet")


alfred <- collect(open_dataset("datasets/alfred", format = "parquet")) %>% 
  unique() %>% 
  as.data.frame() %>% 
  tidyr::separate_rows(keywords, sep = "\\|") %>%
  tidyr::separate(keywords, into = c("key", "value"), sep = ":", fill = "right") %>%
  tidyr::pivot_wider(names_from = key, values_from = value, values_fn = list) %>%
  mutate(across(everything(), ~ map(., ~ if (is.null(.x) || all(is.na(.x))) NA_character_ else str_split(.x, ",")[[1]])))

#dbWriteTable(con, "docu", read_excel("docu_base.xlsx"))
#docu_tbl <- tbl(con, "docu")


#dbWriteTable(con, "dades_c", read_parquet("datasets/df_dades_c.parquet"))

#dades_tbl <- tbl(con, sql("SELECT * FROM 'datasets/dades/dades.parquet'")) %>% 
#  filter(visio != "P")

#dades_r_tbl <- tbl(con, sql("SELECT * FROM 'datasets/dades/dades.parquet'")) %>% 
#  filter(visio != "C")


dades_tbl <- tbl(con, sql("SELECT * FROM 'datasets/dades/dades_c.parquet'")) 

dades_r_tbl <- tbl(con, sql("SELECT * FROM 'datasets/dades/dades_r.parquet'")) 

dades_total <- tbl(con, sql("SELECT * FROM 'datasets/dades/dades.parquet'"))

#dades_tbl = dades_tbl %>% 
#  unique() #%>% 
  #mutate(mitjana = 55)

# Optimized approach - use Arrow directly
#dades_tbl <- open_dataset("datasets/dades_total", format = "parquet") %>% 
#  mutate(mitjana = 55)





#dbWriteTable(con, "dades_r", read_parquet("datasets/df_dades_r.parquet"))
#dades_r_tbl <- tbl(con, sql("SELECT * FROM 'datasets/df_dades_r.parquet'"))

#dbWriteTable(con, "dades_ambit_ind", read_parquet("datasets/dades_ambit_ind.parquet"))
#dades_ambit_ind_tbl <- tbl(con, sql("SELECT * FROM 'datasets/dades_ambit_ind.parquet'"))

# Create DuckDB tbl objects for processing
#codebook_tbl <- tbl(con, "codebook")
#catalegs_tbl <- tbl(con, "catalegs")


#dades_rank_tbl = dades_tbl %>% 
#  filter(grup_edat == "Total") %>% 
#  select(-grup_edat)

#dbWriteTable(con, "dades_c_rank", read_parquet("datasets/df_dades_c_rank.parquet"))
#dades_rank_tbl <- tbl(con, "dades_c_rank")

#dbWriteTable(con, "dades_r_rank", read_parquet("datasets/df_dades_r_rank.parquet"))
#dades_r_rank_tbl <- tbl(con, "dades_r_rank")


#dades_evolutiu_tbl <- tbl(con, "dades_c_rank")
#dades_r_evolutiu_tbl <- tbl(con, "dades_r_rank")

#dades_evolutiu_tbl = dades_tbl %>% 
#  filter(grup_edat == "Total", sexe == "Total") %>% 
#  select(-grup_edat)

# Filter data directly in DuckDB using dplyr
#dades_resum_tbl <- dades_tbl %>%
#  filter(sexe == "Total", grup_edat == "Total") %>%
#  mutate(extra_info = case_when(
#    !is.na(dg_extra) & !is.na(subtipologia) ~ paste(dg_extra, ":", subtipologia),
#    TRUE ~ NA_character_
#  ))

#dades_resum = read_parquet("datasets/df_dades_c_resum.parquet") #%>% 
#  mutate(ambit_curt = case_when(
#    ambit == "Atenció Primària" ~ "APiC",
#    ambit == "Atenció Hospitalària" ~ "AH",
#    ambit == "Atenció Intermèdia" ~ "AI",
#    ambit == "Salut Mental i Addiccions" ~ "SMiA",
#    ambit == "Emergències i Urgències" ~ "EU",
#    ambit == "Salut Pública" ~ "SP",
#    TRUE ~ NA
#  ))

#dades_r_resum = read_parquet("datasets/df_dades_r_resum.parquet") #%>% 
#  mutate(ambit_curt = case_when(
#    ambit == "Atenció Primària" ~ "APiC",
#    ambit == "Atenció Hospitalària" ~ "AH",
#    ambit == "Atenció Intermèdia" ~ "AI",
#    ambit == "Salut Mental i Addiccions" ~ "SMiA",
#    ambit == "Emergències i Urgències" ~ "EU",
#    ambit == "Salut Pública" ~ "SP",
#    TRUE ~ NA
#  ))

#dbWriteTable(con, "dades_r_resum", read_parquet("datasets/df_dades_r_resum.parquet"))

#dades_resum_tbl <- tbl(con, sql("SELECT * FROM 'datasets/df_dades_c_resum.parquet'"))

#dades_r_resum_tbl <- tbl(con, sql("SELECT * FROM 'datasets/df_dades_r_resum.parquet'"))


#docu = read_excel("docu_base.xlsx")

#dades_rank = read_parquet("datasets/df_dades_c_rank.parquet")

#
## Retrieve unique values for UI selectors from DuckDB
#nivell_geo_nocat <- dades_resum %>%
#  filter(Granularitat != "Catalunya") %>%
#  select(Granularitat) %>%
#  distinct() %>%
#  pull(Granularitat)

#nivell_geo_nocat = unique(dades_resum$Granularitat[dades_resum$Granularitat != "Catalunya"]) # Primer selector de la pestanya Resum


# Calculate the min and max year
#min_year <- dades_tbl %>%
#  summarise(min_year = min(any)) %>%
#  pull(min_year)

#max_year <- dades_tbl %>%
#  summarise(max_year = max(any)) %>%
#  pull(max_year)



colpal <- topo.colors(5)


map_abs_layer <- sf::st_read("maps/abs_simplificat.geojson") 


map_aga_layer <- sf::st_read("maps/aga.geojson") 


map_rs_layer <- sf::st_read("maps/rs.geojson")


map_rs2_layer <- sf::st_read("maps/rs2.geojson") 



ambits = dades_total %>% pull(ambit) %>%  unique()

dimensions = dades_total %>% pull(dimensio) %>%  unique()

indicadors = dades_total %>% pull(nom_indicador) %>%  unique() %>% sort()

anys = dades_total %>% pull(any) %>% unique()

nivell_geo = dades_total %>% pull(Granularitat) %>% unique()

#centre_territori = unique(dades$`Centre/Territori`)

rs = dades_total %>% filter(Granularitat == "Regió Sanitària") %>% pull(`Centre/Territori`) %>% unique() 
aga = dades_total %>% filter(Granularitat == "Àrea de Gestió Assistencial") %>% pull(`Centre/Territori`) %>% unique()
abs = dades_total %>% filter(Granularitat == "Àrea Bàsica de Salut") %>% pull(`Centre/Territori`) %>% unique()
centre = dades_total %>% filter(Granularitat == "Centre (Unitat proveïdora)") %>% pull(`Centre/Territori`) %>% unique()


min_year <- dades_total %>% pull(any) %>% min()
max_year <- dades_total %>% pull(any) %>% max()



# Load and repair ABS layer
#map_abs_layer <- sf::st_read("maps/abs.geojson") %>%
#  sf::st_make_valid() %>%  # First make geometries valid
#  sf::st_buffer(0) %>%     # Fix self-intersections
#  sf::st_simplify(dTolerance = 20, preserveTopology = TRUE) # Simplify while preserving topology
#
## Load and repair AGA layer
#map_aga_layer <- sf::st_read("maps/aga.geojson") %>%
#  sf::st_make_valid() %>%
#  sf::st_buffer(0) %>%
#  sf::st_simplify(dTolerance = 20, preserveTopology = TRUE)
#
## Load and repair RS layer
#map_rs_layer <- sf::st_read("maps/rs.geojson") %>%
#  sf::st_make_valid() %>%
#  sf::st_buffer(0) %>%
#  sf::st_simplify(dTolerance = 20, preserveTopology = TRUE)
#
## Load and repair RS2 layer
#map_rs2_layer <- sf::st_read("maps/rs2.geojson") %>%
#  sf::st_make_valid() %>%
#  sf::st_buffer(0) %>%
#  sf::st_simplify(dTolerance = 20, preserveTopology = TRUE)


#abs_data = read_parquet("datasets/map_abs_df_2025-01-28.parquet")


# 4. temes i estètica  -------------------------------------------------------------

aquas_theme <- bs_theme(
  # high level theming
  version = 5, # bootstrap v5 required to use bslib components (like cards etc.)
  bg = "white", # make background white
  fg = "#222", # make foreground darkgrey/black
  #bootswatch = "shiny", # use default shiny theme
  #primary = "#0078D4", # make primary colour blue - this will change i.e. active pill colour
  #"form-label-font-weight" = "550"#, # font-weight for filter labels
  #"nav-tabs-link-active-bg" = phs_colours(colourname = "phs-magenta-10"), # multi-tab cards colour when selected
  #"nav-tabs-link-active-color" = "black" # multi-tab cards font colour when selected
) |>
  # create colour variables to use below
  bs_add_variables(
    "salut-blue" = "#8FC2F5",
    "phs-blue" = "#0078D4"
    
  ) 


# common parameters for plots
xaxis_plots <- list(title = FALSE, 
                    tickfont = list(size=14), 
                    titlefont = list(size=14), 
                    showline = TRUE, 
                    tickangle = 270, 
                    fixedrange=TRUE)


yaxis_plots <- list(title = FALSE, 
                    rangemode="tozero", 
                    fixedrange=TRUE, 
                    size = 4, 
                    tickfont = list(size=14), 
                    titlefont = list(size=14))

font_plots <- list(family = '"Helvetica Neue", 
                              Helvetica, 
                              Arial, 
                              sans-serif')


# common parameters for plots

yaxis_plots <- list(title = FALSE, 
                    rangemode="tozero", 
                    fixedrange=TRUE, 
                    size = 4, 
                    tickfont = list(size=14), 
                    titlefont = list(size=14)) 



# 6. sourcing functions created for app (see functions folder) -------------------------------
list.files("functions") %>% 
  map(~ source(paste0("functions/", .)))
