# Preparació dels datasets que alimenten l'aplicació


library(arrow)
library(dplyr)
library(purrr)
library(stringr)

# Ruta a la taula dins OneLake Desktop


# Ruta de la taula original
taula_path <- "C:/Users/47854262w/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY"

# Llegeix el dataset amb Arrow
dataset <- open_dataset(taula_path, format = "parquet")

# Transformacions bàsiques (encara sense filtrar per any)
df_query <- dataset %>%
  rename(
    ambit = `Àmbit`,
    dimensio = `Dimensió`,
    etiqueta = `Etiqueta`,
    grup_edat = Grup_edat
  ) %>%
  mutate(
    `any` = str_trim(`any`, side = "both"),
    ambit = if_else(ambit == "", "Salut Pública", ambit),
    ambit_curt = case_when(
      ambit == "Atenció Primària i Comunitària" ~ "APiC",
      ambit == "Atenció Hospitalària" ~ "AH",
      ambit == "Atenció Intermèdia" ~ "AI",
      ambit == "Salut Mental i Addiccions" ~ "SMiA",
      ambit == "Emergències i Urgències" ~ "EU",
      ambit == "Salut Pública" ~ "SP",
      TRUE ~ NA_character_
    ),
    `Centre/Territori` = case_when(
      `Centre/Territori` == "ALT PIRINEU I ARAN" & Granularitat == "Regió Sanitària" ~ "Alt Pirineu i Aran",
      `Centre/Territori` == "BARCELONA - METROPOLITÀ NORD" & Granularitat == "Regió Sanitària" ~ "Barcelona Metropolitana Nord",
      `Centre/Territori` == "BARCELONA - METROPOLITÀ SUD" & Granularitat == "Regió Sanitària" ~ "Barcelona Metropolitana Sud",
      `Centre/Territori` == "BARCELONA - BARCELONA CIUTAT" & Granularitat == "Regió Sanitària" ~ "Barcelona Ciutat",
      `Centre/Territori` == "CAMP DE TARRAGONA" & Granularitat == "Regió Sanitària" ~ "Camp de Tarragona",
      `Centre/Territori` == "CATALUNYA CENTRAL" & Granularitat == "Regió Sanitària" ~ "Catalunya Central",
      `Centre/Territori` == "LLEIDA" & Granularitat == "Regió Sanitària" ~ "Lleida",
      `Centre/Territori` == "GIRONA" & Granularitat == "Regió Sanitària" ~ "Girona",
      `Centre/Territori` == "TERRES DE L'EBRE" & Granularitat == "Regió Sanitària" ~ "Terres de l'Ebre",
      `Centre/Territori` == "PENEDÈS" & Granularitat == "Regió Sanitària" ~ "Penedès",
      TRUE ~ `Centre/Territori`
    )
  ) %>%
  filter(!(any %in% c("2014", "2015", "2016", "2025")),
         !is.na(any))

# Carreguem dimup
dimup_path <- "C:/Users/47854262w/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Tables/catalegs/dim_up_abs_context"
dimup_dataset <- open_dataset(dimup_path, format = "parquet")

dimup <- dimup_dataset %>% 
  select(id_up, desc_nivell_curta) %>% 
  mutate(id_up = as.character(id_up)) %>% 
  distinct() %>% 
  collect()

# --- PROCESS PER ANY ---
anys <- df_query %>% distinct(any) %>% collect() %>% pull(any)

for (a in anys) {
  message("Processing year: ", a)
  
  df_year <- df_query %>% filter(any == a) %>% collect()
  
  df_year <- df_year %>%
    filter(!is.na(sexe), !is.na(r), !is.na(`Centre/Territori`)) %>%
    mutate(`Centre/Territori` = ifelse(`Centre/Territori` == "CAT", "Catalunya", 
                                       ifelse(`Centre/Territori` == "CSM  Gavà", "CSM Gavà", `Centre/Territori`))) %>%
    arrange(codi_indicador, Granularitat)
  
  # --- Visió C ---
  c <- df_year %>% filter(visio != "P")
  cat_values <- c %>% filter(Granularitat == "Catalunya") %>%
    select(codi_indicador, any, grup_edat, sexe, r) %>%
    distinct(codi_indicador, any, grup_edat, sexe, .keep_all = TRUE)
  
  dades_c <- c %>%
    left_join(cat_values, by = c("codi_indicador","any","grup_edat","sexe")) %>%
    rename(r = r.x, mitjana = r.y) %>%
    mutate(any = as.numeric(any)) %>% 
    unique()
  
  # Join amb dimup només per Granularitat == Centre (Unitat proveïdora)
  dades_c <- dades_c %>%
    mutate(tmp_id = ifelse(Granularitat == "Centre (Unitat proveïdora)", Codi, NA)) %>%
    left_join(dimup, by = c("tmp_id" = "id_up")) %>%
    select(-tmp_id)
  
  write_parquet(dades_c, paste0("datasets/dades_c/dades_c_", a, ".parquet"))
  
  # --- Visió R ---
  p <- df_year %>% filter(visio != "C")
  cat_values <- p %>% filter(Granularitat == "Catalunya") %>%
    select(codi_indicador, any, grup_edat, sexe, r) %>%
    distinct(codi_indicador, any, grup_edat, sexe, .keep_all = TRUE)
  
  dades_r <- p %>%
    left_join(cat_values, by = c("codi_indicador","any","grup_edat","sexe")) %>%
    rename(r = r.x, mitjana = r.y) %>%
    mutate(any = as.numeric(any)) %>% 
    unique()
  
  dades_r <- dades_r %>%
    mutate(tmp_id = ifelse(Granularitat == "Centre (Unitat proveïdora)", Codi, NA)) %>%
    left_join(dimup, by = c("tmp_id" = "id_up")) %>%
    select(-tmp_id)
  
  write_parquet(dades_r, paste0("datasets/dades_r/dades_r_", a, ".parquet"))
  
  # --- Visió Totes ---
  cat_values <- df_year %>% filter(Granularitat == "Catalunya") %>%
    select(codi_indicador, any, grup_edat, sexe, r) %>%
    distinct(codi_indicador, any, grup_edat, sexe, .keep_all = TRUE)
  
  dades <- df_year %>%
    left_join(cat_values, by = c("codi_indicador","any","grup_edat","sexe")) %>%
    rename(r = r.x, mitjana = r.y) %>%
    mutate(any = as.numeric(any)) %>% 
    unique()
  
  dades <- dades %>%
    mutate(tmp_id = ifelse(Granularitat == "Centre (Unitat proveïdora)", Codi, NA)) %>%
    left_join(dimup, by = c("tmp_id" = "id_up")) %>%
    select(-tmp_id)
  
  write_parquet(dades, paste0("datasets/dades/dades_", a, ".parquet"))
}



#Dades menu

df_query <- dataset %>%
  #mutate(any = cast(any, string())) %>%  # Force 'any' to string
  rename(
    ambit = `Àmbit`,
    dimensio = `Dimensió`,
    etiqueta = `Etiqueta`,
    grup_edat = Grup_edat
  ) %>%
  mutate(
    `any` = str_trim(`any`, side = "both"),
    ambit = if_else(ambit == "", "Salut Pública", ambit),
    ambit_curt = case_when(
      ambit == "Atenció Primària i Comunitària" ~ "APiC",
      ambit == "Atenció Hospitalària" ~ "AH",
      ambit == "Atenció Intermèdia" ~ "AI",
      ambit == "Salut Mental i Addiccions" ~ "SMiA",
      ambit == "Emergències i Urgències" ~ "EU",
      ambit == "Salut Pública" ~ "SP",
      TRUE ~ NA_character_
    ),
    `Centre/Territori` = if_else(`Centre/Territori` == "ALT PIRINEU I ARAN", "Alt Pirineu i Aran",
                                 if_else(`Centre/Territori` == "BARCELONA - METROPOLITÀ NORD", "Barcelona Metropolitana Nord",
                                         if_else(`Centre/Territori` == "BARCELONA - METROPOLITÀ SUD", "Barcelona Metropolitana Sud",
                                                 if_else(`Centre/Territori` == "BARCELONA - BARCELONA CIUTAT", "Barcelona Ciutat",
                                                         if_else(`Centre/Territori` == "CAMP DE TARRAGONA" & Granularitat == "Regió Sanitària", "Camp de Tarragona",
                                                                 if_else(`Centre/Territori` == "CATALUNYA CENTRAL" & Granularitat == "Regió Sanitària", "Catalunya Central",
                                                                         if_else(`Centre/Territori` == "LLEIDA" & Granularitat == "Regió Sanitària", "Lleida",
                                                                                 if_else(`Centre/Territori` == "GIRONA" & Granularitat == "Regió Sanitària", "Girona",
                                                                                         if_else(`Centre/Territori` == "TERRES DE L'EBRE" & Granularitat == "Regió Sanitària", "Terres de l'Ebre", 
                                                                                                 if_else(`Centre/Territori` == "PENEDÈS" & Granularitat == "Regió Sanitària", "Penedès", `Centre/Territori`))))))))
                                 ))
  ) %>%
  filter(!(any %in% c("2014", "2015", "2016", "2025")),
         !is.na(any))


dades_menu = df_query %>% 
  select(c(ambit_curt, Granularitat, `Centre/Territori`)) %>% 
  mutate(tipus_centre = if_else(Granularitat == "Centre (Unitat proveïdora)" & (ambit_curt == "APiC" | ambit_curt == "SP"), "Equips d'atenció primària",
                                if_else(Granularitat == "Centre (Unitat proveïdora)" & ambit_curt == "AH", "Centres hospitalaris",
                                        if_else(Granularitat == "Centre (Unitat proveïdora)" & ambit_curt == "AI", "Recursos i centres d'Atenció Intermèdia",
                                                if_else(Granularitat == "Centre (Unitat proveïdora)" & ambit_curt == "SMiA", "Centres d'atenció a la Salut Mental i Addiccions", 
                                                        Granularitat)
                                        ))),
         `Centre/Territori` = ifelse(`Centre/Territori` == "CAT", "Catalunya", 
                                     ifelse(`Centre/Territori` == "CSM  Gavà", "CSM Gavà", `Centre/Territori`))) %>% 
  filter(!is.na(`Centre/Territori`), !is.na(tipus_centre)) %>% 
  select(c(Granularitat, `Centre/Territori`, tipus_centre)) %>% 
  distinct() %>% 
  arrange(tipus_centre, `Centre/Territori`)

write_dataset(dades_menu, path = "datasets/dades_menu", format = "parquet")







###################################################################################

library(arrow)
library(dplyr)
library(purrr)
library(stringr)

# Ruta de la taula original
taula_path <- "C:/Users/47854262w/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY"

# Llegeix el dataset amb Arrow
dataset <- open_dataset(taula_path, format = "parquet")

# Transformacions bàsiques (encara sense filtrar per any)
df_query <- dataset %>%
  rename(
    ambit = `Àmbit`,
    dimensio = `Dimensió`,
    etiqueta = `Etiqueta`,
    grup_edat = Grup_edat
  ) %>%
  mutate(
    `any` = str_trim(`any`, side = "both"),
    ambit = if_else(ambit == "", "Salut Pública", ambit),
    ambit_curt = case_when(
      ambit == "Atenció Primària i Comunitària" ~ "APiC",
      ambit == "Atenció Hospitalària" ~ "AH",
      ambit == "Atenció Intermèdia" ~ "AI",
      ambit == "Salut Mental i Addiccions" ~ "SMiA",
      ambit == "Emergències i Urgències" ~ "EU",
      ambit == "Salut Pública" ~ "SP",
      TRUE ~ NA_character_
    ),
    `Centre/Territori` = case_when(
      `Centre/Territori` == "ALT PIRINEU I ARAN" & Granularitat == "Regió Sanitària" ~ "Alt Pirineu i Aran",
      `Centre/Territori` == "BARCELONA - METROPOLITÀ NORD" & Granularitat == "Regió Sanitària" ~ "Barcelona Metropolitana Nord",
      `Centre/Territori` == "BARCELONA - METROPOLITÀ SUD" & Granularitat == "Regió Sanitària" ~ "Barcelona Metropolitana Sud",
      `Centre/Territori` == "BARCELONA - BARCELONA CIUTAT" & Granularitat == "Regió Sanitària" ~ "Barcelona Ciutat",
      `Centre/Territori` == "CAMP DE TARRAGONA" & Granularitat == "Regió Sanitària" ~ "Camp de Tarragona",
      `Centre/Territori` == "CATALUNYA CENTRAL" & Granularitat == "Regió Sanitària" ~ "Catalunya Central",
      `Centre/Territori` == "LLEIDA" & Granularitat == "Regió Sanitària" ~ "Lleida",
      `Centre/Territori` == "GIRONA" & Granularitat == "Regió Sanitària" ~ "Girona",
      `Centre/Territori` == "TERRES DE L'EBRE" & Granularitat == "Regió Sanitària" ~ "Terres de l'Ebre",
      `Centre/Territori` == "PENEDÈS" & Granularitat == "Regió Sanitària" ~ "Penedès",
      TRUE ~ `Centre/Territori`
    )
  ) %>%
  filter(!(any %in% c("2014", "2015", "2016", "2025")),
         !is.na(any))

# --- PROCESS PER ANY ---
anys <- df_query %>% distinct(any) %>% collect() %>% pull(any)

for (a in anys) {
  message("Processing year: ", a)
  
  # Filtra només aquell any
  df_year <- df_query %>% filter(any == a) %>% collect()
  
  # Ordenació i neteja
  df_year <- df_year %>%
    filter(!is.na(sexe), !is.na(r), !is.na(`Centre/Territori`)) %>%
    mutate(`Centre/Territori` = ifelse(`Centre/Territori` == "CAT", "Catalunya", 
                                       ifelse(`Centre/Territori` == "CSM  Gavà", "CSM Gavà", `Centre/Territori`))) %>%
    arrange(codi_indicador, Granularitat)
  
  # --- Visió C ---
  c <- df_year %>% filter(visio != "P")
  cat_values <- c %>% filter(Granularitat == "Catalunya") %>%
    select(codi_indicador, any, grup_edat, sexe, r) %>%
    distinct(codi_indicador, any, grup_edat, sexe, .keep_all = TRUE)
  dades_c <- c %>%
    left_join(cat_values, by = c("codi_indicador","any","grup_edat","sexe")) %>%
    rename(r = r.x, mitjana = r.y) %>%
    mutate(any = as.numeric(any)) %>% 
    unique()
  write_parquet(dades_c, paste0("datasets/dades_c/dades_c_", a, ".parquet"))
  
  # --- Visió R ---
  p <- df_year %>% filter(visio != "C")
  cat_values <- p %>% filter(Granularitat == "Catalunya") %>%
    select(codi_indicador, any, grup_edat, sexe, r) %>%
    distinct(codi_indicador, any, grup_edat, sexe, .keep_all = TRUE)
  dades_r <- p %>%
    left_join(cat_values, by = c("codi_indicador","any","grup_edat","sexe")) %>%
    rename(r = r.x, mitjana = r.y) %>%
    mutate(any = as.numeric(any)) %>% 
    unique()
  write_parquet(dades_r, paste0("datasets/dades_r/dades_r_", a, ".parquet"))
  
  # --- Visió Totes ---
  cat_values <- df_year %>% filter(Granularitat == "Catalunya") %>%
    select(codi_indicador, any, grup_edat, sexe, r) %>%
    distinct(codi_indicador, any, grup_edat, sexe, .keep_all = TRUE)
  dades <- df_year %>%
    left_join(cat_values, by = c("codi_indicador","any","grup_edat","sexe")) %>%
    rename(r = r.x, mitjana = r.y) %>%
    mutate(any = as.numeric(any)) %>% 
    unique()
  write_parquet(dades, paste0("datasets/dades/dades_", a, ".parquet"))
}


#####################################################################################
library(arrow)
library(dplyr)

library(purrr)
library(stringr)

# Ruta a la taula dins OneLake Desktop
taula_path <- "C:/Users/47854262W/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY.parquet/part-00000-cfb64ccc-7f7e-49a7-adb6-37a8f269db33-c000.snappy.parquet"

# Llegeix tots els fitxers parquet de la carpeta com si fos una taula
dataset <- open_dataset(taula_path, format = "parquet")


# lectura d'un indicador
df <- dataset %>%
  #filter(codi_indicador == "AIDG0001-03") %>%
  collect()   # fa el "materialize" en memòria com a data.frame


df = read_parquet("C:/Users/47854262W/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY.parquet/part-00000-cfb64ccc-7f7e-49a7-adb6-37a8f269db33-c000.snappy.parquet")





# Ruta a la taula dins OneLake Desktop
taula_path <- "C:/Users/47854262W/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY.parquet/"

# Llegeix tots els fitxers parquet de la carpeta com si fos una taula
dataset <- open_dataset(taula_path, format = "parquet")


# lectura d'un indicador
df <- dataset %>%
  filter(codi_indicador == "AIDG0001-03") %>%
  collect()   # fa el "materialize" en memòria com a data.frame





library(arrow)
library(dplyr)

# Ruta al fitxer parquet
taula_path <- "C:/Users/47854262W/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY.parquet/part-00000-cfb64ccc-7f7e-49a7-adb6-37a8f269db33-c000.snappy.parquet"

# Llegeix el fitxer parquet
df <- read_parquet(taula_path)

# Desa'l com un parquet "normal"
write_parquet(df, "C:/Users/47854262W/Documents/TAULA_BB_JOIA_SHINY.parquet")






library(arrow)
library(dplyr)

# Path to the dataset folder
folder_path <- "C:/Users/47854262W/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY.parquet"

# Disable memory mapping globally
options(arrow.use_mmap = FALSE)

f <- "C:/Users/47854262W/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY.parquet/part-00000-ba035273-10b2-4bfb-b47c-f1e7a3824ff6-c000.snappy.parquet"
tbl <- read_parquet(f)


# Open the dataset (all parts together)
dataset <- open_dataset(folder_path, format = "parquet")

list.files(folder_path)

# List only parquet files
files <- list.files(folder_path, full.names = TRUE, pattern = "\\.parquet$")


files <- list.files(folder_path, full.names = TRUE, pattern = "\\.parquet$")
for (f in files) {
  cat("Testing:", f, "\n")
  tryCatch({
    read_parquet(f, as_data_frame = FALSE)
    cat("  ✅ OK\n")
  }, error = function(e) {
    cat("  ❌ ERROR:", conditionMessage(e), "\n")
  })
}



# Open them explicitly
dataset <- open_dataset(files, format = "parquet")

files <- list.files(folder_path, full.names = TRUE, pattern = "\\.parquet$")

df <- lapply(files, read_parquet) %>%
  bind_rows()

read_parquet(files[1]) %>% glimpse()

# Read the single Parquet file
df <- read_parquet(file_path)

# Save it as a new Parquet file
write_parquet(df, "C:/Users/47854262W/Documents/TAULA_BB_JOIA_SHINY_output.parquet")


folder_path <- "C:/Users/47854262W/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY.parquet/"

dataset <- open_dataset(folder_path, format = "parquet")
df <- collect(dataset)
write_parquet(df, "C:/Users/47854262W/Documents/TAULA_BB_JOIA_SHINY_output.parquet")


# Load necessary libraries
library(arrow)
library(dplyr)

# Path to the folder containing multiple Parquet parts
taula_path <- "C:/Users/47854262W/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Files/2_PROCESSED_DATA/GOLD/TAULA_BB_JOIA_SHINY"

# Read all Parquet files in the folder as a single dataset
dataset <- open_dataset(taula_path, format = "parquet")

# Collect the dataset into memory as a data frame
df <- collect(dataset)

# Save the combined dataset as a single Parquet file
write_parquet(df, "C:/Users/47854262W/Documents/TAULA_BB_JOIA_SHINY_output.parquet")










# Ruta a la taula dins OneLake Desktop
taula_path <- "C:/Users/47854262w/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Tables/BB_JOIA/05_calc_r_preparacio_shiny"

# Llegeix tots els fitxers parquet de la carpeta com si fos una taula
dataset <- open_dataset(taula_path, format = "parquet")

#nrow(dataset)


df_query <- dataset %>%
  #mutate(any = cast(any, string())) %>%  # Force 'any' to string
  rename(
    ambit = `Àmbit`,
    dimensio = `Dimensió`,
    etiqueta = `Etiqueta`,
    grup_edat = Grup_edat
  ) %>%
  mutate(
    `any` = str_trim(`any`, side = "both"),
    ambit = if_else(ambit == "", "Salut Pública", ambit),
    ambit_curt = case_when(
      ambit == "Atenció Primària i Comunitària" ~ "APiC",
      ambit == "Atenció Hospitalària" ~ "AH",
      ambit == "Atenció Intermèdia" ~ "AI",
      ambit == "Salut Mental i Addiccions" ~ "SMiA",
      ambit == "Emergències i Urgències" ~ "EU",
      ambit == "Salut Pública" ~ "SP",
      TRUE ~ NA_character_
    ),
    `Centre/Territori` = if_else(`Centre/Territori` == "ALT PIRINEU I ARAN", "Alt Pirineu i Aran",
                                 if_else(`Centre/Territori` == "BARCELONA - METROPOLITÀ NORD", "Barcelona Metropolitana Nord",
                                         if_else(`Centre/Territori` == "BARCELONA - METROPOLITÀ SUD", "Barcelona Metropolitana Sud",
                                                 if_else(`Centre/Territori` == "BARCELONA - BARCELONA CIUTAT", "Barcelona Ciutat",
                                                         if_else(`Centre/Territori` == "CAMP DE TARRAGONA" & Granularitat == "Regió Sanitària", "Camp de Tarragona",
                                                                 if_else(`Centre/Territori` == "CATALUNYA CENTRAL" & Granularitat == "Regió Sanitària", "Catalunya Central",
                                                                         if_else(`Centre/Territori` == "LLEIDA" & Granularitat == "Regió Sanitària", "Lleida",
                                                                                 if_else(`Centre/Territori` == "GIRONA" & Granularitat == "Regió Sanitària", "Girona",
                                                                                         if_else(`Centre/Territori` == "TERRES DE L'EBRE" & Granularitat == "Regió Sanitària", "Terres de l'Ebre", 
                                                                                                 if_else(`Centre/Territori` == "PENEDÈS" & Granularitat == "Regió Sanitària", "Penedès", `Centre/Territori`))))))))
                                 ))
  ) %>%
  filter(!(any %in% c("2014", "2015", "2016", "2025")),
         !is.na(any))



write_dataset(df_query, path = "datasets/dades_total", format = "parquet", partitioning = c("any"))  # or another column with a few categories


dades_menu = df_query %>% 
  select(c(ambit_curt, Granularitat, `Centre/Territori`)) %>% 
  mutate(tipus_centre = if_else(Granularitat == "Centre (Unitat proveïdora)" & (ambit_curt == "APiC" | ambit_curt == "SP"), "Equips d'atenció primària",
                                if_else(Granularitat == "Centre (Unitat proveïdora)" & ambit_curt == "AH", "Centres hospitalaris",
                                        if_else(Granularitat == "Centre (Unitat proveïdora)" & ambit_curt == "AI", "Recursos i centres d'Atenció Intermèdia",
                                                if_else(Granularitat == "Centre (Unitat proveïdora)" & ambit_curt == "SMiA", "Centres d'atenció a la Salut Mental i Addiccions", 
                                                        Granularitat)
                                        ))),
         `Centre/Territori` = ifelse(`Centre/Territori` == "CAT", "Catalunya", 
                                     ifelse(`Centre/Territori` == "CSM  Gavà", "CSM Gavà", `Centre/Territori`))) %>% 
  filter(!is.na(`Centre/Territori`)) %>% 
  select(c(Granularitat, `Centre/Territori`, tipus_centre)) %>% 
  distinct() %>% 
  arrange(tipus_centre, `Centre/Territori`)

write_dataset(dades_menu, path = "datasets/dades_menu", format = "parquet")








# First: collect the list of indicators in visio == "C"
indicadors_c <- df_query %>%
  filter(visio == "C") %>%
  select(codi_indicador) %>%
  distinct()

indicadors_cat = df_query %>% 
  filter(visio == "CAT") %>% 
  filter(codi_indicador %in% indicadors_c$codi_indicador)


# Combine both filtered datasets lazily
c <- union(
  df_query %>% filter(visio == "C", codi_indicador %in% indicadors_c$codi_indicador),
  indicadors_cat
)


# Escriu la taula en format parquet, sense carregar-la tota a memòria
write_dataset(c, path = "datasets/dades_c", format = "parquet")


indicadors_p <- df_query %>%
  filter(visio == "P") %>%
  select(codi_indicador) %>%
  distinct()

p = df_query %>% 
  filter(visio == "CAT") %>%
  semi_join(indicadors_p, by = "codi_indicador") %>%
  union_all(
    df_query %>% filter(visio == "P")
  )

# Escriu la taula en format parquet, sense carregar-la tota a memòria
write_dataset(p, path = "datasets/dades_r", format = "parquet", partitioning = c("any"))





# Ruta a la taula dins OneLake Desktop
alfred_path <- "C:/Users/47854262w/OneLake - Microsoft/fws-QiA-Observatori/lh_QiA_Observatori_Gold.Lakehouse/Tables/catalegs/indicadors_alfred"

# Llegeix tots els fitxers parquet de la carpeta com si fos una taula
alfred <- #collect(
  open_dataset(alfred_path, format = "parquet") %>% #) %>% 
  select(c(code, name, keywords, provider, source, desc, reason, num_def, den_def, units, formula, mult, standardized, criteria, exclusions, limitations, interpretation_criteria, interpretation_criteria_text, refs))

write_dataset(alfred, path = "datasets/alfred", format = "parquet")






files <- list.files(
  path = "datasets/dades_total",
  pattern = "part-.*\\.parquet$",
  recursive = TRUE,
  full.names = TRUE
)

# Bind all files with "any" extracted from the folder path
my_df <- map_dfr(files, function(f) {
  any_val <- str_match(f, "any=([0-9]{4})")[,2]  # extract year from path
  arrow::read_parquet(f) %>%
    mutate(any = as.integer(any_val))
})

my_df = my_df %>% 
  #filter(any > 2016 & any < 2025) %>% 
  #mutate(mitjana = 55) %>% 
  filter(!is.na(sexe),
         !is.na(r),
         !is.na(`Centre/Territori`)) %>% 
  mutate(`Centre/Territori` = ifelse(`Centre/Territori` == "CAT", "Catalunya", 
                                     ifelse(`Centre/Territori` == "CSM  Gavà", "CSM Gavà", `Centre/Territori`))) %>% 
  arrange(codi_indicador, any, Granularitat)

custom_order <- c("Atenció Primària i Comunitària", "Atenció Hospitalària", "Atenció Intermèdia", 
                  "Salut Mental i Addiccions", "Emergències i Urgències", "Salut Pública")

my_df$ambit <- factor(my_df$ambit, levels = custom_order)

my_df = my_df %>% 
  arrange(ambit)


# Visió C

c = my_df %>% 
  filter(visio != "P")

cat_values = c %>% 
  ungroup() %>% 
  filter(Granularitat == "Catalunya") %>%
  select(c(codi_indicador, any, grup_edat, sexe, r)) %>%
  distinct(codi_indicador, any, grup_edat, sexe, .keep_all = TRUE)  # Ensure there's only one row per id_indicator and year


df = c %>% 
  left_join(cat_values, by = c("codi_indicador", "any", "grup_edat", "sexe")) %>% 
  rename(r = r.x, mitjana = r.y) %>%
  unique()


# Save back to parquet
write_parquet(df, "datasets/dades/dades_c.parquet")


# Visió P

p = my_df %>% 
  filter(visio != "C")

cat_values = p %>% 
  ungroup() %>% 
  filter(Granularitat == "Catalunya") %>%
  select(c(codi_indicador, any, grup_edat, sexe, r)) %>%
  distinct(codi_indicador, any, grup_edat, sexe, .keep_all = TRUE)  # Ensure there's only one row per id_indicator and year


df = p %>% 
  left_join(cat_values, by = c("codi_indicador", "any", "grup_edat", "sexe")) %>% 
  rename(r = r.x, mitjana = r.y) %>%
  unique()


# Save back to parquet
write_parquet(df, "datasets/dades/dades_r.parquet")


cat_values = my_df %>% 
  ungroup() %>% 
  filter(Granularitat == "Catalunya") %>%
  select(c(codi_indicador, any, grup_edat, sexe, r)) %>%
  distinct(codi_indicador, any, grup_edat, sexe, .keep_all = TRUE)  # Ensure there's only one row per id_indicator and year


df = my_df %>% 
  left_join(cat_values, by = c("codi_indicador", "any", "grup_edat", "sexe")) %>% 
  rename(r = r.x, mitjana = r.y) %>%
  unique()


# Save back to parquet
write_parquet(df, "datasets/dades/dades.parquet")

















