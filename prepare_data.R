# Preparació dels datasets que alimenten l'aplicació

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
                                                                                         if_else(`Centre/Territori` == "TERRES DE L'EBRE" & Granularitat == "Regió Sanitària", "Terres de l'Ebre", `Centre/Territori`)))))))
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


