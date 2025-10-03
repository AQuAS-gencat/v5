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





# Create pre-calculated resum dataset with quartiles
create_resum_dataset <- function() {
  message("Creating pre-calculated resum dataset...")
  
  # Process each year separately to manage memory
  resum_list <- list()
  
  for (year in anys) {
    message("Processing year: ", year)
    
    # Read the year-specific data
    year_data <- dades_total %>%
      filter(
        any == !!year, 
        grup_edat == "Total", 
        sexe == "Total"
      ) %>%
      collect()
    
    if(nrow(year_data) == 0) {
      message("No data for year ", year)
      next
    }
    
    # Calculate quartiles for 'r' variable
    quartiles_r <- year_data %>%
      filter(!is.na(r)) %>%
      group_by(codi_indicador, nom_indicador, Granularitat, any) %>%
      summarise(
        Q0_r = min(r, na.rm = TRUE),
        Q25_r = quantile(r, 0.25, na.rm = TRUE),
        Q75_r = quantile(r, 0.75, na.rm = TRUE),
        Q100_r = max(r, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate quartiles for 'oe' variable (for indicators that have it)
    quartiles_oe <- year_data %>%
      filter(!is.na(oe)) %>%
      group_by(codi_indicador, nom_indicador, Granularitat, any) %>%
      summarise(
        Q0_oe = min(oe, na.rm = TRUE),
        Q25_oe = quantile(oe, 0.25, na.rm = TRUE),
        Q75_oe = quantile(oe, 0.75, na.rm = TRUE),
        Q100_oe = max(oe, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join back with main data
    year_processed <- year_data %>%
      left_join(quartiles_r, by = c("codi_indicador", "nom_indicador", "Granularitat", "any")) %>%
      left_join(quartiles_oe, by = c("codi_indicador", "nom_indicador", "Granularitat", "any")) %>%
      mutate(
        # Pre-calculate marker colors for 'r' variable
        marker_colour = case_when(
          r > mitjana & invers == 0 ~ '#91BFDB',
          r > mitjana & invers == 1 ~ '#FC8D59',
          r < mitjana & invers == 1 ~ '#91BFDB',
          r < mitjana & invers == 0 ~ '#FC8D59',
          invers == 2 ~ '#FFFFFF',
          TRUE ~ '#FFFFFF'
        ),
        
        # Pre-calculate scale values for 'r' variable
        scale_min_r = pmin(Q0_r, mitjana - (Q100_r - mitjana), na.rm = TRUE),
        scale_max_r = pmax(Q100_r, mitjana + (mitjana - Q0_r), na.rm = TRUE),
        
        # Pre-calculate normalized values for 'r' variable
        chosen_value_norm_r = (r - scale_min_r) / (scale_max_r - scale_min_r),
        q0_norm_r = (Q0_r - scale_min_r) / (scale_max_r - scale_min_r),
        q25_norm_r = (Q25_r - scale_min_r) / (scale_max_r - scale_min_r),
        q75_norm_r = (Q75_r - scale_min_r) / (scale_max_r - scale_min_r),
        q100_norm_r = (Q100_r - scale_min_r) / (scale_max_r - scale_min_r),
        
        # For 'oe' variable, we don't need normalization since we use raw values
        # But we can pre-calculate some values if needed
        
        # Pre-calculate confidence intervals
        ic = paste0("[", ic_inf, " - ", ic_sup, "]")
      )
    
    resum_list[[as.character(year)]] <- year_processed
  }
  
  # Combine all years
  resum_final <- bind_rows(resum_list)
  
  return(resum_final)
}

# Create and save the dataset
resum_dataset <- create_resum_dataset()
write_dataset(resum_dataset, path = "datasets/dades_resum_precalc", format = "parquet")



# Create pre-calculated r_resum dataset with quartiles
create_r_resum_dataset <- function() {
  message("Creating pre-calculated r_resum dataset...")
  
  # Process each year separately to manage memory
  resum_list <- list()
  
  for (year in anys) {
    message("Processing year: ", year)
    
    # Read the year-specific data for visió "r" (residential)
    year_data <- dades_total %>%
      filter(
        any == !!year, 
        grup_edat == "Total", 
        sexe == "Total",
        visio != "C"  # This filters for the "r" vision
      ) %>%
      collect()
    
    if(nrow(year_data) == 0) {
      message("No data for year ", year)
      next
    }
    
    # Calculate quartiles for 'r' variable
    quartiles_r <- year_data %>%
      filter(!is.na(r)) %>%
      group_by(codi_indicador, nom_indicador, Granularitat, any) %>%
      summarise(
        Q0_r = min(r, na.rm = TRUE),
        Q25_r = quantile(r, 0.25, na.rm = TRUE),
        Q75_r = quantile(r, 0.75, na.rm = TRUE),
        Q100_r = max(r, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate quartiles for 'oe' variable (for indicators that have it)
    quartiles_oe <- year_data %>%
      filter(!is.na(oe)) %>%
      group_by(codi_indicador, nom_indicador, Granularitat, any) %>%
      summarise(
        Q0_oe = min(oe, na.rm = TRUE),
        Q25_oe = quantile(oe, 0.25, na.rm = TRUE),
        Q75_oe = quantile(oe, 0.75, na.rm = TRUE),
        Q100_oe = max(oe, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join back with main data
    year_processed <- year_data %>%
      left_join(quartiles_r, by = c("codi_indicador", "nom_indicador", "Granularitat", "any")) %>%
      left_join(quartiles_oe, by = c("codi_indicador", "nom_indicador", "Granularitat", "any")) %>%
      mutate(
        # Pre-calculate marker colors for 'r' variable
        marker_colour = case_when(
          r > mitjana & invers == 0 ~ '#91BFDB',
          r > mitjana & invers == 1 ~ '#FC8D59',
          r < mitjana & invers == 1 ~ '#91BFDB',
          r < mitjana & invers == 0 ~ '#FC8D59',
          invers == 2 ~ '#FFFFFF',
          TRUE ~ '#FFFFFF'
        ),
        
        # Pre-calculate scale values for 'r' variable
        scale_min_r = pmin(Q0_r, mitjana - (Q100_r - mitjana), na.rm = TRUE),
        scale_max_r = pmax(Q100_r, mitjana + (mitjana - Q0_r), na.rm = TRUE),
        
        # Pre-calculate normalized values for 'r' variable
        chosen_value_norm_r = (r - scale_min_r) / (scale_max_r - scale_min_r),
        q0_norm_r = (Q0_r - scale_min_r) / (scale_max_r - scale_min_r),
        q25_norm_r = (Q25_r - scale_min_r) / (scale_max_r - scale_min_r),
        q75_norm_r = (Q75_r - scale_min_r) / (scale_max_r - scale_min_r),
        q100_norm_r = (Q100_r - scale_min_r) / (scale_max_r - scale_min_r),
        
        # Pre-calculate confidence intervals
        ic = paste0("[", ic_inf, " - ", ic_sup, "]")
      )
    
    resum_list[[as.character(year)]] <- year_processed
  }
  
  # Combine all years
  resum_final <- bind_rows(resum_list)
  
  return(resum_final)
}

# Create and save the r_resum dataset
r_resum_dataset <- create_r_resum_dataset()
write_dataset(r_resum_dataset, path = "datasets/dades_r_resum_precalc", format = "parquet")




###################################################################################

