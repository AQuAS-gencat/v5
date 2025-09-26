library(dplyr)
library(arrow)
library(readxl)

# Directoris
output_dir = "datasets/"


codebook = read_parquet("codebook5.parquet")

catalegs = read_excel("catalegs.xlsx", sheet = "dg_extra") %>% 
  select(c(dg_extra, codi_cat, cat))


# Dataset dades
dades = read_parquet("df3.parquet") %>%  # Dataset de prova bàsic amb 3 indicadors d'AP, estructura tidy i només amb filtres de data i granularitat
  # Invers = 1 si l'indicador té una interpretació inversa (valor alt és millor), 0 si no, 2 si no importa (de moment cap cas)
  left_join(codebook) %>% 
  left_join(catalegs, by = c("id_subtipologia" = "codi_cat")) %>% 
  filter(!(id_resum == "AHEE00401113" & dg_extra == "Diagnòstic"),
         !(id_resum == "SPSL00101226" & dg_extra == "Diagnòstic")) %>% # Eliminem la subtipologia 113 corresponent a SM, que es solapa amb la 113 d'AH, i la 226 de SM, que es solapa amb la de SPSL
  filter(!(id_resum == "SMDG01101226" & dg_extra == "Subgrup de malaltia"),
         !(id_resum == "SMDG01201226" & dg_extra == "Subgrup de malaltia"),
         !(id_resum == "SMDG00701226" & dg_extra == "Subgrup de malaltia"),
         !(id_resum == "SMDG00301226" & dg_extra == "Subgrup de malaltia"),
         !(id_resum == "SMDG00201226" & dg_extra == "Subgrup de malaltia"),
         !(id_resum == "SMDG00601226" & dg_extra == "Subgrup de malaltia"),
         !(id_resum == "SMDG00602226" & dg_extra == "Subgrup de malaltia"),
         !(id_resum == "SMDG01501226" & dg_extra == "Subgrup de malaltia"),
         !(id_resum == "SMDG01101113" & dg_extra == "Patologies"),
         !(id_resum == "SMDG01201113" & dg_extra == "Patologies"),
         !(id_resum == "SMDG00201113" & dg_extra == "Patologies"),
         !(id_resum == "SMDG00601113" & dg_extra == "Patologies"),
         !(id_resum == "SMDG01501113" & dg_extra == "Patologies"),
         !(id_resum == "SMDG00301113" & dg_extra == "Patologies"),
         !(id_resum == "SMDG00602113" & dg_extra == "Patologies"),
         !(id_resum == "SMDG00701113" & dg_extra == "Patologies")) %>% 
  dplyr::rename(subtipologia = cat)

dades = dades %>% 
  mutate(`Centre/Territori` = ifelse(Granularitat == "Unitat de Salut Laboral", paste0("USL ", `Centre/Territori`),
                                     ifelse(Granularitat == "Unitat territorial (protecció de la salut)", paste0("UT ", `Centre/Territori`), `Centre/Territori`)),
         `Centre/Territori` = ifelse(`Centre/Territori` == "CSM  Gavà", "CSM Gavà", 
                                            ifelse(`Centre/Territori` == "EAP  Lluçanès", "EAP Lluçanès",
                                                   ifelse(`Centre/Territori` == "Parc Sanitari Sant  Joan Déu - HG", "Parc Sanitari Sant Joan Déu - HG",
                                                          ifelse(`Centre/Territori` == "Hospital Univ.  Joan XXIII de Tarragona", "Hospital Univ. Joan XXIII de Tarragona",
                                                                 `Centre/Territori`)))))


dades = dades %>% 
  arrange(desc(any)) # Per mostrar els selectors d'any en ordre ###### ES POT ELIMINAR?


# Ordenem els àmbits pq surtin ordenats a la taula resum
custom_order <- c("Atenció Primària", "Atenció Hospitalària", "Atenció Intermèdia", 
                  "Salut Mental i Addiccions", "Emergències Mèdiques", "Salut Pública")

dades$ambit <- factor(dades$ambit, levels = custom_order)

#dades_conselleria = dades %>% 
#  filter((id_indicador == "SMDG00201" | id_indicador == "SMDG00501") & Granularitat == "Catalunya") %>% 
#  select(c(any, indicador, dg_extra, subtipologia, sexe, n, d, resultat)) %>% 
#  arrange(indicador, dg_extra, subtipologia, sexe, any)

#write_xlsx(dades_conselleria, paste0(output_dir, "csmij_visor.xlsx"))

dades = dades %>% 
  arrange(ambit) %>% 
  select(-c(n, d))

write_parquet(dades, paste0(output_dir, "df_dades.parquet"))

# VISIÓ CENTRE ----

dades_c = dades %>% 
  filter(visio == "c" | Granularitat == "Centre (Unitat proveïdora)" | Granularitat == "Catalunya") %>% 
  mutate(resultat2 = rbeta(n(), shape1 = 2, shape2 = 2) * 2,
         ci_width = runif(n(), min = 0.05, max = 0.2),  # Random CI width
         ic_inferior = pmax(0, resultat2 - ci_width / 2),  # Ensure lower bound ≥ 0
         ic_superior = pmin(2, resultat2 + ci_width / 2),    # Ensure upper bound ≤ max scale) %>% 
         ambit_curt = case_when(
           ambit == "Atenció Primària" ~ "APiC",
           ambit == "Atenció Hospitalària" ~ "AH",
           ambit == "Atenció Intermèdia" ~ "AI",
           ambit == "Salut Mental i Addiccions" ~ "SMiA",
           ambit == "Emergències i Urgències" ~ "EU",
           ambit == "Salut Pública" ~ "SP",
           TRUE ~ NA
         )) %>% 
  filter(!(id_indicador == "APDG00201" | id_indicador == "APEE00701" | id_indicador == "APEI00301")) %>% 
  select(-ci_width)

write_parquet(dades_c, paste0(output_dir, "df_dades_c.parquet"))



dades_c_resum = dades %>% 
  filter(
    visio == "c",
    sexe == "Total",
    grup_edat == "Total") %>% 
  mutate(extra_info = ifelse(!is.na(dg_extra) & !is.na(subtipologia),
                             paste(dg_extra, ":", subtipologia),
                             NA),
         resultat2 = rbeta(n(), shape1 = 2, shape2 = 2) * 2,
         ci_width = runif(n(), min = 0.05, max = 0.2),  # Random CI width
         ic_inferior = pmax(0, resultat2 - ci_width / 2),  # Ensure lower bound ≥ 0
         ic_superior = pmin(2, resultat2 + ci_width / 2),
         ambit_curt = case_when(
           ambit == "Atenció Primària" ~ "APiC",
           ambit == "Atenció Hospitalària" ~ "AH",
           ambit == "Atenció Intermèdia" ~ "AI",
           ambit == "Salut Mental i Addiccions" ~ "SMiA",
           ambit == "Emergències i Urgències" ~ "EU",
           ambit == "Salut Pública" ~ "SP",
           TRUE ~ NA
         )) %>%    # Ensure upper bound ≤ max scale) %>% 
  filter(!(id_indicador == "APDG00201" | id_indicador == "APEE00701" | id_indicador == "APEI00301")) %>% 
  select(-ci_width)


write_parquet(dades_c_resum, paste0(output_dir, "df_dades_c_resum.parquet"))

dades_c_rank = dades_c %>% 
  filter(
    grup_edat == "Total") %>% 
  select(-grup_edat) %>% 
  mutate(resultat2 = rbeta(n(), shape1 = 2, shape2 = 2) * 2,
         ci_width = runif(n(), min = 0.05, max = 0.2),  # Random CI width
         ic_inferior = pmax(0, resultat2 - ci_width / 2),  # Ensure lower bound ≥ 0
         ic_superior = pmin(2, resultat2 + ci_width / 2)) %>%    # Ensure upper bound ≤ max scale) %>% 
  filter(!(id_indicador == "APDG00201" | id_indicador == "APEE00701" | id_indicador == "APEI00301")) %>% 
  select(-ci_width)


write_parquet(dades_c_rank, paste0(output_dir, "df_dades_c_rank.parquet"))

dades_simplificades = dades %>% 
  select(c(Granularitat, `Centre/Territori`, tipus_centre)) %>% 
  #filter(Granularitat != "Catalunya") %>% 
  #mutate(tipus_centre = ifelse(is.na(tipus_centre), "Regió Sanitària", tipus_centre)) %>% 
  filter(!is.na(`Centre/Territori`)) %>% 
  unique()

write_parquet(dades_simplificades, paste0(output_dir, "df_dades_simplificades.parquet"))

#write.csv2(dades, paste0(output_dir, "prova.csv"), row.names = F)


# VISIÓ RESIDÈNCIA PACIENT ----

dades_r = dades %>% 
  filter(is.na(visio) | Granularitat != "Centre (Unitat proveïdora)") %>% 
  mutate(resultat2 = rbeta(n(), shape1 = 2, shape2 = 2) * 2,
         ci_width = runif(n(), min = 0.05, max = 0.2),  # Random CI width
         ic_inferior = pmax(0, resultat2 - ci_width / 2),  # Ensure lower bound ≥ 0
         ic_superior = pmin(2, resultat2 + ci_width / 2),   # Ensure upper bound ≤ max scale) %>% 
         ambit_curt = case_when(
           ambit == "Atenció Primària" ~ "APiC",
           ambit == "Atenció Hospitalària" ~ "AH",
           ambit == "Atenció Intermèdia" ~ "AI",
           ambit == "Salut Mental i Addiccions" ~ "SMiA",
           ambit == "Emergències i Urgències" ~ "EU",
           ambit == "Salut Pública" ~ "SP",
           TRUE ~ NA
         )) %>% 
  filter(!(id_indicador == "APDG00201" | id_indicador == "APEE00701" | id_indicador == "APEI00301")) %>% 
  select(-ci_width)

write_parquet(dades_r, paste0(output_dir, "df_dades_r.parquet"))


dades_r_resum = dades_r %>% 
  filter(
    is.na(visio),
    sexe == "Total",
    grup_edat == "Total") %>% 
  mutate(extra_info = ifelse(!is.na(dg_extra) & !is.na(subtipologia),
                             paste(dg_extra, ":", subtipologia),
                             NA),
         resultat2 = rbeta(n(), shape1 = 2, shape2 = 2) * 2,
         ci_width = runif(n(), min = 0.05, max = 0.2),  # Random CI width
         ic_inferior = pmax(0, resultat2 - ci_width / 2),  # Ensure lower bound ≥ 0
         ic_superior = pmin(2, resultat2 + ci_width / 2),
         ambit_curt = case_when(
           ambit == "Atenció Primària" ~ "APiC",
           ambit == "Atenció Hospitalària" ~ "AH",
           ambit == "Atenció Intermèdia" ~ "AI",
           ambit == "Salut Mental i Addiccions" ~ "SMiA",
           ambit == "Emergències i Urgències" ~ "EU",
           ambit == "Salut Pública" ~ "SP",
           TRUE ~ NA
         )) %>%    # Ensure upper bound ≤ max scale) %>% 
  filter(!(id_indicador == "APDG00201" | id_indicador == "APEE00701" | id_indicador == "APEI00301")) %>% 
  select(-ci_width)


write_parquet(dades_r_resum, paste0(output_dir, "df_dades_r_resum.parquet"))


dades_r_rank = dades_r %>% 
  filter(
    grup_edat == "Total") %>% 
  select(-grup_edat)

write_parquet(dades_r_rank, paste0(output_dir, "df_dades_r_rank.parquet"))


dades_ambit_ind = dades_r %>% 
  filter(Granularitat != "Catalunya") %>% 
  select(c(id_indicador, indicador, dimensio, ambit, tag1, tag2, tag3)) %>% 
  unique()

write_parquet(dades_ambit_ind, paste0(output_dir, "dades_ambit_ind.parquet"))




# Dataset dades per descarregar

dades_descarrega = dades %>%
  select(c("ambit", 
           "dimensio",
           "centre_territori" = "Centre/Territori", 
           "codi" = "Codi",
           "granularitat" = "Granularitat",
           "indicador",
           "subcategoria" = "subtipologia",
           "sexe",
           "any",
           "resultat", 
           "catalunya" = "mitjana", 
           "unitat" = "mesura"))

write.csv2(dades_descarrega, paste0(output_dir, "dades_CentralDeResultats.csv"), row.names = F)
#write.csv2(dades_descarrega, gzfile(paste0(output_dir, "dades_CentralDeResultats.csv.gz")), row.names = F)


df <- rsconnect::showMetrics("container_status",
                             c("connect_count"), 
                             account = "aquas-gencat",
                             server="shinyapps.io", 
                             appName = "centralderesultats")


df <- rsconnect::showMetrics("container_status",
                             c("connect_count"), 
                             account = "aquas-gencat",
                             server="shinyapps.io", 
                             appName = "centralderesultats",
                             from = "2w",
                             interval = "1d")

df <- rsconnect::showMetrics("container_status",
                             c("connect_count"), 
                             account = "aquas-gencat",
                             server="shinyapps.io", 
                             appName = "centralderesultats",
                             from = "1w",
                             interval = "3h")


#prova_mapa = dades_resum %>% 
#  filter(id_indicador == "APAD00201",
#         Granularitat == "Àrea Bàsica de Salut") %>% 
#  select(c(any, Codi, `Centre/Territori`, resultat))

#write.csv2(prova_mapa, "C:/Sergi/prova_mapa/prova_mapa.csv", row.names = F)



#dades_cusco = read_parquet("sm_cusco2.parquet") %>%  # Dataset de prova bàsic amb 3 indicadors d'AP, estructura tidy i només amb filtres de data i granularitat
#  # Invers = 1 si l'indicador té una interpretació inversa (valor alt és millor), 0 si no, 2 si no importa (de moment cap cas)
#  left_join(codebook) %>% 
#  left_join(catalegs, by = c("id_subtipologia" = "codi_cat")) %>% 
#  filter(!(id_resum == "AHEE00401113" & dg_extra == "Diagnòstic"),
#         !(id_resum == "SPSL00101226" & dg_extra == "Diagnòstic")) %>% # Eliminem la subtipologia 113 corresponent a SM, que es solapa amb la 113 d'AH, i la 226 de SM, que es solapa amb la de SPSL
#  filter(!(id_resum == "SMDG01101226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG01201226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00701226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00301226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00201226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00601226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00602226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG01501226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG01101113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG01201113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00201113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00601113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG01501113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00301113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00602113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00701113" & dg_extra == "Patologies")) %>% 
#  dplyr::rename(subtipologia = cat) %>% 
#  select(-c(invers, mitjana, n_mitjana))
#
#
#write_parquet(dades_cusco, paste0(output_dir, "df_dades_sm.parquet"))








#dades_cusco = read_parquet("sm_cusco2.parquet") %>%  # Dataset de prova bàsic amb 3 indicadors d'AP, estructura tidy i només amb filtres de data i granularitat
#  # Invers = 1 si l'indicador té una interpretació inversa (valor alt és millor), 0 si no, 2 si no importa (de moment cap cas)
#  left_join(codebook) %>% 
#  left_join(catalegs, by = c("id_subtipologia" = "codi_cat")) %>% 
#  filter(!(id_resum == "AHEE00401113" & dg_extra == "Diagnòstic"),
#         !(id_resum == "SPSL00101226" & dg_extra == "Diagnòstic")) %>% # Eliminem la subtipologia 113 corresponent a SM, que es solapa amb la 113 d'AH, i la 226 de SM, que es solapa amb la de SPSL
#  filter(!(id_resum == "SMDG01101226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG01201226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00701226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00301226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00201226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00601226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG00602226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG01501226" & dg_extra == "Subgrup de malaltia"),
#         !(id_resum == "SMDG01101113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG01201113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00201113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00601113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG01501113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00301113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00602113" & dg_extra == "Patologies"),
#         !(id_resum == "SMDG00701113" & dg_extra == "Patologies")) %>% 
#  dplyr::rename(subtipologia = cat) %>% 
#  select(-c(invers, mitjana, n_mitjana))
#
#
#write_parquet(dades_cusco, paste0(output_dir, "df_dades_sm.parquet"))
#
#
## persones ateses i casos nous a CSMA 
#dades_csma = dades_cusco %>% 
#  filter(id_indicador == "SMDG01101" | id_indicador == "SMDG01401") %>% 
#  filter(Granularitat == "Catalunya") %>% 
#  select(c(any, indicador, dg_extra, subtipologia, sexe, n, d, resultat))
#
#library(xlsx)
#
#write.xlsx(dades_csma, "dades_csma.xlsx", row.names = F)#

docu_base = read_excel("docu_base.xlsx") %>% # Excel extret del WP visualització 2024, arxiu docu_base.xlsx
  mutate(
    tag1 = case_when(
      id_indicador == "APDG00201" | id_indicador == "APDG00601" | id_indicador == "APDG00701" ~ "Visites",
      id_indicador == "APEE00701" | id_indicador == "APEI00201" | id_indicador == "APEI00301" ~ "Despesa farmacèutica",
      id_indicador == "AHAD00101" | id_indicador == "AHAD00102" | id_indicador == "AHAD00103" | id_indicador == "AHAD00104" | id_indicador == "AHAD00105" ~ "Urgències i emergències",
      TRUE ~ NA
    ),
    tag2 = case_when(
      id_indicador == "APDG00201" | id_indicador == "APDG00601" | id_indicador == "APDG00701" ~ "Accessibilitat",
      TRUE ~ NA
    ),
    tag3 = case_when(
      id_indicador == "APEI00301" | id_indicador == "AHAD00101" ~ "Prova",
      TRUE ~ NA
    )
  ) %>% 
  ungroup()

write_xlsx(docu_base, "docu_base.xlsx")
  