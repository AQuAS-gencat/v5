# CREACIÓ DEL DATASET DEL COMPARADOR

library(arrow)
library(tidyr)
library(plotly)
library(dplyr)
library(lubridate)
library(readxl)
library(stringr)
library(xlsx)

######
# AP # ----
######

ap = read_parquet("AP_dataset_def.parquet")

dimup = read_excel("DimUp_2024-10-28.xlsx") %>% 
  select(c(id_up:abs, AP_titularitat, AP_es_eap)) %>% 
  filter(!str_detect(tipus_up, "PROVEÏDORS DE PRESTACIONS"))

dimup_up = dimup %>% 
  select(c(id_up, up, AP_titularitat, AP_es_eap)) %>% 
  unique()

dimup_abs = dimup %>% 
  select(c(id_abs, abs)) %>% 
  unique()

dimup_aga = dimup %>% 
  select(c(id_abs, id_aga, aga)) %>% 
  unique()

dimup_rs = dimup %>% 
  select(c(id_abs, id_rs, rs)) %>% 
  unique()

dimup_rs2 = dimup %>% 
  select(c(id_rs2, rs2)) %>% 
  unique()

ctlg_rs = read_excel("ctlg_rs.xlsx") %>% 
  mutate(tipus = ifelse(tipus == "simple", "rs",
                        ifelse(tipus == "ampliat", "rs2",
                               ifelse(tipus == "ampliat-2", "rs3",
                                      tipus))))

ctlg_rs_rs = ctlg_rs %>% 
  filter(tipus == "rs")

ctlg_rs_rs2 = ctlg_rs %>% 
  filter(tipus == "rs2")

ctlg_rs_curt = ctlg_rs %>% 
  select(c(codi_rs, rs)) %>% unique()


indicadors_ap = c("APDG00201", "APDG00601", "APDG00701", "APEE00701", "APEI00201", 
                  "APEI00301")

indicadors_ap_edat = c("APDG00201", "APDG00601", "APDG00701", "APEE00701", "APEI00201", 
                  "APEI00301")

indicadors_ap_dir = c("APDG00601", "APDG00701", "APEE00101", "APEE00102", "APEE00103",
                      "APEE00202", "APEE00401", "APEE00402", "APEE00403", "APEE00501",
                      "APEE00502", "APEE00601", "APEE00801", "APEE00802", "APEU00101", 
                      "APEU00201", "APEU00202", "APEU00203", "APEU00204", "APEU00301", 
                      "APTI00101", "APTI00102", "APTI00201")

indicadors_ap_neu = c("APDG00201")

indicadors_ap_sexe = c("APAD00201", "APDG00701","APEI00201", "APTI00201")

indicadors_ap_sub_sexe = c("APDG00201", "APEE00701", "APEI00301")

indicadors_ap_sub_edat = c("APDG00201", "APEE00701", "APEI00301")

indicadors_ap_mensuals = c("APAD00301", "APAD00401", "APAD00501", "APAD00601", "APDG00601",
                           "APEE00101", "APEE00102", "APEE00103", "APEE00201", "APEE00202",
                           "APEE00301", "APEE00302", "APEE00401", "APEE00402", "APEE00403",
                           "APEE00501", "APEE00502", "APEE00601", "APEE00801", "APEE00802",
                           "APTI00101", "APTI00102")

indicadors_ap_mensuals_sexe = c("APAD00301", "APAD00401", "APAD00501", "APAD00601", "APDG00601",
                                "APEE00101", "APEE00102", "APEE00103", "APEE00201", "APEE00202",
                                "APEE00301", "APEE00302", "APEE00401", "APEE00402", "APEE00403",
                                "APEE00501", "APEE00502", "APEE00601", "APEE00801", "APEE00802")


ap2 = ap %>% 
  filter(codi_indicador_23 %in% indicadors_ap) %>% 
  mutate(any = substr(data, 1, 4))


# SENSE GRANULARITAT ---- 

# Mensuals ----

mensuals = ap2 %>% 
  filter(codi_indicador_23 %in% indicadors_ap_mensuals) %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_abs, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_aga, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_rs, by = c("codi_abs_a" = "id_abs")) %>%
  select(-rs) %>% 
  left_join(ctlg_rs, by = c("id_rs" = "codi_rs_custom")) %>% 
  filter(AP_es_eap == 1) %>% 
  filter(!is.na(den)) %>% 
  filter(str_ends(data, "12-01")) %>% 
  select(-tipus)


# Visió centre ----

ap_up_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_up, up) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         visio = "c")


# Visió pacient ----

ap_abs_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_abs_a, abs) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_aga_m = mensuals %>% 
  group_by(codi_indicador_23, any, id_aga, aga) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_rs_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_rs, rs) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_cat_m = mensuals %>% 
  group_by(codi_indicador_23, any) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

nogran_ap_m = bind_rows(ap_up_m, ap_abs_m, ap_aga_m, ap_rs_m, ap_cat_m) %>% 
  arrange(codi_indicador_23, any, Granularitat) %>% 
  filter(!is.nan(resultat))


# SEXE ----

mensuals_sexe = mensuals %>% 
  filter(codi_indicador_23 %in% indicadors_ap_mensuals_sexe)


# Visió centre ----

ap_up_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_up, up, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"),
         visio = "c")


# Visió pacient ----

ap_abs_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_abs_a, abs, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_aga_m = mensuals %>% 
  group_by(codi_indicador_23, any, id_aga, aga, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_rs_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_rs, rs, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_cat_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

nogran_ap_m_sexe = bind_rows(ap_up_m, ap_abs_m, ap_aga_m, ap_rs_m, ap_cat_m) %>% 
  arrange(codi_indicador_23, any, Granularitat) %>% 
  filter(!is.nan(resultat)) #%>% 
  #mutate(id_subtipologia = NA)


# EDAT ----

mensuals = ap2 %>% 
  filter(codi_indicador_23 %in% indicadors_ap_mensuals & codi_indicador_23 %in% indicadors_ap_edat) %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_abs, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_aga, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_rs, by = c("codi_abs_a" = "id_abs")) %>%
  select(-rs) %>% 
  left_join(ctlg_rs, by = c("id_rs" = "codi_rs_custom")) %>% 
  filter(AP_es_eap == 1) %>% 
  filter(!is.na(den)) %>% 
  filter(str_ends(data, "12-01")) %>% 
  select(-tipus)


# Visió centre ----

ap_up_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_up, up, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         visio = "c")


# Visió pacient ----

ap_abs_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_abs_a, abs, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_aga_m = mensuals %>% 
  group_by(codi_indicador_23, any, id_aga, aga, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_rs_m = mensuals %>% 
  group_by(codi_indicador_23, any, codi_rs, rs, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_cat_m = mensuals %>% 
  group_by(codi_indicador_23, any, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

nogran_ap_m_edat = bind_rows(ap_up_m, ap_abs_m, ap_aga_m, ap_rs_m, ap_cat_m) %>% 
  arrange(codi_indicador_23, any, Granularitat, grup_edat) %>% 
  filter(!is.nan(resultat))





# EDAT I SEXE ----

mensuals_edat_sexe = mensuals %>% 
  filter(codi_indicador_23 %in% indicadors_ap_mensuals_sexe & codi_indicador_23 %in% indicadors_ap_edat)


# Visió centre ----

ap_up_m = mensuals_edat_sexe %>% 
  group_by(codi_indicador_23, any, codi_up, up, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"),
         visio = "c")


# Visió pacient ----

ap_abs_m = mensuals_edat_sexe %>% 
  group_by(codi_indicador_23, any, codi_abs_a, abs, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_aga_m = mensuals_edat_sexe %>% 
  group_by(codi_indicador_23, any, id_aga, aga, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_rs_m = mensuals_edat_sexe %>% 
  group_by(codi_indicador_23, any, codi_rs, rs, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_cat_m = mensuals_edat_sexe %>% 
  group_by(codi_indicador_23, any, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

nogran_ap_m_edat_sexe = bind_rows(ap_up_m, ap_abs_m, ap_aga_m, ap_rs_m, ap_cat_m) %>% 
  arrange(codi_indicador_23, any, Granularitat) %>% 
  filter(!is.nan(resultat)) #%>% 
#mutate(id_subtipologia = NA)





# ANUALS ----

anuals = ap2 %>% 
  filter(!(codi_indicador_23 %in% indicadors_ap_mensuals | codi_indicador_23 %in% indicadors_ap_sub_sexe)) %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_abs, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_aga, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_rs, by = c("codi_abs_a" = "id_abs")) %>%
  select(-rs) %>% 
  left_join(ctlg_rs, by = c("id_rs" = "codi_rs_custom")) %>% 
  filter(AP_es_eap == 1) %>% 
  filter(!is.na(den)) %>% 
  select(-tipus)


# Visió centre ----

ap_up = anuals %>% 
  group_by(codi_indicador_23, any, codi_up, up) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         visio = "c")


# Visió pacient ----

ap_abs = anuals %>% 
  group_by(codi_indicador_23, any, codi_abs_a, abs) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_aga = anuals %>% 
  group_by(codi_indicador_23, any, id_aga, aga) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_rs = anuals %>% 
  group_by(codi_indicador_23, any, codi_rs, rs) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_cat = anuals %>% 
  group_by(codi_indicador_23, any) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

nogran_ap = bind_rows(ap_up, ap_abs, ap_aga, ap_rs, ap_cat) %>% 
  arrange(codi_indicador_23, any, Granularitat) %>% 
  filter(!is.nan(resultat))


# SEXE ----

anuals_sexe = anuals %>% 
  filter(codi_indicador_23 %in% indicadors_ap_sexe)


# Visió centre ----

ap_up = anuals %>% 
  group_by(codi_indicador_23, any, codi_up, up, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"),
         visio = "c")


# Visió pacient ----

ap_abs = anuals %>% 
  group_by(codi_indicador_23, any, codi_abs_a, abs, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_aga = anuals %>% 
  group_by(codi_indicador_23, any, id_aga, aga, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_rs = anuals %>% 
  group_by(codi_indicador_23, any, codi_rs, rs, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_cat = anuals %>% 
  group_by(codi_indicador_23, any, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

nogran_ap_sexe = bind_rows(ap_up, ap_abs, ap_aga, ap_rs, ap_cat) %>% 
  arrange(codi_indicador_23, any, Granularitat) %>% 
  filter(!is.nan(resultat)) %>% 
  mutate(id_subtipologia = NA)


# EDAT ----

anuals_edat = anuals %>% 
  filter(codi_indicador_23 %in% indicadors_ap_edat)


# Visió centre ----

ap_up = anuals_edat %>% 
  group_by(codi_indicador_23, any, codi_up, up, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         visio = "c")


# Visió pacient ----

ap_abs = anuals_edat %>% 
  group_by(codi_indicador_23, any, codi_abs_a, abs, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_aga = anuals_edat %>% 
  group_by(codi_indicador_23, any, id_aga, aga, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_rs = anuals_edat %>% 
  group_by(codi_indicador_23, any, codi_rs, rs, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

ap_cat = anuals_edat %>% 
  group_by(codi_indicador_23, any, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))))

nogran_ap_edat = bind_rows(ap_up, ap_abs, ap_aga, ap_rs, ap_cat) %>% 
  arrange(codi_indicador_23, any, Granularitat) %>% 
  filter(!is.nan(resultat)) %>% 
  mutate(id_subtipologia = NA)



# EDAT I SEXE ----

anuals_sexe = anuals %>% 
  filter(codi_indicador_23 %in% indicadors_ap_sexe)


# Visió centre ----

ap_up = anuals %>% 
  group_by(codi_indicador_23, any, codi_up, up, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"),
         visio = "c")


# Visió pacient ----

ap_abs = anuals %>% 
  group_by(codi_indicador_23, any, codi_abs_a, abs, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_aga = anuals %>% 
  group_by(codi_indicador_23, any, id_aga, aga, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_rs = anuals %>% 
  group_by(codi_indicador_23, any, codi_rs, rs, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

ap_cat = anuals %>% 
  group_by(codi_indicador_23, any, codi_sexe, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APDG00201" | codi_indicador_23 == "APEI00101" | codi_indicador_23 == "APEI00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant",
                         ifelse(codi_indicador_23 == "APEI00201", "Receptes",
                                ifelse(codi_indicador_23 == "APEI00101", "Euros", "%"))),
         sexe = ifelse(codi_sexe == 1, "Dona", "Home"))

nogran_ap_edat_sexe = bind_rows(ap_up, ap_abs, ap_aga, ap_rs, ap_cat) %>% 
  arrange(codi_indicador_23, any, Granularitat) %>% 
  filter(!is.nan(resultat)) %>% 
  mutate(id_subtipologia = NA)


# INDICADORS AMB SUBTIPOLOGIA ----

# GRANULARITAT ----

# SENSE GRANULARITAT ----

ap2 = ap %>% 
  filter(codi_indicador_23 %in% indicadors_ap_sub_sexe) %>% 
  mutate(any = substr(data, 1, 4)) %>% 
  rename(id_subtipologia = codi_dg_extra_1)

nogran_ap_sub = ap2 %>% 
  filter(is.na(uta_c)) %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_abs, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_aga, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_rs, by = c("codi_abs_a" = "id_abs")) %>%
  select(-rs) %>% 
  left_join(ctlg_rs, by = c("id_rs" = "codi_rs_custom")) %>% 
  filter(AP_es_eap == 1) %>% 
  filter(!is.na(den)) %>% 
  select(-tipus)




# Visió centre ----

ap_up = nogran_ap_sub %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_up, up) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total",
         visio = "c") %>% 
  filter(!is.na(Codi))



# Visió pacient ----


ap_abs = nogran_ap_sub %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_abs_a, abs) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total") %>% 
  filter(!is.na(Codi))

ap_aga = nogran_ap_sub %>% 
  group_by(codi_indicador_23, id_subtipologia, any, id_aga, aga) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total") %>% 
  filter(!is.na(Codi))

ap_rs = nogran_ap_sub %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_rs, rs) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total") %>% 
  filter(!is.na(Codi))

ap_cat = nogran_ap_sub %>% 
  group_by(codi_indicador_23, id_subtipologia, any) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total")

nogran_ap_sub = bind_rows(ap_up, ap_abs, ap_aga, ap_rs, ap_cat) %>% 
  arrange(codi_indicador_23, id_subtipologia, any, Granularitat)

# AMB GRANULARITAT I SEXE ----

# SENSE GRANULARITAT I SEXE ----

ap2 = ap %>% 
  filter(codi_indicador_23 %in% indicadors_ap_sub_sexe) %>% 
  mutate(any = substr(data, 1, 4)) %>% 
  rename(id_subtipologia = codi_dg_extra_1)

nogran_ap_sub_sexe = ap2 %>% 
  filter(is.na(uta_c)) %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_abs, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_aga, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_rs, by = c("codi_abs_a" = "id_abs")) %>%
  select(-rs) %>% 
  left_join(ctlg_rs, by = c("id_rs" = "codi_rs_custom")) %>% 
  filter(AP_es_eap == 1) %>% 
  filter(!is.na(den)) %>% 
  select(-tipus)

# Visió centre ----

ap_up = nogran_ap_sub_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_up, up, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total")),
         visio = "c") %>% 
  filter(!is.na(Codi))


# Visió pacient ----

ap_abs = nogran_ap_sub_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_abs_a, abs, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(Codi))

ap_aga = nogran_ap_sub_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, id_aga, aga, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(Codi))

ap_rs = nogran_ap_sub_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_rs, rs, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(Codi))

ap_cat = nogran_ap_sub_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total")))

nogran_ap_sub_sexe = bind_rows(ap_up, ap_abs, ap_aga, ap_rs, ap_cat) %>% 
  arrange(codi_indicador_23, id_subtipologia, any, Granularitat, sexe)


# EDAT ----

ap2 = ap %>% 
  filter(codi_indicador_23 %in% indicadors_ap_sub_edat) %>% 
  mutate(any = substr(data, 1, 4)) %>% 
  rename(id_subtipologia = codi_dg_extra_1)

nogran_ap_sub_edat = ap2 %>% 
  filter(is.na(uta_c)) %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_abs, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_aga, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_rs, by = c("codi_abs_a" = "id_abs")) %>%
  select(-rs) %>% 
  left_join(ctlg_rs, by = c("id_rs" = "codi_rs_custom")) %>% 
  filter(AP_es_eap == 1) %>% 
  filter(!is.na(den)) %>% 
  select(-tipus)




# Visió centre ----

ap_up = nogran_ap_sub_edat %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_up, up, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total",
         visio = "c") %>% 
  filter(!is.na(Codi))



# Visió pacient ----


ap_abs = nogran_ap_sub_edat %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_abs_a, abs, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total") %>% 
  filter(!is.na(Codi))

ap_aga = nogran_ap_sub_edat %>% 
  group_by(codi_indicador_23, id_subtipologia, any, id_aga, aga, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total") %>% 
  filter(!is.na(Codi))

ap_rs = nogran_ap_sub_edat %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_rs, rs, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total") %>% 
  filter(!is.na(Codi))

ap_cat = nogran_ap_sub_edat %>% 
  group_by(codi_indicador_23, id_subtipologia, any, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = "Total")

nogran_ap_sub_edat = bind_rows(ap_up, ap_abs, ap_aga, ap_rs, ap_cat) %>% 
  arrange(codi_indicador_23, id_subtipologia, any, Granularitat, grup_edat)

# EDAT I SEXE ----

ap2 = ap %>% 
  filter(codi_indicador_23 %in% indicadors_ap_sub_sexe & codi_indicador_23 %in% indicadors_ap_sub_edat) %>% 
  mutate(any = substr(data, 1, 4)) %>% 
  rename(id_subtipologia = codi_dg_extra_1)

nogran_ap_sub_edat_sexe = ap2 %>% 
  filter(is.na(uta_c)) %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_abs, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_aga, by = c("codi_abs_a" = "id_abs")) %>% 
  left_join(dimup_rs, by = c("codi_abs_a" = "id_abs")) %>%
  select(-rs) %>% 
  left_join(ctlg_rs, by = c("id_rs" = "codi_rs_custom")) %>% 
  filter(AP_es_eap == 1) %>% 
  filter(!is.na(den)) %>% 
  select(-tipus)

# Visió centre ----

ap_up = nogran_ap_sub_edat_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_up, up, grup_edat, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = up, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total")),
         visio = "c") %>% 
  filter(!is.na(Codi))


# Visió pacient ----

ap_abs = nogran_ap_sub_edat_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_abs_a, abs, grup_edat, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = abs, Codi = codi_abs_a) %>% 
  mutate(Granularitat = "Àrea Bàsica de Salut",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(Codi))

ap_aga = nogran_ap_sub_edat_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, id_aga, aga, grup_edat, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = id_aga) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(Codi))

ap_rs = nogran_ap_sub_edat_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, codi_rs, rs, grup_edat, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(Codi))

ap_cat = nogran_ap_sub_edat_sexe %>% 
  group_by(codi_indicador_23, id_subtipologia, any, grup_edat, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya",
         `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "APEI00301" | codi_indicador_23 == "APDG00201", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "APEI00301", "Euros",
                         ifelse(codi_indicador_23 == "APDG00201", "Visites per habitant", "%")),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total")))

nogran_ap_sub_edat_sexe = bind_rows(ap_up, ap_abs, ap_aga, ap_rs, ap_cat) %>% 
  arrange(codi_indicador_23, id_subtipologia, any, Granularitat, grup_edat, sexe)



### UNIM ### ----

ap = bind_rows(nogran_ap_m, nogran_ap_m_sexe, nogran_ap_m_edat, nogran_ap_m_edat_sexe, 
               nogran_ap, nogran_ap_sexe, nogran_ap_edat, nogran_ap_edat_sexe,
               nogran_ap_sub, nogran_ap_sub_sexe, nogran_ap_sub_edat, nogran_ap_sub_edat_sexe) %>% 
  arrange(codi_indicador_23, id_subtipologia, any, Granularitat, grup_edat, sexe) %>% 
  mutate(
    sexe = ifelse(is.na(sexe), "Total", sexe),
    grup_edat = ifelse(is.na(grup_edat), "Total", grup_edat),
    invers = ifelse(codi_indicador_23 %in% indicadors_ap_dir, 0,
                    ifelse(codi_indicador_23 %in% indicadors_ap_neu, 2, 1))) # afegir invers = 2 quan calgui (TIC, ATDOM, evolució visites)


catalunya_values <- ap %>%
  ungroup() %>% 
  filter(Granularitat == "Catalunya") %>%
  select(c(codi_indicador_23, id_subtipologia, any, grup_edat, sexe, resultat, n)) %>%
  distinct(codi_indicador_23, id_subtipologia, any, grup_edat, sexe, .keep_all = TRUE)  # Ensure there's only one row per id_indicator and year

ap_final = ap %>% 
  left_join(catalunya_values, by = c("codi_indicador_23", "id_subtipologia", "any", "grup_edat", "sexe")) %>% 
  mutate(tipus_centre = ifelse(Granularitat == "Centre (Unitat proveïdora)", "Equips d'atenció primària",
                               Granularitat
                               #NA
  )) %>% 
  rename(resultat = resultat.x, mitjana = resultat.y, codi_indicador = codi_indicador_23, n = n.x, n_mitjana = n.y)



######
# AH # ----
######


ah = read_parquet("AH_dataset_def.parquet")

indicadors_ah = c("AHAD00101", "AHAD00102", "AHAD00103", "AHAD00104", "AHAD00105")


indicadors_ah_dir = c("AHAD00101", "AHAD00102", "AHAD00201", "AHAD00504", "AHEE00101",
                      "AHEE00102", "AHEE00103", "AHEE00201", "AHEE00301", "AHEE00601", 
                      "AHEE00602", "AHEE00603", "AHEE00701", "AHEI00201", "AHEU00101", 
                      "AHEU00201", "AHEU00202", "AHEU00203", "AHEU00204", "AHEU00205", 
                      "AHEU00206", "AHEU00301", "AHSE00101")

#indicadors_ah_neu = c("AHEI00101")


#indicadors_ah_inv = c("AHEU00101", "AHEU00201", "AHEU00202", "AHEU00203", 
#                      "AHEU00204", "AHEU00205", "AHEU00206", "AHEU00301") # Es fan servir una vegada ja s'han modificat els codis per treure els desdoblaments


indicadors_ah_sexe = c("AHAD00101", "AHAD00102", "AHAD00103", "AHAD00104", "AHAD00105"#,
                       #"AHAD00201", "AHAD00504", "AHEE00101", "AHEE00102", "AHEE00103", 
                       #"AHEE00601", "AHEE00602", "AHEE00603", "AHEE00701", "AHEE00801"
                       )

indicadors_ah_edat = c("AHAD00101", "AHAD00102", "AHAD00103", "AHAD00104", "AHAD00105")

indicadors_ah_sub_gran_falsa = c("AHEE00401", "AHEE00501", "AHEE00502")

indicadors_ah_sub = c("AHEE00901", "AHEI00201", "AHSE00101", "AHSE00201", "AHSE00301")

indicadors_ah_md = c("AHAD00503", "AHAD00601")

indicadors_ah_md_sexe = c("AHAD00503")

indicadors_ah_m = c("AHDG00201")

indicadors_ah_m_sexe = c("AHDG00201")

indicadors_ah_gran_falsa = c("AHAD00301", 
                             "AHSE00501", "AHSE00701", "AHSE00702", "AHEI00101")


dimup = read_excel("DimUp_2024-10-28.xlsx") %>% 
  select(c(id_up:aga, desc_nivell_curta, H_centre)) 

#%>% 
#  filter(tipus_up == "ASSISTÈNCIA HOSPITALÀRIA - ATENCIÓ HOSPITALÀRIA" | tipus_up == "ESTR. RELAC. DERIVACIÓ PACIENTS - CONTRACTE PROGRAMA ICS"
#         |# tipus_up == "ASSISTÈNCIA EXTRAHOSPITALARIA - HEMODIALISI AMBULATÓRIA")

dimup_up = dimup %>% 
  select(c(id_up, up, H_centre, desc_nivell_curta)) %>% 
  unique()

dimup_aga = dimup %>% 
  select(c(id_aga, aga)) %>% 
  unique()

dimup_rs = dimup %>% 
  select(c(id_rs, rs)) %>% 
  unique()

dimup_rs2 = dimup %>% 
  select(c(id_rs2, rs2)) %>% 
  unique()


ah2 = ah %>% 
  filter(codi_indicador_23 %in% indicadors_ah) %>% 
  mutate(any = substr(data, 1, 4),
         codi_up = ifelse(codi_up == 435, 6046, codi_up)
  )





# SENSE GRANULARITAT ----


dimup_aga = dimup %>% 
  select(c(id_aga, aga)) %>% 
  unique()

dimup_rs = dimup %>% 
  select(c(id_rs, rs, id_aga)) %>% 
  unique()

dimup_rs2 = dimup %>% 
  select(c(id_rs2, rs2, id_aga)) %>% 
  unique()

dimup_rs_rs = dimup %>% 
  select(c(id_rs2, rs2)) %>% 
  unique()

ctlg_rs = read_excel("ctlg_rs.xlsx") %>% 
  mutate(tipus = ifelse(tipus == "simple", "rs",
                        ifelse(tipus == "ampliat", "rs2",
                               ifelse(tipus == "ampliat-2", "rs3",
                                      tipus))))

ctlg_rs_rs = ctlg_rs %>% 
  filter(tipus == "rs")

ctlg_rs_rs2 = ctlg_rs %>% 
  filter(tipus == "rs2")

ctlg_rs_curt = ctlg_rs %>% 
  select(c(codi_rs, rs)) %>% unique()


nogran_ah = ah2 %>% 
  filter(is.na(uta_c)) %>%
  filter(!(codi_indicador_23 %in% indicadors_ah_sub | codi_indicador_23 %in% indicadors_ah_gran_falsa | codi_indicador_23 %in% indicadors_ah_sub_gran_falsa)) %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_aga, by = c("codi_aga_a" = "id_aga")) %>% 
  #left_join(dimup_rs, by = c("codi_up" = "id_up", "up" = "up")) %>% Només agrupem per rs2, encara que algun indicador tingui RS
  left_join(dimup_rs2, by = c("codi_aga_a" = "id_aga")) %>% 
  left_join(ctlg_rs_rs2, by = c("id_rs2" = "codi_rs_custom")) %>%  # Canviem els codis d'rs2 pels reals
  select(-c(id_rs2, rs2, tipus)) %>% 
  filter(!is.na(up))


# Visió centre ----

ah_up = nogran_ah %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md | codi_indicador_23 %in% indicadors_ah_m)) %>% 
  group_by(codi_indicador_23, any, codi_up, H_centre, desc_nivell_curta) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = H_centre, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2),
                           ifelse(codi_indicador_23 == "AHSE00401", round((n/d)*1000, 2), round((n/d)*100, 2))),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient",
                         ifelse(codi_indicador_23 == "AHSE00401", "Taxa per 1000", "%")),
         visio = "c") %>% 
  filter(!is.na(`Centre/Territori`))


# Visió pacient ----

ah_aga = nogran_ah %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md | codi_indicador_23 %in% indicadors_ah_m)) %>% 
  group_by(codi_indicador_23, any, codi_aga_a, aga) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = codi_aga_a) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2),
                           ifelse(codi_indicador_23 == "AHSE00401", round((n/d)*1000, 2), round((n/d)*100, 2))),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient",
                         ifelse(codi_indicador_23 == "AHSE00401", "Taxa per 1000", "%"))) 

#%>% 
#  filter(!is.na(`Centre/Territori`))


ah_rs = nogran_ah %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md | codi_indicador_23 %in% indicadors_ah_m)) %>% 
  group_by(codi_indicador_23, any, codi_rs, rs) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2),
                           ifelse(codi_indicador_23 == "AHSE00401", round((n/d)*1000, 2), round((n/d)*100, 2))),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient",
                         ifelse(codi_indicador_23 == "AHSE00401", "Taxa per 1000", "%")))


ah_cat = nogran_ah %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md | codi_indicador_23 %in% indicadors_ah_m)) %>% 
  group_by(codi_indicador_23, any) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den, na.rm = T)) %>% 
  mutate(Granularitat = "Catalunya", `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2),
                           ifelse(codi_indicador_23 == "AHSE00401", round((n/d)*1000, 2), round((n/d)*100, 2))),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient",
                         ifelse(codi_indicador_23 == "AHSE00401", "Taxa per 1000", "%")))



nogran_ah = bind_rows(ah_up, ah_aga, ah_rs, ah_cat) %>% 
  arrange(codi_indicador_23, any, Granularitat) %>% 
  filter(!is.nan(resultat))


# (GRANULARITAT I SEXE) ----


# SENSE GRANULARITAT I SEXE ----

ah2 = ah %>% 
  filter(codi_indicador_23 %in% indicadors_ah_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe | codi_indicador_23 %in% indicadors_ah_md_sexe) %>% 
  mutate(any = substr(data, 1, 4),
         codi_up = ifelse(codi_up == 435, 6046, codi_up)) # Modifiquem el codi up de l'Hospital Vall d'Hebron pq sigui el correcte

nogran_ah_sexe = ah2 %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_aga, by = c("codi_aga_a" = "id_aga")) %>% 
  #left_join(dimup_rs, by = c("codi_up" = "id_up", "up" = "up")) %>% Només agrupem per rs2, encara que algun indicador tingui RS
  left_join(dimup_rs2, by = c("codi_aga_a" = "id_aga")) %>% 
  left_join(ctlg_rs_rs2, by = c("id_rs2" = "codi_rs_custom")) %>%  # Canviem els codis d'rs2 pels reals
  select(-c(id_rs2, rs2, tipus)) 

#%>% 
#  filter(!is.na(up))

# Visió centre ----

ah_up = nogran_ah_sexe %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_up, H_centre, desc_nivell_curta, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = H_centre, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%"),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total")),
         visio = "c") %>% 
  filter(!is.na(`Centre/Territori`))

# Visió pacient ----


ah_aga = nogran_ah_sexe %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_aga_a, aga, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = codi_aga_a) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%"),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(`Centre/Territori`))


ah_rs = nogran_ah_sexe %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_rs, rs, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%"),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(`Centre/Territori`))


ah_cat = nogran_ah_sexe %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya", `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%"),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(`Centre/Territori`))

nogran_ah_sexe = bind_rows(ah_up, ah_aga, ah_rs, ah_cat) %>% 
  arrange(codi_indicador_23, any, Granularitat, sexe) %>% 
  filter(!is.nan(resultat))




# SENSE GRANULARITAT I EDAT ----

ah2 = ah %>% 
  filter(codi_indicador_23 %in% indicadors_ah_edat) %>% 
  mutate(any = substr(data, 1, 4),
         codi_up = ifelse(codi_up == 435, 6046, codi_up)) # Modifiquem el codi up de l'Hospital Vall d'Hebron pq sigui el correcte

nogran_ah_edat = ah2 %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_aga, by = c("codi_aga_a" = "id_aga")) %>% 
  #left_join(dimup_rs, by = c("codi_up" = "id_up", "up" = "up")) %>% Només agrupem per rs2, encara que algun indicador tingui RS
  left_join(dimup_rs2, by = c("codi_aga_a" = "id_aga")) %>% 
  left_join(ctlg_rs_rs2, by = c("id_rs2" = "codi_rs_custom")) %>%  # Canviem els codis d'rs2 pels reals
  select(-c(id_rs2, rs2, tipus)) 

#%>% 
#  filter(!is.na(up))

# Visió centre ----

ah_up = nogran_ah_edat %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_up, H_centre, desc_nivell_curta, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = H_centre, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%"),
         visio = "c") %>% 
  filter(!is.na(`Centre/Territori`))

# Visió pacient ----


ah_aga = nogran_ah_edat %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_aga_a, aga, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = codi_aga_a) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%")) %>% 
  filter(!is.na(`Centre/Territori`))


ah_rs = nogran_ah_edat %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_rs, rs, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%")) %>% 
  filter(!is.na(`Centre/Territori`))


ah_cat = nogran_ah_edat %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, grup_edat) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya", `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%")) %>% 
  filter(!is.na(`Centre/Territori`))

nogran_ah_edat = bind_rows(ah_up, ah_aga, ah_rs, ah_cat) %>% 
  arrange(codi_indicador_23, any, Granularitat, grup_edat) %>% 
  filter(!is.nan(resultat))


# SENSE GRANULARITAT I EDAT I SEXE ----

ah2 = ah %>% 
  filter(codi_indicador_23 %in% indicadors_ah_edat & codi_indicador_23 %in% indicadors_ah_sexe) %>% 
  mutate(any = substr(data, 1, 4),
         codi_up = ifelse(codi_up == 435, 6046, codi_up)) # Modifiquem el codi up de l'Hospital Vall d'Hebron pq sigui el correcte

nogran_ah_edat_sexe = ah2 %>% 
  left_join(dimup_up, by = c("codi_up" = "id_up")) %>% 
  left_join(dimup_aga, by = c("codi_aga_a" = "id_aga")) %>% 
  #left_join(dimup_rs, by = c("codi_up" = "id_up", "up" = "up")) %>% Només agrupem per rs2, encara que algun indicador tingui RS
  left_join(dimup_rs2, by = c("codi_aga_a" = "id_aga")) %>% 
  left_join(ctlg_rs_rs2, by = c("id_rs2" = "codi_rs_custom")) %>%  # Canviem els codis d'rs2 pels reals
  select(-c(id_rs2, rs2, tipus)) 

#%>% 
#  filter(!is.na(up))

# Visió centre ----

ah_up = nogran_ah_edat_sexe %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_up, H_centre, desc_nivell_curta, grup_edat, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = H_centre, Codi = codi_up) %>% 
  mutate(Granularitat = "Centre (Unitat proveïdora)",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%"),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total")),
         visio = "c") %>% 
  filter(!is.na(`Centre/Territori`))

# Visió pacient ----


ah_aga = nogran_ah_edat_sexe %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_aga_a, aga, grup_edat, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = aga, Codi = codi_aga_a) %>% 
  mutate(Granularitat = "Àrea de Gestió Assistencial",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%"),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(`Centre/Territori`))


ah_rs = nogran_ah_edat_sexe %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, codi_rs, rs, grup_edat, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  rename(`Centre/Territori` = rs, Codi = codi_rs) %>% 
  mutate(Granularitat = "Regió Sanitària",
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%"),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(`Centre/Territori`))


ah_cat = nogran_ah_edat_sexe %>% 
  filter(!(codi_indicador_23 %in% indicadors_ah_md_sexe | codi_indicador_23 %in% indicadors_ah_m_sexe)) %>% 
  group_by(codi_indicador_23, any, grup_edat, codi_sexe) %>% 
  summarise(n = sum(num, na.rm = T), d = sum(den)) %>% 
  mutate(Granularitat = "Catalunya", `Centre/Territori` = "Catalunya", Codi = NA,
         resultat = ifelse(codi_indicador_23 == "AHEE00701", round(n/d, 2), round((n/d)*100, 2)),
         mesura = ifelse(codi_indicador_23 == "AHEE00701", "Òrgans per pacient", "%"),
         sexe = ifelse(codi_sexe == 0, "Home",
                       ifelse(codi_sexe == 1, "Dona", "Total"))) %>% 
  filter(!is.na(`Centre/Territori`))


nogran_ah_edat_sexe = bind_rows(ah_up, ah_aga, ah_rs, ah_cat) %>% 
  arrange(codi_indicador_23, any, Granularitat, grup_edat, sexe) %>% 
  filter(!is.nan(resultat))



### UNIM ### ----

ah = bind_rows(nogran_ah, nogran_ah_sexe, nogran_ah_edat, nogran_ah_edat_sexe) %>% 
  mutate(id_subtipologia = NA) %>% 
  arrange(codi_indicador_23, id_subtipologia, any, Granularitat, grup_edat, sexe) %>% 
  mutate(sexe = ifelse(is.na(sexe), "Total", sexe),
         grup_edat = ifelse(is.na(grup_edat), "Total", grup_edat),
         invers = ifelse(codi_indicador_23 %in% indicadors_ah_dir, 0, 1
                         #ifelse(codi_indicador_23 %in% indicadors_ah_neu, 2, 0
                         #)
         )) %>% 
  distinct() %>% 
  filter(!(Granularitat == "Centre (Unitat proveïdora)" & Codi == 3941), !(Granularitat == "Centre (Unitat proveïdora)" & Codi == 2012))



catalunya_values <- ah %>%
  ungroup() %>% 
  filter(Granularitat == "Catalunya") %>%
  select(c(codi_indicador_23, id_subtipologia, any, grup_edat, sexe, resultat, n)) %>%
  distinct(codi_indicador_23, id_subtipologia, any, grup_edat, sexe, .keep_all = TRUE)  # Ensure there's only one row per id_indicator, year and sexe

ah_final = ah %>% 
  left_join(catalunya_values, by = c("codi_indicador_23", "id_subtipologia", "any", "grup_edat", "sexe")) %>% 
  mutate(tipus_centre = ifelse(Granularitat == "Centre (Unitat proveïdora)", "Centres hospitalaris", 
                               Granularitat
                               #NA
  ),
  desc_nivell_curta = ifelse(is.na(desc_nivell_curta) & Granularitat == "Centre (Unitat proveïdora)", "Altres", desc_nivell_curta)) %>% 
  rename(resultat = resultat.x, mitjana = resultat.y, codi_indicador = codi_indicador_23, n = n.x, n_mitjana = n.y, nivell = desc_nivell_curta) 


########
# UNIM #
########




dades_comparador = bind_rows(ap_final, ah_final) %>% 
  rename(id_indicador = codi_indicador) %>% 
  select(-codi_sexe) %>% 
  mutate(any = as.numeric(any)) %>% 
  group_by(id_indicador, Codi, Granularitat, `Centre/Territori`, grup_edat, sexe, id_subtipologia) %>% 
  mutate(lag_resultat = lag(resultat, 1),
         trend_resultat = round(resultat/lag_resultat - 1, 2),
         trend_icona = case_when(
           trend_resultat >= 0.01 & !is.na(trend_resultat) & invers == 0 ~ "betterup",
           trend_resultat >= 0.01 & !is.na(trend_resultat) & invers == 1 ~ "worseup",
           trend_resultat >= 0.01 & !is.na(trend_resultat) & invers == 2 ~ "up",
           trend_resultat >= 0 & trend_resultat < 0.01 ~ "unchanged",
           trend_resultat <= -0.01 & !is.na(trend_resultat) & invers == 0 ~ "worsedown",
           trend_resultat <= -0.01 & !is.na(trend_resultat) & invers == 1 ~ "betterdown",
           trend_resultat <= -0.01 & !is.na(trend_resultat) & invers == 2 ~ "down",
           trend_resultat <= 0 & trend_resultat > 0.01 ~ "unchanged",
           is.na(trend_resultat) ~ "new",
           TRUE ~ "unchanged")
  )


dades_comparador = dades_comparador %>% 
  mutate(id_resum = ifelse(is.na(id_subtipologia), id_indicador, paste(id_indicador, id_subtipologia, sep = "")),
         resultat = ifelse(is.infinite(resultat), NA, resultat),
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


write_parquet(dades_comparador, "df3.parquet")
