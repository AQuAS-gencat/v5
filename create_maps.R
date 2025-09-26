library(sf)
library(readxl)
library(leaflet)
library(dplyr)

map_abs_layer <- sf::st_read("maps/abs.geojson") %>%
  sf::st_make_valid() %>%  # First make geometries valid
  #sf::st_buffer(0) %>%     # Fix self-intersections
  sf::st_simplify(dTolerance = 10, preserveTopology = TRUE) # Simplify while preserving topology
st_write(map_abs_layer, "maps/abs_simplificat.geojson", delete_dsn = TRUE)


map_abs_layer = sf::st_read("maps/abs_20250122.geojson") %>% 
  filter(codi != "998" & codi != "999")

dimup = read_excel("DimUp_2024-10-28.xlsx")

aga = dimup %>% 
  select(c(id_abs, id_aga, aga)) %>% unique()

rs = dimup %>% 
  select(c(id_abs, id_rs, rs)) %>% unique()

rs2 = dimup %>% 
  select(c(id_abs, id_rs2, rs2)) %>% unique()

map_aga_layer = map_abs_layer %>% 
  select(c(codi, nom, geometry)) %>% 
  mutate(codi = as.numeric(codi)) %>% 
  left_join(aga, by = c("codi" = "id_abs")) %>% 
  mutate(geometry = st_make_valid(geometry)) %>%  # Fix invalid geometries
  group_by(id_aga, aga) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  dplyr::rename(codi_geo = id_aga) %>% 
  mutate(geometry = st_cast(geometry, "MULTIPOLYGON")) %>%   # Convert GEOMETRYCOLLECTION to MULTIPOLYGON
  sf::st_make_valid() %>%
  sf::st_simplify(dTolerance = 10, preserveTopology = TRUE)

#leaflet(map_abs_layer) %>%
#  addTiles() %>%  # Add base map
#  addPolygons(
#    fillColor = "blue",  # Temporary color to visualize polygons
#    color = "white",     # Border color
#    weight = 1,
#    fillOpacity = 0.5,
#    label = ~nom  # Show 'aga' name on hover
#  )

#leaflet(map_aga_layer) %>%
#  addTiles() %>%  # Add base map
#  addPolygons(
#    fillColor = "blue",  # Temporary color to visualize polygons
#    color = "white",     # Border color
#    weight = 1,
#    fillOpacity = 0.5,
#    label = ~aga  # Show 'aga' name on hover
#  )

st_write(map_aga_layer, "maps/aga.geojson", delete_dsn = TRUE)


#ctlg_rs = read_excel("ctlg_rs.xlsx") %>% 
#  mutate(tipus = ifelse(tipus == "simple", "rs",
#                        ifelse(tipus == "ampliat", "rs2",
#                               ifelse(tipus == "ampliat-2", "rs3",
#                                      tipus))))



#ctlg_rs_rs2 = ctlg_rs %>% 
#  filter(tipus == "rs2")

#ctlg_rs_curt = ctlg_rs %>% 
#  select(c(codi_rs, rs)) %>% unique()

map_rs2_layer = map_abs_layer %>% 
  select(c(codi, nom, geometry)) %>% 
  mutate(codi = as.numeric(codi)) %>% 
  left_join(rs2, by = c("codi" = "id_abs")) %>% 
  #left_join(ctlg_rs_rs2, by = c("id_rs2" = "codi_rs_custom")) %>% 
  mutate(geometry = st_make_valid(geometry)) %>%  # Fix invalid geometries
  group_by(id_rs2, rs2) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  dplyr::rename(codi_geo = id_rs2) %>%   # Convert GEOMETRYCOLLECTION to MULTIPOLYGON
  sf::st_make_valid() %>%
  sf::st_simplify(dTolerance = 10, preserveTopology = TRUE)



#leaflet(map_rs2_layer) %>%
#  addTiles() %>%  # Add base map
#  addPolygons(
#    fillColor = "blue",  # Temporary color to visualize polygons
#    color = "white",     # Border color
#    weight = 1,
#    fillOpacity = 0.5,
#    label = ~rs2  # Show 'rs2' name on hover
#  )

st_write(map_rs2_layer, "maps/rs2.geojson", delete_dsn = TRUE)


#ctlg_rs_rs = ctlg_rs %>% 
#  filter(tipus == "rs")

map_rs_layer = map_abs_layer %>% 
  select(c(codi, nom, geometry)) %>% 
  mutate(codi = as.numeric(codi)) %>% 
  left_join(rs, by = c("codi" = "id_abs")) %>% 
  #select(-rs) %>% 
  #left_join(ctlg_rs_rs, by = c("id_rs" = "codi_rs_custom")) %>% 
  mutate(geometry = st_make_valid(geometry)) %>%  # Fix invalid geometries
  group_by(id_rs, rs) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  dplyr::rename(codi_geo = id_rs) %>%   # Convert GEOMETRYCOLLECTION to MULTIPOLYGON
  sf::st_make_valid() %>%
  sf::st_simplify(dTolerance = 10, preserveTopology = TRUE)
  



# Creem mapa amb RS Penedès

penedes = c(406, 405, 398, 397, 394, 393 ,380 ,372, 371, 359, 357, 338, 337, 317, 316, 314, 269, 235, 229, 228, 218, 131, 013)

map_rs3_layer = map_abs_layer %>% 
  select(c(codi, nom, geometry)) %>% 
  mutate(codi = as.numeric(codi)) %>% 
  left_join(rs2, by = c("codi" = "id_abs")) %>% 
  dplyr::rename(rs3 = rs2, id_rs3 = id_rs2) %>% 
  mutate(rs3 = ifelse(codi %in% penedes, "PENEDÈS", rs3),
         codi = ifelse(rs3 == "PENEDÈS", 2100, id_rs3)) %>% 
  mutate(geometry = st_make_valid(geometry)) %>%  # Fix invalid geometries
  group_by(codi, rs3) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  dplyr::rename(codi_geo = codi) %>%   # Convert GEOMETRYCOLLECTION to MULTIPOLYGON %>% 
  #filter(!(rs3 == "PENEDÈS" & codi_geo != 2100)) %>% 
  sf::st_make_valid() %>%
  sf::st_simplify(dTolerance = 10, preserveTopology = TRUE)


st_write(map_rs3_layer, "maps/rs3.geojson", delete_dsn = TRUE)

#leaflet(map_rs_layer) %>%
#  addTiles() %>%  # Add base map
#  addPolygons(
#    fillColor = "blue",  # Temporary color to visualize polygons
#    color = "white",     # Border color
#    weight = 1,
#    fillOpacity = 0.5,
#    label = ~rs  # Show 'rs2' name on hover
#  )

st_write(map_rs_layer, "maps/rs.geojson", delete_dsn = TRUE)