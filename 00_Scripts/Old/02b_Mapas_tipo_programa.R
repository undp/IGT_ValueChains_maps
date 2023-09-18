#-------------------------------------------------------#
# Rastreador de crecimiento inclusivo ----
# Ultima fecha de modificacion: 1 junio, 2023
# Mapa interactivo por tipo de programa (Jovenes en Accion, SARE, PROEMPLEO)
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf, leaflet, rgdal, htmltools, leafem, leaflegend)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 0. Mapas ----
#-------------------------------------------------------#

# Paises ALC
# https://www.fao.org/3/v8300s/v8300s0o.htm
no_alc <- c("Canadá", "Groenlandia (Dinamarca)", 'San Pedro y Miquelón (Francia)', 
            'Islas Georgias del Sur y Sandwich del Sur (RU)', 'Estados Unidos')

# Abrimos mapa de america y filtramos para ALC
america <- st_read(glue("{datos_ori}/Mapas/mapa_america/América.shp")) %>%
  janitor::clean_names() %>%
  dplyr::filter(!(pais %in% no_alc)) %>% 
  rename(country = pais)

#-------------------------------------------------------#
# 1. Abrir datos ----
#-------------------------------------------------------#

# Datos con resultados de evaluaciones de impacto
meta <- readRDS(glue('{datos}/Matriz/base_pais_tipo_programa.rds')) %>%
  pivot_wider(names_from = "value_outcome", values_from = "n") %>%
  replace(is.na(.), 0) %>%
  mutate(type_outcome = gsub("out_", "", type_outcome)) %>%
  arrange(country, name_of_the_program, type_outcome)

# Corregir nombres paises
meta$country[meta$country == "Brazil"] <- "Brasil"
meta$country[meta$country == "Dominican Republic"] <- "República Dominicana"
meta$country[meta$country == "Mexico"] <- "México"
meta$country[meta$country == "Panama"] <- "Panamá"
meta$country[meta$country == "Peru"] <- "Perú"

# Emparejamos con mapa
meta_map <- america %>% left_join(meta, by = "country") %>% drop_na(type_outcome)

# Etiquetas de pais
meta_map <- st_make_valid(meta_map)
meta_map <- cbind(meta_map, st_coordinates(st_centroid(meta_map)))

#-------------------------------------------------------#
# 2. Mapa interactivo ----
#-------------------------------------------------------#

#--------------------------#
# A. Colores y adiciones ----
#--------------------------#

# Colores sencillos: mismo color todos los paises con informacion
col_palette <- c("white", "#004591", "#27BFE6", "#FBC412", "#A31C44", "#59BA47", "#F1692D", "#E5233D","#72ACAC", "#D4D4D4")


# Titulo para mapa
tag.map.title <- htmltools::tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 20px;
  }
"))

title <- htmltools::tags$div(
  tag.map.title, HTML("Compendio de Evaluaciones de Impacto de políticas de Mercado Laboral Activo, Formalidad e Informalidad en ALC")
)

# Pop ups de outcomes-programas
# pop.up.name <- tags$style(
#     ".leaflet-popup-content-wrapper {
#     background: black;
#     color: #ffffff;
#     padding: 2px;
#     border-radius: 0px;
#     }
# ")


# Marcador de ubicacion del pais
awesome <- makeAwesomeIcon(
  icon = "circle-thin",
  iconColor = "white",
  markerColor = "lightblue",
  library = "fa"
)

file_text <- readr::read_file(
  paste0(.libPaths()[1], 
         "/leaflet/htmlwidgets/plugins/Leaflet.awesome-markers/font-awesome.min.css")
)

icon_names <- stringr::str_extract_all(file_text, "(fa-)([^:]+)")[[1]]

#--------------------------#
# B. Organizar variables ----
#--------------------------#

# Mapa base
mapa_base <- meta_map %>% distinct(country, X, Y, .keep_all = T)
label_pais <- paste0(mapa_base$country)

# Empleo
mapa_empleo <- meta_map[meta_map$type_outcome == "employment",] %>%
  arrange(country, -positive) %>%
  mutate(tag = ifelse(positive > 0, glue("{name_of_the_program}: <strong> ↑ </strong>"),
                      ifelse(negative > 0, glue("{name_of_the_program}: <strong> ↓ </strong>"), 
                             glue("{name_of_the_program}: —")))) %>%
  group_by(country, type_outcome) %>%
  mutate(tag = str_c(tag, collapse = " <br>"),
         tag = glue("<strong> Efectos sobre el empleo </strong> <br> {tag}")) %>%
  ungroup() %>%
  distinct(country, type_outcome, tag, X, Y, geometry) 

# Empleo formal
mapa_empleo_f <- meta_map[meta_map$type_outcome == "formal_em",] %>%
  arrange(country, -positive) %>%
  mutate(tag = ifelse(positive > 0, glue("{name_of_the_program}: <strong> ↑ </strong>"),
                      ifelse(negative > 0, glue("{name_of_the_program}: <strong> ↓ </strong>"), 
                             glue("{name_of_the_program}: —")))) %>%
  group_by(country, type_outcome) %>%
  mutate(tag = str_c(tag, collapse = " <br>"),
         tag = glue("<strong> Efectos sobre el empleo formal </strong> <br> {tag}")) %>%
  ungroup() %>%
  distinct(country, type_outcome, tag, X, Y, geometry)

# Empleo informal
mapa_empleo_i <- meta_map[meta_map$type_outcome == "informal_em",] %>%
  arrange(country, -positive) %>%
  mutate(tag = ifelse(positive > 0, glue("{name_of_the_program}: <strong> ↑ </strong>"),
                      ifelse(negative > 0, glue("{name_of_the_program}: <strong> ↓ </strong>"), 
                             glue("{name_of_the_program}: —")))) %>%
  group_by(country, type_outcome) %>%
  mutate(tag = str_c(tag, collapse = " <br>"),
         tag = glue("<strong> Efectos sobre el empleo informal </strong> <br> {tag}")) %>%
  ungroup() %>%
  distinct(country, type_outcome, tag, X, Y, geometry)

# Ingresos
mapa_ing <- meta_map[meta_map$type_outcome == "earnings",] %>%
  arrange(country, -positive) %>%
  mutate(tag = ifelse(positive > 0, glue("{name_of_the_program}: <strong> ↑ </strong>"),
                      ifelse(negative > 0, glue("{name_of_the_program}: <strong> ↓ </strong>"), 
                             glue("{name_of_the_program}: —")))) %>%
  group_by(country, type_outcome) %>%
  mutate(tag = str_c(tag, collapse = " <br>"),
         tag = glue("<strong> Efectos sobre el ingreso </strong> <br> {tag}")) %>%
  ungroup() %>%
  distinct(country, type_outcome, tag, X, Y, geometry) 

# Ingreso formal
mapa_ing_f <- meta_map[meta_map$type_outcome == "formal_ear",] %>%
  arrange(country, -positive) %>%
  mutate(tag = ifelse(positive > 0, glue("{name_of_the_program}: <strong> ↑ </strong>"),
                      ifelse(negative > 0, glue("{name_of_the_program}: <strong> ↓ </strong>"), 
                             glue("{name_of_the_program}: —")))) %>%
  group_by(country, type_outcome) %>%
  mutate(tag = str_c(tag, collapse = " <br>"),
         tag = glue("<strong> Efectos sobre el ingreso formal </strong> <br> {tag}")) %>%
  ungroup() %>%
  distinct(country, type_outcome, tag, X, Y, geometry) 

# Ingreso informal
mapa_ing_i <- meta_map[meta_map$type_outcome == "informal_ear",] %>%
  arrange(country, -positive) %>%
  mutate(tag = ifelse(positive > 0, glue("{name_of_the_program}: <strong> ↑ </strong>"),
                      ifelse(negative > 0, glue("{name_of_the_program}: <strong> ↓ </strong>"), 
                             glue("{name_of_the_program}: —")))) %>%
  group_by(country, type_outcome) %>%
  mutate(tag = str_c(tag, collapse = " <br>"),
         tag = glue("<strong> Efectos sobre el ingreso informal </strong> <br> {tag}")) %>%
  ungroup() %>%
  distinct(country, type_outcome, tag, X, Y, geometry) 

# Horas trabajadas
mapa_horas <- meta_map[meta_map$type_outcome == "hours",] %>%
  arrange(country, -positive) %>%
  mutate(tag = ifelse(positive > 0, glue("{name_of_the_program}: <strong> ↑ </strong>"),
                      ifelse(negative > 0, glue("{name_of_the_program}: <strong> ↓ </strong>"), 
                             glue("{name_of_the_program}: —")))) %>%
  group_by(country, type_outcome) %>%
  mutate(tag = str_c(tag, collapse = " <br>"),
         tag = glue("<strong> Efectos sobre las horas trabajadas </strong> <br> {tag}")) %>%
  ungroup() %>%
  distinct(country, type_outcome, tag, X, Y, geometry) 

#--------------------------#
# C. Mapa ----
#--------------------------#

# URL hyperlink to program web page
# popup = paste0(
#   "<b>Country: </b>"
#   , world.borders$NAME
#   , "<br>"
#   , "<a href='"
#   , world.borders$wiki
#   , "' target='_blank'>"
#   , "Click Here to View Wiki</a>"
# )

country_text <- "22px"

america_map <-
  leaflet(mapa_base, height = "98vh", width = "190vh") %>%
    
  # Capa open street map (continente)
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Mapa base y marcador de pais
  addAwesomeMarkers(icon = awesome, lng = ~X, lat = ~Y) %>%
  
  # Centrar mapa en Colombia 
  clearBounds() %>%
  setView(lng = -73.06911, lat = 3.890399, zoom = 5) %>%
  
  # Empleo
  # addPolygons(label = ~mapa_empleo$country, fillOpacity = 0, group = 'Empleo', opacity = 0) %>%
  addPolygons(data = mapa_empleo, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5, fillColor = col_palette[2],
              highlightOptions = highlightOptions(color = col_palette[2], weight = 2, bringToFront = TRUE),
              label = ~mapa_empleo$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"), 
              group = "Empleo",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Empleo formal
  addPolygons(data = mapa_empleo_f, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5, fillColor = col_palette[3],
              highlightOptions = highlightOptions(color = col_palette[3], weight = 2, bringToFront = TRUE),
              label = ~mapa_empleo_f$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),  
              group = "Empleo formal",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Empleo informal
  addPolygons(data = mapa_empleo_i, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5, fillColor = col_palette[4],
              highlightOptions = highlightOptions(color = col_palette[4], weight = 2, bringToFront = TRUE),
              label = ~mapa_empleo_i$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),
              group = "Empleo informal",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Ingreso
  addPolygons(data = mapa_ing, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5, fillColor = col_palette[5],
              highlightOptions = highlightOptions(color = col_palette[5], weight = 2, bringToFront = TRUE),
              label = ~mapa_ing$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),
              group = "Ingreso",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Ingreso formal
  addPolygons(data = mapa_ing_f, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5, fillColor = col_palette[6],
              highlightOptions = highlightOptions(color = col_palette[6], weight = 2, bringToFront = TRUE),
              label = ~mapa_ing_f$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),
              group = "Ingreso formal",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Ingreso informal
  addPolygons(data = mapa_ing_i, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5, fillColor = col_palette[7],
              highlightOptions = highlightOptions(color = col_palette[7], weight = 2, bringToFront = TRUE),
              label = ~mapa_ing_i$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),  
              group = "Ingreso informal",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Horas trabajadas
  addPolygons(data = mapa_horas, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5, fillColor = col_palette[8],
              highlightOptions = highlightOptions(color = col_palette[8], weight = 2, bringToFront = TRUE),
              label = ~mapa_horas$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),
              group = "Horas trabajadas",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Layers control
  addLayersControl(
    overlayGroups  = c("Empleo", "Empleo formal", "Empleo informal", "Ingreso", "Ingreso formal", "Ingreso informal", 'Horas trabajadas'),
    options = layersControlOptions(collapsed = F),
    position = "bottomright") %>%
  # Solo mostrar la primera capa
  hideGroup(c("Empleo formal", "Empleo informal", "Ingreso", "Ingreso formal", "Ingreso informal", 'Horas trabajadas')) %>%
  
  # Title
  addControl(title, position = "topright", className="map-title") 
# %>%
#   
#   # Logo PNUD
#   addLogo(src = "local",
#           img = glue("{datos}/logo_pnud_oit.png"),
#           position = "bottomleft",
#           offset.x = 5,
#           offset.y = 40,
#           width = 120,
#           height = 80)

# Modificar tamano de pop-ups y capas
mapa_final <- browsable(
  tagList(list(tags$head(tags$style(
    # Pop-ups
  ".leaflet-popup-content-wrapper {
    background: white;
    color: black;
    padding: 2px;
    border-radius: 0px;
    font-size: 20px;
    }",
  # Control de capas
  ".leaflet-control-layers-expanded {
                 line-height: 30px;
                 font-size: 20px;
                 }",
  ".leaflet-control-layers-list {
                width: 230px;
                height: 210px;
                 }"
  
  )
  ),
  america_map
  ))
)

# Exportar
saveRDS(mapa_final, glue("{graficas}/mapa_almp_programa.rds"))
save_html(mapa_final, glue("{graficas}/mapa_almp_programa.html"))
# htmlwidgets::saveWidget(america_map, glue("{graficas}/mapa_almp_programa.html"))

# Abrir mapa
america_map <- readRDS(glue("{graficas}/mapa_almp_programa.rds"))
america_map




