#-------------------------------------------------------#
# ODS Cadenas de Valor ----
# Ultima fecha de modificacion: 02 agosto, 2023
# Este mapa incluye resultados de programas ODS Cadenas de Valor por pais
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

mes <- "Junio"
year <- 2023

country_text <- "22px"
opacity <- 0.8

#-------------------------------------------------------#
# 1. Mapas ----
#-------------------------------------------------------#

# Mapa UN filtrado para America
mapa_un_all <- st_read(glue("{datos_ori}/Mapas/UN_Geodata_simplified/BNDA_simplified.shp")) %>%
  janitor::clean_names() %>%
  dplyr::select(georeg, nam_en, lbl_en) %>%
  dplyr::filter(georeg == "AME")

# Abrir mapa UN filtrado para ALC
mapa_un <- st_read(glue("{datos}/Mapas/mapa_un.shp")) %>% dplyr::select(country)
plot(mapa_un$geometry)

#-------------------------------------------------------#
# 2. Abrir datos ----
#-------------------------------------------------------#

# Datos pais
data_res <- readRDS(glue("{datos}/datos_pais_{str_to_lower(mes)}_{year}.rds")) %>% 
  mutate(country = str_to_upper(country))

# Datos pais-programa
data_all <- readRDS(glue("{datos}/datos_pais_empresa_{str_to_lower(mes)}_{year}.rds")) %>% 
  mutate(country = str_to_upper(country))

# Tenemos información para 7 de 53 países en ALC
count(data_res, country)

# Emparejamos con mapa
meta_map <- mapa_un %>% left_join(data_all, by = "country") %>% drop_na(num_empresas)
meta_map_con <- mapa_un %>% left_join(data_res, by = "country") %>% drop_na(num_empresas)

# Reproyectar
mapa_un_all <- st_transform(mapa_un_all, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
mapa_un_all <- st_set_crs(mapa_un_all, 4326)
meta_map <- st_transform(meta_map, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
meta_map <- st_set_crs(meta_map, 4326)
meta_map_con <- st_transform(meta_map_con, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
meta_map_con <- st_set_crs(meta_map_con, 4326)
plot(meta_map$geometry)

# Etiquetas de pais
meta_map <- st_make_valid(meta_map)
meta_map <- cbind(meta_map, st_coordinates(st_centroid(meta_map)))

meta_map_con <- st_make_valid(meta_map_con)
meta_map_con <- cbind(meta_map_con, st_coordinates(st_centroid(meta_map_con)))

#-------------------------------------------------------#
# 3. Mapa interactivo ----
#-------------------------------------------------------#

#--------------------------#
# A. Colores y adiciones ----
#--------------------------#

# Colores sencillos: mismo color todos los paises con informacion
col_palette <- c("white", "#27BFE6", "#FBC412", "#A31C44", "#59BA47", "#F1692D", "#E5233D","#72ACAC", "#D4D4D4")
pnud <- "#004591"
  
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
  tag.map.title, HTML("Programa ODS Cadenas de Valor")
)

# Marcador de ubicacion del pais
awesome <- makeAwesomeIcon(
  icon = "circle-thin",
  iconColor = "darkblue",
  markerColor = "darkblue",
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
mapa_base <- meta_map %>% 
  distinct(country, X, Y, .keep_all = T)

label_pais <- paste0(mapa_base$country)

# Etiqueta del pais y estadisticas a nivel pais
data_labels <- lapply(1:nrow(meta_map_con), function(x){
  
  labels <- sprintf(
    "<strong>%s</strong><br/> <font size='-1'>Empresas participantes: %s <br/>Personas participantes: %s </font>",
    meta_map_con$country[x], meta_map_con$num_empresas[x], meta_map_con$num_total_personas[x]) %>%
    lapply(htmltools::HTML) 
  
  df_lab <- data.frame(country = meta_map_con$country[x], country_tag = labels[[1]])
  
  return(df_lab)
  
}) %>% bind_rows()

meta_map <- left_join(meta_map, data_labels, by = 'country')

# Pop-up con estadisticas de empresas por pais
meta_map <- meta_map %>%
  mutate(tag = glue("<strong> {metodologia} </strong>"),
         tag = glue("{tag} <br> <font size='-1'> Número de empresas: {num_empresas}"),
         tag = glue("{tag} <br> Hombres participantes: {num_hombres}"),
         tag = glue("{tag} <br> Mujeres participantes: {num_mujeres} </font>"))

#--------------------------#
# C. Mapa UN ----
#--------------------------#

map_ods <- 
  
  leaflet(mapa_base, height = "98vh", width = "190vh") %>%
  
  # Capa LAC
  addPolygons(data = mapa_un_all, color = "white", weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = "white") %>%
  
  # Mapa base y marcador de pais
  addAwesomeMarkers(icon = awesome, lng = ~X, lat = ~Y) %>%
  
  # Centrar mapa en La Paz
  clearBounds() %>%
  setView(lng = -68.0986, lat = -16.4907, zoom = 3) %>%
  addPolygons(data = meta_map, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = col_palette[2],
              highlightOptions = highlightOptions(color = col_palette[2], weight = 2, bringToFront = TRUE),
              label = ~meta_map$country_tag,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  # Title
  addControl(title, position = "topright", className="map-title")


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
  map_ods
  ))
)

mapa_final
save_html(mapa_final, glue("{graficas}/mapa_ods.html"))
