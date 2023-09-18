#-------------------------------------------------------#
# Rastreador de crecimiento inclusivo ----
# Ultima fecha de modificacion: 18 sept, 2023
# Mapa interactivo por tipo de programa (Jovenes en Accion, SARE, PROEMPLEO)
# Este mapa incluye resultados de programas de jovenes y mujeres dentro de la categoria "general"
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
# 0. Funciones ----
#-------------------------------------------------------#

fun_outcome <- function(y, y_tag){
  
  # y = "employment"
  # y_tag = 'el empleo'
  
  print(y)
  
  # Organizar etiquetas y base de datos para mapa
  data <- meta_map[meta_map$type_outcome == y,] %>%
    # Dividir las URL de los programas para aquellos con multiples evaluaciones
    # Crea tantas columnas como sea necesario
    tidyr::separate_wider_delim(url, delim = '; ', names_sep = "", too_few = "align_start") %>%
    arrange(country, -positive) %>%
    rename(url = url1)  %>%
    # Nombre del programa y link al programa
    mutate(tag = ifelse(url_prog_d == 1, glue("<a href='{url_prog}' target='_blank'> {name_of_the_program}</a>"), name_of_the_program),
           tag = ifelse(positive > 0, glue("{tag}: <strong> ↑ </strong>"),
                        ifelse(negative > 0, glue("{tag}: <strong> ↓ </strong>"), glue("{tag}: <strong> —  </strong>")))) %>%
    # Abstract
    mutate(tag = glue("{tag} <br> <font size='-1'> {abstract} </font>")) 
  
  if(!("url2" %in% colnames(data))){
    data <- data %>% mutate(url2 = NA)
  }
  
  if(!("url3" %in% colnames(data))){
    data <- data %>% mutate(url3 = NA)
  }
  
  # Organizar URL para programas con 1 evaluacion
  df1 <- data %>%
    dplyr::filter(is.na(url2)) %>%
    mutate(tag = glue("{tag}<font size='-1'> Ver evaluaciones <a href='{url}' target='_blank'> [1]</a> </font>"))
  
  # Organizar URL para programas con >1 evaluaciones
  if(sum(!is.na(data$url2)) > 0){

    # Para 2 evaluaciones
    df2 <- data %>%
      dplyr::filter(!is.na(url2) & is.na(url3)) %>%
      mutate(tag = glue("{tag}<font size='-1'> Ver evaluaciones <a href='{url}' target='_blank'> [1]</a> <a href='{url2}' target='_blank'> [2]</a> </font>"))
    
    # Para 3 evaluaciones
    df3 <- data %>%
      dplyr::filter(!is.na(url3)) %>%
      mutate(tag = glue("{tag}<font size='-1'> Ver evaluaciones <a href='{url}' target='_blank'> [1]</a> <a href='{url2}' target='_blank'> [2]</a> <a href='{url3}' target='_blank'> [3]</a> </font>"))
    
    if(nrow(df3) > 0){
      df1 <- bind_rows(df1, df2, df3)
    } else {
      df1 <- bind_rows(df1, df2)
    }
  
  }
  
  # Organizar base final
  data <- df1 %>%
    group_by(country, type_outcome) %>%
    mutate(
      tag = str_c(tag, collapse = " <br>"),
      tag = glue("<strong> Efectos sobre {y_tag} </strong> <br> {tag}")) %>%
    ungroup() %>%
    distinct(country, type_outcome, tag, X, Y)
  
  # El procesamiento elimina la clase "sf" necesaria para el mapa, unimos de nuevo con mapa UN
  data <- mapa_base %>% dplyr::select(country, geometry) %>% left_join(data, by = "country") %>% drop_na(type_outcome)
  
  return(data)
  
}

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

# Datos con resultados de evaluaciones de impacto
meta <- readRDS(glue('{datos}/Matriz/base_pais_tipo_programa_demografia.rds')) %>%
  pivot_wider(names_from = "value_outcome", values_from = "n") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  mutate(type_outcome = gsub("out_", "", type_outcome)) %>%
  arrange(country, name_of_the_program, type_outcome)
  
# Corregir nombres paises
meta$country[meta$country == "Brazil"] <- "Brasil"
meta$country[meta$country == "Dominican Republic"] <- "República Dominicana"
meta$country[meta$country == "Mexico"] <- "México"
meta$country[meta$country == "Panama"] <- "Panamá"
meta$country[meta$country == "Peru"] <- "Perú"

meta <- meta %>% mutate(country = str_to_upper(country))

# Tenemos información para 13 de 53 países en ALC
count(meta, country)

# Emparejamos con mapa
meta_map <- mapa_un %>% left_join(meta, by = "country") %>% drop_na(type_outcome)

# Reproyectar
mapa_un_all <- st_transform(mapa_un_all, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
mapa_un_all <- st_set_crs(mapa_un_all, 4326)
meta_map <- st_transform(meta_map, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
meta_map <- st_set_crs(meta_map, 4326)
plot(meta_map$geometry)

# Etiquetas de pais
meta_map <- st_make_valid(meta_map)
meta_map <- cbind(meta_map, st_coordinates(st_centroid(meta_map)))

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
  tag.map.title, HTML("Compendio de Evaluaciones de Impacto de políticas de Mercado Laboral Activo, Formalidad e Informalidad en ALC")
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

# Empleo
mapa_empleo <- fun_outcome(y = "employment", y_tag = 'el empleo')

# Empleo formal
mapa_empleo_f <- fun_outcome(y = "formal_em", y_tag = 'el empleo formal')

# Empleo informal
mapa_empleo_i <- fun_outcome(y = "informal_em", y_tag = 'el empleo informal')

# Ingresos
mapa_ing <- fun_outcome(y = "earnings", y_tag = 'el ingreso')

# Ingresos formales
mapa_ing_f <- fun_outcome(y = "formal_ear", y_tag = 'el ingreso formal')

# Ingresos informales
mapa_ing_i <- fun_outcome(y = "informal_ear", y_tag = 'el ingreso informal')

# Horas trabajadas
mapa_horas <- fun_outcome(y = "hours", y_tag = 'las horas trabajadas')

#--------------------------#
# C. Mapa UN ----
#--------------------------#

country_text <- "22px"
opacity <- 0.8

#--------------------------#
# B. Mapa ----
#--------------------------#

# Mapa base: UN LAC
mapa_almp <-
  
  leaflet(mapa_base, height = "98vh", width = "190vh") %>%
  
  # Capa LAC
  addPolygons(data = mapa_un_all, color = "white", weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = "white") %>%
  
  # Mapa base y marcador de pais
  addAwesomeMarkers(icon = awesome, lng = ~X, lat = ~Y) %>%
  
  # Centrar mapa en La Paz
  clearBounds() %>%
  setView(lng = -68.0986, lat = -16.4907, zoom = 3) %>%
  
  # Empleo
  addPolygons(data = mapa_empleo, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = col_palette[2],
              highlightOptions = highlightOptions(color = col_palette[2], weight = 2, bringToFront = TRUE),
              label = ~mapa_empleo$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"), 
              group = "Empleo",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Empleo formal
  addPolygons(data = mapa_empleo_f, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = col_palette[3],
              highlightOptions = highlightOptions(color = col_palette[3], weight = 2, bringToFront = TRUE),
              label = ~mapa_empleo_f$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),  
              group = "Empleo formal",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Empleo informal
  addPolygons(data = mapa_empleo_i, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = col_palette[4],
              highlightOptions = highlightOptions(color = col_palette[4], weight = 2, bringToFront = TRUE),
              label = ~mapa_empleo_i$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),
              group = "Empleo informal",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Ingreso
  addPolygons(data = mapa_ing, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = col_palette[5],
              highlightOptions = highlightOptions(color = col_palette[5], weight = 2, bringToFront = TRUE),
              label = ~mapa_ing$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),
              group = "Ingreso",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Ingreso formal
  addPolygons(data = mapa_ing_f, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = col_palette[6],
              highlightOptions = highlightOptions(color = col_palette[6], weight = 2, bringToFront = TRUE),
              label = ~mapa_ing_f$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),
              group = "Ingreso formal",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Ingreso informal
  addPolygons(data = mapa_ing_i, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = col_palette[7],
              highlightOptions = highlightOptions(color = col_palette[7], weight = 2, bringToFront = TRUE),
              label = ~mapa_ing_i$country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"), textsize = country_text, direction = "auto"),  
              group = "Ingreso informal",
              popup = ~tag, popupOptions = popupOptions(maxWidth = 500)) %>%
  
  # Horas trabajadas
  addPolygons(data = mapa_horas, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = opacity, fillOpacity = opacity, fillColor = col_palette[8],
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
  mapa_almp
  ))
)

mapa_final

# Exportar
# saveRDS(mapa_final, glue("{graficas}/mapa_almp_un-ilo.rds"))
save_html(mapa_final, "index.html")

