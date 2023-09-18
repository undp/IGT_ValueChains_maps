#-------------------------------------------------------#
# Rastreador de crecimiento inclusivo ----
# Ultima fecha de modificacion: 19 mayo, 2023
# Mapa interactivo por tipo de politica (ALMP, Informalidad)
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf, leaflet, rgdal, htmltools, leafem)
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

# Sujeto a modificaciones (esto excluye a la mayoria del Caribe)
# no_alc <- c("Canadá", "Groenlandia (Dinamarca)", 'Guadalupe (Francia)', 
#             'Martinica (Francia)', 'Monserrat (RU)', 'San Pedro y Miquelón (Francia)',
#             'San Cristóbal y Nieves', 'Santa Lucia', 'Islas Georgias del Sur y Sandwich del Sur (RU)',
#             'Islas Vírgenes Británicas (RU)', 'Islas Vírgenes Estadounidenses (EEUU)',
#             'Estados Unidos', 'Curazao (Paises Bajos)', 'Bonaire (Paises Bajos)', 'Bermudas (RU)',
#             'Bahamas', 'Anguilla (RU)', 'Belice', 'Islas Caimán (RU)', 'Aruba (Paises Bajos)',
#             'Antigua y Barbuda', 'Granada', 'Surinam', 'San Vicente y las Granadinas',
#             'Guayana Francesa (Francia)', 'Islas Malvinas (RU)', 'Montserrat (RU)', 'Turcos y Caicos (RU)',
#             'Trinidad y Tobago', 'Barbados', 'Guyana', 'Dominica', 'Jamaica')

# Paises ALC
# https://www.fao.org/3/v8300s/v8300s0o.htm
no_alc <- c("Canadá", "Groenlandia (Dinamarca)", 'San Pedro y Miquelón (Francia)', 
            'Islas Georgias del Sur y Sandwich del Sur (RU)', 'Estados Unidos')

# Abrimos mapa de america y filtramos para ALC
america <- st_read(glue("{datos_ori}/Mapas/mapa_america/América.shp")) %>%
  janitor::clean_names() %>%
  dplyr::filter(!(pais %in% no_alc)) %>% 
  rename(country = pais)

# plot(america$geometry)
# table(america$country)

#-------------------------------------------------------#
# 1. Abrir datos ----
#-------------------------------------------------------#

# Datos con resultados de evaluaciones de impacto
meta <- readRDS(glue('{datos}/Matriz/base_pais_tipo_politica.rds')) %>%
  pivot_wider(names_from = "value_outcome", values_from = "n") %>%
  replace(is.na(.), 0) %>%
  mutate(type_outcome = gsub("out_", "", type_outcome))

# Corregir nombres paises
meta$country[meta$country == "Brazil"] <- "Brasil"
meta$country[meta$country == "Dominican Republic"] <- "República Dominicana"
meta$country[meta$country == "Mexico"] <- "México"
meta$country[meta$country == "Panama"] <- "Panamá"
meta$country[meta$country == "Peru"] <- "Perú"

# table(meta$country)

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
    font-size: 15px;
  }
"))

title <- htmltools::tags$div(
  tag.map.title, HTML("Resultados de evaluaciones de impacto de políticas de Mercado Laboral Activo e Informalidad en ALC")
)

# Marcador de ubicacion del pais
awesome <- makeAwesomeIcon(
  icon = "circle-thin",
  iconColor = "white",
  markerColor = "lightblue",
  library = "fa"
)

greenLeafIcon <- makeIcon(
  iconUrl = "https://www.pngkit.com/png/full/109-1093253_find-a-location-icon-png-white.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "https://www.pngkit.com/png/full/109-1093253_find-a-location-icon-png-white.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

file_text <- readr::read_file(
  paste0(.libPaths()[1], 
         "/leaflet/htmlwidgets/plugins/Leaflet.awesome-markers/font-awesome.min.css")
)

icon_names <- stringr::str_extract_all(file_text, "(fa-)([^:]+)")[[1]]

#--------------------------#
# B. Etiquetas ----
#--------------------------#

### Pop-up describe el efecto sobre cada variable

# Empleo
popup_empleo <- paste0("<b>", "Efectos sobre el empleo", "</b>", "<br>", 
                       "Positivos (+): ", "</b>", meta_map$positive[meta_map$type_outcome == "employment"], "<br>",
                       "Negativos (-): ", "</b>", meta_map$negative[meta_map$type_outcome == "employment"], "<br>",
                       "Sin efecto (ns): ", "</b>", meta_map$ns[meta_map$type_outcome == "employment"], "<br>")

# Empleo formal
popup_empleo_for <- paste0("<b>", "Efectos sobre el empleo formal", "</b>", "<br>", 
                       "Positivos (+): ", "</b>", meta_map$positive[meta_map$type_outcome == "formal_em"], "<br>",
                       "Negativos (-): ", "</b>", meta_map$negative[meta_map$type_outcome == "formal_em"], "<br>",
                       "Sin efecto (ns): ", "</b>", meta_map$ns[meta_map$type_outcome == "formal_em"], "<br>")

# Empleo informal
popup_empleo_inf <- paste0("<b>", "Efectos sobre el empleo informal", "</b>", "<br>", 
                       "Positivos (+): ", "</b>", meta_map$positive[meta_map$type_outcome == "informal_em"], "<br>",
                       "Negativos (-): ", "</b>", meta_map$negative[meta_map$type_outcome == "informal_em"], "<br>",
                       "Sin efecto (ns): ", "</b>", meta_map$ns[meta_map$type_outcome == "informal_em"], "<br>")

# Ingresos
popup_ingresos <- paste0("<b>", "Efectos sobre los ingresos", "</b>", "<br>",
                         "Positivos (+): ", "</b>", meta_map$positive[meta_map$type_outcome == "earnings"], "<br>",
                         "Negativos (-): ", "</b>", meta_map$negative[meta_map$type_outcome == "earnings"], "<br>",
                         "Sin efecto (ns): ", "</b>", meta_map$ns[meta_map$type_outcome == "earnings"], "<br>")

# Ingresos formales
popup_ingresos_for <- paste0("<b>", "Efectos sobre los ingresos formales", "</b>", "<br>",
                         "Positivos (+): ", "</b>", meta_map$positive[meta_map$type_outcome == "formal_ear"], "<br>",
                         "Negativos (-): ", "</b>", meta_map$negative[meta_map$type_outcome == "formal_ear"], "<br>",
                         "Sin efecto (ns): ", "</b>", meta_map$ns[meta_map$type_outcome == "formal_ear"], "<br>")

# Ingresos informales
popup_ingresos_inf <- paste0("<b>", "Efectos sobre los ingresos informales", "</b>", "<br>",
                         "Positivos (+): ", "</b>", meta_map$positive[meta_map$type_outcome == "informal_ear"], "<br>",
                         "Negativos (-): ", "</b>", meta_map$negative[meta_map$type_outcome == "formal_ear"], "<br>",
                         "Sin efecto (ns): ", "</b>", meta_map$ns[meta_map$type_outcome == "formal_ear"], "<br>")

# Horas trabajadas
popup_horas <- paste0("<b>", "Efectos sobre las horas trabajadas", "</b>", "<br>",
                      "Positivos (+): ", "</b>", meta_map$positive[meta_map$type_outcome == "hours"], "<br>",
                      "Negativos (-): ", "</b>", meta_map$negative[meta_map$type_outcome == "hours"], "<br>",
                      "Sin efecto (nulo): ", "</b>", meta_map$ns[meta_map$type_outcome == "hours"], "<br>")

#--------------------------#
# C. Mapa ----
#--------------------------#

# Mapa interactivo
america_map <- leaflet(meta_map) %>% 
  
  # Capa open street map (continente)
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Marcador de pais
  # addMarkers(lng = ~X, lat = ~Y, icon = greenLeafIcon) %>%
  addAwesomeMarkers(icon = awesome, lng = ~X, lat = ~Y)%>%
  
  # 1. Empleo
  # addLabelOnlyMarkers(lng = ~X, lat = ~Y, label = ~country, group = "Empleo",
  #                     labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addPolygons(data = meta_map, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 1, fillColor = col_palette[2],
              group = "Empleo", highlightOptions = highlightOptions(color = "white",
                                                                        weight = 2, bringToFront = TRUE),
              label = ~meta_map$country, labelOptions = labelOptions(direction = "auto"),
              popup = popup_empleo) %>%

  # 2. Empleo formal
  # addLabelOnlyMarkers(lng = ~X, lat = ~Y, label = ~country, group = "Empleo formal",
  #                     labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addPolygons(data = meta_map, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 1, fillColor = col_palette[3],
              group = "Empleo formal", highlightOptions = highlightOptions(color = "white",
                                                                    weight = 2, bringToFront = TRUE),
              label = ~meta_map$country, labelOptions = labelOptions(direction = "auto"),
              popup = popup_empleo_for) %>%

  # 3. Empleo informal
  # addLabelOnlyMarkers(lng = ~X, lat = ~Y, label = ~country, group = "Empleo informal",
  #                     labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addPolygons(data = meta_map, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 1, fillColor = col_palette[4],
              group = "Empleo informal", highlightOptions = highlightOptions(color = "white",
                                                                           weight = 2, bringToFront = TRUE),
              label = ~meta_map$country, labelOptions = labelOptions(direction = "auto"),
              popup = popup_empleo_inf) %>%

  # 4. Ingresos
  # addLabelOnlyMarkers(lng = ~X, lat = ~Y, label = ~country, group = "Ingresos",
  #                     labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addPolygons(data = meta_map, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 1, fillColor = col_palette[5],
              group = "Ingresos", highlightOptions = highlightOptions(color = "white",
                                                                    weight = 2, bringToFront = TRUE),
              label = ~meta_map$country, labelOptions = labelOptions(direction = "auto"),
              popup = popup_ingresos) %>%

  # 5. Ingresos formales
  # addLabelOnlyMarkers(lng = ~X, lat = ~Y, label = ~country, group = "Ingresos formales",
  #                     labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addPolygons(data = meta_map, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 1, fillColor = col_palette[6],
              group = "Ingresos formales", highlightOptions = highlightOptions(color = "white",
                                                                      weight = 2, bringToFront = TRUE),
              label = ~meta_map$country, labelOptions = labelOptions(direction = "auto"),
              popup = popup_ingresos_for) %>%

  # 6. Ingresos informales
  # addLabelOnlyMarkers(lng = ~X, lat = ~Y, label = ~country, group = "Ingresos informales",
  #                     labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addPolygons(data = meta_map, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 1, fillColor = col_palette[7],
              group = "Ingresos informales", highlightOptions = highlightOptions(color = "white",
                                                                               weight = 2, bringToFront = TRUE),
              label = ~meta_map$country, labelOptions = labelOptions(direction = "auto"),
              popup = popup_ingresos_inf) %>%

  # 7. Horas trabajadas
  # addLabelOnlyMarkers(lng = ~X, lat = ~Y, label = ~country, group = "Horas trabajadas",
  #                     labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addPolygons(data = meta_map, color = col_palette[1], weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 1, fillColor = col_palette[8],
              group = "Horas trabajadas", highlightOptions = highlightOptions(color = "white",
                                                                      weight = 2, bringToFront = TRUE),
              label = ~meta_map$country, labelOptions = labelOptions(direction = "auto"),
              popup = popup_horas) %>%

  # Layers control
  addLayersControl(
    overlayGroups  = c("Empleo", "Empleo formal", "Empleo informal",
                       "Ingresos", "Ingresos formales", "Ingresos informales","Horas trabajadas"),
    options = layersControlOptions(collapsed = F),
    position = "bottomright") %>%
  
  # Title
  addControl(title, position = "topright", className="map-title") %>%
  
  # Logo PNUD
  addLogo(src = "local",
          img = glue("{datos}/logo_pnud_oit.png"),
          position = "bottomleft",
          offset.x = 5,
          offset.y = 40,
          width = 120,
          height = 80)

america_map

# Exportar
saveRDS(america_map, glue("{graficas}/mapa_almp_general.rds"))
htmlwidgets::saveWidget(america_map, glue("{graficas}/mapa_almp_general.html"))

# Abrir mapa
america_map <- readRDS(glue("{graficas}/mapa_almp_general.rds"))
america_map





