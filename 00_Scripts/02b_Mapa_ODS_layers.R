#-------------------------------------------------------#
# ODS Cadenas de Valor ----
# Ultima fecha de modificacion: 26 sept, 2023
# Este mapa incluye resultados de programas ODS Cadenas de Valor por pais y programa
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf, leaflet, rgdal, htmltools, leafem, leaflegend, tableHTML, leaftime)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

mes <- "ago"
currentyear <- 2023
# mes <- "Junio"

# Text sizes and other options
country_text <- "22px"
link_text <- "18px"
opacity <- 1
text_width <- 80*0.8
met_width <- 130
logo_width <- 150*1.5
logo_size_h <- 150
logo_size_w <- 200
# logo_size_h <- 250*2.5
# logo_size_w <- 140*2.5
sector_width <- 250
part_width <- 150*1.5
num_width <- 80
popup_w <- 1200
popup_h <- 800

#-------------------------------------------------------#
# 0. Logos y URL ----
#-------------------------------------------------------#

# Disclaimer para mapa
disclaimer <- HTML("<p style='text-align: left;'>Las designaciones utilizadas y la presentación de material en este mapa no implican la expresión de ninguna opinión por parte de la Secretaría de las Naciones Unidas o el PNUD con respecto al estado legal de ningún país, territorio, ciudad o área, ni de sus autoridades, ni con respecto a la delimitación de sus fronteras o límites.</p>")

#--------------------------#
# A. Url programas ----
#--------------------------#

# URL programas 
# url_ods <- "https://www.undp.org/es/sdgvaluechains/nuestros-resultados"

url_ods <- readxl::read_excel(glue("{datos_ori}/links_paises.xlsx")) %>%
  mutate(country = str_to_upper(country))

#--------------------------#
# B. Banderas ----
#--------------------------#

url_undp <- "https://www.undp.org/sites/g/files/zskgke326/files/migration/jposc/UNDP-Logo-Blue-Large-Transparent.png"

# Albania
albania <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Albania.svg.png?raw=true"

# Colombia
colombia <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Colombia.svg.png?raw=true"

# Ecuador
ecuador <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Ecuador.svg.png?raw=true"

# Guyana
guyana <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Guyana.svg.png?raw=true"

# Mexico
mexico <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Mexico.svg.png?raw=true"

# Peru
peru <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Peru.svg.png?raw=true"

# Venezuela
venezuela <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Venezuela.svg.png?raw=true"

# Rep Dominicana
rep_dom <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_the_Dominican_Republic.svg.png?raw=true" 

# Botswana


# Angola


#--------------------------#
# C. Metodologias ----
#--------------------------#

# Iniciando con su negocio
icsn <- "https://github.com/undp/IGT_ValueChains_maps/blob/ed4a587cd7e0fb1063895c5aceb5f8304ce75d7e/01_Datos_originales/MATERIALODS_Regional/LOGOTIPOS%20METODOLOGIAS-01/LOGOTIPOS%20METODOLOGIAS-01.jpg?raw=true"

# Creciendo con su negocio
ccsn <- "https://github.com/undp/IGT_ValueChains_maps/blob/ed4a587cd7e0fb1063895c5aceb5f8304ce75d7e/01_Datos_originales/MATERIALODS_Regional/LOGOTIPOS%20METODOLOGIAS-01/LOGOTIPOS%20METODOLOGIAS-02.jpg?raw=true"

# Desarrollo de proveedores
mdp <- "https://github.com/undp/IGT_ValueChains_maps/blob/ed4a587cd7e0fb1063895c5aceb5f8304ce75d7e/01_Datos_originales/MATERIALODS_Regional/LOGOTIPOS%20METODOLOGIAS-01/LOGOTIPOS%20METODOLOGIAS-03.jpg?raw=true"

# En Marcha
enmar <- "https://github.com/undp/IGT_ValueChains_maps/blob/ed4a587cd7e0fb1063895c5aceb5f8304ce75d7e/01_Datos_originales/MATERIALODS_Regional/LOGOTIPOS%20METODOLOGIAS-01/LOGOTIPOS%20METODOLOGIAS-08.jpg?raw=true"

# En Marcha digital
enmar_dig <- "https://github.com/undp/IGT_ValueChains_maps/blob/ed4a587cd7e0fb1063895c5aceb5f8304ce75d7e/01_Datos_originales/MATERIALODS_Regional/LOGOTIPOS%20METODOLOGIAS-01/LOGOTIPOS%20METODOLOGIAS-09.jpg?raw=true" 

# Negocios inclusivos
neg <- "https://github.com/undp/IGT_ValueChains_maps/blob/ed4a587cd7e0fb1063895c5aceb5f8304ce75d7e/01_Datos_originales/MATERIALODS_Regional/LOGOTIPOS%20METODOLOGIAS-01/LOGOTIPOS%20METODOLOGIAS-06.jpg?raw=true"

# Adaptando su negocio
asn <- "https://github.com/undp/IGT_ValueChains_maps/blob/ed4a587cd7e0fb1063895c5aceb5f8304ce75d7e/01_Datos_originales/MATERIALODS_Regional/LOGOTIPOS%20METODOLOGIAS-01/LOGOTIPOS%20METODOLOGIAS-07.jpg"

#-------------------------------------------------------#
# 1. Organizar mapas ----
#-------------------------------------------------------#

# Mapa UN subregion sin antartica
mapa_cont <- st_read(glue("{datos_ori}/Mapas/UN_Geodata_simplified/SDGA_simplified.shp")) %>%
  janitor::clean_names()

# Mapa UN Mundial
# mapa_un_all <- st_read(glue("{datos_ori}/Mapas/map_un_world.shp")) %>%
mapa_un_all <- st_read(glue("{datos_ori}/Mapas/UN_Geodata_simplified/BNDA_simplified.shp")) %>%
  janitor::clean_names() %>%
  dplyr::select(georeg, nam_en, lbl_en) %>%
  # Eliminamos la antartica
  dplyr::filter(georeg != "ANT") %>%
  # dplyr::filter(lbl_en != "Greenland (Denmark)") %>%
  rename(country = lbl_en) %>%
  dplyr::select(country, georeg, geometry)

# Mapa UN filtrado para America y Europa
mapa_un <- st_read(glue("{datos_ori}/Mapas/UN_Geodata_simplified/BNDA_simplified.shp")) %>%
  janitor::clean_names() %>%
  dplyr::select(georeg, nam_en, lbl_en) %>%
  dplyr::filter(georeg == "AME" | georeg == "EUR") %>%
  rename(country = lbl_en) %>%
  dplyr::select(country, geometry)

# plot(mapa_un_all$geometry)
# plot(mapa_un$geometry, add = T, col = "red")
# table(mapa_un_all$georeg)

# Nombres de pais en espanol
mapa_un$country[mapa_un$country == 'MEXICO'] <- 'MÉXICO'
mapa_un$country[mapa_un$country == 'PERU'] <- 'PERÚ'
mapa_un$country[mapa_un$country == 'DOMINICAN REPUBLIC'] <- 'REPÚBLICA DOMINICANA'
mapa_un_all$country[mapa_un_all$country == 'MEXICO'] <- 'MÉXICO'
mapa_un_all$country[mapa_un_all$country == 'PERU'] <- 'PERÚ'
mapa_un_all$country[mapa_un_all$country == 'DOMINICAN REPUBLIC'] <- 'REPÚBLICA DOMINICANA'

#-------------------------------------------------------#
# 2. Organizar datos ----
#-------------------------------------------------------#

# Datos pais
data_res <- readRDS(glue("{datos}/datos_pais_{str_to_lower(mes)}_{currentyear}.rds")) %>% 
  mutate(
    country = str_to_upper(country)
    ) %>% dplyr::filter(country != "TOTAL")

# Datos pais-programa
data_all <- readRDS(glue("{datos}/datos_pais_empresa_{str_to_lower(mes)}_{currentyear}.rds")) %>% 
  mutate(
    country = str_to_upper(country)
  ) %>% dplyr::filter(country != "TOTAL")


# URL Logos bandera paises
banderas <- data.frame(country = unique(data_res$country), logo = NA)
banderas$logo[banderas$country == "ALBANIA"] <- albania
banderas$logo[banderas$country == "COLOMBIA"] <- colombia
banderas$logo[banderas$country == "ECUADOR"] <- ecuador
banderas$logo[banderas$country == "GUYANA"] <- guyana
banderas$logo[banderas$country == "MÉXICO"] <- mexico
banderas$logo[banderas$country == "PERÚ"] <- peru
banderas$logo[banderas$country == "REPÚBLICA DOMINICANA"] <- rep_dom
banderas$logo[banderas$country == "VENEZUELA"] <- venezuela

# Tenemos información para 8 paises (7 ALC, 1 Europa)
# count(data_res, country)

# Emparejamos con mapa
meta_map <- mapa_un %>% left_join(data_all, by = "country") %>% drop_na(num_empresas)
meta_map_con <- mapa_un %>% left_join(data_res, by = "country") %>% drop_na(num_empresas)

# Reproyectar (importante para poder usar leaflet)
mapa_un_all <- st_transform(mapa_un_all, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
mapa_un_all <- st_set_crs(mapa_un_all, 4326)
mapa_un <- st_transform(mapa_un, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
mapa_un <- st_set_crs(mapa_un, 4326)
meta_map <- st_transform(meta_map, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
meta_map <- st_set_crs(meta_map, 4326)
meta_map_con <- st_transform(meta_map_con, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
meta_map_con <- st_set_crs(meta_map_con, 4326)

# Etiquetas de pais
meta_map <- st_make_valid(meta_map)
meta_map <- cbind(meta_map, st_coordinates(st_centroid(meta_map)))

meta_map_con <- st_make_valid(meta_map_con)
meta_map_con <- cbind(meta_map_con, st_coordinates(st_centroid(meta_map_con)))

#-------------------------------------------------------#
# 3. Calculos mapa ----
#-------------------------------------------------------#

#--------------------------#
# A. Colores y adiciones ----
#--------------------------#

# Colores sencillos: mismo color todos los paises con informacion
col_palette <- c("white", "#26bde2","#A21942", "#B03B5E","#56C02B", "#4C9F38", "#FCC30B", "#FD6925", "#DD1367", "#00689D")
pnud <- "#0468B1"
  
# Mapa base
mapa_base <- meta_map %>% distinct(country, X, Y, .keep_all = T)

#--------------------------#
# B. Estadisticas nivel Pais ----
#--------------------------#

# Organizar estadisticas a nivel pais
data_pais <- data_res %>% 
  dplyr::filter(country != "TOTAL") %>%
  dplyr::select(-c(period)) %>%
  pivot_longer(cols = starts_with("num"), names_to = "tipo", values_to = "numero") 

data_pais$tipo[data_pais$tipo == "num_empresas"] <- "Empresas"
data_pais$tipo[data_pais$tipo == "num_hombres"] <- "Hombres"
data_pais$tipo[data_pais$tipo == "num_mujeres"] <- "Mujeres"
data_pais$tipo[data_pais$tipo == "num_total_personas"] <- "Total personas"

data_pais <- data_pais %>% rename(`Tipo de participante` = tipo, `Número` = numero)

# URL de pais
data_pais <- data_pais %>% left_join(., url_ods, by = "country")

# Tabla de estadisticas por pais
pais_table <- lapply(unique(data_pais$country), function(x){
  
  # x <- "VENEZUELA"
  
  data <- data_pais[data_pais$country == x,] %>% dplyr::select(-c(country, year, link))
  flag <- banderas[banderas$country == x,] %>% dplyr::select(-country) %>% as.character()
  link <- data_pais[data_pais$country == x,] %>% dplyr::select(link) %>% unique() %>% as.character()
  
  tab <- tableHTML(data,
                   rownames = FALSE,
                   widths = c(part_width, num_width),
                   # caption = glue("<strong>{x}</strong>"),
                   caption = glue('<img src="{flag}" width={logo_size_w*0.1} height={logo_size_h*0.1}> <strong>{x}</strong>'),
                   # footer = url_ods,
                   # footer = glue("<a href='{link}' target='_blank'> Click para ver más información sobre el país"),
                   footer = ifelse(is.na(link), "", glue("<a href='{link}' target='_blank'> Click para ver más información sobre el país")),
                   # footer = glue("<a href='{url_ods}' target='_blank'> Click para ver más información sobre el país"),
                   border = 0,
                   escape = F) %>%
    add_css_caption(css = list(c('color', 'font-size', 'text-align'), c('black', link_text, 'center'))) %>%
    add_css_thead(css = list('background-color', '#F1F1F1')) %>%
    add_css_footer(css = list(c('color', 'font-size', 'text-align'), c(pnud, link_text, 'left')))
  
  tab <- as.character(tab)
  tab <- data.frame(country = x, table_agg = tab)
  
  return(tab)
  
}) %>% bind_rows()

# Unir tabla de estadisticas con cada pais 
meta_map_con <- meta_map_con %>% left_join(pais_table, by = "country") 

#--------------------------#
# C. Estadisticas nivel programa ----
#--------------------------#

# Organizar estadisticas a nivel pais-programa
# Agregamos a nivel de pais, metodologia y sector
data_prog <- data_all %>% 
  # Corregir nombre de metodologia en casos puntuales
  mutate(
    metodologia = ifelse(
      str_detect(metodologia, "Otra, por favor especificar"), 
      comentarios, 
      metodologia)
  ) %>%
  dplyr::select(c(country, metodologia, num_empresas, num_hombres, num_mujeres, num_total_personas, act_econ)) %>%
  mutate(across(.cols = starts_with("num"), .fns = ~as.numeric(.))) %>%
  group_by(country, metodologia,act_econ) %>%
  summarise(across(.cols = starts_with("num"), .fns = ~sum(., na.rm = T))) %>%
  ungroup() %>%
  rename(Metodologia = metodologia, Empresas = num_empresas, Hombres = num_hombres, Mujeres = num_mujeres, 
         `Total personas` = num_total_personas, Sector = act_econ)

# Tabla de estadisticas por pais
prog_table <- lapply(unique(data_prog$country), function(x){
  
  # x <- "GUYANA"
  flag <- banderas[banderas$country == x,] %>% dplyr::select(-country) %>% as.character()
  
  data <- data_prog[data_prog$country == x,] %>%
    mutate(Logo = ifelse(Metodologia == "Creciendo Con Su Negocio", ccsn,
                         ifelse(Metodologia == "Desarrollo de Proveedores", mdp,
                                ifelse(Metodologia == "En Marcha Digital", enmar_dig,
                                       ifelse(Metodologia == "En Marcha", enmar,
                                              ifelse(Metodologia == "Iniciando Con Su Negocio", icsn,
                                                     ifelse(Metodologia == "Adaptando Su Negocio", asn,
                                                            ifelse(Metodologia == "Negocios inclusivos", neg, NA)))))))) %>%
    mutate(Logo = ifelse(!is.na(Logo), glue('<img src="{Logo}" width={logo_size_w} height={logo_size_h}>'), "")) %>%
    relocate(Logo, .before = country) %>% relocate(Sector, .after = `Total personas`) %>%
    rename(`Metodología` = Metodologia) %>%
    dplyr::select(-country)
  
  data$`Metodología`[data$`Metodología` == 'Programa de mentoría y acompañamiento para las iniciativas ganadoras del concurso "Solucionespara la transformación hacia un planeta sano", en el marco de Estocolmo+50'] <- 'Soluciones para la transformación hacia un planeta sano'
  
  tab <- tableHTML(data,
                   rownames = FALSE,
                   widths = c(logo_width, met_width, text_width, text_width, text_width, text_width, sector_width),
                   # caption = glue("<strong>{x}</strong>"),
                   caption = glue('<img src="{flag}" width={logo_size_w*0.1} height={logo_size_h*0.1}> <strong>{x}</strong>'),
                   border = 0,
                   escape = F) %>%
    # add_css_table(css = list('text-align', 'center')) %>%
    add_css_caption(css = list(c('color', 'font-size', 'text-align'), c('black', link_text, 'center'))) %>%
    add_css_thead(css = list('background-color', '#F1F1F1'))
  
  tab <- as.character(tab)
  tab <- data.frame(country = x, table_prog = tab)
  
  return(tab)
  
}) %>% bind_rows()

# Unir tabla de estadisticas con cada pais
mapa_prog <- meta_map %>% left_join(prog_table, by = "country")

#-------------------------------------------------------#
# 4. Mapas ----
#-------------------------------------------------------#

mapa_ods <-
  leaflet(mapa_base, height = "100vh", width = "100vw", 
          options = leafletOptions(zoomControl = FALSE, scrollWheelZoom = FALSE)) %>%
  # Boton de zoom en esquina superior derecha
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this);
        map.scrollWheelZoom.disable();
        L.Map.scrollWheelZoom = false;
    }") %>%
  # Capa LAC
  fitBounds( -180, -90, 180, 90) %>%
  addControl(disclaimer, position = "bottomleft") %>%
  # fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
  addPolygons(data = mapa_un_all,
    # data = mapa_un_all,          
              # Color de los limites de los paises
              color = "white",
              # Grosor de limites de paises
              weight = 0,
              # Color de relleno de los poligonos
              fillColor = "white",
              # Transparencia del relleno de los poligonos
              fillOpacity = opacity*0.35
              ) %>%
  # Centrar mapa en Oceano atlantico
  clearBounds() %>%
  setView(lng = -30.700545262011225, lat = 8.384568291083305, zoom = 3) %>%
  # Informacion por pais
  addPolygons(data = meta_map_con,
              popup = ~table_agg,
              popupOptions = popupOptions(closeButton = F, closeOnClick = T, maxWidth = 400, maxHeight = 1000, autoPan = T),
              group = "Información por país",
              # Lineas de los paises
              color = "white", weight = 1, stroke = T,
              fillColor = col_palette[2], fillOpacity = opacity,
              highlightOptions = highlightOptions(
                fillColor = pnud,
                fillOpacity = opacity*1,
                bringToFront = TRUE),
              label = ~country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = country_text, direction = "auto")) %>%
  
  # Informacion por programa-pais
  addPolygons(data = mapa_prog,
              popup = ~table_prog,
              popupOptions = popupOptions(closeButton = F, closeOnClick = T, maxWidth = popup_w, maxHeight = popup_h, autoPan = T),
              group = "Información por país y programa",
              # Lineas de los paises
              color = "white", weight = 1, stroke = T,
              fillColor = col_palette[3], fillOpacity = opacity,
              highlightOptions = highlightOptions(
                fillColor = col_palette[4],
                fillOpacity = opacity*1,
                bringToFront = TRUE),
              label = ~country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = country_text, direction = "auto")) %>%
  # Control de capas
  addLayersControl(
    baseGroups  = c("Información por país", "Información por país y programa"),
    options = layersControlOptions(collapsed = F),
    position = "topright") %>%
  
  # Solo mostrar la primera capa
  hideGroup(c("Información por país y programa"))

# Modificar tamano de pop-ups y capas
mapa_final <- browsable(
  tagList(list(tags$head(tags$style(
    # Pop-ups
    ".leaflet-popup-content-wrapper {
    background: white;
    color: black;
    padding: 1px;
    border-radius: 0px;
    }",
    "body {font-family: ProximaNova}",
    # Control de capas
    ".leaflet-control-layers-expanded {
                 line-height: 30px;
                 font-size: 20px;
                 }",
    ".leaflet-control-layers-list {
                 font-size: 20px;
                 }",
    ".leaflet-container { 
    font: 12px/1.5 ProximaNova, sans-serif;
    }",
    ".info { 
    font: 12px/1.5 ProximaNova, sans-serif;
    }"
    # style = "font-family: ProximaNova; sans-serif"
    ),
    # includeCSS("https://cdn.jsdelivr.net/npm/@undp/design-system/docs/css/base-minimal-no-grid.min.css"),
    # style = "font-family: ProximaNova; sans-serif",
    tags$link(href="https://cdn.jsdelivr.net/npm/@undp/design-system/docs/css/base-minimal-no-grid.min.css",rel="stylesheet")
    ),
    mapa_ods
    )),
  )

mapa_final
  
# mapa_final
# save_html(mapa_final, glue("{graficas}/test/index.html"))
save_html(mapa_final, glue("{graficas}/mapa_ods_cv_guides.html"))


