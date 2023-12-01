#-------------------------------------------------------#
# ODS Cadenas de Valor ----
# Ultima fecha de modificacion: 1 dic, 2023
# Este mapa incluye resultados de programas ODS Cadenas de Valor por pais y programa, por año
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf, leaflet, rgdal, htmltools, leafem, leaflegend, tableHTML, leaftime)

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

mes <- "ago"
currentyear <- 2023

# Text sizes and other options
country_text <- "22px"
link_text <- "18px"
opacity <- 1
text_width <- 80*0.6
met_width <- 130
logo_width <- 150*1.5
logo_size_h <- 150
logo_size_w <- 200
sector_width <- 220
part_width <- 150*1.5
num_width <- 80
popup_w <- 1200
popup_h <- 800
pad <- "1px 4px"

# Desactivar coordenadas esfericas: importante para manejar SHP de UN (no esta en long-lat)
sf_use_s2(FALSE)

#-------------------------------------------------------#
# 0. Funciones y otros datos ----
#-------------------------------------------------------#

#--------------------------#
# A. Funciones ----
#--------------------------#

# Tabla por pais
fun_pais_table <- function(x, y, base){
  
  # base <- data_pais
  # x <- "ALBANIA"
  # y <- "2008 - Agosto 2022"
  
  print(x)
  
  # Filtrar periodo e informacion de pais
  base <- base[base$period == y,]
  data <- base[base$country == x,] %>% dplyr::select(-c(country, link, period))
  flag <- banderas[banderas$country == x,] %>% dplyr::select(-country) %>% as.character()
  link <- base[base$country == x,] %>% dplyr::select(link) %>% unique() %>% as.character()
  
  tab <- tableHTML(data,
                   rownames = FALSE,
                   widths = c(part_width, num_width),
                   caption = glue('<img src="{flag}" width={logo_size_w*0.1} height={logo_size_h*0.1}> <strong>{x}</strong>'),
                   footer = ifelse(is.na(link), "", glue("<a href='{link}' target='_blank'> Click para ver más información sobre el país")),
                   border = 0,
                   escape = F) %>%
    add_css_caption(css = list(c('color', 'font-size', 'text-align'), c('black', link_text, 'center'))) %>%
    add_css_thead(css = list('background-color', '#F1F1F1')) %>%
    add_css_footer(css = list(c('color', 'font-size', 'text-align'), c(pnud, link_text, 'left')))
  
  tab <- as.character(tab)
  tab <- data.frame(country = x, table_agg = tab)
  
  return(tab)
  
}

# Tabla de estadisticas por pais-programa
fun_prog_table <- function(x, y, base){
  
  # base <- data_prog
  # x <- "COLOMBIA"
  # y <- "2008 - Agosto 2022"
  
  print(x)
  
  # Filtrar periodo e informacion de pais
  base <- base[base$period == y,]

  flag <- banderas[banderas$country == x,] %>% dplyr::select(-country) %>% as.character()
  
  data <- base[base$country == x,] %>%
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
    dplyr::select(-c(country, period))
  
  data$`Metodología`[data$`Metodología` == 'Programa de mentoría y acompañamiento para las iniciativas ganadoras del concurso "Solucionespara la transformación hacia un planeta sano", en el marco de Estocolmo+50'] <- 'Soluciones para la transformación hacia un planeta sano'
  
  tab <- tableHTML(data,
                   rownames = FALSE,
                   widths = c(logo_width, met_width, text_width, text_width, text_width, text_width, sector_width),
                   caption = glue('<img src="{flag}" width={logo_size_w*0.1} height={logo_size_h*0.1}> <strong>{x}</strong>'),
                   border = 0,
                   escape = F) %>%
    add_css_caption(css = list(c('color', 'font-size', 'text-align'), c('black', link_text, 'center'))) %>%
    add_css_thead(css = list('background-color', '#F1F1F1'))
  
  tab <- as.character(tab)
  tab <- data.frame(country = x, table_prog = tab)
  
  return(tab)
  
}


#--------------------------#
# B. Url, disclaimer ----
#--------------------------#

# URL programas 
url_ods <- readxl::read_excel(glue("{datos_ori}/links_paises.xlsx")) %>%
  mutate(country = str_to_upper(country))

# Nota sobre el periodo
nota_periodo <- HTML("<p style='text-align: left;'>Los datos históricos corresponden a la información recopilada desde el inicio del programa en 2008 hasta agosto de 2022.</p>")

# Disclaimer para mapa
disclaimer <- HTML("<p style='text-align: left;'> <strong>Nota: Los datos históricos corresponden a la información recopilada en Agosto del año 2022, y cubren el periodo 2008-Agosto 2022.</strong> <br> Las designaciones utilizadas y la presentación de material en este mapa no implican la expresión de ninguna opinión por parte de la Secretaría de las Naciones Unidas o el PNUD con respecto al estado legal de ningún país, territorio, ciudad o área, ni de sus autoridades, ni con respecto a la delimitación de sus fronteras o límites.</p>")
# disclaimer <- HTML("<p style='text-align: left;'>Las designaciones utilizadas y la presentación de material en este mapa no implican la expresión de ninguna opinión por parte de la Secretaría de las Naciones Unidas o el PNUD con respecto al estado legal de ningún país, territorio, ciudad o área, ni de sus autoridades, ni con respecto a la delimitación de sus fronteras o límites.</p>")

#--------------------------#
# C. Banderas ----
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

# Barbados
barbados <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Barbados.svg.png?raw=true"

# Haiti
haiti <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Haiti.svg.png?raw=true"

# Honduras
honduras <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Honduras.svg.png?raw=true"

# Pakistan
pakistan <- "https://github.com/undp/IGT_ValueChains_maps/blob/main/01_Datos_originales/Country_flags/Flag_of_Pakistan.svg.png?raw=true"

#--------------------------#
# D. Metodologias ----
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
mapa_un_all <- st_read(glue("{datos_ori}/Mapas/UN_Geodata_simplified/BNDA_simplified.shp")) %>%
  janitor::clean_names() %>%
  dplyr::select(georeg, nam_en, lbl_en) %>%
  rename(country = lbl_en) %>%
  dplyr::select(country, georeg, geometry)

# Mapa UN filtrado para America y Europa
mapa_un <- st_read(glue("{datos_ori}/Mapas/UN_Geodata_simplified/BNDA_simplified.shp")) %>%
  janitor::clean_names() %>%
  dplyr::select(georeg, nam_en, lbl_en) %>%
  dplyr::filter(georeg == "AME" | georeg == "EUR" | georeg == "ASI") %>%
  rename(country = lbl_en) %>%
  dplyr::select(country, geometry)

# Nombres de pais en espanol
mapa_un$country[mapa_un$country == 'MEXICO'] <- 'MÉXICO'
mapa_un$country[mapa_un$country == 'PERU'] <- 'PERÚ'
mapa_un$country[mapa_un$country == 'DOMINICAN REPUBLIC'] <- 'REPÚBLICA DOMINICANA'
mapa_un$country[mapa_un$country == 'HAITI'] <- 'HAITÍ'
mapa_un$country[mapa_un$country == 'PAKISTAN'] <- 'PAKISTÁN'
mapa_un_all$country[mapa_un_all$country == 'MEXICO'] <- 'MÉXICO'
mapa_un_all$country[mapa_un_all$country == 'PERU'] <- 'PERÚ'
mapa_un_all$country[mapa_un_all$country == 'DOMINICAN REPUBLIC'] <- 'REPÚBLICA DOMINICANA'
mapa_un_all$country[mapa_un_all$country == 'HAITI'] <- 'HAITÍ'
mapa_un_all$country[mapa_un_all$country == 'PAKISTAN'] <- 'PAKISTÁN'

#-------------------------------------------------------#
# 2. Organizar datos ----
#-------------------------------------------------------#

#--------------------------#
# A. Historico 2008-2022 ----
#--------------------------#

# Datos pais
data_pais_h <- readRDS(glue("{datos}/datos_pais_2008_2022.rds")) %>%
  mutate(num_total_personas = num_hombres + num_mujeres)

# Datos pais-programa
data_prog_h <- readRDS(glue("{datos}/datos_pais_empresa_2008_2022.rds")) %>%
  mutate(num_total_personas = num_hombres + num_mujeres)

#--------------------------#
# B. 2023 ----
#--------------------------#

# Datos pais
data_res <- readRDS(glue("{datos}/datos_pais_{str_to_lower(mes)}_{currentyear}.rds")) %>% 
  mutate(
    country = str_to_upper(country)
  ) %>% dplyr::filter(country != "TOTAL")

# Datos pais-programa
data_all <- readRDS(glue("{datos}/datos_pais_empresa_{str_to_lower(mes)}_{currentyear}.rds")) %>% 
  mutate(
    country = str_to_upper(country)
  ) %>% dplyr::filter(country != "TOTAL") %>%
  mutate(num_hombres = as.numeric(num_hombres), num_mujeres = as.numeric(num_mujeres), 
         num_total_personas = as.numeric(num_total_personas), num_empresas = as.numeric(num_empresas))

# URL Logos bandera paises
lista_paises <- bind_rows(data_pais_h, data_res)

banderas <- data.frame(country = unique(lista_paises$country), logo = NA)
banderas$logo[banderas$country == "ALBANIA"] <- albania
banderas$logo[banderas$country == "COLOMBIA"] <- colombia
banderas$logo[banderas$country == "ECUADOR"] <- ecuador
banderas$logo[banderas$country == "GUYANA"] <- guyana
banderas$logo[banderas$country == "MÉXICO"] <- mexico
banderas$logo[banderas$country == "PERÚ"] <- peru
banderas$logo[banderas$country == "REPÚBLICA DOMINICANA"] <- rep_dom
banderas$logo[banderas$country == "VENEZUELA"] <- venezuela
banderas$logo[banderas$country == "HONDURAS"] <- honduras
banderas$logo[banderas$country == "BARBADOS"] <- barbados
banderas$logo[banderas$country == "HAITÍ"] <- haiti
banderas$logo[banderas$country == "PAKISTÁN"] <- pakistan

#--------------------------#
# C. Emparejar con mapas ----
#--------------------------#

# Unir historicos con 2023
data_res <- bind_rows(data_pais_h, data_res)

data_all <- bind_rows(data_prog_h, data_all) %>%
  group_by(metodologia, country) %>%
  fill(act_econ, .direction = "downup") %>%
  ungroup()

# Emparejamos con mapa
meta_map <- mapa_un %>% left_join(data_all, by = "country") %>% drop_na(num_empresas)
meta_map_con <- mapa_un %>% left_join(data_res, by = "country") %>% drop_na(num_empresas)

rm(data_pais_h, data_prog_h, lista_paises)

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
  dplyr::select(-c(year)) %>%
  pivot_longer(cols = starts_with("num"), names_to = "tipo", values_to = "numero") %>%
  mutate(period = ifelse(str_detect(period, "2023"), "2023", period)) %>% 
  # URL de pais
  left_join(., url_ods, by = "country")

data_pais$tipo[data_pais$tipo == "num_empresas"] <- "Empresas"
data_pais$tipo[data_pais$tipo == "num_hombres"] <- "Hombres"
data_pais$tipo[data_pais$tipo == "num_mujeres"] <- "Mujeres"
data_pais$tipo[data_pais$tipo == "num_total_personas"] <- "Total personas"

data_pais <- data_pais %>% rename(`Tipo de participante` = tipo, `Número` = numero)

# Tabla de estadisticas por pais
tabla_h <- lapply(unique(data_pais$country[data_pais$period == "2008 - Agosto 2022"]), fun_pais_table, y = "2008 - Agosto 2022", base = data_pais) %>%
  bind_rows() %>% mutate(period = "2008 - Agosto 2022")

tabla_23 <- lapply(unique(data_pais$country[data_pais$period == "2023"]), fun_pais_table, y = "2023", base = data_pais) %>%
  bind_rows() %>% mutate(period = "2023")

pais_table <- bind_rows(tabla_h, tabla_23)

# Unir tabla de estadisticas con cada pais 
meta_map_con <- meta_map_con %>% dplyr::select(-period) %>% left_join(pais_table, by = "country") 
rm(pais_table, tabla_23, tabla_h, data_pais, data_res)

#--------------------------#
# C. Estadisticas nivel programa ----
#--------------------------#

# Organizar estadisticas a nivel pais-programa
data_prog <- data_all %>% 
  # Corregir nombre de metodologia en casos puntuales
  mutate(metodologia = ifelse( str_detect(metodologia, "Otra, por favor especificar"), comentarios, metodologia)) %>%
  dplyr::select(c(country, metodologia, num_empresas, num_hombres, num_mujeres, num_total_personas, act_econ, period)) %>%
  mutate(across(.cols = starts_with("num"), .fns = ~as.numeric(.))) %>%
  # Agregamos a nivel de pais, metodologia y sector
  group_by(country, metodologia, act_econ, period) %>%
  summarise(across(.cols = starts_with("num"), .fns = ~sum(., na.rm = T))) %>%
  ungroup() %>%
  rename(Metodologia = metodologia, Empresas = num_empresas, Hombres = num_hombres, Mujeres = num_mujeres, 
         `Total personas` = num_total_personas, Sector = act_econ) %>%
  arrange(country, Metodologia)  %>%
  # Corregimos programas sin sector economico definido, y periodo de analisis
  mutate(period = ifelse(str_detect(period, "2023"), "2023", period),
         Sector = ifelse(is.na(Sector), "", Sector))

# Traducir sectores a espanol
data_prog$Sector[data_prog$Sector == "Food industry & services"] <- "Industria y servicios alimentarios"
data_prog$Sector[data_prog$Sector == "MSMEs (Beauty salon, retail trade, sale of prepared food, household appliance repair, welding services, etc.)"] <- "Mipymes (salones de belleza, comercio al por menor, venta de comida preparada, reparación de electrodomésticos, servicios de soldadura, etc.)"
data_prog$Sector[data_prog$Sector == "Tourism & food industry"] <- "Turismo e industria alimentaria"

# Tabla de estadisticas por pais
tabla_h <- lapply(unique(data_prog$country[data_prog$period == "2008 - Agosto 2022"]), fun_prog_table, y = "2008 - Agosto 2022", base = data_prog) %>%
  bind_rows() %>% mutate(period = "2008 - Agosto 2022")

tabla_23 <- lapply(unique(data_prog$country[data_prog$period == "2023"]), fun_prog_table, y = "2023", base = data_prog) %>%
  bind_rows() %>% mutate(period = "2023")

prog_table <- bind_rows(tabla_h, tabla_23)

# Unir tabla de estadisticas con cada pais
mapa_prog <- meta_map %>% dplyr::select(-period) %>% left_join(prog_table, by = "country")

#-------------------------------------------------------#
# 4. Mapas ----
#-------------------------------------------------------#

# Cortar mapas según area de interes (America, Europa, Africa)
# Bounding box coordinates
# https://boundingbox.klokantech.com/
bound_cords <- st_as_sfc(st_bbox(c(xmin = -140.5, xmax = 152.5, ymax = 59.1, ymin = -58.4), crs = st_crs(4326)))
mapa_un_all <- st_make_valid(mapa_un_all)
mapa_un_all <- st_intersection(mapa_un_all, bound_cords)

#--------------------------#
# A. Nivel pais ----
#--------------------------#

mapa_pais <- leaflet(mapa_base, height = "100vh", width = "100vw", 
          options = leafletOptions(zoomControl = FALSE, scrollWheelZoom = FALSE)) %>%
  # Boton de zoom en esquina superior derecha
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this);
        map.scrollWheelZoom.disable();
        L.Map.scrollWheelZoom = false;
    }") %>%
  # Capa LAC
  addControl(disclaimer, position = "bottomleft") %>%
  # addControl(nota_periodo, position = "bottomleft") %>%
  fitBounds(lat1 = -140.5, lat2 = 61.3, lng1 = 60.1, lng2 = -57.1) %>%
  addPolygons(data = mapa_un_all,
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
  setView(lng = -25.770042719697965, lat = -5.290852554307293, zoom = 3) %>%
  
  # 2008-2022 (Historico)
  addPolygons(data = meta_map_con %>% dplyr::filter(period == "2008 - Agosto 2022"),
              popup = ~table_agg,
              popupOptions = popupOptions(closeButton = F, closeOnClick = T, maxWidth = 400, maxHeight = 1000, autoPan = T),
              group = "2008 - Agosto 2022 (Histórico)",
              # Lineas de los paises
              color = "white", weight = 1, stroke = T,
              fillColor = col_palette[2], fillOpacity = opacity,
              highlightOptions = highlightOptions(
                fillColor = pnud,
                fillOpacity = opacity*1,
                bringToFront = TRUE),
              label = ~country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = pad),
                textsize = country_text, direction = "auto")) %>%
  
  # 2023
  addPolygons(data = meta_map_con %>% dplyr::filter(period == "2023"),
              popup = ~table_agg,
              popupOptions = popupOptions(closeButton = F, closeOnClick = T, maxWidth = 400, maxHeight = 1000, autoPan = T),
              group = "Año 2023",
              # Lineas de los paises
              color = "white", weight = 1, stroke = T,
              fillColor = col_palette[3], fillOpacity = opacity,
              highlightOptions = highlightOptions(
                fillColor = col_palette[4],
                fillOpacity = opacity*1,
                bringToFront = TRUE),
              label = ~country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = pad),
                textsize = country_text, direction = "auto")) %>%
  # Control de capas
  addLayersControl(
    baseGroups  = c("2008 - Agosto 2022 (Histórico)", "Año 2023"),
    options = layersControlOptions(collapsed = F),
    position = "topright") %>%
  
  # Solo mostrar la primera capa
  hideGroup(c("Año 2023"))


# Modificar tamano de pop-ups y capas
mapa_pais_final <- browsable(
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
  ),
  tags$link(href="https://cdn.jsdelivr.net/npm/@undp/design-system/docs/css/base-minimal-no-grid.min.css",rel="stylesheet")
  ),
  mapa_pais
  )),
)

mapa_pais_final

#--------------------------#
# B. Nivel pais-programa ----
#--------------------------#

map_prog <- leaflet(mapa_base, height = "100vh", width = "100vw", 
                     options = leafletOptions(zoomControl = FALSE, scrollWheelZoom = FALSE)) %>%
  # Boton de zoom en esquina superior derecha
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this);
        map.scrollWheelZoom.disable();
        L.Map.scrollWheelZoom = false;
    }") %>%
  # Capa LAC
  addControl(disclaimer, position = "bottomleft") %>%
  # addControl(nota_periodo, position = "bottomleft") %>%
  fitBounds(lat1 = -140.5, lat2 = 61.3, lng1 = 60.1, lng2 = -57.1) %>%
  addPolygons(data = mapa_un_all,
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
  setView(lng = -25.770042719697965, lat = -5.290852554307293, zoom = 3) %>%
  
  # 2008-2022 (Historico)
  addPolygons(data = mapa_prog %>% dplyr::filter(period == "2008 - Agosto 2022"),
              popup = ~table_prog,
              popupOptions = popupOptions(closeButton = F, closeOnClick = T, maxWidth = popup_w, maxHeight = popup_h*0.6, autoPan = T),
              group = "2008 - Agosto 2022 (Histórico)",
              # Lineas de los paises
              color = "white", weight = 1, stroke = T,
              fillColor = col_palette[2], fillOpacity = opacity,
              highlightOptions = highlightOptions(
                fillColor = pnud,
                fillOpacity = opacity*1,
                bringToFront = TRUE),
              label = ~country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = pad),
                textsize = country_text, direction = "auto")) %>%
  
  # 2023
  addPolygons(data = mapa_prog %>% dplyr::filter(period == "2023"),
              popup = ~table_prog,
              popupOptions = popupOptions(closeButton = F, closeOnClick = T, maxWidth = popup_w, maxHeight = popup_h*0.6, autoPan = T),
              group = "Año 2023",
              # Lineas de los paises
              color = "white", weight = 1, stroke = T,
              fillColor = col_palette[3], fillOpacity = opacity,
              highlightOptions = highlightOptions(
                fillColor = col_palette[4],
                fillOpacity = opacity*1,
                bringToFront = TRUE),
              label = ~country,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = pad),
                textsize = country_text, direction = "auto")) %>%
  # Control de capas
  addLayersControl(
    baseGroups  = c("2008 - Agosto 2022 (Histórico)", "Año 2023"),
    options = layersControlOptions(collapsed = F),
    position = "topright") %>%
  
  # Solo mostrar la primera capa
  hideGroup(c("Año 2023"))


# Modificar tamano de pop-ups y capas
mapa_prog_final <- browsable(
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
                 font-size: 10px;
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
  ),
  tags$link(href="https://cdn.jsdelivr.net/npm/@undp/design-system/docs/css/base-minimal-no-grid.min.css",rel="stylesheet")
  ),
  map_prog
  )),
)

mapa_prog_final

#--------------------------#
# C. Exportar ----
#--------------------------#

# Exportar
save_html(mapa_pais_final, glue("{graficas}/mapa_ods_cv_pais_2008-2023.html"))
save_html(mapa_pais_final, "index1.html")
save_html(mapa_prog_final, glue("{graficas}/mapa_ods_cv_prog_2008-2023.html"))
save_html(mapa_prog_final, "index2.html")
