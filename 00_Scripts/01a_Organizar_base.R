#-------------------------------------------------------#
# ODS Cadenas de Valor ----
# Ultima fecha de modificacion: 20 de sept, 2023
# Organizar base de datos de empresas
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, janitor)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
options(scipen = 999)
# mes <- "Junio"
mes <- "Ago"
currentyear <- 2023

#-------------------------------------------------------#
# 1. Abrir datos ----
#-------------------------------------------------------#

#--------------------------#
# A. Nivel Pais ----
#--------------------------#

# Abrimos excel original del empresas 
data_res <- readxl::read_excel(glue("{datos_ori}/Matriz Seguimiento ODS CV {mes} {currentyear} Compilada.xlsx"),
                               sheet = 'Tabla dinamica resumen') %>%
  clean_names() 

# Identificar periodo de analisis
period <- data_res %>% 
  dplyr::filter(programa_ods_cadenas_de_valor == "Periodo Reporte") %>%
  dplyr::select(x2) %>%
  as.character()

# Organizar resumen de datos a nivel pais
data_res <- data_res %>% 
  # Clean dataset and set column names
  drop_na(x3) %>% row_to_names(row_number = 1) %>% clean_names() %>%
  # Renombrar variables
  rename(country = row_labels,
         num_empresas = sum_of_numero_de_empresas_participantes,
         num_hombres = sum_of_participantes_hombres,
         num_mujeres = sum_of_participantes_mujeres, 
         num_total_personas = sum_of_total_personas_participantes) %>%
  mutate(country = ifelse(country == "Grand Total", "Total", country),
         across(.cols = starts_with('num'), ~as.numeric(.x)), 
         period = period, year = currentyear)

# Corregir nombres de paises
data_res$country[data_res$country == 'Mexico'] <- 'México'
data_res$country[data_res$country == 'Peru'] <- 'Perú'

#--------------------------#
# B. Nivel empresas ----
#--------------------------#

data_all <- readxl::read_excel(glue("{datos_ori}/Matriz Seguimiento ODS CV {mes} {currentyear} Compilada.xlsx"),
                               sheet = 'Matriz resultados Compilada') %>% 
  # Clean dataset and set column names
  drop_na(`...3`) %>% row_to_names(row_number = 1) %>% clean_names() %>%
  # Renombrar variables
  rename(country = pais,
         num_empresas = numero_de_empresas_participantes,
         num_total_personas = total_personas_participantes,
         num_hombres = participantes_hombres,
         num_mujeres = participantes_mujeres,
         act_econ = actividad_economica,
         aliado = contraparte_aliado,
         links = links_relevantes) %>%
  mutate(period = period, year = currentyear)

# Hay que split los links porque son mas de uno

# Corregir nombres de paises
data_all$country[data_all$country == 'Mexico'] <- 'México'
data_all$country[data_all$country == 'Peru'] <- 'Perú'

#--------------------------#
# C. Exportar bases ----
#--------------------------#

saveRDS(data_res, glue("{datos}/datos_pais_{str_to_lower(mes)}_{currentyear}.rds"))
saveRDS(data_all, glue("{datos}/datos_pais_empresa_{str_to_lower(mes)}_{currentyear}.rds"))
# data_cv <- data_res %>% left_join(data_all, by = "country")
