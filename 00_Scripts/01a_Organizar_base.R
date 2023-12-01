#-------------------------------------------------------#
# ODS Cadenas de Valor ----
# Ultima fecha de modificacion: 1 dic, 2023
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
# 1. Abrir datos 2023 ----
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
# B. Nivel programa ----
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

#-------------------------------------------------------#
# 2. Datos historicos 2008-2022 ----
#-------------------------------------------------------#

#--------------------------#
# A. Nivel pais ----
#--------------------------#

# Abrimos excel original con informacion historica por pais y programa 
data_icsn <- readxl::read_excel(glue("{datos_ori}/SDGVC METHODOLOGIES BREAKDOWN.xlsx"),
                               sheet = 'ICSN') %>%
  clean_names() %>%
  mutate(metodologia = "Iniciando Con Su Negocio") %>%
  dplyr::filter(str_detect(pais, "SUBTOT")) %>%
  mutate(hombres = as.numeric(hombres), mujeres = as.numeric(mujeres), empresas_participantes = as.numeric(empresas_participantes))

data_ccsn <- readxl::read_excel(glue("{datos_ori}/SDGVC METHODOLOGIES BREAKDOWN.xlsx"),
                                sheet = 'CCSN') %>%
  clean_names() %>%
  mutate(metodologia = "Creciendo Con Su Negocio") %>%
  dplyr::filter(str_detect(pais, "SUBTOT")) %>%
  mutate(hombres = as.numeric(hombres), mujeres = as.numeric(mujeres), empresas_participantes = as.numeric(empresas_participantes))

data_em <- readxl::read_excel(glue("{datos_ori}/SDGVC METHODOLOGIES BREAKDOWN.xlsx"),
                                sheet = 'EM') %>%
  clean_names() %>%
  mutate(metodologia = "En Marcha") %>%
  dplyr::filter(str_detect(pais, "SUBTOT")) %>%
  mutate(hombres = as.numeric(hombres), mujeres = as.numeric(mujeres), empresas_participantes = as.numeric(empresas_participantes))

data_emd <- readxl::read_excel(glue("{datos_ori}/SDGVC METHODOLOGIES BREAKDOWN.xlsx"),
                                sheet = 'EMD') %>%
  clean_names() %>%
  mutate(metodologia = "En Marcha Digital") %>%
  dplyr::filter(str_detect(pais, "SUBTOT")) %>%
  mutate(hombres = as.numeric(hombres), mujeres = as.numeric(mujeres), empresas_participantes = as.numeric(empresas_participantes))

data_pdp <- readxl::read_excel(glue("{datos_ori}/SDGVC METHODOLOGIES BREAKDOWN.xlsx"),
                                sheet = 'PDP') %>%
  clean_names()  %>%
  mutate(metodologia = "Desarrollo de Proveedores") %>%
  dplyr::filter(str_detect(pais, "SUBTOT")) %>%
  mutate(hombres = as.numeric(hombres), mujeres = as.numeric(mujeres), empresas_participantes = as.numeric(empresas_participantes))

# Unir informacion por programa
data_pais <- bind_rows(data_icsn, data_ccsn, data_em, data_emd, data_pdp) %>%
  dplyr::select(-adaptacion) %>%
  mutate(pais = gsub("SUBTOT ", "", pais)) %>%
  rename(country = pais, num_empresas = empresas_participantes, num_hombres = hombres, num_mujeres = mujeres)

# Corregir nombres de pais
data_pais$country[data_pais$country == 'MEXICO'] <- 'MÉXICO'
data_pais$country[data_pais$country == 'PERU'] <- 'PERÚ'
data_pais$country[data_pais$country == 'DOMINICANA'] <- 'REPÚBLICA DOMINICANA'
data_pais$country[data_pais$country == 'HAITI'] <- 'HAITÍ'
data_pais$country[data_pais$country == 'PAKISTAN'] <- 'PAKISTÁN'

# Agrupar datos a nivel de pais
data_pais_clean <- data_pais %>%
  dplyr::select(-metodologia) %>%
  group_by(country) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  mutate(period = "2008 - Agosto 2022") 

#--------------------------#
# B. Nivel programa ----
#--------------------------#

data_prog <- data_pais %>%
  mutate(period = "2008 - Agosto 2022")

#--------------------------#
# C. Exportar bases ----
#--------------------------#

saveRDS(data_pais_clean, glue("{datos}/datos_pais_2008_2022.rds"))
saveRDS(data_prog, glue("{datos}/datos_pais_empresa_2008_2022.rds"))










