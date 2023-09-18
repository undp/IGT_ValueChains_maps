#-------------------------------------------------------#
# Rastreador de crecimiento inclusivo ----
# Ultima fecha de modificacion: 18 de septiembre 2023
# Organizar base de datos del metaanalisis
#-------------------------------------------------------#
    
#--------------------------#
# packages ----ccc
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, janitor)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

# datos_ori <- "C:/Users/User/Dropbox/Meta-análisis ALMPs conponente género_PNUD&ILO/1. Papers used/Tables and papers from ALMP and Informality meta-analysis"
datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
options(scipen = 999)
mes <- "Julio"

#-------------------------------------------------------#
# 1. Abrir datos ----
#-------------------------------------------------------#

# Abrimos excel original del metaanalisis
meta <- readxl::read_excel(glue("{datos_ori}/Matriz/[{mes}-2023] Matriz_Meta-analisis_ALMP_Informalidad.xlsx")) %>%
  clean_names() %>% 
  # Renombrar columnas
  rename(
    # Empleo
    out_employment = measured_outcomes, out_employment_w = x15, out_employment_m = x16, out_employment_y = x17,
    # Ingresos
    out_earnings = x18, out_earnings_w = x19, out_earnings_m = x20, out_earnings_y = x21,
    # Horas trabajadas
    out_hours = x22, out_hours_w = x23, out_hours_m = x24, out_hours_y = x25,
    # Empleo formal
    out_formal_em = x26, out_formal_em_w = x27, out_formal_em_m = x28, out_formal_em_y = x29,
    # Ingreso formal
    out_formal_ear = x30, out_formal_ear_w = x31, out_formal_ear_m = x32, out_formal_ear_y = x33,
    # Ingreso informal
    out_informal_ear = x34, out_informal_ear_w = x35, out_informal_ear_m = x36, out_informal_ear_y = x37,
    # Empleo informal
    out_informal_em = x38, out_informal_em_w = x39, out_informal_em_m = x40, out_informal_em_y = x41,
    # Duracion de la politica
    effect_short_run = effect_of_the_policy, effect_long_run = x43,
    productivity = x44,
    # Muestra
    sample_universe = sample, sample_size = x51, sample_demographics = x52, sample_age = x53,
    id = no, title = title_of_the_paper, 
    # URL evaluacion
    url = link_source,
    # Abstract y link al programa 
    abstract = abstract_es, url_prog = link_of_the_program, url_prog_d = link_related_to_program
  ) %>%
  drop_na(title)

# Variables de interes
meta_sample <- meta %>%
  drop_na(outcome_variable, data) %>%
  dplyr::select(id, title, year, type_of_policy, country, name_of_the_program, policy_evaluated, 
                starts_with("out"), type_of_impact_evaluation, abstract, starts_with("url")) %>%
  # Correccion en la dummy del link del programa
  # Solo se consideran links al programa si esta dummy indica que el link esta asociado al programa
  # Hay casos en los que los programas cambian y el link corresponde al nuevo programa
  mutate(url_prog_d = ifelse(is.na(url_prog_d), 0, 1),
         url_prog = ifelse(url_prog_d == 1, url_prog, NA))

colSums(is.na(meta_sample))

#-------------------------------------------------------#
# 2. Estadisticas pais ----
#-------------------------------------------------------#

#--------------------------#
# A. Por tipo de politica ----
# ALMP - Informalidad
#--------------------------#

meta_policy <- meta_sample %>%
  # Quitar el signo a los no significativos
  mutate(across(.cols = starts_with("out_"), gsub, pattern = "\\\\.*", replacement = "")) %>%
  mutate(across(.cols = starts_with("out_"), gsub, pattern = "/.*", replacement = "")) %>%
  # Solo politica, pais y resultados
  dplyr::select(type_of_policy, country, starts_with("out_")) %>%
  # Pasar de wide a long
  pivot_longer(cols = starts_with("out_"), names_to = "type_outcome", values_to = "value_outcome") %>%
  # Renombrar signos
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '+', 'positive'))) %>%
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '-', 'negative'))) %>%
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '−', 'negative'))) %>%
  drop_na(value_outcome) %>%
  # Contar tipo de resultados por politica y pais
  count(country, type_of_policy, type_outcome, value_outcome) %>%
  # Sin distinguir por grupo poblacional (genero, edad)
  dplyr::filter(!str_detect(type_outcome, '_m|_w|_y'))

#--------------------------#
# B. Por tipo de programa ----
# Jovenes en Accion, SARE, ProEmpleo etc
#--------------------------#

meta_prog <- meta_sample %>%
  # Quitar el signo a los no significativos
  mutate(across(.cols = starts_with("out_"), gsub, pattern = "\\\\.*", replacement = "")) %>%
  mutate(across(.cols = starts_with("out_"), gsub, pattern = "/.*", replacement = "")) %>%
  # Solo pais, programa y resultados
  dplyr::select(country, name_of_the_program, starts_with("out_")) %>%
  # Pasar de wide a long
  pivot_longer(cols = starts_with("out_"), names_to = "type_outcome", values_to = "value_outcome") %>%
  # Renombrar signos
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '+', 'positive'))) %>%
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '-', 'negative'))) %>%
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '−', 'negative'))) %>%
  # Solo observaciones con resultados reportados
  drop_na(value_outcome) %>%
  # Contar tipo de resultados por programa y pais
  count(country, name_of_the_program, type_outcome, value_outcome) %>%
  # Sin distinguir por grupo poblacional (genero, edad)
  dplyr::filter(!str_detect(type_outcome, '_m|_w|_y'))

# Numero de programas por pais
num_country <- meta_prog %>% distinct(country, name_of_the_program)
num_country %>% count(country)

# Corregimos siglas en los programas
meta_prog$name_of_the_program[meta_prog$name_of_the_program == "PANES" & meta_prog$country == "Uruguay"] <- "Plan de Atención Nacional a la Emergencia Social"
meta_prog$name_of_the_program[meta_prog$name_of_the_program == "PRODIAT" & meta_prog$country == "Mexico"] <- "Programa para el Desarrollo de las Industrias de Alta Tecnología"
meta_prog$name_of_the_program[meta_prog$name_of_the_program == "PROBECAT" & meta_prog$country == "Mexico"] <- "Programa de Becas de Capacitacion para Trabajadores Desempleados"
meta_prog$name_of_the_program[meta_prog$name_of_the_program == "PROBECAT/SICAT" & meta_prog$country == "Mexico"] <- "Programa de Becas de Capacitacion para Trabajadores Desempleados (PROBECAT) y Sistema de Capacitación para el Trabajo (SICAT)"
meta_prog$name_of_the_program[meta_prog$name_of_the_program == "PRONATEC" & meta_prog$country == "Brazil"] <- "Programa Nacional de Enseñanza Técnica y Empleo (PRONATEC)"
meta_prog$name_of_the_program[meta_prog$name_of_the_program == "PLANE" & meta_prog$country == "Bolivia"] <- "Plan Nacional de Empleo de Emergencia"
meta_prog$name_of_the_program[meta_prog$name_of_the_program == "REPRO" & meta_prog$country == "Argentina"] <- "Programa de Recuperación y Sostenimiento Productivo"

#--------------------------#
# C. Por tipo de programa (con URL y abstract) ----
#--------------------------#

meta_link <- meta_sample %>%
  # Quitar el signo a los no significativos
  mutate(across(.cols = starts_with("out_"), gsub, pattern = "\\\\.*", replacement = "")) %>%
  mutate(across(.cols = starts_with("out_"), gsub, pattern = "/.*", replacement = "")) %>%
  # Solo pais, programa y resultados
  dplyr::select(country, name_of_the_program, abstract, starts_with('url'), starts_with("out_")) %>%
  # Pasar de wide a long
  pivot_longer(cols = starts_with("out_"), names_to = "type_outcome", values_to = "value_outcome") %>%
  # Renombrar signos
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '+', 'positive'))) %>%
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '-', 'negative'))) %>%
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '−', 'negative'))) %>%
  # Solo observaciones con resultados reportados
  drop_na(value_outcome) %>%
  # Contar tipo de resultados por programa y pais
  count(country, name_of_the_program, abstract, url, url_prog, url_prog_d, type_outcome, value_outcome) %>%
  # Sin distinguir por grupo poblacional (genero, edad)
  dplyr::filter(!str_detect(type_outcome, '_m|_w|_y')) %>% 
  # Un mismo programa puede ser evaluado varias veces y tener resultados distintos
  # Es importante citar la evaluacion que reporta X resultado
  group_by(country, name_of_the_program, abstract, url_prog, url_prog_d, type_outcome, value_outcome) %>% 
  summarise(url = paste0(url, collapse = "; "),
            # abstract = paste0(abstract, collapse = "; "),
            # url_prog = paste0(url_prog, collapse = "; "),
            # url_prog_d = paste0(url_prog_d, collapse = "; "),
            n = sum(n, na.rm = T)) %>% 
  ungroup()

# Corregimos siglas en los programas
meta_link$name_of_the_program[meta_link$name_of_the_program == "PANES" & meta_link$country == "Uruguay"] <- "Plan de Atención Nacional a la Emergencia Social"
meta_link$name_of_the_program[meta_link$name_of_the_program == "PRODIAT" & meta_link$country == "Mexico"] <- "Programa para el Desarrollo de las Industrias de Alta Tecnología"
meta_link$name_of_the_program[meta_link$name_of_the_program == "PROBECAT" & meta_link$country == "Mexico"] <- "Programa de Becas de Capacitacion para Trabajadores Desempleados"
meta_link$name_of_the_program[meta_link$name_of_the_program == "PROBECAT/SICAT" & meta_link$country == "Mexico"] <- "Programa de Becas de Capacitacion para Trabajadores Desempleados (PROBECAT) y Sistema de Capacitación para el Trabajo (SICAT)"
meta_link$name_of_the_program[meta_link$name_of_the_program == "PRONATEC" & meta_link$country == "Brazil"] <- "Programa Nacional de Enseñanza Técnica y Empleo (PRONATEC)"
meta_link$name_of_the_program[meta_link$name_of_the_program == "PLANE" & meta_link$country == "Bolivia"] <- "Plan Nacional de Empleo de Emergencia"
meta_link$name_of_the_program[meta_link$name_of_the_program == "REPRO" & meta_link$country == "Argentina"] <- "Programa de Recuperación y Sostenimiento Productivo"

# Las bases con y sin url son identicas en los outcomes; exportamos solo la que incluye url's
all(meta_prog$name_of_the_program == meta_link$name_of_the_program)
all(meta_prog$n == meta_link$n)
all(meta_prog$country == meta_link$country)

#--------------------------#
# D. Demografia -por tipo de programa (con URL y abstract) ----
# Incluimos outcomes de programas orientados a jovenes y mujeres en la categoria general
#--------------------------#

meta_link_d <- meta_sample %>%
  # Quitar el signo a los no significativos
  mutate(across(.cols = starts_with("out_"), gsub, pattern = "\\\\.*", replacement = "")) %>%
  mutate(across(.cols = starts_with("out_"), gsub, pattern = "/.*", replacement = "")) %>%
  # Solo pais, programa y resultados
  dplyr::select(country, name_of_the_program, abstract, starts_with('url'), starts_with("out_")) %>%
  # Pasar de wide a long
  pivot_longer(cols = starts_with("out_"), names_to = "type_outcome", values_to = "value_outcome") %>%
  # Renombrar signos
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '+', 'positive'))) %>%
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '-', 'negative'))) %>%
  mutate(across(.cols = value_outcome, ~replace(., value_outcome == '−', 'negative'))) %>%
  # Solo observaciones con resultados reportados
  drop_na(value_outcome) %>%
  # Contar tipo de resultados por programa y pais
  count(country, name_of_the_program, abstract, url, url_prog, url_prog_d, type_outcome, value_outcome) %>%
  # Incluir a jovenes
  mutate(type_outcome = gsub("_y", "", type_outcome)) %>%
  # Incluir a mujeres solo si el programa es enfocado en ellas 
  # (para evitar reportar efectos heterogeneos de genero sin considerar hombres)
  mutate(type_outcome = ifelse(str_detect(abstract, "mujer"), gsub("_w", "", type_outcome), type_outcome)) %>%
  # Sin distinguir por grupo poblacional (genero, edad)
  dplyr::filter(!str_detect(type_outcome, '_m|_w|_y')) %>% 
  # Un mismo programa puede ser evaluado varias veces y tener resultados distintos
  # Es importante citar la evaluacion que reporta X resultado
  group_by(country, name_of_the_program, abstract, url_prog, url_prog_d, type_outcome, value_outcome) %>% 
  summarise(url = paste0(unique(url), collapse = "; "),
            # abstract = paste0(abstract, collapse = "; "),
            # url_prog = paste0(url_prog, collapse = "; "),
            # url_prog_d = paste0(url_prog_d, collapse = "; "),
            n = sum(n, na.rm = T)) %>% 
  ungroup()

# Corregimos siglas en los programas
meta_link_d$name_of_the_program[meta_link_d$name_of_the_program == "PANES" & meta_link_d$country == "Uruguay"] <- "Plan de Atención Nacional a la Emergencia Social"
meta_link_d$name_of_the_program[meta_link_d$name_of_the_program == "PRODIAT" & meta_link_d$country == "Mexico"] <- "Programa para el Desarrollo de las Industrias de Alta Tecnología"
meta_link_d$name_of_the_program[meta_link_d$name_of_the_program == "PROBECAT" & meta_link_d$country == "Mexico"] <- "Programa de Becas de Capacitacion para Trabajadores Desempleados"
meta_link_d$name_of_the_program[meta_link_d$name_of_the_program == "PROBECAT/SICAT" & meta_link_d$country == "Mexico"] <- "Programa de Becas de Capacitacion para Trabajadores Desempleados (PROBECAT) y Sistema de Capacitación para el Trabajo (SICAT)"
meta_link_d$name_of_the_program[meta_link_d$name_of_the_program == "PRONATEC" & meta_link_d$country == "Brazil"] <- "Programa Nacional de Enseñanza Técnica y Empleo (PRONATEC)"
meta_link_d$name_of_the_program[meta_link_d$name_of_the_program == "PLANE" & meta_link_d$country == "Bolivia"] <- "Plan Nacional de Empleo de Emergencia"
meta_link_d$name_of_the_program[meta_link_d$name_of_the_program == "REPRO" & meta_link_d$country == "Argentina"] <- "Programa de Recuperación y Sostenimiento Productivo"

# Numero de programas por pais
num_country <- meta_link_d %>% distinct(country, name_of_the_program)
num_country %>% count(country)

#--------------------------#
# E. Exportar ----
#--------------------------#

saveRDS(meta_policy, glue('{datos}/Matriz/base_pais_tipo_politica.rds'))
saveRDS(meta_link, glue('{datos}/Matriz/base_pais_tipo_programa.rds'))
saveRDS(meta_link_d, glue('{datos}/Matriz/base_pais_tipo_programa_demografia.rds'))



