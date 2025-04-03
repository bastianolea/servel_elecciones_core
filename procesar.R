library(janitor)
library(readxl)
library(readr)
library(dplyr)
library(stringr)

source("funciones.R")

# https://www.servel.cl/centro-de-datos/resultados-electorales-historicos-gw3/


# cargar ----
cores <- read_xlsx("datos/datos_originales/2024_10_ConsejerosRegionales_DatosEleccion.xlsx", 
                   sheet = "Votación por comuna", 
                   skip = 3) |> 
  clean_names()

# calculado en clasificar_genero.R
genero <- readr::read_rds("genero.rds")


# cargar comunas
comunas <- read_rds("datos/cut_comuna.rds") |> 
  mutate(comuna_b = tolower(nombre_comuna),
         comuna_b = stringr::str_replace_all(comuna_b, 
                                             c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u",
                                               "ü"="u"))
  )


# limpiar ----
cores_2 <- cores |>
  corregir_comunas() |> 
  # solo electos
  filter(!is.na(cargo)) |>
  # sumar por región
  group_by(nombre_region, codigo_region, lista, pacto, partido, nombres, primer_apellido) |> 
  summarize(votos = sum(votos)) |> 
  ungroup() |> 
  left_join(genero, by = "nombres") |>
  select(nombre_region, codigo_region, lista, pacto, partido, nombres, primer_apellido, votos, genero) |> 
  # partidos
  mutate(partido = str_to_title(partido),
         partido = str_replace_all(partido, c(" De "=" de ", " La "=" la ")))



# guardar ----
write_csv2(cores_2, "datos/resultados_servel_2024_cores.csv")
