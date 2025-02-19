library(janitor)
library(readxl)
library(readr)
library(dplyr)
library(stringr)

source("funciones.R")

# cargar ----
concejales <- read_xlsx("datos/datos_originales/2024_10_Concejales_DatosEleccion.xlsx", 
                        sheet = "Votación por comuna", 
                        skip = 3) |> 
  clean_names()

alcaldes <- read_xlsx("datos/datos_originales/2024_10_Alcaldes_Datos_Eleccion.xlsx", 
                      sheet = "Votación por comuna", 
                      skip = 3) |> 
  clean_names()

cores <- read_xlsx("datos/datos_originales/2024_10_ConsejerosRegionales_DatosEleccion.xlsx", 
                   sheet = "Votación por comuna", 
                   skip = 3) |> 
  clean_names()

# calculado en clasificar_genero.R
genero <- readr::read_rds("genero.rds")


alcaldes |> 
  distinct(region) |> 
  mutate(region = stringr::str_to_title(region),
         region = stringr::str_replace(region, " La ", " la"),
         region = stringr::str_replace(region, " De ", " de "),
         region = stringr::str_replace(region, " Del ", " del "),
         region = stringr::str_replace(region, " Y ", " y "))


# cargar comunas
comunas <- read_rds("datos/cut_comuna.rds") |> 
  mutate(comuna_b = tolower(nombre_comuna),
         comuna_b = stringr::str_replace_all(comuna_b, 
                                             c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u",
                                               "ü"="u"))
  )


# limpiar ----
concejales_2 <- concejales |>
  corregir_comunas() |> 
  # solo electos
  filter(!is.na(cargo)) |>
  left_join(genero, by = "nombres") |>
  select(nombre_region, codigo_region, nombre_comuna, codigo_comuna, lista, pacto, nombres, primer_apellido, votos, genero)

alcaldes_2 <- alcaldes |>
  corregir_comunas() |> 
  # solo electos
  filter(!is.na(cargo)) |>
  left_join(genero, by = "nombres") |>
  select(nombre_region, codigo_region, nombre_comuna, codigo_comuna, lista, pacto, nombres, primer_apellido, votos, genero)

cores_2 <- cores |>
  corregir_comunas() |> 
  # solo electos
  filter(!is.na(cargo)) |>
  # sumar por región
  group_by(nombre_region, codigo_region, lista, pacto, nombres, primer_apellido) |> 
  summarize(votos = sum(votos)) |> 
  left_join(genero, by = "nombres") |>
  select(nombre_region, codigo_region, lista, pacto, nombres, primer_apellido, votos, genero)



# guardar ----
write_csv2(concejales_2, "datos/resultados_servel_2024_concejales.csv")
write_csv2(alcaldes_2, "datos/resultados_servel_2024_alcaldes.csv")
write_csv2(cores_2, "datos/resultados_servel_2024_cores.csv")



# # candidaturas ----
# 
# concejales_b <- concejales |>
#   # solo electos
#   filter(!is.na(cargo)) |>
#   left_join(genero, by = "nombres") |>
#   select(region, comuna, lista, pacto, nombres, primer_apellido, votos, genero)
# 
# alcaldes_b <- alcaldes |>
#   # solo electos
#   filter(!is.na(cargo)) |>
#   left_join(genero, by = "nombres") |>
#   select(region, comuna, lista, pacto, nombres, primer_apellido, votos, genero)
# 
# cores_b <- cores |>
#   # solo electos
#   filter(!is.na(cargo)) |>
#   # sumar por región
#   group_by(region, lista, pacto, nombres, primer_apellido) |> 
#   summarize(votos = sum(votos)) |> 
#   left_join(genero, by = "nombres") |>
#   select(region, lista, pacto, nombres, primer_apellido, votos, genero)
# 
