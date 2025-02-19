library(janitor)
library(readxl)
library(readr)
library(dplyr)

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
                             


# limpiar ----

concejales_2 <- concejales |>
  # solo electos
  filter(!is.na(cargo)) |>
  left_join(genero, by = "nombres") |>
  select(region, comuna, lista, pacto, nombres, primer_apellido, votos, genero)

alcaldes_2 <- alcaldes |>
  # solo electos
  filter(!is.na(cargo)) |>
  left_join(genero, by = "nombres") |>
  select(region, comuna, lista, pacto, nombres, primer_apellido, votos, genero)

cores_2 <- cores |>
  # solo electos
  filter(!is.na(cargo)) |>
  # sumar por región
  group_by(region, lista, pacto, nombres, primer_apellido) |> 
  summarize(votos = sum(votos)) |> 
  left_join(genero, by = "nombres") |>
  select(region, lista, pacto, nombres, primer_apellido, votos, genero)



# guardar ----
write_csv2(concejales_2, "datos/resultados_servel_2024_concejales.csv")
write_csv2(alcaldes_2, "datos/resultados_servel_2024_alcaldes.csv")
write_csv2(cores_2, "datos/resultados_servel_2024_cores.csv")