
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
  filter(!is.na(cargo)) |>
  left_join(genero, by = "nombres") |>
  select(region, comuna, lista, pacto, nombres, primer_apellido, votos, genero)

alcaldes_2 <- alcaldes |>
  filter(!is.na(cargo)) |>
  left_join(genero, by = "nombres") |>
  select(region, comuna, lista, pacto, nombres, primer_apellido, votos, genero)

cores_2 <- cores |>
  filter(!is.na(cargo)) |>
  left_join(genero, by = "nombres") |>
  select(region, comuna, lista, pacto, nombres, primer_apellido, votos, genero)



# concejales ----

## comuna ----
concejales_2 |> 
  group_by(region, comuna, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

## region ----
concejales_2 |> 
  group_by(region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

## pais ----
concejales_2 |> 
  group_by(genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))


# alcaldes ----

## region ----
alcaldes_2 |> 
  group_by(region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

## pais ----
alcaldes_2 |> 
  group_by(genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))



# cores ----
## comuna ----
cores_2 |> 
  group_by(region, comuna, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

## region ----
cores_2 |> 
  group_by(region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

## pais ----
cores_2 |> 
  group_by(genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

