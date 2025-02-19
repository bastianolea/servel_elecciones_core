library(readr)
library(dplyr)
library(tidyr)

source("funciones.R")


# cargar ----
concejales <- read_csv2("datos/resultados_servel_2024_concejales.csv") |> ordenar_regiones()
alcaldes <- read_csv2("datos/resultados_servel_2024_alcaldes.csv") |> ordenar_regiones()
cores <- read_csv2("datos/resultados_servel_2024_cores.csv") |> ordenar_regiones()



# concejales ----

concejales |> 
  filter(genero == "femenino") |> 
  slice_sample(n = 10) |> 
  relocate(nombres, .before = genero)

# calcular
concejales_w <- concejales |> 
  group_by(nombre_region, nombre_comuna, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha =  p_femenino - p_masculino) |> 
  mutate(mayoria = case_when(p_masculino > p_femenino ~ "masculino",
                             p_femenino > p_masculino ~ "femenino",
                             .default = "igualdad")) |> 
  mutate(igualdad = if_else(p_femenino == 0.5, TRUE, FALSE)) |> 
  ungroup()

concejales_w


# nombre_comunas sin mujeres
concejales_w |> 
  filter(n_femenino == 0) |> 
  print(n=Inf)

# nombre_comunas mas cercanas a la igualdad
concejales_w |> 
  filter(mayoria != "femenino" & mayoria != "igualdad") |> 
  arrange(desc(brecha))

# nombre_comunas más desiguales
concejales_w |> 
  filter(mayoria == "masculino" & n_femenino != 0) |> 
  arrange(brecha)



## nombre_comuna ----
concejales |> 
  group_by(nombre_region, nombre_comuna, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha = p_femenino - p_masculino)

## nombre_region ----
concejales |> 
  group_by(nombre_region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha = p_femenino - p_masculino) |> 
  arrange(brecha)

## pais ----
concejales |> 
  group_by(genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha = p_femenino - p_masculino)



# alcaldes ----
alcaldes_w <- alcaldes |> 
  group_by(nombre_region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha =  p_femenino - p_masculino) |> 
  mutate(mayoria = case_when(p_masculino > p_femenino ~ "masculino",
                             p_femenino > p_masculino ~ "femenino",
                             .default = "igualdad")) |> 
  mutate(igualdad = if_else(p_femenino == 0.5, TRUE, FALSE)) |> 
  ungroup()

alcaldes_w

alcaldes |> 
  filter(nombre_region == "DE ÑUBLE") |> 
  print(n=Inf)

alcaldes |> 
  filter(nombre_region == "DEL MAULE") |> 
  print(n=Inf)

alcaldes |> 
  filter(nombre_region == "DE ARICA Y PARINACOTA") |> 
  print(n=Inf)

alcaldes |> 
  filter(nombre_region == "METROPOLITANA DE SANTIAGO") |> 
  arrange(genero) |> 
  print(n=Inf)


## nombre_region ----
alcaldes |> 
  group_by(nombre_region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha = p_femenino - p_masculino) |> 
  arrange(brecha)

## pais ----
alcaldes |> 
  group_by(genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha = p_femenino - p_masculino)





# cores ----
cores |> 
  filter(nombre_region == "DE ATACAMA") |> 
  arrange(desc(votos)) |> 
  distinct(nombres, primer_apellido, .keep_all = TRUE) |> 
  print(n=Inf)

cores |> 
  filter(nombre_region == "DE MAGALLANES Y DE LA ANTARTICA CHILENA") |> 
  arrange(desc(votos)) |> 
  distinct(nombres, primer_apellido, .keep_all = TRUE) |> 
  print(n=Inf)


cores_w <- cores |> 
  group_by(nombre_region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha =  p_femenino - p_masculino) |> 
  mutate(mayoria = case_when(p_masculino > p_femenino ~ "masculino",
                             p_femenino > p_masculino ~ "femenino",
                             .default = "igualdad")) |> 
  mutate(igualdad = if_else(p_femenino == 0.5, TRUE, FALSE)) |> 
  ungroup()

cores_w

## nombre_region ----
cores |> 
  group_by(nombre_region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha = p_femenino - p_masculino)

## pais ----
cores |> 
  group_by(genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha = p_femenino - p_masculino)




# guardar ----
writexl::write_xlsx(list(
  concejales = concejales_w,
                         alcaldes = alcaldes_w,
                         cores = cores_w), 
  path = "datos/servel_brechas.xlsx")
