library(readr)
library(dplyr)
library(tidyr)

source("funciones.R")


# cargar ----
concejales <- read_csv2("datos/resultados_servel_2024_concejales.csv")
alcaldes <- read_csv2("datos/resultados_servel_2024_alcaldes.csv")
cores <- read_csv2("datos/resultados_servel_2024_cores.csv")



# concejales ----

concejales |> 
  filter(genero == "femenino") |> 
  slice_sample(n = 10) |> 
  relocate(nombres, .before = genero)

# calcular
concejales_w <- concejales |> 
  group_by(region, comuna, genero) |> 
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


# comunas sin mujeres
concejales_w |> 
  filter(n_femenino == 0) |> 
  print(n=Inf)

# comunas mas cercanas a la igualdad
concejales_w |> 
  filter(mayoria != "femenino" & mayoria != "igualdad") |> 
  arrange(desc(brecha))

# comunas más desiguales
concejales_w |> 
  filter(mayoria == "masculino" & n_femenino != 0) |> 
  arrange(brecha)



## comuna ----
concejales |> 
  group_by(region, comuna, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  pivot_wider(names_from = genero, values_from = c(n, p), values_fill = 0) |> 
  mutate(brecha = p_femenino - p_masculino)

## region ----
concejales |> 
  group_by(region, genero) |> 
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
  group_by(region, genero) |> 
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
  filter(region == "DE ÑUBLE") |> 
  print(n=Inf)

alcaldes |> 
  filter(region == "DEL MAULE") |> 
  print(n=Inf)

alcaldes |> 
  filter(region == "DE ARICA Y PARINACOTA") |> 
  print(n=Inf)

alcaldes |> 
  filter(region == "METROPOLITANA DE SANTIAGO") |> 
  arrange(genero) |> 
  print(n=Inf)


## region ----
alcaldes |> 
  group_by(region, genero) |> 
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
  filter(region == "DE ATACAMA") |> 
  arrange(desc(votos)) |> 
  distinct(nombres, primer_apellido, .keep_all = TRUE) |> 
  print(n=Inf)

cores |> 
  filter(region == "DE MAGALLANES Y DE LA ANTARTICA CHILENA") |> 
  arrange(desc(votos)) |> 
  distinct(nombres, primer_apellido, .keep_all = TRUE) |> 
  print(n=Inf)


cores_w <- cores |> 
  group_by(region, genero) |> 
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

## region ----
cores |> 
  group_by(region, genero) |> 
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

