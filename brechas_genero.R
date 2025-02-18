library(readr)
library(dplyr)
library(tidyr)

# cargar ----
concejales <- read_csv2("datos/resultados_servel_2024_concejales.csv")
alcaldes <- read_csv2("datos/resultados_servel_2024_alcaldes.csv")
cores <- read_csv2("datos/resultados_servel_2024_cores.csv")



# concejales ----

concejales |> 
  filter(genero == "femenino") |> 
  slice_sample(n = 10) |> 
  relocate(nombres, .before = genero)


## comuna ----
concejales |> 
  group_by(region, comuna, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n)) |> 
  select(-n) |> 
  pivot_wider(names_from = genero, values_from = p, values_fill = 0)

## region ----
concejales |> 
  group_by(region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

## pais ----
concejales |> 
  group_by(genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))


# alcaldes ----

## region ----
alcaldes |> 
  group_by(region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

## pais ----
alcaldes |> 
  group_by(genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))



# cores ----
## comuna ----
cores |> 
  group_by(region, comuna, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

## region ----
cores |> 
  group_by(region, genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

## pais ----
cores |> 
  group_by(genero) |> 
  summarize(n = n()) |> 
  mutate(p = n/sum(n))

