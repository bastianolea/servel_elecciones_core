source("funciones.R")



# calcular
concejales <- concejales |> 
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
  
  
# comunas sin mujeres
concejales |> 
  filter(n_femenino == 0) |> 
  print(n=Inf)

# comunas mas cercanas a la igualdad
concejales |> 
  filter(mayoria != "femenino" & mayoria != "igualdad") |> 
  arrange(desc(brecha))

# comunas más desiguales
concejales |> 
  filter(mayoria == "masculino" & n_femenino != 0) |> 
  arrange(brecha)
