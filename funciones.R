calcular_brechas <- function(data) {
  if (!("n" %in% names(data))) {
    data <- data |> 
      summarize(n = n(), .groups = "drop_last")
  }
    
  data |>
    mutate(p = n/sum(n)) |> 
    mutate(brecha = p - max(p)) |> 
    mutate(mayoria = case_when(p == max(p) & p != 0.5 ~ genero, 
                               p == 0.5 ~ "igualdad",
                               .default = NA)) |>
    fill(mayoria, .direction = "updown") |> 
    mutate(desigual = case_when(genero == "femenino" & p != max(p) ~ TRUE,
                                genero == "femenino" & p == max(p) ~ FALSE,
                                genero == "masculino" & p == 1 ~ TRUE,
                                .default = NA)) |> 
    fill(desigual, .direction = "updown")
}
