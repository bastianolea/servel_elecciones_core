library(readxl)
library(janitor)
library(dplyr)

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

library(tictoc)
library(mall)

# llm_use("ollama", "llama3.1:8b", temperature = 0.5, .cache = "") 
llm_use("ollama", "llama3.2:3b", temperature = 0.3, .cache = "") 

# unir nombres de electos
nombres <- bind_rows(concejales |> filter(!is.na(cargo)) |> select(nombres),
                     alcaldes |> filter(!is.na(cargo)) |> select(nombres),
                     cores |> filter(!is.na(cargo)) |> select(nombres)) |> distinct()


tic()
genero <- nombres |> 
  llm_classify(col = nombres, pred_name = "genero",
               labels = c("masculino", "femenino", "desconocido"), 
               additional_prompt = "obtener el género desde nombres de personas")

genero |> print(n=Inf)

toc()


genero |>
  filter(genero == "desconocido")

# correcciones manuales
genero2 <- genero |> 
  mutate(genero = case_match(nombres,
                             "SEGUNDO" ~ "masculino",
                             "WASHINGTON" ~ "masculino",
                             "EMA" ~ "femenino",
                             "THAE" ~ "femenino",
                             "NEMER" ~ "masculino",
                             "JHON" ~ "masculino",
                             "DALIVOR" ~ "masculino",
                             "ARCIDES" ~ "masculino",
                             "ALIRO" ~ "masculino",
                             "ANAHI" ~ "femenino",
                             "NANO" ~ "masculino",
                             "OSIEL" ~ "masculino",
                             "BERNES" ~ "masculino",
                             "ALEN" ~ "masculino",
                             .default = genero))


# guardar resultados
readr::write_rds(genero2, "genero.rds")
