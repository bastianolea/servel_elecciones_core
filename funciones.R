library(ggplot2)
library(ggtext)

masculino = "#957dad"
femenino = "#d291bc"


ordenar_regiones <- function(data) {
  data |> 
    mutate(nombre_region = factor(nombre_region,
                                  levels = c("Arica y Parinacota",
                                             "Tarapacá",
                                             "Antofagasta",
                                             "Atacama",
                                             "Coquimbo",
                                             "Valparaíso",
                                             "Metropolitana de Santiago",
                                             "Libertador General Bernardo O'Higgins",
                                             "Maule",
                                             "Ñuble",
                                             "Biobío",
                                             "La Araucanía",
                                             "Los Ríos",
                                             "Los Lagos",
                                             "Aysén del General Carlos Ibáñez del Campo",
                                             "Magallanes y de la Antártica Chilena"))
    )
}


corregir_comunas <- function(data) {
  data |> 
    mutate(comuna_b = tolower(comuna),
           comuna_b = recode(comuna_b, 
                             "paihuano" = "paiguano",
                             "trehuaco" = "treguaco",
                             "llay-llay" = "llaillay",
                             "marchigue" = "marchihue",
                             "cabo de hornos(ex-navarino)" = "cabo de hornos")) |> 
    left_join(comunas,
              by = "comuna_b") |> 
    select(-comuna, -comuna_b) |> 
    relocate(codigo_region, nombre_region, codigo_comuna, nombre_comuna,
             .before = 1) |> 
    # orden geográfico de regiones
    ordenar_regiones()
}



grafico_pais <- function(data) {
  data |> 
    ggplot(aes(x = 1, y = p, fill = genero)) +
    geom_col(color = "white", linewidth = 0.7) +
    geom_text(aes(label = percent(p)), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
    coord_polar(theta = "y") +
    theme_void(base_family = "Helvetica") +
    guides(fill = guide_none()) +
    scale_fill_manual(values = c("masculino" = masculino, "femenino" = femenino))
}



color <- function(texto, color) {
  paste0("<span style = 'color:", color, ";'>", texto, "</span>")
}

tema_torta <- list(
  theme(plot.title = element_markdown(size = 15),
        plot.subtitle = element_markdown(size = 13, margin = margin(t = 6, b = -15))) +
    theme(plot.margin = unit(c(2, 0, -4, 0), "mm"))
)


tema_barras_horizontales <- list(
  theme(plot.margin = unit(c(4, 6, 4, 6), "mm")),
  theme(plot.title = element_markdown(size = 15),
        plot.subtitle = element_markdown(size = 13, margin = margin(t = 4, b = 8)),
        plot.title.position = "plot")
)

grafico_regiones <- function(data) {
  data |> 
    ggplot(aes(x = p, y = nombre_region, fill = genero)) +
    geom_col(color = "white", linewidth = 0.7, width = .7) +
    geom_text(aes(label = percent(p, 1)), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
    theme_void() +
    theme(axis.text.y = element_text(hjust = 1)) +
    scale_y_discrete(labels = label_wrap(30)) +
    scale_x_continuous(expand = expansion(c(0.02, 0))) +
    guides(fill = guide_none()) +
    scale_fill_manual(values = c("masculino" = masculino, "femenino" = femenino)) +
    scale_y_discrete(limits = rev) 
}