source("procesar.R")

library(tidyverse)
library(ggplot2)
library(ggview)
library(patchwork)

cores_2

.region = 15


# cores ----

for (.region in 1:16) {
  
  cores_region <- cores_2 |> 
    filter(codigo_region == .region)
  
  cores_conteo <- cores_region |> 
    count(partido, sort = TRUE) |> 
    mutate(partido = fct_reorder(partido, n, .desc = T))
  
  nombre_region <- cores_region |> 
    distinct(nombre_region) |> pull()
  
  message(nombre_region)
  
  n_partidos <- nrow(cores_conteo)
  max_votos <- max(cores_conteo$n)
  
  cores_conteo_unc <- cores_conteo |> 
    uncount(n) |> 
    group_by(partido) |> 
    mutate(n = row_number(),
           total = max(n))
  
  cores_conteo_unc |> 
    ggplot() +
    aes(partido, n, color = partido) +
    geom_point(size = 12) +
    geom_text(aes(label = total, y = total + 0.6), size = 5, check_overlap = TRUE, 
              family = "gobCL-Heavy") +
    # scale_y_continuous(limits = c(min(cores_conteo_unc$n)-0.5, max(cores_conteo_unc$n)+0.5), 
    #                    expand = expansion(c(0, 0))) +
    scale_y_continuous(limits = c(min(cores_conteo_unc$n)-0.6, max(cores_conteo_unc$n)+0.7),
                       expand = expansion(c(0, 0.05))) +
    scale_x_discrete(labels = label_wrap_gen(10)) +
    # coord_fixed(clip = "off") +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = .85) +
    guides(color = guide_none()) +
    theme_void(base_size = 14, base_family = "gobCL") +
    theme(plot.margin = margin(t = 6, b = 12, l = 6, r = 0)) +
    theme(axis.text.x = element_text(size = 9, lineheight = .8, face = "bold", vjust = 1)) +
    theme(plot.subtitle = element_text(family = "gobCL-Heavy")) +
    labs(title = "Consejeros regionales",
         subtitle = nombre_region,
         y = "Consejeros Regionales electos",
         caption = "Fuente: Servicio Electoral de Chile",
         x = NULL)
    # canvas(n_partidos*0.8, 1+(max_votos*0.8))
  
  # ggsave(paste0("graficos/cores_", .region, ".jpg"), width = n_partidos*0.8, height = 1+(max_votos*0.8))
}

# cores_conteo_unc
# 
# cores_region |> 
#   pivot_longer(cols = c(nombres, partido, pacto))
# 
# 
# cores_region |> 
#   filter(partido == "Independientes") |> 
#   count(pacto)


# género ----


for (.region in 1:16) {
  
  cores_region <- cores_2 |> 
    filter(codigo_region == .region)
  
  cores_conteo <- cores_region |> 
    count(partido, genero, sort = TRUE) |> 
    mutate(total = sum(n), .by = partido) |> 
    mutate(partido = fct_reorder(partido, n, .desc = T))
  
  nombre_region <- cores_region |> 
    distinct(nombre_region) |> pull()
  
  message(nombre_region)
  
  n_partidos <- n_distinct(cores_conteo$partido)
  max_votos <- max(cores_conteo$total)
  
  cores_conteo_unc <- cores_conteo |> 
    uncount(n) |> 
    group_by(partido) |> 
    mutate(n = row_number(),
           total = max(n)) |> 
    ungroup() |> 
    mutate(partido = fct_reorder(partido, total, .desc = T))
  
  cores_conteo_unc |> 
    ggplot() +
    aes(partido, n, 
        color = partido, shape = genero, size = genero) +
    geom_point() +
    geom_text(aes(label = total, y = total + 0.6), size = 5, check_overlap = TRUE, 
              family = "gobCL-Heavy") +
    # scale_y_continuous(limits = c(min(cores_conteo_unc$n)-0.5, max(cores_conteo_unc$n)+0.5), 
    #                    expand = expansion(c(0, 0))) +
    scale_y_continuous(limits = c(min(cores_conteo_unc$n)-0.6, max(cores_conteo_unc$n)+0.7),
                       expand = expansion(c(0, 0.05))) +
    scale_x_discrete(labels = label_wrap_gen(10)) +
    # coord_fixed(clip = "off") +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = .85) +
    scale_shape_manual(values = c("masculino" = "square", "femenino" = "circle")) +
    scale_size_manual(values = c("masculino" = 12, "femenino" = 13)) +
    guides(color = guide_none(),
           size = guide_none(),
           shape = guide_legend(override.aes = list(size = 5, color = "#4B1E4B"), position = "inside", title = "Género")) +
    theme_void(base_size = 14, base_family = "gobCL") +
    theme(plot.margin = margin(t = 6, b = 12, l = 6, r = 0)) +
    theme(axis.text.x = element_text(size = 9, lineheight = .8, face = "bold", vjust = 1)) +
    theme(plot.subtitle = element_text(family = "gobCL-Heavy")) +
    theme(legend.position.inside = c(.95, 1.1),
          legend.justification = c(1, 1),
          legend.text = element_text(margin = margin(l = 3))) +
    labs(title = "Consejeros regionales",
         subtitle = nombre_region,
         y = "Consejeros Regionales electos",
         caption = "Fuente: Servicio Electoral de Chile",
         x = NULL) +
  canvas(n_partidos*0.8, 1+(max_votos*0.8))
  
  # ggsave(paste0("graficos/cores_genero_", .region, ".jpg"), width = n_partidos*0.8, height = 1+(max_votos*0.8))
  # save_ggplot(last_plot(), paste0("graficos/cores_genero_", .region, ".jpg"), width = n_partidos*0.8, height = 1+(max_votos*0.8))
}



# genero + porcentaje ----

for (.region in 1:16) {
  
  cores_region <- cores_2 |> 
    filter(codigo_region == .region)
  
  cores_conteo <- cores_region |> 
    count(partido, genero, sort = TRUE) |> 
    mutate(total = sum(n), .by = partido) |> 
    mutate(partido = fct_reorder(partido, n, .desc = T))
  
  nombre_region <- cores_region |> 
    distinct(nombre_region) |> pull()
  
  message(nombre_region)
  
  n_partidos <- n_distinct(cores_conteo$partido)
  max_votos <- max(cores_conteo$total)
  
  cores_conteo_unc <- cores_conteo |> 
    uncount(n) |> 
    group_by(partido) |> 
    mutate(n = row_number(),
           total = max(n)) |> 
    ungroup() |> 
    mutate(partido = fct_reorder(partido, total, .desc = T))
  
  grafico_conteo <- cores_conteo_unc |> 
    ggplot() +
    aes(partido, n, 
        color = partido, shape = genero, size = genero) +
    geom_point() +
    geom_text(aes(label = total, y = total + 0.6), size = 5, check_overlap = TRUE, 
              family = "gobCL-Heavy") +
    # scale_y_continuous(limits = c(min(cores_conteo_unc$n)-0.5, max(cores_conteo_unc$n)+0.5), 
    #                    expand = expansion(c(0, 0))) +
    scale_y_continuous(limits = c(min(cores_conteo_unc$n)-0.6, max(cores_conteo_unc$n)+0.7),
                       expand = expansion(c(0, 0.05))) +
    scale_x_discrete(labels = label_wrap_gen(10)) +
    # coord_fixed(clip = "off") +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = .85) +
    scale_shape_manual(values = c("masculino" = "square", "femenino" = "circle"),
                       labels = c("femenino" = "Femenino", "masculino" = "Masculino")) +
    scale_size_manual(values = c("masculino" = 12, "femenino" = 13)) +
    guides(color = guide_none(),
           size = guide_none(),
           shape = guide_legend(nrow = 1, override.aes = list(size = 5, color = "#4B1E4B"), position = "inside", title = "Género")) +
    theme_void(base_size = 14, base_family = "gobCL") +
    theme(plot.margin = margin(t = 6, b = 12, l = 6, r = 0)) +
    theme(axis.text.x = element_text(size = 9, lineheight = .8, face = "bold", vjust = 1)) +
    theme(plot.subtitle = element_text(family = "gobCL-Heavy")) +
    theme(legend.position.inside = c(.98, 1.04),
          legend.justification = c(1, 1),
          legend.text = element_text(margin = margin(l = 3))) +
    labs(title = "Consejeros regionales",
         subtitle = nombre_region,
         y = "Consejeros Regionales electos",
         x = NULL) +
    canvas(n_partidos*0.8, 1+(max_votos*0.8))
  
  grafico_porcentaje <- cores_conteo |>
  # cores_conteo |> 
    summarize(n = sum(n), .by = genero) |> 
    mutate(p = n/sum(n)) |> 
    ggplot(aes(genero, p)) +
    geom_col(aes(y = c(1, 1)), width = .7, fill = "grey95") +
    geom_col(aes(fill = genero), width = .7, show.legend = F) +
    geom_text(aes(label = scales::percent(p, 0.1, decimal.mark = ","),
                  vjust = ifelse(p > .4, 1, 0),
                  y = ifelse(p > .4, p - .02, p + .02),
                  color = ifelse(p > .4, "white", "black")),
              family = "gobCL-Bold", size = 3) +
    scale_color_identity() +
    scale_y_continuous(limits = c(0, 1),
                       expand = expansion(c(0.05, 0.05))) +
    scale_x_discrete(labels = c("femenino" = "Femenino", "masculino" = "Masculino")) +
    scale_fill_manual(values = c("femenino" = "#9B1B5B", "masculino" = "#E13442")) +
    theme_void(base_size = 14, base_family = "gobCL") +
    theme(axis.text.x = element_text(size = 9, lineheight = .8, face = "bold", vjust = 1)) +
    labs(subtitle = "Porcentajes")
  
  # unir
  grafico_conteo + plot_spacer() + grafico_porcentaje +
    plot_layout(nrow = 1, widths = c(3, .06, .7)) +
    plot_annotation(caption = "Fuente: Servicio Electoral de Chile, resultados electorales de Consejeros Regionales 2024") &
    theme(plot.caption = element_text(family = "gobCL", coloR = "#7F7F7F"))
    # theme(plot.margin = margin(b = 4, r = 4, l = 4)) +

  
  save_ggplot(last_plot(), paste0("graficos/cores_genero_porcentaje_", .region, ".jpg"),
              width = 2.2+(n_partidos*0.8), height = 1+(max_votos*0.8))
}
 