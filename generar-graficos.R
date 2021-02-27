library(tidyverse)


# datos iniciales ---------------------------------------------------------

# rangos etáreos
rng_edad_lvl <- c(
  "0-9",
  "10-19",
  "20-29",
  "30-39",
  "40-49",
  "50-59",
  "60-69",
  "70-79",
  "80+"
)

positivos <- readRDS("datos/20210223-positivos-covid19-peru.rds")

fallecidos <- readRDS("datos/20210223-fallecidos-covid19-peru.rds")

poblacion <- readRDS("datos/peru-pob2020-departamentos.rds") %>%
  select(c(1:2,4:36)) %>%
  rowwise() %>%
  mutate(
    "grp_0-9" = sum(edad_0, edad_1, edad_2, edad_3, edad_4,
                    edad_5, edad_6, edad_7, edad_8, edad_9),
    "grp_10-19" = sum(edad_10, edad_11, edad_12, edad_13, edad_14,
                      edad_15, edad_16, edad_17, edad_18, edad_19),
    "grp_20-29" = sum(edad_20_24, edad_25_29),
    "grp_30-39" = sum(edad_30_34, edad_35_39),
    "grp_40-49" = sum(edad_40_44, edad_45_49),
    "grp_50-59" = sum(edad_50_54, edad_55_59),
    "grp_60-69" = sum(edad_60_64, edad_65_69),
    "grp_70-79" = sum(edad_70_74, edad_75_79)
  ) %>%
  rename(
    "grp_80+" = edad_80_mas
  ) %>%
  select(!starts_with("edad_")) %>%
  pivot_longer(
    cols = c(starts_with("grp_")),
    names_to = "grp_edad",
    names_prefix = "grp_",
    values_to = "pob2020"
  ) %>%
  group_by(departamento) %>%
  mutate(
    grp_edad = factor(grp_edad, labels = rng_edad_lvl, ordered = TRUE),
    pct_pob = pob2020 / sum(pob2020)
  ) %>%
  arrange(
    ubigeo,
    grp_edad
  )


# Perú --------------------------------------------------------------------

positivos_peru <- positivos %>%
  filter(!is.na(edad)) %>%
  mutate(
    grp_edad = cut(edad, c(seq(0, 80, 10), 130),
                   labels = rng_edad_lvl,
                   include.lowest = TRUE,
                   right = FALSE,
                   ordered = TRUE)
  ) %>%
  group_by(grp_edad) %>%
  tally(name = "n_positivos") %>%
  mutate(
    pct_positivos = n_positivos / sum(n_positivos)
  )

fallecidos_peru <- fallecidos %>%
  filter(!is.na(edad)) %>%
  mutate(
    grp_edad = cut(edad, c(seq(0, 80, 10), 130),
                   labels = rng_edad_lvl,
                   include.lowest = TRUE,
                   right = FALSE,
                   ordered = TRUE)
  ) %>%
  group_by(grp_edad) %>%
  tally(name = "n_fallecidos") %>%
  mutate(
    pct_fallecidos = n_fallecidos / sum(n_fallecidos)
  )

combinado_peru_long <- poblacion %>%
  filter(departamento == "PERU") %>%
  left_join(
    positivos_peru,
    by = c("grp_edad")
  ) %>%
  left_join(
    fallecidos_peru,
    by = c("grp_edad")
  ) %>%
  select(departamento, grp_edad,
         pct_pob, pct_positivos, pct_fallecidos) %>%
  pivot_longer(
    cols = c(starts_with("pct_")),
    names_to = "metrica",
    values_to = "pct"
  ) %>%
  mutate(
    metrica = case_when(
      metrica == "pct_pob" ~ "Población",
      metrica == "pct_positivos" ~ "Casos Positivos",
      metrica == "pct_fallecidos" ~ "Fallecidos"
    ),
    metrica = factor(
      metrica,
      levels = c("Población", "Casos Positivos", "Fallecidos"),
      ordered = TRUE
    )
  ) %>%
  ungroup() %>%
  arrange(
    departamento,
    grp_edad
  )

peru <- ggplot(
  combinado_peru_long,
  aes(x = pct, y = grp_edad,
      group = metrica, fill = grp_edad)
) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = sprintf("%.1f%%", 100 * pct)),
             nudge_x = .025, size = 5,
             label.size = 0,
             label.padding = unit(0, "cm"),
             fill = "white", show.legend = FALSE) +
  scale_fill_viridis_d(direction = -1) +
  scale_x_continuous(labels = scales::percent, expand = c(0.02, .02)) +
  labs(
    x = "",
    y = "Grupo etáreo (años)",
    title = "PERU: Distribución de población, positivos y fallecidos por COVID-19.",
    subtitle = "Rango de fechas: del 2020-03-06 al 2021-02-23. Población estimada al 2020 (INEI/MINSA).",
    caption = "Fuente: Datos abiertos de positivos y fallecidos por COVID-19 (MINSA)\n@jmcastagnetto, Jesus M. Castagnetto, 2021-02-26"
  ) +
  facet_wrap(~metrica) +
  ggthemes::theme_few(24) +
  theme(
    panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "gray50", size = 18),
    axis.text.x = element_blank(),
    plot.caption = element_text(family = "Inconsolata"),
    strip.text = element_text(face = "bold")
  )

ggsave(
  plot = peru,
  width = 16,
  height = 9,
  filename = "plots/20210226-peru-poblacion-positivos-fallecidos-dist.png"
)

# Departamentos -----------------------------------------------------------

positivos_dpto <- positivos %>%
  filter(!is.na(edad)) %>%
  mutate(
    departamento = as.character(departamento),
    departamento = if_else(
      departamento == "LIMA REGION",
      "LIMA",
      departamento
    ),
    grp_edad = cut(edad, c(seq(0, 80, 10), 130),
                   labels = rng_edad_lvl,
                   include.lowest = TRUE,
                   right = FALSE,
                   ordered = TRUE)
  ) %>%
  group_by(departamento, grp_edad) %>%
  tally(name = "n_positivos") %>%
  group_by(departamento) %>%
  mutate(
    pct_positivos = n_positivos / sum(n_positivos)
  )

fallecidos_dpto <- fallecidos %>%
  filter(!is.na(edad)) %>%
  mutate(
    departamento = as.character(departamento),
    departamento = if_else(
      departamento == "LIMA REGION",
      "LIMA",
      departamento
    ),
    grp_edad = cut(edad, c(seq(0, 80, 10), 130),
                   labels = rng_edad_lvl,
                   include.lowest = TRUE,
                   right = FALSE,
                   ordered = TRUE)
  ) %>%
  group_by(departamento, grp_edad) %>%
  tally(name = "n_fallecidos") %>%
  group_by(departamento) %>%
  mutate(
    pct_fallecidos = n_fallecidos / sum(n_fallecidos)
  )


combinados_dpto <- pob_dpto %>%
  filter(departamento != "PERU") %>%
  left_join(
    positivos_dpto,
    by = c("departamento", "grp_edad")
  ) %>%
  left_join(
    fallecidos_dpto,
    by = c("departamento", "grp_edad")
  )

combinados_dpto_long <- combinados_dpto %>%
  select(departamento, grp_edad,
         pct_pob, pct_positivos, pct_fallecidos) %>%
  pivot_longer(
    cols = c(starts_with("pct_")),
    names_to = "metrica",
    values_to = "pct"
  ) %>%
  mutate(
    metrica = case_when(
      metrica == "pct_pob" ~ "Población",
      metrica == "pct_positivos" ~ "Casos Positivos",
      metrica == "pct_fallecidos" ~ "Fallecidos"
    ),
    metrica = factor(
      metrica,
      levels = c("Población", "Casos Positivos", "Fallecidos"),
      ordered = TRUE
    )
  ) %>%
  ungroup() %>%
  arrange(
    departamento,
    grp_edad
  )

mk_comb_plot <- function(df, dpto) {
  df <- combinados_dpto_long %>%
    filter(departamento == dpto)
  ggplot(
    df,
    aes(x = pct, y = grp_edad,
        group = metrica, fill = grp_edad)
  ) +
    geom_col(show.legend = FALSE) +
    geom_label(aes(label = sprintf("%.1f%%", 100 * pct)),
               nudge_x = .025, size = 5,
               label.size = 0,
               label.padding = unit(0, "cm"),
               fill = "white", show.legend = FALSE) +
    scale_fill_viridis_d(direction = -1) +
    scale_x_continuous(labels = scales::percent, expand = c(0.02, .02)) +
    labs(
      x = "",
      y = "Grupo etáreo (años)",
      title = glue::glue("{dpto}: Distribución de población, positivos y fallecidos por COVID-19."),
      subtitle = "Rango de fechas: del 2020-03-06 al 2021-02-23. Población estimada al 2020 (INEI/MINSA).",
      caption = "Fuente: Datos abiertos de positivos y fallecidos por COVID-19 (MINSA)\n@jmcastagnetto, Jesus M. Castagnetto, 2021-02-26"
    ) +
    facet_wrap(~metrica) +
    ggthemes::theme_few(24) +
    theme(
      panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
      plot.margin = unit(rep(1, 4), "cm"),
      plot.title.position = "plot",
      plot.subtitle = element_text(color = "gray50", size = 18),
      axis.text.x = element_blank(),
      plot.caption = element_text(family = "Inconsolata"),
      strip.text = element_text(face = "bold")
    )
}

dptos <- unique(combinados_dpto$departamento)

for (dpto in dptos) {
  dpto_lbl <- str_to_lower(dpto) %>%
    str_replace_all(" ", "_")
  fn <- glue::glue("plots/20210226-{dpto_lbl}-poblacion-positivos-fallecidos-dist.png")
  p1 <- mk_comb_plot(combinados_dpto_long, dpto)
  ggsave(
    plot = p1,
    width = 16,
    height = 9,
    filename = fn
  )
}

