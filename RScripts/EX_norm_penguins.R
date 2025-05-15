pacman::p_load(conflicted, wrappedtools, tidyverse, here, ggbeeswarm, ggsci, ggsignif, ggthemes, ggridges, patchwork)
conflicts_prefer(dplyr::filter)

# Explore Normallity in the Penguins data set.
# Use figures and various Tests, in total sample as well as sub-groups

numvars <-
  ColSeeker(
    data = penguins, # can be omitted, as it is the default
    namepattern = "_"
  )

clean_data_penguins <- penguins |>
  drop_na()

# ungrouped visual analysis

G1 <- clean_data_penguins |>
  ggplot(aes(x = `flipper_len`)) +
  geom_density(alpha = .4, fill = "pink")

G2 <- clean_data_penguins |>
  ggplot(aes(x = `bill_len`)) +
  geom_density(alpha = .4, fill = "pink")

G3 <- clean_data_penguins |>
  ggplot(aes(x = `bill_dep`)) +
  geom_density(alpha = .4, fill = "pink")

G4 <- clean_data_penguins |>
  ggplot(aes(x = `body_mass`)) +
  geom_density(alpha = .4, fill = "pink")

# alle Graphen nebeneinander darstellen lassen
G5 <- G1 + G2 + G3 + G4


# creating the figures in a loop
for (var_i in numvars$name) {
  plot_temp <-
    ggplot(clean_data_penguins, aes(x = .data[[var_i]])) +
    geom_density()
  print(plot_temp)
}

# ungrouped testing with a loop

# eine leere Tabelle mit den numerischen Variabeln erstellen
penguins_normal <- tibble(
  Variables = numvars$names,
  pKS = NA_real_,
  pSh = NA_real_
)

# Tabelle mit den p Values füllen
for (var_i in seq_len(numvars$count)) {
  penguins_normal$pKS[var_i] <-
    ksnormal(clean_data_penguins[[numvars$names[var_i]]])
  penguins_normal$pSh[var_i] <-
    shapiro.test(clean_data_penguins |>
      pull(numvars$names[var_i]))$p.value
}

head(penguins_normal)

# Tabelle formatieren
penguins_normal |>
  mutate(
    pKS = formatP(pKS, ndigits = 5, mark = TRUE),
    pSh = formatP(pSh, ndigits = 5, mark = TRUE)
  )

# ALTERNATIVE zum Loop
penguins_normal1 <-
  clean_data_penguins |>
  summarize(across(all_of(numvars$names),
    .fns = list( # Wenn du mehr als eine Funktion pro Spalte mit across() anwenden willst, musst du .fns = list(...) verwenden, um diese Funktionen benennen und gruppieren zu können. Da hier pKS und PSh
      pKS = ~ ksnormal(.x) |>
        formatP(mark = TRUE),
      pSh = ~ shapiro.test(.x) |>
        pluck("p.value") |>
        formatP(mark = TRUE)
    )
  )) |>
  pivot_longer(everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.+)_(p.+)$"
  )

# one variable grouped analyses
penguins_mass_sex_species <-
  clean_data_penguins |>
  group_by(species, sex) |>
  summarize(
    pKS = ksnormal(body_mass) |>
      formatP(mark = TRUE),
    pSh = shapiro.test(body_mass) |>
      pluck("p.value") |>
      formatP(mark = TRUE),
    .groups = "drop"
  )
ggplot(clean_data_penguins, aes(x = `body_mass`)) +
  geom_density(alpha = .4) +
  geom_label(
    data = penguins_mass_sex_species,
    aes(label = paste0("Shapiro: ", pSh, "\nKS: ", pKS)),
    family = "xkcd Script",
    x = Inf, # 6000,
    hjust = 1.1,
    y = Inf, # 1.2*10^-3
    vjust = 1.1,
    size = 2.5
  ) +
  facet_grid(cols = vars(sex), rows = vars(species))

# severl variables and grouped analyses
penguins_norm_sex_species <-
  clean_data_penguins |>
  group_by(species, sex) |>
  summarize(
    across(all_of(numvars$names),
      .fns = list(
        pKS = ~ ksnormal(.x) |>
          formatP(mark = TRUE),
        pSh = ~ shapiro.test(.x) |>
          pluck("p.value") |>
          formatP(mark = TRUE)
      )
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -c(species, sex),
    names_to = c("Variable", "Test"),
    names_pattern = "^(.+_.+)_(.+)$",
  ) |>
  arrange(Variable) |>
  pivot_wider(
    names_from = c(species, sex, Test),
    names_sep = "_",
    values_from = value
  )

# um ein Graph zu erstellen brauch man die long Version der Tabelle
penguins_norm_sex_species2 <-
  clean_data_penguins |>
  group_by(species, sex) |>
  summarize(
    across(all_of(numvars$names),
      .fns = list(
        pKS = ~ ksnormal(.x) |>
          formatP(mark = TRUE),
        pSh = ~ shapiro.test(.x) |>
          pluck("p.value") |>
          formatP(mark = TRUE)
      )
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -c(species, sex),
    names_to = c("Variable", "Test"),
    names_pattern = "^(.+_.+)_(.+)$",
  )

# loop über die Variablen und den P WErten


for (var_i in numvars$name) {
  # 1. Filter für die aktuelle Variable
  val_filtered <- penguins_norm_sex_species2 |>
    filter(Variable == var_i) |>
    pivot_wider(names_from = Test, values_from = value) |>
    mutate(
      label_text = paste0("Shapiro: ", pSh, "\nKS: ", pKS)
    )

  # 2. Plot mit nur einem gemeinsamen Label pro Facet
  plot_temp <- ggplot(clean_data_penguins, aes(x = .data[[var_i]])) +
    geom_density(fill = "pink", alpha = 0.4) +
    geom_label(
      data = val_filtered,
      aes(
        x = Inf,
        y = Inf,
        label = label_text
      ),
      inherit.aes = FALSE,
      hjust = 1.1,
      vjust = 1.1,
      size = 2.5,
      family = "xkcd Script"
    ) +
    facet_grid(cols = vars(sex), rows = vars(species)) +
    labs(
      title = paste0("Normalverteilung von\n", var_i, " nach Art und Geschlecht gruppiert"),
      x = var_i,
      y = "Density"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5)
    )

  print(plot_temp)
}
