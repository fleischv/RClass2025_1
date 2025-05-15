pacman::p_load(
  conflicted, tidyverse, here,
  grid, gridExtra, car,
  ggsci, ggsignif, ggthemes, ggridges,
  # gganimate,
  ggforce,
  ggbeeswarm,
  wrappedtools,
  emojifont,
  patchwork,
  ggh4x
)
conflicts_prefer(dplyr::filter)
data(penguins)
head(penguins)




# count sex within species

penguins |> filter(!is.na(sex)) |> # daten mit sex = NA entfernen
  ggplot(aes(x = species, fill = sex)) +
  geom_bar()

penguins |>
  filter(!is.na(sex)) |>
  ggplot(data = penguins, aes(x = sex, fill = species)) +
  geom_bar()

# boxplot+beeswarm weight vs. species
ggplot(penguins, aes(x = species, y = body_mass)) +
  geom_boxplot(outlier.alpha = 0) + # to avoid plotting outliers twice
  geom_beeswarm(cex = 1, size = 3, alpha = .25)

# boxplot+beeswarm weight vs. species AND sex
ggplot(penguins, aes(x = species, y = body_mass, fill = sex)) +
  geom_boxplot(outlier.alpha = 0) + # to avoid plotting outliers twice
  geom_beeswarm(cex = 1, size = 2, alpha = .5, dodge.width = .75, shape = 10) # verteilt die Daten auch azf die geschlechte


# scatterplot flipper length vs. body mass with regression line
penguins |>
  ggplot(aes(y = flipper_len, x = body_mass)) +
  geom_point(shape = 10, color = "red") +
  geom_smooth(method = "lm") + # regression line
  geom_smooth()


# local group by species and sex and
penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(y = flipper_len, x = body_mass)) +
  geom_point(aes(color = species, shape = sex)) +
  geom_smooth(method = "lm")

# global grouping

penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(y = flipper_len, x = body_mass, color = species, shape = sex)) +
  geom_point() +
  geom_smooth(method = "lm")

# global grouping of the specias and local grouping of the sex

penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(y = flipper_len, x = body_mass, color = species)) +
  geom_point(aes(shape = sex)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(method = "lm", aes(linetype = sex), se = F)



# define your own colors for species (scale or name or rgb) and symbols for sex

penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(y = flipper_len, x = body_mass)) +
  geom_point(aes(shape = sex, color = species), size = 4) +
  scale_color_manual(values = c("Adelie" = "royalblue", "Chinstrap" = "orangered4", "Gentoo" = "#FFA500")) +
  scale_shape_manual(values = c("\u2640", "\u2642")) +
  scale_size(range = 9) +
  geom_smooth(method = "lm", se = F)

# Visualise bill depth vs. body mass, scatter only

penguins |>
  ggplot(aes(y = bill_dep, x = body_mass)) +
  geom_point()

# Add / compare linear/non-linear regression
penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(y = bill_dep, x = body_mass)) +
  geom_point(alpha = 0.25) +
  geom_smooth() +
  geom_smooth(method = "lm", se = FALSE, color = "orange")


penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(y = bill_dep, x = body_mass)) +
  geom_point(aes(color = species, shape = sex)) +
  geom_smooth() +
  geom_smooth(method = "lm", se = FALSE, color = "orange")



# Facet by sex and species, use margins
penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(y = bill_dep, x = body_mass)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_point(size = 1, alpha = 0.5) +
  facet_grid(
    rows = vars(sex),
    cols = vars(species),
    labeller = label_both, margins = TRUE
  )
