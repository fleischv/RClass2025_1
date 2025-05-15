pacman::p_load(
  conflicted, tidyverse, wrappedtools,
  readxl, foreign, haven, here
)
# Solution 1

fast_growth <- read_excel(
  path = here("data/UntidyImportChallenge.xlsx"),
  range = "A4:E11"
) |>
  mutate("tumor_growth" = "fast")

middle_growth <- read_excel(
  path = here("data/UntidyImportChallenge.xlsx"),
  range = "G4:K12"
) |>
  mutate("tumor_growth" = "intermediär")

slow_growth <- read_excel(
  path = here("data/UntidyImportChallenge.xlsx"),
  range = "M4:Q7"
) |>
  mutate("tumor_growth" = "slow")

combined_table1 <- bind_rows(fast_growth, middle_growth, slow_growth) |>
  rename(
    "Treatment_Group" = "Start-Day :",
    "Mouse_ID" = "Meas./Treatm."
  ) |>
  rename_with(
    .fn = ~ paste("body weight [g] after", .x),
    .cols = contains(" h")
  ) |>
  relocate(tumor_growth, .after = Mouse_ID) |>
  fill(Treatment_Group, .direction = "down")


# Solution 2

fast <- read_excel(
  path = here("data/UntidyImportChallenge.xlsx"),
  range = "A4:E11"
)

intermediär <- read_excel(
  path = here("data/UntidyImportChallenge.xlsx"),
  range = "G4:K12"
)

slow <- read_excel(
  path = here("data/UntidyImportChallenge.xlsx"),
  range = "M4:Q7"
)

add_name_column <- function(df) {
  name <- deparse(substitute(df))
  df %>% mutate(Tumor_growth = name)
}

combined_table2 <- bind_rows(
  add_name_column(fast),
  add_name_column(intermediär),
  add_name_column(slow)
) |>
  rename(
    "Treatment_Group" = "Start-Day :",
    "Mouse_ID" = "Meas./Treatm."
  ) |>
  rename_with(
    .fn = ~ paste("body weight [g] after", .x),
    .cols = contains(" h")
  ) |>
  fill(Treatment_Group, .direction = "down") |>
  relocate(Tumor_growth, .after = Mouse_ID)

# Solution 3 of the combined tibble

combined_table3 <- bind_rows(fast = fast, interm. = intermediär, slow = slow, .id = "Tumorgrowth") |>
  rename(
    "Treatment_Group" = "Start-Day :",
    "Mouse_ID" = "Meas./Treatm."
  ) |>
  rename_with(
    .fn = ~ paste("body weight [g] after", .x),
    .cols = contains(" h")
  ) |>
  fill(Treatment_Group, .direction = "down") |>
  relocate(Tumorgrowth, .after = Mouse_ID) |>
  arrange(Treatment_Group)
saveRDS

{
  r
}
pacman::p_load(conflicted, tidyverse, wrappedtools)

clean_long <-
  pivot_longer(
    data = EX03_import_clean,
    cols = contains("body weight"),
    names_to = c(".value", "Timepoint"),
    names_pattern = "(.+ \\[g\\]) after (\\d+ h)"
  )
clean_wide <-
  pivot_wider(clean_long,
    values_from = c("body weight [g]"),
    # values come from 2 sources, names will used in names_glue
    names_from = Timepoint,
    names_glue = "{.value} after {Timepoint}"
  )
