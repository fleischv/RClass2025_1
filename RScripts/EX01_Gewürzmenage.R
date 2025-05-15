Sys.setenv(LANG = "en_EN.UTF-8") # to get errors/warnings in English
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  conflicted, # tests/solutions for name conflicts
  tidyverse, # metapackage
  wrappedtools # my own tools package
  # randomNames # used to create pseudo names unless you want fany names, here not nessasary
)
conflict_scout()
conflicts_prefer(
  dplyr::filter,
  stats::lag
)

# define n_elements <- 5*10^3
n_elements <- 5 * 10^3
# create a tibble “menage” with columns saltshaker, peppercaster and n_elements each
# for saltgrain and pepperflake
menage <- tibble(
  saltshaker = paste0("saltgrain", 1:n_elements),
  peppercaster = rep("pepperflake", n_elements)
)
# print saltshaker
menage[, 1]
menage[1] # alternative
menage |> select(saltshaker) # alternative
select(.data = menage, saltshaker) # nested

# print salt
menage[[1]]
menage$saltshaker # alternative
menage |> pull(saltshaker) # alternative
pull(menage, saltshaker) # alternative

# print 100 saltgrains
menage$saltshaker[1:100]
menage[[1]][1:100] # macht das gleiche!
sample(menage$saltshaker, 100) # alternative
menage |>
  slice(1:100) |>
  pull(saltshaker) # the slice must be befor the pulling because slice needs a 2D dimention
menage |>
  slice_sample(n = 100) |> # random samples and not the first 100
  pull(saltshaker)
