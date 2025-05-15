pacman::p_load(tidyverse)
testset1 <- c("Meier", "Mayer", "Maier", "Meyer", "Mayr", "Maya", "Mayor", "Hans Meier")
# find all variations of the name "Meier" (not Maya or Mayor)
str_detect(string = testset1, pattern = "M\\w+er$")
testset1 |> str_detect("M\\w+er$")
str_extract(string = testset1, pattern = "M\\w+er$")
str_subset(string = testset1, pattern = "M\\w+er$")
str_detect(string = testset1, pattern = "M[ea][iy]e?r$")

testset2 <- c("weight_mm", "height_cm", "age_yr", "temp_c")
# replace _ with space

str_replace_all(
  string = testset2,
  pattern = c(
    "_" = " "
  )
)
# replace _ with space and add unit in brackets

str_replace_all(
  string = testset2,
  pattern = c(
    "_(\\w+)" = " [\\1]"
  )
)
testset3 <- c("1980_12_30", "13.04.2005", "2005/04/25", "24121990")
# transform into YYYY-MM-DD

str_replace_all(
  string = testset3,
  pattern = c(
    "(\\d{4}).*(\\d{2}).*(\\d{2})" = "\\1-\\2-\\3",
    "(\\d{2}).*(\\d{2}).*(\\d{4})" = "\\3-\\2-\\1"
  )
)

testset4 <- c("pw2000", "That1sb3tt3r", "M@kesSense?", "NoDigits@this1")
# test pw strength, rules: Upper, lower, special char, number, min 8 char long
