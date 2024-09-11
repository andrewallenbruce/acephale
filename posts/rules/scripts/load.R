# Packages ####

library(tidyverse)
library(fuimus)
library(strex)
library(here)

# Load CSV ####

cleaned_steps <- read_csv(
  here("posts/rules/data/cleaned_steps.csv"),
  col_types = cols(
    number = col_integer(),
    order = col_integer(),
    variable = col_character(),
    action = col_character(),
    value = col_character(),
    condition = col_character())) |>
  mutate(condition = glue::as_glue(condition))


cleaned_definitions <- read_csv(
  here("posts/rules/data/cleaned_definitions.csv"),
  col_types = cols(
    number = col_integer(),
    definition = col_character()
  )
)

descriptors <- read_csv(
  here("posts/rules/data/descriptors.csv"),
  col_types = cols(
    number = col_integer(),
    identifier = col_character(),
    category = col_character(),
    rationale = col_character()
  )
)

source(here("posts/rules/scripts", "functions.R"))
