library(tidyverse)
library(here)
library(fuimus)
library(northstar)

claims_spec <- cols(
  id          = col_character(),
  enc         = col_integer(),
  dos         = col_date(format = ""),
  dob         = col_date(format = ""),
  age         = col_integer(),
  dor         = col_date(format = ""),
  lag         = col_double(),
  ref         = col_character(),
  ref_cred    = col_character(),
  ref_tax     = col_logical(),
  ren         = col_character(),
  ren_cred    = col_character(),
  ren_tax     = col_logical(),
  icd         = col_character(),
  ord         = col_integer(),
  hcpcs       = col_character(),
  desc        = col_character(),
  units       = col_integer(),
  mod1        = col_character(),
  mod2        = col_character(),
  pos         = col_character(),
  pos_name    = col_character(),
  loc         = col_character(),
  ins_class   = col_character(),
  ins_prim    = col_character(),
  ins_sec     = col_character(),
  charges     = col_double(),
  allowed     = col_double(),
  payments    = col_double(),
  adjustments = col_double(),
  adj1        = col_character(),
  adj2        = col_character(),
  adj3        = col_character()
)

claims <- read_csv(
  here(
    "posts/claims/data",
    "clean_rpt_rend.csv"),
  col_types = claims_spec
) |>
  remove_quiet() |>
  left_join(
    search_pos() |>
      select(
        -c(pos_name, pos_description)),
    by = join_by(pos == pos_code)
  )

claims <- claims |>
  full_join(
    expand_date_range(claims$dos, "dos"),
    by = join_by(dos)
  ) |>
  arrange(id, enc) |>
  mutate(
    ppa = payments + adjustments,
    gt = charges > ppa,
    allowed = case_when(
      is.na(allowed) & gt ~ ppa,
      is.na(allowed) & !gt ~ payments,
      .default = allowed),
    ppa = NULL,
    gt = NULL,
    .after = charges)

rvu_spec <- cols(
  date_start  = col_date(format = ""),
  date_end    = col_date(format = ""),
  hcpcs       = col_character(),
  description = col_character(),
  work_rvu    = col_double(),
  pe_rvu      = col_double(),
  mp_rvu      = col_double(),
  rvu_total   = col_double(),
  conv_factor = col_double(),
  pctc_ind    = col_character(),
  glob_days   = col_character(),
  mult_proc   = col_character(),
  dos         = col_date(format = ""),
  pos         = col_character()
)

pprrvu <- read_csv(
  here("posts/claims/data", "results.csv"),
  col_types = rvu_spec)

blog_theme <-
  theme_minimal(18, base_family = "IBM Plex Mono") +
  theme(
    plot.background = element_rect(fill = "#f9fafa", color = NA),
    plot.title.position = "plot",
    plot.title = element_text(size = 24, margin = margin(b = 1, unit = "line")),
    legend.position = c(0, 1),
    legend.direction = "horizontal",
    legend.justification = c(0, 1),
    legend.title.align = 1,
    axis.title.y = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0),
    panel.grid.major = element_line(color = "#d3d9db"),
    panel.grid.minor = element_blank()
  )

theme_set(blog_theme)

type_colors <- c(reply = "#5e5b7f", tweet = "#ef8c02", retweet = "#7ab26f")
