library(tidyverse)
library(fuimus)
library(strex)
library(here)

head_tail <- function(x, n = 5, by = NULL) {
  dplyr::bind_rows(
    dplyr::slice_head(x, n = n, by = dplyr::all_of(by)),
    dplyr::slice_tail(x, n = n, by = dplyr::all_of(by)))
}

deparse_substitute <- \(x) {
  deparse(substitute(x))
}

gt_var <- \(df) {
  df |>
    gt() |>
    cols_align(align = "left") |>
    opt_table_font(font = google_font(name = "Roboto Condensed")) |>
    opt_all_caps() |>
    tab_style(
      style = cell_text(
        align = "left",
        weight = "bold",
        size = px(14),
        whitespace = "break-spaces",
        font = google_font(name = "JetBrains Mono")
      ),
      locations = cells_body(columns = c(value))
    ) |>
    tab_style(
      style = cell_text(
        align = 'left',
        weight = "bold",
        font = google_font(name = "Roboto Mono")
      ),
      locations = cells_body(columns = c(condition))
    ) |>
    opt_stylize() |>
    tab_options(table.width = pct(50),
                quarto.disable_processing = TRUE)
}

rules <- read_csv(
  here("posts/rules/data/rules_raw.csv"),
  col_types = cols(
    row       = col_integer(),
    id_rule   = col_integer(),
    id_name   = col_character(),
    id_order  = col_integer(),
    category  = col_factor(),
    alert     = col_character(),
    var       = col_character(),
    action    = col_character(),
    value     = col_character(),
    rule      = col_character(),
    x9        = col_character(),
    x10       = col_character())) |>
  mutate(
    category = remove_quotes(as.character(category)),
    category = str_remove_all(category, "\\r|\\n"),
    alert = remove_quotes(alert),
    alert = str_remove_all(alert, "\\r|\\n"),
    rule = remove_quotes(rule),
    rule = str_remove_all(rule, "\\r|\\n")) |>
  fill(rule) |>
  select(
    index = row,
    number = id_rule,
    order = id_order,
    identifier = id_name,
    variable = var,
    action,
    value,
    category,
    definition = rule,
    rationale  = alert
  ) |>
  filter(
    !number %in% c(273:274, 293, 450, 466:468, 701),
    definition != "Delete"
    )

rules[1, 9, drop = TRUE] <- "CPT Code is [43760] AND Encounter Date of Service after [01/01/2019]"
rules[2, 9, drop = TRUE] <- "CPT Code is [43760] AND Encounter Date of Service after [01/01/2019]"

descriptors <- rules |>
  select(index, number, identifier, category, definition, rationale) |>
  distinct() |>
  mutate(
    source = str_extract_all(category, "https?://[^\\s]+"),
    category = str_remove_all(category, "https?://[^\\s]+")
  ) |>
  unnest(source, keep_empty = TRUE) |>
  separate_longer_delim(source, delim = "https:") |>
  mutate(
    source = na_if(source, "") |> str_squish(),
    source = if_else(!is.na(source), str_c("https:", source), source),
    category = na_if(category, "") |> str_squish(),
    definition = str_squish(definition),
    definition = str_remove_all(definition, "►"),
    definition = gsub(",(?!\\s)", ", ", definition, perl = TRUE) |> str_squish(),
    rationale = str_squish(rationale),
    rationale = str_remove_all(rationale, "►"),
    rationale = gsub(",(?!\\s)", ", ", rationale, perl = TRUE) |> str_squish()
  )

identifier <- descriptors |>
  distinct(number, identifier) |>
  reframe(
    number,
    identifier = str_remove_all(identifier, "-"),
    payer = if_else(
      str_detect(identifier, "BCBS|MCD|VFC"),
      str_extract(identifier, "BCBS|MCD|VFC"),
      NA_character_
    ),
    state = if_else(
      !is.na(payer),
      str_extract(identifier, paste0(state.abb, collapse = "|")),
      NA_character_
    ),
    split = str_split_by_numbers(identifier)
  ) |>
  unnest_wider(split, names_sep = "_") |>
  mutate(
    split_1 = case_match(
      split_1,
      c("VACCN", "COVID") ~ "VAX",
      "ICD" ~ "ICD:CM",
      "ANEST" ~ "ANES",
      "TELEH" ~ "TELE",
      "DIGES" ~ "GASTRO",
      "PRVNT" ~ "PREVENT",
      "OPTHL" ~ "OPHTH",
      "FOOT" ~ "PODIA",
      "FERTL" ~ "FERT",
      .default = split_1
    )
  ) |>
  group_by(payer) |>
  mutate(split_2 = if_else(!is.na(payer), row_number(), as.integer(split_2))) |>
  ungroup() |>
  mutate(split_2 = str_pad(
    as.character(as.integer(split_2)),
    width = 3,
    side = "left",
    pad = "0"
  )) |>
  combine(name = payer_state,
          columns = c("payer", "state"),
          sep = ":") |>
  mutate(split_1 = if_else(!is.na(payer_state), payer_state, split_1),
         payer_state = NULL) |>
  combine(
    name = identifier,
    columns = c("split_1", "split_2"),
    sep = ":"
  ) |>
  mutate(
    split_1 = NULL,
    identifier = str_replace(identifier, "VAX:19", "VAX:00"),
    identifier = str_replace(identifier, "ICD:CM:10", "ICD:CM:")
  )

identifier[1, 2, drop = TRUE] <- "HCPCS:001"

descriptors <- descriptors |>
  select(-identifier) |>
  left_join(identifier, by = "number") |>
  relocate(identifier, .before = category)

components <- rules |>
  select(number, order, variable, action, value) |>
  distinct() |>
  left_join(identifier, by = "number") |>
  relocate(identifier, .before = variable) |>
  mutate(variable = case_match(variable,
      c(
        "CPT CODE",
        "CPT Code",
        "CPT code",
        "CPT",
        "Code Check Expired CPT Alert (Presence)",
        "CPT is one of"
      ) ~ "hcpcs",
      c(
        "Mod1",
        "CPT Mod1",
        "CPT Mod 1",
        "CPTMod1",
        "Code Check CPT Mod Alert (Presence)",
        "CPT MOD1",
        "CPT Mod1 (Presence)"
      ) ~ "mod_1",
      c(
        "Mod2",
        "CPT MOD2",
        "CPT Mod 2",
        "CPT Mod2 (Presence)",
        "CPTMod2",
        "CPT Mod2"
      ) ~ "mod_2",
      "CPT Mod3" ~ "mod_3",
      "CPT Mod4" ~ "mod_4",
      "CPT Units" ~ "unit",
      c(
        "Dx code",
        "DX Code",
        "Dx Code",
        "Code Check Expired DX Alert (Presence)"
      ) ~ "icd",
      c("Encounter Date of Service", "Date of Service") ~ "dos",
      c("Location", "Place of Service") ~ "pos",
      c("CPT UB04", "UB04 Bill Type") ~ "ub04",
      "CPT NDC (Presence)" ~ "ndc",
      "CPT Rev Code" ~ "rev_code",
      "Code Check CCI Alert (Presence)" ~ "cci",
      "Code Check LCD Alert (Presence)" ~ "lcd",
      "Code Check NCD Alert (Presence)" ~ "ncd",
      c("Patient Age", "Code Check Age Alert (Presence)") ~ "age",
      c("Patient Sex", "Code Check Gender Alert (Presence)") ~ "sex",
      "NA" ~ NA_character_,
      "Rendering Provider" ~ "rendering",
      "Referring Provider (Presence)" ~ "referring",
      c(
        "Primary Insurance Class",
        "Primary Insurance class",
        "Primary insurance class"
      ) ~ "primary_class",
      "Primary Insurance" ~ "primary_name",
      "Secondary Insurance Class" ~ "secondary_class",
      "Secondary Insurance" ~ "secondary_name",
      c(
        "Primary Insurance Authorization (Presence)",
        "Primary Insurance Authorization (Presence) is not [Present]"
      ) ~ "primary_auth",
      .default = variable
    )
  )

components <- components |>
  mutate(
    class = case_match(
      variable,
      c(
        "sex",
        "rendering",
        "referring",
        "primary_auth",
        "primary_class",
        "primary_name",
        "secondary_class",
        "secondary_name",
        "ncd",
        "lcd",
        "cci",
        "ndc",
        "ub04",
        "pos",
        "icd",
        "hcpcs",
        "mod_1",
        "mod_2",
        "mod_3",
        "mod_4",
        "rev_code"
      ) ~ "<chr>",
      c("age", "unit") ~ "<int>",
      c("dos", "dob") ~ "<date>"
    ) |> as_factor())

components <- components |>
    mutate(group = case_match(
      as.character(variable),
      c(
        "sex",
        "dos",
        "age",
        "pos",
        "rendering",
        "referring") ~ "encounter",
      c(
        "hcpcs",
        "mod_1",
        "mod_2",
        "mod_3",
        "mod_4",
        "rev_code",
        "unit",
        "ndc",
        "pos",
        "icd",
        "ub04"
      ) ~ "coding",
      c("ncd", "lcd", "cci") ~ "ncci",
      c(
        "primary_auth",
        "primary_class",
        "primary_name",
        "secondary_class",
        "secondary_name"
      ) ~ "payer"
    ) |> as_factor()
  )


dos <- components |>
  filter(variable == "dos") |>
  mutate(value = anytime::anydate(value) |> as.character(),
         method = if_else(str_detect(action, "after"), ">", "<"),
         condition = glue::glue('{variable} {method} "{value}"')) |>
  select(number, identifier, order, variable, value, condition)

age <- components |>
  filter(variable == "age") |>
  mutate(age = strex::str_extract_numbers(value),
         period = strex::str_extract_non_numerics(value),
         .before = variable) |>
  unnest(c(age, period)) |>
  mutate(age = as.integer(age),
         period = str_remove_all(period, "\\s|,"),
         value = NULL,
         method = case_match(
           action,
           c("is younger than", "younger than") ~ "<",
           "is older than" ~ ">",
           "is" ~ "==",
           .default = NA_character_),
         value = case_when(
           period == "years" ~ as.duration(years(age)) / ddays(1),
           period == "months" ~ as.duration(months(age)) / ddays(1),
           period == "days" ~ as.duration(days(age)) / ddays(1),
           .default = NA_real_),
         value = as.integer(value) |> as.character(),
         condition = glue::glue('{variable} {method} {value}')) |>
  select(number, identifier, order, variable, value, condition)

ndc <- components |>
  filter(variable == "ndc") |>
  mutate(condition = glue::glue('!is.na({variable})')) |>
  select(number, identifier, order, variable, value, condition)

unit <- components |>
  filter(variable == "unit") |>
  mutate(value = as.integer(value),
         method = if_else(str_detect(action, "is not"), "!=", "=="),
         value = as.integer(value) |> as.character(),
         condition = glue::glue('{variable} {method} {value}')) |>
  select(number, identifier, order, variable, value, condition)

sex <- components |>
  filter(variable == "sex") |>
  mutate(
    value = str_to_title(value),
    method = if_else(str_detect(action, "is"), "==", "!="),
    condition = glue::glue('!is.na({variable})'),
    condition = if_else(value == "Present", glue::glue('!is.na({variable})'), glue::glue('{variable} {method} "{value}"'))) |>
  select(number, identifier, order, variable, value, condition)

ub04 <- components |>
  filter(variable == "ub04") |>
  mutate(
    value = str_to_upper(value),
    value = if_else(action == "is not", "FALSE", value),
    action = "is",
    method = if_else(str_detect(action, "is"), "==", "!="),
    condition = glue::glue('{variable} {method} {value}')
  ) |>
  select(number, identifier, order, variable, value, condition)

pos <- components |>
  filter(variable == "pos") |>
  select(number, identifier, order, variable, action, value) |>
  mutate(pos = str_extract_all(value, r'{\((\d+)\)(?:\s*\([^)]*\))?}')) |>
  unnest(pos, keep_empty = TRUE) |>
  mutate(pos = substr(pos, 2, 3),
         pos = if_else(is.na(pos) & str_detect(value, "^\\d{2}$"), str_extract(value, "^\\d{2}$"), pos),
         pos = if_else(is.na(pos) & str_detect(value, "^\\d{2}\\s+"), str_extract(value, "^\\d{2}"), pos),
         value = pos,
         pos = glue::glue("'{pos}'")
  ) |>
  nest(pos = c(pos),
       value = c(value)) |>
  rowwise() |>
  mutate(pos = map(pos, ~paste0(., collapse = ", ")),
         value = map(value, ~paste0(., collapse = ", "))) |>
  unnest(cols = c(pos, value)) |>
  ungroup() |>
  mutate(
    pos = case_when(
      number == 1007 ~ "'19', '21', '22'",
      number == 1013 ~ "'31', '32', '54', '56'",
      number == 1014 ~ "'12', '13', '14', '16', '33', '55'",
      number %in% c(1891, 930, 1036) ~ "'02', '10'",
      number %in% c(275, 276) ~ "'81'",
      .default = pos),
    value = case_when(
      number == 1007 ~ "19, 21, 22",
      number == 1013 ~ "31, 32, 54, 56",
      number == 1014 ~ "12, 13, 14, 16, 33, 55",
      number %in% c(1891, 930, 1036) ~ "02, 10",
      number %in% c(275, 276) ~ "81",
      .default = value),
    pos = glue::glue("c({pos})"),
    action = if_else(action == "is one of", "is", action),
    method = "%in%",
    variable = case_match(action,
                          "is not" ~ "!pos",
                          .default = variable),
    condition = glue::glue('{variable} {method} {pos}'),
    variable = "pos"
  ) |>
  select(number, identifier, order, variable, value, condition)

mod_base <- components |>
  filter(variable %in% c("mod_1", "mod_2", "mod_3", "mod_4")) |>
  select(-c(class, group)) |>
  mutate(value = if_else(identifier == "MCD:CA:013" & variable == "mod_1", "SL", value),
         chars = nchar(value)) |>
  arrange(desc(chars))

mod_singles <- mod_base |>
  filter(
    chars == 2 | value == "Present",
    str_detect(value, fixed("*"), negate = TRUE)
  ) |>
  mutate(
    method = if_else(str_detect(action, "is"), "==", "!="),
    condition = if_else(
      value == "Present",
      glue::glue('!is.na({variable})'),
      glue::glue('{variable} {method} "{value}"'))) |>
  select(number, identifier, order, variable, value, condition)

mod_wildcards <- mod_base |>
  filter(str_detect(value, fixed("*"))) |>
  mutate(
    method = case_when(
      action == "is not" & value == "F*, T*" ~ glue::glue("^[^F|T][A-Z0-9]$"),
      action == "is" & value == "P*" ~ glue::glue("^[P][A-Z0-9]$"),
      .default = NA_character_),
    condition = glue::glue('func({variable}, "{method}")')
  ) |>
  select(number, identifier, order, variable, value, condition)

mod_multi <- mod_base |>
  filter(
    chars > 2,
    value != "Present",
    str_detect(value, fixed("*"), negate = TRUE)
  ) |>
  mutate(value = str_remove_all(value, " "),
         value = str_replace_all(value, ";", ","),
         chars = NULL) |>
  separate_longer_delim(cols = value, delim = ",") |>
  mutate(mods = glue::glue("'{value}'")) |>
  nest(mods = c(mods), value = c(value)) |>
  rowwise() |>
  mutate(mods = map(mods, ~paste0(., collapse = ", ")),
         value = map(value, ~paste0(., collapse = ", "))) |>
  unnest(cols = c(mods, value)) |>
  ungroup() |>
  mutate(mods = glue::glue("c({mods})"),
         method = "%in%",
         condition = if_else(action == "is not", glue::glue('!{variable} {method} {mods}'), glue::glue('{variable} {method} {mods}'))) |>
  select(number, identifier, order, variable, value, condition)

modifiers <- vctrs::vec_c(
  mod_singles,
  mod_wildcards,
  mod_multi
)

rev_code <- components |>
  filter(variable == "rev_code") |>
  mutate(
    method = if_else(str_detect(action, "is not"), "!=", "=="),
    condition = glue::glue('{variable} {method} "{value}"')) |>
  select(number, identifier, order, variable, value, condition)

referring <- components |>
  filter(variable == "referring") |>
  mutate(action = "is not",
         value = "Present",
         condition = glue::glue('!is.na({variable})')) |>
  select(number, identifier, order, variable, value, condition)
