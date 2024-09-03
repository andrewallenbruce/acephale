r"[nav\to\file\path.ext]"


brackets <- function(x, y) {
  paste0(r"--{[}--", x, r"--{]}--")
}

parentheses <- function(x, y) {
  paste0(r"--{(}--", x, r"--{)}--")
}

arrows <- function(x, y) {
  paste0(r"--{<}--", x, r"--{>}--")
}

br("x")

keywords <- function(x) {

  keys <- c(
    "hcpcs", "unit", "mod_1", "mod_2", "mod_3", "mod_3", "pos",
    "pos_type", "dos", "age", "sex", "icd", "ndc", "rev", "claim",
    "referring", "rendering", "credential",
    "primary_name", "secondary_name", "tertiary_name",
    "primary_class", "secondary_class", "tertiary_class",
    "primary_state", "secondary_state", "tertiary_state"
               )

  if (grepl(paste0(keys, collapse = "|"), x)) {
    return(paste0(r"--(@)--", x))
    }
}

keywords("hcpcs")

fuimus::construct_regex(c('90476', '90477', '905', '906', '9071', '9072', '9073', '9074', '9075', '913'))

hcpcs <- northstar::search_descriptions() |>
  select(
    hcpcs = hcpcs_code,
    section = hcpcs_section,
    type = hcpcs_desc_type,
    description = hcpcs_description) |>
  distinct(hcpcs)

c(
  "^[9][0][4][7][67]$"     = '^9047[67]$|^90[56]\\d+$|^907[1-5]\\d+$|^913[01]\\d+$',
  "^[9][0][56][1-9][0-9]$" = '^90[56]\\d+$',
  "^[9][0][7][1-5][02-9]$" = '^907[1-5]\\d+$',
  "^[9][1][3][01][0-9]$"   = '^913[01]\\d+$'
  )

x <- c(hcpcs = c('9071', '9072', '9073', '9074', '9075'))



collapse::vlengths(x, use.names = TRUE)


# regex method
vctrs::vec_slice(
  hcpcs,
  stringfish::sf_grepl(
    hcpcs$hcpcs,
    "^9047[67]$|^90[56]\\d+$|^907[1-5]\\d+$|^913[01]\\d+$"))


# substring method
x <- "S"
x <- c('904', '905', '906', '907', '913')

vctrs::vec_slice(
  hcpcs,
  stringfish::sf_substr(
    hcpcs$hcpcs, 1,
    collapse::fmin(stringfish::sf_nchar(x))) %in% x)


"<[^>]+>" # Find any HTML tag
"https?:\/\/[\w\.\/\-?=&%,]+" # Find any URL
"https?://[^\\s]+"

library(tidytext)


descriptors |>
  select(id, description) |>
  unnest_tokens(word, description, to_lower = FALSE, token = "ptb") |>
  mutate(word = if_else(id == 1 & word %in% "*****", "43760", word))


brack_txt <- "This is [some text] between [brackets]"
brack_reg <- r"{\[(.*?)\]}"

paren_txt <- "This is (some text) between (brackets)"
paren_reg <- r"{\(.*?\)}"

nested_txt <- "This (has brackets) and [square brackets] mixed (together [nested])"
nested_reg <- r"{\[.*?\]|\(.*?\)}"

keyword_txt <- "@hcpcs @pos"
keyword_reg <- r"{@(\w+)}"

stringr::str_extract_all(brack_txt, brack_reg)[[1]]
stringr::str_extract_all(paren_txt, paren_reg)[[1]]
stringr::str_extract_all(nested_txt, nested_reg)[[1]]
stringr::str_extract_all(keyword_txt, keyword_reg)[[1]]

gsub(paren_reg, "[XXXXX]", paren_txt)

glob2rx("[J*]")

srchcol <- function(df, col, search, ignore = TRUE, ...) {

  dplyr::filter(
    df,
    stringr::str_detect(!!rlang::sym(col),
      stringr::regex(search, ignore_case = ignore)))
}

# clean_combine |>
#   pivot_longer(
#     cols = c(definition, nested),
#     names_to = "variable",
#     values_to = "definition") |>
#   filter(!is.na(definition)) |>
#   select(number, nested) |>
#   print(n = 200)
#   separate_longer_delim(cols = definition, delim = " ( ") |>
#   separate_longer_delim(cols = definition, delim = " ) ") |>
#   separate_longer_delim(cols = definition, delim = " AND ") |>
#   mutate(parentheses = str_extract_all(definition, regex("\\s*\\(\\s*\\d+\\s*\\)\\s*")),
#     OR = str_detect(definition, fixed(" OR ")),
#          PARA = str_detect(definition, regex("\\(\\s|\\s\\)")),
#          definition = if_else(PARA, str_remove_all(definition, fixed(" )")), definition),
#          definition = if_else(PARA|OR, glue_chr("<<--{definition}-->>"), definition),
#          OR = NULL,
#          PARA = NULL,
#          definition = str_remove_all(definition, regex(r"{\(\d+\)}")),
#          definition = str_remove_all(definition, regex(r"{\(\D+\)}")),
#          definition = str_replace_all(definition, " ; ", ", "),
#          definition = str_replace_all(definition, ";", ", "),
#          definition = str_replace_all(definition, fixed(" ]"), "]"),
#          definition = str_replace_all(definition, " ] ", "]"),
#          definition = str_replace_all(definition, " , ", ", "),
#          definition = str_replace_all(definition, " ,", ", "),
#          definition = str_squish(definition)
#          ) |>
#   slice(1000:2000) |>
#   print(n = 100)

cat(c(
  "@hcpcs@ is not [9200*, 92012, 92014]
<OR>
@hcpcs@ is one of ^[0-8A-CEGHJ-MP-R]|^[9][0134-8]|^[9][2][1-9]|^[9][2][0][26-8]|^[9][2][0][1][589]
<AND>
@hcpcs@ is not ^[^9][^9][0-9]"
))

uniq_nona <- \(x) collapse::funique(collapse::na_rm(x))

x <- c(
  "0",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "A",
  "B",
  "C",
  "E",
  "G",
  "H",
  "J",
  "K",
  "L",
  "M",
  "P",
  "Q",
  "R",
  "90",
  "91",
  "93",
  "94",
  "95",
  "96",
  "97",
  "98",
  "921",
  "922",
  "923",
  "924",
  "925",
  "926",
  "927",
  "928",
  "929",
  "9202",
  "9206",
  "9207",
  "9208",
  "92015",
  "92018",
  "92019"
)

ln <- collapse::vlengths(x)

data.frame(
  x = x,
  ln = ln
) |>
  group_by(ln) |>
  nest() |>
  collapse::rsplit(~ ln)


#   str_split_fixed("", n = 5) |>
#     as.data.frame() |>
#     purrr::map(na_if_common) |>
#     purrr::map(uniq_nona)
#
# "^[0-8A-CEGHJ-MP-R]|^[9][0134-8]|^[9][2][1-9]|^[9][2][0][26-8]|^[9][2][0][1][589]"
