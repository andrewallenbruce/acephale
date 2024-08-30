r"[nav\to\file\path.ext]"


br <- function(x) {
  paste0(
    r"--{[}--",
    x,
    r"--{]}--")
}

br("x")

vrbl <- function(x) {

  grk_ltr <- c("alpha", "beta", "gamma", "Gamma",
               "delta", "Delta", "epsilon", "varepsilon", "zeta",
               "eta", "theta", "vartheta", "Theta", "iota", "kappa",
               "lambda", "Lambda", "mu", "nu", "xi", "Xi", "pi", "Pi", "varpi",
               "rho", "varrho", "sigma", "varsigma", "Sigma", "tau", "upsilon", "Upsilon",
               "phi", "varphi", "Phi", "chi", "psi", "Psi", "omega", "Omega")

  if(grepl(paste0(grk_ltr, collapse = "|"), x)) return(paste0(r"--( {\)--", x ,r"--(})--"))

  paste0(r"--(\text{)--", x , r"--(})--")
}

prmtr <- function(x, .invert = FALSE) {

  if (.invert) return(paste0(r"--(\frac{1}{)--", vrbl(x), r"--(})--"))

  paste0(r"--({)--", vrbl(x), r"--( })--")
}


fuimus::construct_regex(c('90476', '90477', '905', '906', '9071', '9072', '9073', '9074', '9075', '913'))

library(northstar)

hcpcs <- search_descriptions() |>
  select(
    hcpcs = hcpcs_code,
    section = hcpcs_section,
    type = hcpcs_desc_type,
    description = hcpcs_description)

c(
  "^[9][0][4][7][67]$"     = '9047(6|7)',
  "^[9][0][56][1-9][0-9]$" = '905*|906*',
  "^[9][0][7][1-5][02-9]$" = '907(1|2|3|4|5)',
  "^[9][1][3][01][0-9]$"   = '913'
  )

x <- c('9071', '9072', '9073', '9074', '9075')

hcpcs |>
  distinct(hcpcs) |>
  filter(substr(hcpcs, 1, nchar(x)) %in% x) |>
  pull(hcpcs) |>
  fuimus::construct_regex()


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

stringr::str_extract_all(brack_txt, brack_reg)[[1]]
stringr::str_extract_all(paren_txt, paren_reg)[[1]]
stringr::str_extract_all(nested_txt, nested_reg)[[1]]

gsub(paren_reg, "[XXXXX]", paren_txt)

glob2rx("[J*]")
