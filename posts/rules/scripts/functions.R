# Functions ####

head_tail <- function(x, n = 5, by = NULL) {
  dplyr::bind_rows(
    dplyr::slice_head(x, n = n, by = dplyr::all_of(by)),
    dplyr::slice_tail(x, n = n, by = dplyr::all_of(by)))
}

deparse_substitute <- \(x) {
  deparse(substitute(x))
}

mask <- function(data, expr) {
  rlang::eval_tidy(rlang::enquo(expr), data)
}

mtcars |> mask(mpg * 20)

# REGEX FUNCTIONS ####
known_patterns <- c("[0-9]", "[A-Z]", "[[:punct:]]")

split_by_length <- \(x) split(x, nchar(x))

escape_regex <-\(x) gsub(".", "\\.", x, fixed = TRUE)

find_common_substrings <- \(s, tolerance = 0.95, missing = "#") {

  df <- t(
    as.data.frame(
      purrr::map(s, ~strsplit(.x, NULL)[[1]])))

  most_matching <- apply(
    df, 2,
    \(x) names(which.max(table(x))))

  prop_matching <- sapply(
    seq_len(ncol(df)),
    \(x) sum(df[ , x] == most_matching[x]) / nrow(df))

  exact_matches <- prop_matching >= tolerance

  paste0(
    ifelse(
      exact_matches,
      most_matching,
      missing
      ),
    collapse = "")

}

detect_pattern <- function(s, ...) {

  charvec <- strsplit(find_common_substrings(s, ...), NULL)[[1]]

  unknown_symbols <- which(charvec == missing_char)

  best_pat <- unknown_symbols[NA]

  for (symbol in unknown_symbols) {

    s_char <- purrr::map_chr(strsplit(s, NULL), symbol)

    pat <- known_patterns[NA]

    pat <- sapply(known_patterns, function(kp) sum(!is.na(purrr::map_chr(s_char, ~stringr::str_match(.x, kp)))))

    best_pat[symbol] <- known_patterns[which.max(pat)]
  }

  detected_pattern <- escape_regex(paste0(ifelse(charvec == missing_char, best_pat, charvec), collapse = ""))

  return(detected_pattern)

}

categorise_regex <- function(strings, tolerance = 0.95) {

  string_list <- split_by_length(strings)

  guess <- purrr::map(string_list, detect_pattern, tolerance = tolerance)

  matches <- purrr::map2(string_list, guess, ~stringr::str_match(.x, .y))

  ## remove non-matches
  matches <- purrr::map(matches, ~.x[!is.na(.x)])

  nonmatches <- purrr::map2(string_list, matches, ~.x[! .x %in% .y])

  result <- purrr::pmap(list(guess, matches, nonmatches),
                        ~list(regex = ..1, matches = ..2, nonmatches = ..3))

  message("   ** CATEGORISATION SUMMARY **")



  message("   ** Detected ",
          length(result),
          " categories and matched\n    ",
          length(unlist(purrr::map(result, "matches"))) ," / ", (length(unlist(purrr::map(result, "nonmatches"))) + length(unlist(purrr::map(result, "matches")))),
          " ( ",
          format(length(unlist(purrr::map(result, "matches"))) / (
            length(unlist(purrr::map(result, "nonmatches"))) + length(unlist(purrr::map(result, "matches")))), digits = 3),
          "% ) strings **\n")
  purrr::walk2(result, names(result), ~{
    n_match <- length(.x$matches)
    n_nonmatch <- length(.x$nonmatch)
    n_results <- n_match + n_nonmatch
    message("  nchar: ", .y,
            "\nexample: ", .x$matches[[1]],
            "\n  regex: ", .x$regex,
            "\n  match: ", n_match, " / ", n_results,
            " ( ", format(100 * n_match / n_results, digits = 3), "% )\n")
  })

  return(invisible(result))
}

#####################

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

# str_remove_all(value, regex(r"{\(\d+\)}"))

print_rule <- \(n, x = descriptors, w = 40) {

  x <- stringr::str_replace_all(x[n, 4, drop = TRUE], stringr::fixed(" AND "), " AND \n")

  x <- stringr::str_replace_all(x, stringr::fixed(" OR "), " OR \n")

  cat(x, sep = "\n")

}

print_desc <- \(n) {

  x <- cleaned_definitions[n, 2, drop = TRUE]

  x <- stringr::str_replace_all(x, stringr::fixed(" %AND% "), "\n%AND%\n")

  x <- stringr::str_replace_all(x, stringr::fixed(" %OR% "), "\n%OR%\n")

  cat(x, sep = "\n")

}

print_list <- function(x, pre = "") {

  if (length(x) == 0) {cat("<empty>\n")}

  ns <- names(x)

  if (length(ns) != length(x)) {stop("all elements must be named")}

  x <- lapply(x, as.character)

  cat(sprintf("%s%s : %s", pre, format(ns), x), sep = "\n")

  invisible(x)
}


pos_expr <- northstar::search_pos() |>
  reframe(name = toupper(pos_name) |> str_replace_all("PATIENT’S", "PATIENT"),
          pos = pos_code) |>
  filter(name != "UNASSIGNED") |>
  mutate(expr = glue::glue('definition = str_replace_all(definition, "{name}", "{pos}"), ')) |>
  pull(expr)
#
# pos_expr |>
#   cat(sep = "\n")

pos_name_to_code <- \(df, col) {
  dplyr::mutate(
    df,
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PHARMACY", "01"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TELEHEALTH PROVIDED OTHER THAN IN PATIENT HOME|TELEHEALTH PROVIDED OTHER THAN IN PT HOME|TELEHEALTH PROVIDED OTHER THAN PT HOME", "02"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "SCHOOL", "03"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "HOMELESS SHELTER", "04"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDIAN HEALTH SERVICE FREE-STANDING FACILITY|INDIAN HEALTH SERVICE - FREE-SANDNG", "05"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDIAN HEALTH SERVICE PROVIDER-BASED FACILITY|INDIAN HEALTH SERVICE - PROVIDER-BASED", "06"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TRIBAL 638 FREE-STANDING FACILITY|TRIBAL 638 FREE-SANDNG FACILITY", "07"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TRIBAL 638 PROVIDER-BASED FACILITY|TRIBAL 638 PROVIDER-BASED", "08"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PRISON/CORRECTIONAL FACILITY", "09"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TELEHEALTH PROVIDED IN PATIENT HOME", "10"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "OFFICE", "11"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "HOME", "12"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "ASSISTED LIVING FACILITY", "13"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "GROUP HOME", "14"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "MOBILE UNIT", "15"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TEMPORARY LODGING", "16"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "WALK-IN RETAIL HEALTH CLINIC", "17"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PLACE OF EMPLOYMENT/WORKSITE", "18"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "OFF CAMPUS-OUTPATIENT HOSPITAL", "19"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "URGENT CARE FACILITY", "20"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INPATIENT HOSPITAL", "21"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "ON CAMPUS-OUTPATIENT HOSPITAL", "22"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "EMERGENCY ROOM-HOSPITAL|EMERGENCY ROOM - HOSPITAL", "23"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "AMBULATORY SURGICAL CENTER", "24"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "BIRTHING CENTER", "25"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "MILITARY TREATMENT FACILITY", "26"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "OUTREACH SITE/STREET", "27"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "SKILLED NURSING FACILITY", "31"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "NURSING FACILITY", "32"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "CUSTODIAL CARE FACILITY", "33"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "HOSPICE", "34"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "AMBULANCE-LAND", "41"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "AMBULANCE-AIR OR WATER", "42"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDEPENDENT CLINIC", "49"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "FEDERALLY QUALIFIED HEALTH CENTER", "50"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INPATIENT PSYCHIATRIC FACILITY", "51"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PSYCHIATRIC FACILITY-PARTIAL HOSPITALIZATION|PSYCHIATRIC FACILITY - PARTIAL HOSPITALIZATION", "52"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "COMMUNITY MENTAL HEALTH CENTER", "53"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INTERMEDIATE CARE FACILITY/INDIVIDUALS WITH INTELLECTUAL DISABILITIES", "54"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY", "55"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PSYCHIATRIC RESIDENTIAL TREATMENT CENTER", "56"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "NON-RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY|NON-RESIDENTIAL SUBSTANCE ABUSE FACILITY", "57"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "NON-RESIDENTIAL OPIOID TREATMENT FACILITY", "58"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "MASS IMMUNIZATION CENTER", "60"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "COMPREHENSIVE INPATIENT REHABILITATION FACILITY", "61"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "COMPREHENSIVE OUTPATIENT REHABILITATION FACILITY", "62"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "END-STAGE RENAL DISEASE TREATMENT FACILITY|END STAGE RENAL DISEASE TREATMENT FACILITY", "65"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PROGRAMS OF ALL-INCLUSIVE CARE FOR THE ELDERLY (PACE) CENTER", "66"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "STATE OR LOCAL PUBLIC HEALTH CLINIC", "71"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "RURAL HEALTH CLINIC", "72"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDEPENDENT LABORATORY", "81"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "OTHER PLACE OF SERVICE", "99"))
}


str_vector <- function(x, qchar = c("single", "double")) {

  qchar <- match.arg(qchar, choices = c("single", "double"))
  switch(qchar,
         "single" = return(toString(sprintf("'%s'", x))),
         "double" = return(toString(sprintf("\"%s\"", x)))
  )
}

glue_bracket <- \(x) glue::glue("[{x}]")

glue_parens <- \(x) glue::glue("({x})")

glue_quote_double <- \(x) glue::glue('"{x}"')

glue_quote_single <- \(x) glue::glue("'{x}'")


print_list2 <- function(x, pre = "* ") {

  if (length(x) == 0) cat("<empty>\n")

  ns <- names(x)

  if (length(ns) != length(x)) stop("all elements must be named")

  x <- lapply(x, as.character)

  cat(sprintf("%s%s : %s", pre, format(ns), x), sep = "\n")

  invisible(x)
}


time_format <- function(secs) {
  if (secs > 60) {
    secs <- as.integer(secs)
    sprintf(
      "%02d:%02d:%02d", secs %/% 3600L, (secs %/% 60L) %% 60L,
      secs %% 60L
    )
  } else {
    sprintf(if (secs >= 10) {
      "%.1fs"
    } else {
      "%.3fs"
    }, secs)
  }
}
