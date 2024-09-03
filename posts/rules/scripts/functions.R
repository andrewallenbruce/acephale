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

# mtcars |> mask(mpg * 20)

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
# EMERGENCY ROOM - HOSPITAL
  dplyr::mutate(
    df,
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PHARMACY", "01"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TELEHEALTH PROVIDED OTHER THAN IN PATIENT HOME|TELEHEALTH PROVIDED OTHER THAN IN PT HOME|TELEHEALTH PROVIDED OTHER THAN PT HOME", "02"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "SCHOOL", "03"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "HOMELESS SHELTER", "04"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDIAN HEALTH SERVICE FREE-STANDING FACILITY", "05"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDIAN HEALTH SERVICE PROVIDER-BASED FACILITY", "06"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TRIBAL 638 FREE-STANDING FACILITY", "07"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "TRIBAL 638 PROVIDER-BASED FACILITY", "08"),
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
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PSYCHIATRIC FACILITY-PARTIAL HOSPITALIZATION", "52"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "COMMUNITY MENTAL HEALTH CENTER", "53"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INTERMEDIATE CARE FACILITY/INDIVIDUALS WITH INTELLECTUAL DISABILITIES", "54"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY", "55"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PSYCHIATRIC RESIDENTIAL TREATMENT CENTER", "56"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "NON-RESIDENTIAL SUBSTANCE ABUSE TREATMENT FACILITY", "57"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "NON-RESIDENTIAL OPIOID TREATMENT FACILITY", "58"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "MASS IMMUNIZATION CENTER", "60"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "COMPREHENSIVE INPATIENT REHABILITATION FACILITY", "61"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "COMPREHENSIVE OUTPATIENT REHABILITATION FACILITY", "62"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "END-STAGE RENAL DISEASE TREATMENT FACILITY", "65"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "PROGRAMS OF ALL-INCLUSIVE CARE FOR THE ELDERLY (PACE) CENTER", "66"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "STATE OR LOCAL PUBLIC HEALTH CLINIC", "71"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "RURAL HEALTH CLINIC", "72"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "INDEPENDENT LABORATORY", "81"),
    "{{ col }}" := stringr::str_replace_all({{ col }}, "OTHER PLACE OF SERVICE", "99"))
}
