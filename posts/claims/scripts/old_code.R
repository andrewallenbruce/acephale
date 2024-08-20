torvu <- claims |>
  filter(
    stringfish::sf_grepl(hcpcs, "^[A-CEGHJ-MP-V0-9]\\d{3}[AFMTU0-9]$")
  ) |>
  distinct(dos, hcpcs, pos_type) |>
  reframe(dos, hcpcs, pos = as.character(pos_type))

# readr::write_csv(torvu, file = "C:/Users/Andrew/Desktop/Repositories/rbrvs/data-raw/torvu.csv"
#   )
torvu |>
  arrange(desc(dos))

yq <- claims |>
  filter(!is.na(hcpcs)) |>
  distinct(year, quarter, hcpcs, pos_type)

rvus$rvu22b_apr |>
  select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor)

yq <- split(yq$hcpcs, yq[c("year", "quarter")]) |>
  purrr::compact() |>
  cheapr::enframe_(name = "yq", value = "hcpcs") |>
  arrange(yq)

yqi <- claims |>
  distinct(y = year, q = quarter) |>
  as.list()

yqs <- as.character(str_glue("{yqi$y}.{yqi$q}"))

# get_rvus <- \(rvu, yq, yqi, yqs) {
#   rvu |>
#   filter(hcpcs %in% c(filter(yq, yq == yqs) |> pull(hcpcs_code) |> pluck(1))) |>
#   select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor) |>
#   mutate(year = yqi$y, quarter = yqi$q, .before = 1)
# }

rvu_22.2 <- rvus$rvu22b_apr |>
  filter(hcpcs %in% c(filter(yq, yq == yqs[1]) |> pull(hcpcs) |> pluck(1))) |>
  select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor) |>
  mutate(year = yqi$y[1], quarter = yqi$q[1], .before = 1)

rvu_22.3 <- rvus$rvu22c_jul |>
  filter(hcpcs %in% c(filter(yq, yq == yqs[2]) |> pull(hcpcs) |> pluck(1))) |>
  select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor) |>
  mutate(year = yqi$y[2], quarter = yqi$q[2], .before = 1)

rvu_22.4 <- rvus$rvu22d_oct |>
  filter(hcpcs %in% c(filter(yq, yq == yqs[3]) |> pull(hcpcs) |> pluck(1))) |>
  select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor) |>
  mutate(year = yqi$y[3], quarter = yqi$q[3], .before = 1)

rvu_23.1 <- rvus$rvu23a_jan |>
  filter(hcpcs %in% c(filter(yq, yq == yqs[4]) |> pull(hcpcs) |> pluck(1))) |>
  select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor) |>
  mutate(year = yqi$y[4], quarter = yqi$q[4], .before = 1)

rvu_23.2 <- rvus$rvu23b_apr |>
  filter(hcpcs %in% c(filter(yq, yq == yqs[5]) |> pull(hcpcs) |> pluck(1))) |>
  select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor) |>
  mutate(year = yqi$y[5], quarter = yqi$q[5], .before = 1)

rvu_23.3 <- rvus$rvu23c_jul |>
  filter(hcpcs %in% c(filter(yq, yq == yqs[6]) |> pull(hcpcs) |> pluck(1))) |>
  select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor) |>
  mutate(year = yqi$y[6], quarter = yqi$q[6], .before = 1)

rvu_23.4 <- rvus$rvu23d_oct |>
  filter(hcpcs %in% c(filter(yq, yq == yqs[7]) |> pull(hcpcs) |> pluck(1))) |>
  select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor) |>
  mutate(year = yqi$y[7], quarter = yqi$q[7], .before = 1)

rvu_24.1 <- rvus$rvu24a_jan |>
  filter(hcpcs %in% c(filter(yq, yq == yqs[8]) |> pull(hcpcs) |> pluck(1))) |>
  select(hcpcs, work_rvu, facility_pe_rvu, non_fac_pe_rvu, mp_rvu, conv_factor) |>
  mutate(year = yqi$y[8], quarter = yqi$q[8], .before = 1)

rvu_join <- vctrs::vec_rbind(
  rvu_22.2,
  rvu_22.3,
  rvu_22.4,
  rvu_23.1,
  rvu_23.2,
  rvu_23.3,
  rvu_24.1
)

rvu_join |>
  dplyr::mutate(
    tot_fac_rvu = work_rvu + facility_pe_rvu + mp_rvu,
    tot_non_rvu = work_rvu + non_fac_pe_rvu + mp_rvu,
    par_non_amt = tot_non_rvu * conv_factor,
    par_fac_amt = tot_fac_rvu * conv_factor,
    nonpar_non_amt = par_non_amt * 1.15,
    nonpar_fac_amt = par_fac_amt * 1.15,
    limiting_non = nonpar_non_amt * 0.95,
    limiting_fac = nonpar_fac_amt * 0.95,
  ) |>
  dplyr::filter(par_fac_amt > 0) |>
  gt::gt(
    rowname_col = "year",
    groupname_col = "hcpcs",
    row_group_as_column = TRUE
  ) |>
  gt::opt_table_font(
    font = gt::google_font(name = "Atkinson Hyperlegible")) |>
  gt::fmt_currency(
    columns = c("par_non_amt",
                "par_fac_amt",
                "nonpar_non_amt",
                "nonpar_fac_amt",
                "limiting_non",
                "limiting_fac")
  ) |>
  gt::cols_label(
    quarter = gt::md("**Qtr**"),
    work_rvu = gt::md("<b><small>RVU</small></b><i><sub>wk</sub></i>"),
    facility_pe_rvu = gt::md("<i>f</i><b><small>RVU</small></b><i><sub>pe</sub></i>"),
    non_fac_pe_rvu = gt::md("<i>nf</i><b><small>RVU</small></b><i><sub>pe</sub></i>"),
    mp_rvu = gt::md("<b><small>RVU</small></b><i><sub>mp</sub></i>"),
    conv_factor = gt::md("<b>CF</b>"),
    tot_fac_rvu = gt::md("<i>f</i><b><small>RVU</small></b><i>s</i>"),
    tot_non_rvu = gt::md("<i>nf</i><b><small>RVU</small></b><i>s</i>"),
    par_non_amt = gt::md("<i>nf</i><b><small>PAR</small></b>"),
    par_fac_amt = gt::md("<i>f</i><b><small>PAR</small></b>"),
    nonpar_non_amt = gt::md("<i>nf</i><b><small>NPAR</small></b>"),
    nonpar_fac_amt = gt::md("<i>f</i><b><small>NPAR</small></b>"),
    limiting_non = gt::md("<i>nf</i><b><small>LC</small></b>"),
    limiting_fac = gt::md("<i>f</i><b><small>LC</small></b>")
  ) |>
  gt::cols_align(align = "center") |>
  gt::tab_style(
    style = gt::cell_text(
      align = 'center',
      size = gt::px(16),
      font = gt::google_font(name = "Fira Code"),
      weight = "bold"),
    locations = gt::cells_row_groups()) |>
  gt::tab_style(
    style = gt::cell_text(
      font = gt::google_font(name = "Fira Code")),
    locations = gt::cells_body()) |>
  gt::tab_options(
    quarto.disable_processing = TRUE,
    table.width = "100%"
  )
