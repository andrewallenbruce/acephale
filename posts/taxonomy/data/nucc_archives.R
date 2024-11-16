# "https://www.nucc.org/index.php/code-sets-mainmenu-41/provider-taxonomy-mainmenu-40/csv-mainmenu-57"

urls <- paste0("https://www.nucc.org", paste0("https://www.nucc.org",
    rvest::session("https://www.nucc.org") |>
      rvest::session_follow_link("Code Sets") |>
      rvest::session_follow_link("Taxonomy") |>
      rvest::session_follow_link("CSV") |>
      rvest::html_elements("a") |>
      rvest::html_attr("href") |>
      stringr::str_subset("taxonomy") |>
      stringr::str_subset("csv")) |>
    rvest::read_html() |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("nucc_taxonomy"))

infotable <- readLines(paste0(here::here(), "/posts/taxonomy/data/nucc_csv_titles_dates.txt")) |>
  stringr::str_split("/CSV", simplify = TRUE) |>
  as.data.frame() |>
  dplyr::select(V2) |>
  dplyr::mutate(
    V2 = stringr::str_remove(V2, '/') |>
      stringr::str_remove('"') |>
      stringr::str_replace('>', " ") |>
      stringr::str_remove("</a></li>") |>
      stringr::str_remove(",")) |>
  tidyr::separate_wider_regex(V2,
    c(filename = "nucc_taxonomy_[0-9]{2,3}.csv",
      ' Version ',
      version = "[0-9]{1,2}[.][0-9]{1}",
      " ",
      release_date = "[0-9][/][0-9][/][0-9]{2}")) |>
  dplyr::mutate(release_date = readr::parse_date(release_date, format = "%m/%d/%y"),
                file_url = urls)

fs::dir_create(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))

curl::multi_download(
  urls = infotable$file_url,
  destfile = glue::glue("{here::here()}/posts/taxonomy/data/csvs/{infotable$filename}"),
  resume = TRUE)

fs::dir_info(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))$path

# write_csv(infotable, glue::glue("{here::here()}/posts/taxonomy/data/infotable.csv"))

infotable |> print(n = 800)

nucc_taxonomy_90 <- readr::read_csv(
  fs::dir_info(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))$path[31],
  id = "filename",
  show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::slice(2:n()) |>
  dplyr::mutate(filename = basename(filename),
                dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "")),
                dplyr::across(dplyr::everything(), ~ stringr::str_squish(.)),
                dplyr::across(dplyr::everything(), ~ fuimus::remove_quotes(.))) |>
  dplyr::left_join(infotable, by = "filename") |>
  dplyr::reframe(version, release_date, code, type, classification, specialization, definition, notes)

nucc_taxonomy_91 <- readr::read_csv(fs::dir_info(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))$path[32], id = "filename", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::slice(2:n()) |>
  dplyr::mutate(filename = basename(filename),
                dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "")),
                dplyr::across(dplyr::everything(), ~ stringr::str_squish(.)),
                dplyr::across(dplyr::everything(), ~ fuimus::remove_quotes(.))) |>
  dplyr::left_join(infotable, by = "filename") |>
  dplyr::reframe(version, release_date, code, type, classification, specialization, definition, notes)

nucc_taxonomy_100 <- readr::read_csv(fs::dir_info(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))$path[1], id = "filename", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::slice(2:n()) |>
  dplyr::mutate(filename = basename(filename),
    dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "")),
    dplyr::across(dplyr::everything(), ~ stringr::str_squish(.)),
    dplyr::across(dplyr::everything(), ~ fuimus::remove_quotes(.))) |>
  dplyr::left_join(infotable, by = "filename") |>
  dplyr::reframe(version, release_date, code, type, classification, specialization, definition, notes)


nucc_taxonomy_101 <- readr::read_csv(fs::dir_info(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))$path[2], id = "filename", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::slice(2:n()) |>
  dplyr::mutate(filename = basename(filename),
                dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "")),
                dplyr::across(dplyr::everything(), ~ stringr::str_squish(.)),
                dplyr::across(dplyr::everything(), ~ fuimus::remove_quotes(.))) |>
  dplyr::left_join(infotable, by = "filename") |>
  dplyr::reframe(version, release_date, code, type, classification, specialization, definition, notes)

nucc_taxonomy_110 <- readr::read_csv(fs::dir_info(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))$path[3], id = "filename", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::slice(2:n()) |>
  dplyr::mutate(filename = basename(filename),
                dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "")),
                dplyr::across(dplyr::everything(), ~ stringr::str_squish(.)),
                dplyr::across(dplyr::everything(), ~ fuimus::remove_quotes(.))) |>
  dplyr::left_join(infotable, by = "filename") |>
  dplyr::reframe(version, release_date, code, type, classification, specialization, definition, notes)

nucc_taxonomy_111 <- readr::read_csv(fs::dir_info(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))$path[4], id = "filename", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::slice(2:n()) |>
  dplyr::mutate(filename = basename(filename),
                dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "")),
                dplyr::across(dplyr::everything(), ~ stringr::str_squish(.)),
                dplyr::across(dplyr::everything(), ~ fuimus::remove_quotes(.))) |>
  dplyr::left_join(infotable, by = "filename") |>
  dplyr::reframe(version, release_date, code, type, classification, specialization, definition, notes)

nucc_taxonomy_120 <- readr::read_csv(fs::dir_info(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))$path[5], id = "filename", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::slice(2:n()) |>
  dplyr::mutate(filename = basename(filename),
                dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "")),
                dplyr::across(dplyr::everything(), ~ stringr::str_squish(.)),
                dplyr::across(dplyr::everything(), ~ fuimus::remove_quotes(.))) |>
  dplyr::left_join(infotable, by = "filename") |>
  dplyr::reframe(version, release_date, code, type, classification, specialization, definition, notes)

nucc_taxonomy_121 <- readr::read_csv(fs::dir_info(glue::glue("{here::here()}/posts/taxonomy/data/csvs"))$path[6], id = "filename", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::slice(2:n()) |>
  dplyr::mutate(filename = basename(filename),
                dplyr::across(dplyr::everything(), ~ dplyr::na_if(., "")),
                dplyr::across(dplyr::everything(), ~ stringr::str_squish(.)),
                dplyr::across(dplyr::everything(), ~ fuimus::remove_quotes(.))) |>
  dplyr::left_join(infotable, by = "filename") |>
  dplyr::reframe(version, release_date, code, type, classification, specialization, definition, notes)


vctrs::vec_rbind(
  nucc_taxonomy_90,
  nucc_taxonomy_91,
  nucc_taxonomy_100,
  nucc_taxonomy_101,
  nucc_taxonomy_110,
  nucc_taxonomy_111,
  nucc_taxonomy_120,
  nucc_taxonomy_121
  ) |>
  write_csv(glue::glue("{here::here()}/posts/taxonomy/data/nucc_tax_90_121.csv"))
