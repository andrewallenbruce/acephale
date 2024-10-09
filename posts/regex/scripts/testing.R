
# Takes a vector of full 5-character HCPCS codes and
# outputs a list grouping the most similar together

x <- long_test

is_chr <- \(x) {
  stringfish::sf_grepl(x, "[A-Z]")
  # x <- stringfish::sf_grepl(x, "[A-Z]") |> as.numeric()
  # x[x == 0L] <- NA_real_
}

group_hcpcs_vector <- function(x) {

  x <- gsub(" ", "", fuimus::uniq_rmna(x))

  base <- dplyr::tibble(
    code = x,
    a1 = substr(code, 1, 1),
    a2 = substr(code, 2, 2),
    a3 = substr(code, 3, 3),
    a4 = substr(code, 4, 4),
    a5 = substr(code, 5, 5),
    a45 = paste0(a4, a5)
    )

  base |> print(n = 120)

  chars <- base |>
    dplyr::reframe(code, dplyr::across(a1:a5, is_chr)) |>
    dplyr::filter(dplyr::if_any(a1:a5) == TRUE) |>
    rlang::set_names(stringfish::sf_gsub(names(base[1:6]), "a", "c"))

  chars |> print(n = 120)

  indices <- base |>
    dplyr::mutate(i1 = dplyr::consecutive_id(a1), .after = code) |>
    dplyr::mutate(i2 = dplyr::consecutive_id(a2), .after = i1, .by = a1) |>
    dplyr::mutate(i3 = dplyr::consecutive_id(a3), .after = i2, .by = c(a1, a2)) |>
    dplyr::mutate(i4 = dplyr::consecutive_id(a4), .after = i3, .by = c(a1, a2, a3)) |>
    dplyr::mutate(i5 = dplyr::consecutive_id(a5), .after = i4, .by = c(a1, a2, a3, a4)) |>
    dplyr::select(code, a1:a5, a45, i1:i5)

  indices |> print(n = 120)

  singles <- indices |>
    dplyr::add_count(i1, name = "n1", sort = TRUE) |>
    dplyr::filter((i2 + i3 + i4 + i5 + n1) == 5) |>
    dplyr::pull(code)

  # id_consecutive <- \(x) {
  #
  # }

  last <- indices |>
    dplyr::filter(!code %in% singles) |>
    dplyr::select(code, a1:a3, a5) |>
    fuimus::combine(group_id, columns = c("a1", "a2", "a3"), sep = "")

  last <- dplyr::left_join(
    last|> dplyr::count(group_id, a5),
    last|> dplyr::count(group_id, name = "g")) |>
    dplyr::filter(n == g) |>
    dplyr::right_join(last) |>
    dplyr::filter(!is.na(n))


  rest <- indices |>
    dplyr::filter(!code %in% c(singles, dplyr::pull(last, code))) |>
    fuimus::combine(group_id, columns = c("a1", "a2", "a3", "a4"), sep = "")

    # dplyr::select(code, group_id) |>
    # dplyr::mutate(
    #   i4_ = dplyr::lead(i4) - i4,
    #   i4_ = dplyr::if_else(!is.na(i4_), max(i4), i4_),
    #   i4_ = dplyr::if_else(dplyr::consecutive_id(i4_) == 2, max(i4), i4_),
    #   .after = i4,
    #   .by = c(group_id)) |>
    # fuimus::combine(group_id, columns = c("group_id", "i4_"), sep = "") |>
    # dplyr::mutate(
    #   a45 = readr::parse_number(a45),
    #   a5 = dplyr::lead(a45),
    #   a5 = dplyr::if_else(is.na(a5), max(a45) + 1, a5),
    #   a6 = a45 - (a5 - 1),
    #   .after = a45,
    #   .by = c(group_id)
    # ) |>
    # dplyr::select(-c(a45, a5)) |>
    # dplyr::group_by(group_id, i4_) |>
    # tidyr::fill(i4__) |>
    # dplyr::mutate()
    # dplyr::mutate(
    #   i5_ = dplyr::lead(i5) - i5,
    #   i5_ = dplyr::if_else(!is.na(i5_), max(i5), i5_),
    #   i5_ = dplyr::if_else(dplyr::consecutive_id(i5_) == 2, max(i5), i5_),
    #   .by = c(group_id, i4)) |>
    # fuimus::combine(group_id, columns = c("group_id", "i4_"), sep = "") |>
    # dplyr::mutate(group = dplyr::if_else(is.na(i5_) & i4 != 1)) |>
    # dplyr::rowwise() |>
    # dplyr::mutate() |>
    # print(n = 120)

  vctrs::vec_c(
    as.list(singles),
    vctrs::vec_chop(last$code, sizes = vctrs::vec_run_sizes(last$group_id)),
    vctrs::vec_chop(rest$code, sizes = vctrs::vec_run_sizes(rest$group_id))
    )

  groupids <- indices |>
    dplyr::add_count(i1, name = "n1", sort = TRUE) |>
    dplyr::add_count(i1, i2, name = "n2") |>
    dplyr::add_count(i1, i2, i3, name = "n3") |>
    dplyr::add_count(i1, i2, i3, i4, name = "n4") |>
    dplyr::select(-dplyr::num_range("i", 2:5)) |>
    dplyr::mutate(total = n1 + n2 + n3 + n4, .by = code)

  out2 <- groupids |>
    fuimus::combine(group_id, columns = c("i1", "n1", "n2", "n3", "n4", "total"), sep = "-") |>
    dplyr::arrange(group_id) |>
    print(n = 120)

  vctrs::vec_chop(out2$code, sizes = vctrs::vec_run_sizes(out2$group_id))

  output <- groupids |>
    dplyr::arrange(i1, n1, n2, n3, n4) |>
    dplyr::mutate(group = dplyr::consecutive_id(n1, n2, n3, n4),
                  group = dplyr::if_else(total == 4, dplyr::row_number() + max(group) - 1, group - 1),
                  total = NULL,
                  .after = 1) |>
    dplyr::arrange(group, i1, n1, n2, n3, n4) |>
    fuimus::combine(name = subgroup, columns = c("n1", "n2", "n3", "n4"), sep = "") |>
    dplyr::add_count(group, subgroup, name = "n", sort = TRUE) |> print(n = 120)
    dplyr::mutate(
      codesum = purrr::map_dbl(code, function(x) {
        readr::parse_number(x) |>
          as.character() |>
          fuimus::splitter() |>
          as.integer() |>
          sum()}),
      subgroup = NULL,
      .after = 2) |>
    dplyr::mutate(run = cummin(codesum), .by = group, .after = group) |>
    dplyr::reframe(id = dplyr::consecutive_id(group, run), code)

  output |> print(n = 120)

  vctrs::vec_chop(output$code, sizes = vctrs::vec_run_sizes(output$id))
}


group_hcpcs_vector(long_test)[[1]]


purrr::map(long_test[[1]], group_hcpcs_vector)

datawizard::data_transpose(base)

base_long <- tidyr::pivot_longer(
  base,
  cols = a1:a5,
  names_to = "idx",
  values_to = "char") |>
  dplyr::arrange(char, idx)

dplyr::left_join(
  base_long |> dplyr::filter(idx == "a1") |> dplyr::reframe(code, a1 = char),
  base_long |> dplyr::filter(idx != "a1")) |>
  dplyr::arrange(idx) |>
  print(n = 200)

base_long |> dplyr::filter(idx == "a1") |> dplyr::reframe(code, a1 = char) |>
  print(n = 200)

base_long |>
  dplyr::filter(idx != "a1")


matrix(unlist(strsplit(long_test[1:2], "")), ncol = 2, dimnames = list(letters[1:5], letters[1:2]))

mtest <- matrix(
  unlist(
    strsplit(
      c("55300", "55200", "55250"), "")
    ),
  ncol = 3,
  dimnames = list(
    letters[1:5],
    letters[1:3]
    )
  )

mtest[1, ]
all(mtest["a",] == "5")
