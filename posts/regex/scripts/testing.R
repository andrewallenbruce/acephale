
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

long_test <- c(
  "0214T",
  "0215T",
  "0217T",
  "0218T",
  "0219T",
  "0220T",
  "0221T",
  "0222T",
  "0263T",
  "0265T",
  "0266T",
  "0269T",
  "0274T",
  "0275T",
  "0329T",
  "0330T",
  "0422T",
  "0444T",
  "0445T",
  "0506T",
  "0507T",
  "15777",
  "20939",
  "22510",
  "22511",
  "22512",
  "22513",
  "22514",
  "22515",
  "22526",
  "22527",
  "27197",
  "27198",
  "30801",
  "30802",
  "31231",
  "32673",
  "34713",
  "34714",
  "34715",
  "34716",
  "34717",
  "36221",
  "34812",
  "34820",
  "34833",
  "34834",
  "35572",
  "50300",
  "50540",
  "54420",
  "54430",
  "55200",
  "55250",
  "55300",
  "58575",
  "58600",
  "58605",
  "58700",
  "58720",
  "58800",
  "58805",
  "58900",
  "58920",
  "58925",
  "58940",
  "58943",
  "61000",
  "61001",
  "61253",
  "63035",
  "63043",
  "63044",
  "63045",
  "63046",
  "63047",
  "63048",
  "64421",
  "64480",
  "64484",
  "64491",
  "64492",
  "64494",
  "64495",
  "64634",
  "64636",
  "76514",
  "92025",
  "92081",
  "92082",
  "92083",
  "92132",
  "92133",
  "92134",
  "92145",
  "92201",
  "92202",
  "92227",
  "92228",
  "92229",
  "92235",
  "92240",
  "92242",
  "95870",
  "C7501",
  "C7502",
  "C7504",
  "C7505",
  "C9771",
  "E0675",
  "G0279",
  "G0412",
  "G0413",
  "G0414",
  "G0415",
  "S2342")

length(long_test)

x <- long_test

x <- multi$C

comsub<-function(x) {

  x <- sort(x)

  idx <- strsplit(x, "")

  idx <- t(as.data.frame(idx))

  rownames(idx) <- NULL

  idx <- as.data.frame(idx)

  collapse::fduplicated(idx$V1, all = TRUE)
  duplicated(as.data.frame(idx))

  # search for the first not common element and so, get the last matching one
  der_com <- match(FALSE, do.call("==", idx)) - 1

  # if there is no matching element, return an empty vector, else return the common part
  ifelse(der_com == 0, return(character(0)), return(substr(x[1], 1, der_com)))
}


comsub(multi$C)

idex <- split(x, substr(x, 1, 1))

single <- idex[which(collapse::vlengths(idex) == 1)]

multi <- idex[which(collapse::vlengths(idex) != 1)]

lcPrefix <- function (x, ignore.case = FALSE) {

  x <- as.character(x)

  if (ignore.case) x <- toupper(x)

  nc <- nchar(x, type = "char")

  for (i in 1:min(nc)) {

    ss <- substr(x, 1, i)

    if (any(ss != ss[1])) {

      return(substr(x[1], 1, i - 1))
    }
  }
  substr(x[1], 1, i)
}



x <- long_test


idx <- strsplit(x, substr(x, 1, 1))

grp_lst <- dplyr::tibble(
  g1 = purrr::map_chr(idx, 1) |> vctrs::vec_identify_runs(),
  x = idx) |>
  tidyr::unnest(x) |>
  dplyr::mutate(id = rep.int(1:5, length(idx)), .before = x) |>
  tidyr::pivot_wider(names_from = id, values_from = x, names_prefix = "x", values_fn = list) |>
  tidyr::unnest(dplyr::starts_with("x")) |>
  dplyr::mutate(g2 = dplyr::consecutive_id(x1, x2), .before = x1) |>
  dplyr::group_by(g1, g2) |>
  fuimus::combine(name = x12, c("x1", "x2"), sep = "") |>
  dplyr::group_split() |>
  print(n = 200)

purrr::map_int(grp_lst, collapse::fnrow)

grp_lst[collapse::whichv(purrr::map_int(grp_lst, collapse::fnrow), 1, invert = TRUE)]


grp_lst[collapse::whichv(purrr::map_int(grp_lst, collapse::fnrow), 1)] |>
  purrr::list_rbind() |>
  dplyr::select(-g1, -g2) |>
  dplyr::reframe(x = glue::glue("{x12}{x3}{x4}{x5}"))

strings <- long_test

longest_common_prefix <- \(strings) {

  if (length(strings) == 0) { return("") }

  prefix <- strings[1]

  for (string in strings) {

    while (substring(string, 1, nchar(prefix)) != prefix) {

      prefix <- substring(prefix, 1, nchar(prefix) - 1)

      if (nchar(prefix) == 0) return("")
    }
  }
  return(prefix)
}

longest_common_prefix(long_test)

group_by_variable_prefix <- function(char_vectors) {

  groups <- list()

  while (length(char_vectors) > 0) {

    # Find the longest common prefix for the remaining vectors
    lcp <- longest_common_prefix(char_vectors)

    # Group vectors that start with the longest common prefix
    group <- char_vectors[startsWith(char_vectors, lcp)]

    # Add the group to the list of groups
    groups <- c(groups, list(group))

    # Remove the grouped vectors from the original vector
    char_vectors <- char_vectors[!startsWith(char_vectors, lcp)]
  }
  return(groups)
}

# Example usage
char_vectors <- c("apple", "apply", "ape", "banana", "bandana", "band")

group_by_variable_prefix(char_vectors)

library(vctrs)

purrr <- c("p", "u", "r", "r", "r")
vec_group_id(purrr)
vec_group_rle(purrr)

groups <- mtcars[c("vs", "am")]
vec_group_id(groups)

group_rle <- vec_group_rle(groups)
group_rle


field(group_rle, "group")
field(group_rle, "length")

vec_run_sizes(paste0(vecs$V1, vecs$V2))
vecs[vec_run_sizes(vecs$V1)]



vec_split(vecs, vecs[c("V1", "V2")]) |> dplyr::tibble()

dplyr::tibble(vec_group_loc(vecs[c("V1")])) |>
  tidyr::unnest(loc) |>
  dplyr::rowwise() |>
  dplyr::mutate(n = nrow(loc)) |>
  dplyr::mutate(n = list(purrr::map_int(loc, nrow))) |>
  purrr::map(~nrow(.x)) |>
  tidyr::unnest(loc) |>
  tidyr::unnest(key) |>
  vec_group_loc()


dataframe_to_list <- \(x) as.list(unname(x))

hcpcs_regex_long <- function(x) {

  x <- gsub(" ", "", fuimus::uniq_rmna(x))

  vecs <- stringr::str_split_fixed(
    x, "", n = max(collapse::vlengths(x))) |>
    as.data.frame()

  one  <- dplyr::filter(vecs, V1 %in% names(which(table(vecs$V1) == 1)))
  rest <- dplyr::filter(vecs, V1 %in% names(which(table(vecs$V1) > 1)))

  dplyr::mutate(rest, .id = dplyr::consecutive_id(V1), .before = 1) |>
    # fuimus::combine(name = key, columns = c("V1", "V2"), sep = "") |>
    tidyr::pivot_longer(
      cols = !.id,
      names_to = "key",
      values_to = "val") |>
    dplyr::reframe(
      .id,
      .key = readr::parse_number(key),
      val) |>
    dplyr::arrange(.id) |>
    dplyr::group_by(.id, .key) |>
    dplyr::mutate(n = dplyr::n_distinct(val)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      names_from = n,
      values_from = val) |>
    print(n = 500)

  vctrs::vec_split(rest, rest[c("V1", "V2")]) |>
    dplyr::tibble() |>
    tidyr::unpack(key) |>
    fuimus::combine(name = key, columns = c("V1", "V2"), sep = "") |>
    tidyr::unnest(val) |>
    dplyr::select(-c(V1, V2)) |>
    as.data.frame() |>
    split(rest[1])

  grps <- dplyr::mutate(
    vecs,
    id = dplyr::consecutive_id(V1, V2)) |>
    dplyr::add_count(id) |>
    tidyr::nest(groups = -c(id, n)) |>
    dplyr::arrange(dplyr::desc(n))

  grps |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(groups) |>
    purrr::pluck(1) |>
    purrr::map(fuimus::uniq_rmna) |>
    purrr::map(pos_re)

  to_brackets <- chops[[1]] |>
    purrr::map(fuimus::uniq_rmna) |>
    purrr::map(pos_re)

  if (length(to_brackets) == 1) {
    to_vec <- purrr::map(to_brackets, id_runs2)
  }

  if (length(to_brackets) > 1) {
    to_vec <- purrr::map(to_brackets, id_runs)
  }

  nobrack <- which(stringr::str_detect(to_vec, "\\[|\\]", negate = TRUE))

  to_vec[nobrack] <- purrr::map(to_vec[nobrack], \(x) paste0("[", x, "]"))

  to_vec <- purrr::list_c(to_vec)

  if (collapse::any_duplicated(to_vec)) {

    # TODO probably need to vectorize this, will surely
    # have more than one unique duplicate out of order

    dupe_idx <- which(collapse::fduplicated(to_vec, all = TRUE))

    rp <- paste0(to_vec[dupe_idx][1], "{", length(dupe_idx), "}")

    to_vec[dupe_idx] <- rp

    to_vec <- fuimus::uniq_rmna(to_vec)

  }
  # paste0("^", fuimus::collapser(to_vec), "$")
  fuimus::collapser(to_vec)
}

sapply(hcpcs_tests$two, agrep, hcpcs_tests$two)

mapply(agrep, hcpcs_tests$two, hcpcs_tests$two, value = TRUE)

hcpcs_tests$two[hcpcs_tests$two == hcpcs_tests$three]

Reduce(intersect, list(hcpcs_tests$two, hcpcs_tests$three))

rbind(hcpcs_tests$two, hcpcs_tests$three)


pull_orphs <- function(x) {

  if (vctrs::vec_is_empty(x)) return(character(0))

  x[collapse::whichv(collapse::vlengths(x), 1)] |> purrr::list_c()

}

pull_fams <- function(x) {

  if (vctrs::vec_is_empty(x)) return(character(0))

  x[collapse::whichv(collapse::vlengths(x), 1, TRUE)]

}

get_orphans <- function(x) {
  list(
    o2 = pull_orphs(x$g2),
    o3 = pull_orphs(x$g3),
    o4 = pull_orphs(x$g4),
    o5 = pull_orphs(x$g5))
}

get_families <- function(x) {
  list(
    f1 = x$g1,
    f2 = pull_fams(x$g2),
    f3 = pull_fams(x$g3),
    f4 = pull_fams(x$g4),
    f5 = pull_fams(x$g5))
}

orphans  <- get_orphans(groups)

families <- get_families(groups)

orphans

families

process_orphans <- function(x) {

  list(
    o2 = vectoregex(x$o2, 3),
    o3 = vectoregex(x$o3, 2),
    o4 = vectoregex(x$o4, 1),
    o5 = vectoregex(x$o5, NULL)) # |>
  #purrr::compact()

  # as.character(glue::glue_collapse(orph, sep = "|"))

}

orphan_regex <- process_orphans(orphans)

orphan_regex
