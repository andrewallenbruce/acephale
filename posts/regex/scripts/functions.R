sf <- list(
  sf_c = \(...) stringfish::sf_concat(...),
  sf_paste = \(..., sep = "") stringfish::sf_paste(..., sep = sep, nthreads = 4L),
  sf_collapse = \(x, sep = "") stringfish::sf_collapse(x = x, collapse = sep),
  compare = \(this, inside) stringfish::sf_compare(x = inside, y = this, nthreads = 4L),
  match = \(x, table) stringfish::sf_match(x = x, table = table, nthreads = 4L),
  sf_split = \(x, split) stringfish::sf_split(subject = x, split = split, nthreads = 4L),
  assign = \(x, i, e) stringfish::sf_assign(x = x, i = i, e = e)
)

sf$c("a", "b", "c")
sf$p(c("a", "b", "c"))
sf$gsub("a", "a", "b")
sf$grepl("a", "a")

# Returns length of vector `hcpcs` that match regular expression `rx`
vlen <- \(hcpcs, rx) {

  cheapr::vector_length(
    vctrs::vec_slice(
      hcpcs, stringfish::sf_grepl(hcpcs, rx)
      )
    )

}

# Shortens `x` to length of `y` then
# Returns characters in `x` that are not in `y`
# contrast <- \(x, y, negate = FALSE) {
#
#   end <- collapse::funique(
#     collapse::vlengths(y, use.names = FALSE)
#   )
#
#   ifelse(
#     negate,
#     x[stringfish::sf_substr(x, start = 1, stop = end, nthreads = 4L) %in% y],
#     x[!stringfish::sf_substr(x, start = 1, stop = end, nthreads = 4L) %in% y]
#   )
# }

#' Lump a numeric variable into categorical groups
#'
#' @source
#' A modified version of the eponymous function from the
#' [{dumblump}](https://github.com/selkamand/dumblump/blob/master/R/dumblump.R) package, by Sam Elkamand.
#'
#' @param x <numeric> input vector of length 1
#'
#' @param thresh <numeric> Distance between a number and its nearest neighbour that must be equaled or exceeded to be allocated to the same group. Default is `3`.
#'
#' @param prefix <character> optional string to prepend to the group number, e.g. `**<prefix>**<sep><number>`
#'
#' @param sep <character> optional character between the prefix and number, e.g. `<prefix>**<sep>**<number>`
#'
#' @returns <character> vector
#'
#' @details
#' ## The `dumblump` algorithm:
#'
#'   1. Sort numbers in ascending order
#'   2. For each number, check its distance from the previous number (the closest, lower number in dataset).
#'   3. If `distance >= threshold`, define a new group.
#'   4. If `distance < threshold`, 'lump' with the group of the previous number.
#'
#' ## Disadvantages
#'
#' You can get numbers of substantially different scales in a single group.
#' For example, numbers 1 - 100000 will all be classified as a single group
#' unless there's a 'break' of > threshold somewhere along the sequence.
#'
#' @examples
#' lump(c(1, 1, 2, 5, 5 ,6 , 1, 12, 12))
#'
#' @export
lump <- function(x, thresh = 3, prefix = NULL, sep = NULL){

  xo <- order(x)

  xs <- x[xo]

  diff_prev <- abs(c(0, tail(xs, n = -1) - head(xs, n = -1)))

  binary <- ifelse(diff_prev >= thresh, 1, 0)

  id <- cumsum(binary) + 1

  id <- if (!is.null(c(prefix, sep))) paste0(prefix, sep, id) else as.character(id)

  id[order(xo)]

}

library(RPatternJoin)

# teststrings <- c("cat", "ecast", "bat", "cats", "chat")

similarityJoin(
  strings = c("cat", "ecast", "bat", "cats", "chat"),
  cutoff = 2,
  metric = "Levenshtein",
  # metric = "Hamming",
  method = "partition_pattern",
  drop_deg_one = FALSE,
  output_format = "adj_matrix")

data.table::data.table(
  code = x,
  group_id = stringfish::sf_substr(x, 1, 2, nthreads = 4L),
  a1 = take_at(x, 1),
  a2 = take_at(x, 2),
  a3 = take_at(x, 3),
  i1 = data.table::rleid(take_at(x, 1)),
  y = data.table::shift(x, 1, type = "cyclic"),
  jac = jaccard(x, data.table::shift(x, 1, type = "cyclic")))

detect_string <- function(list, strings) {

  purrr::map_lgl(list, \(x) {

    if (TRUE %in% sf_detect(
      x,
      paste(strings, collapse = "|")
    )) {
      TRUE
    } else {
      FALSE
    }
  }
  )
}

x <- list(
  x1 = list("a", "A", "y"),
  x2 = list("b", "B", "y"),
  x3 = list("c", "C", "y"))

x <- list(
  list("a", "A", "y"),
  list("b", "B", "y"),
  list("c", "C", "y"))

# y <- c("a", "B")


detect_list <- \(ll, re, negate = TRUE) {

  fn <- if (negate) !sf_detect else sf_detect

  purrr::map(ll, \(x) {
    TRUE %in% purrr::map_lgl(x, fn, re)
  }
  )
}


detect_list(x, "a|B", TRUE)

# unique_sum
const <- rlang::set_names(1:36, c(0:9, LETTERS))

const

code_count <- \(x) {
  rlang::set_names(
    stringr::str_count(
      x,
      fuimus::uniq_rmna(
        fuimus::splitter(
          x
        ))),
    fuimus::uniq_rmna(
      fuimus::splitter(
        x
      )))
}

counted <- code_count("0002T")

(unname(counted[order(names(counted))])
  %*%
    unname(const[names(const) %in% names(counted)]))[1,1]

sum(unname(counted[order(names(counted))])
    +
      unname(const[names(const) %in% names(counted)]))
