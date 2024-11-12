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

ascii_box <- \(msg) {
  cat(
    paste0(
      "┌", strrep("─", sf_nchar(msg) + 2), "┐\n",
      "│ ", msg,                          " │\n",
      "└", strrep("─", sf_nchar(msg) + 2), "┘\n"
    )
  )
}

ascii_box("Hello, World!")


random_hcpcs2 <- function(n = 10) {

  p <- sf_concat(sf_extract(LETTERS, "[^DINOW-Z]"), 0:9)

  h <- sf_convert(codex:::get_pin("hcpcs_vec"))

  c(
    cheapr::sample_(x = p, size = sample.int(5, 1)),
    sf_sub(cheapr::sample_(x = h, size = n), stop = 2),
    sf_sub(cheapr::sample_(x = h, size = n), stop = 3),
    sf_sub(cheapr::sample_(x = h, size = n), stop = 4),
    sf_sub(cheapr::sample_(x = h, size = n), stop = 5)
  ) |>
    sf_convert()
}
