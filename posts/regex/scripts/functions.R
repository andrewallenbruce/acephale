sf <- list(
  c = \(...) stringfish::sf_concat(...),
  p = \(..., sep = "") stringfish::sf_paste(..., sep = sep, nthreads = 4L),
  p0 = \(x, sep = "") stringfish::sf_collapse(x = x, collapse = sep),
  gsub = \(x, p, r, ...) stringfish::sf_gsub(subject = x, pattern = p, replacement = r, nthreads = 4L, ...),
  grepl = \(x, p, ...) stringfish::sf_grepl(subject = x, pattern = p, nthreads = 4L, ...),
  substr = \(x, i, j, ...) stringfish::sf_substr(x = x, start = i, stop = j, nthreads = 4L),
  compare = \(long, short) stringfish::sf_compare(x = long, y = short, nthreads = 4L),
  match = \(x, t) stringfish::sf_match(x = x, table = t, nthreads = 4L),
  nchar = \(x) stringfish::sf_nchar(x = x, type = "chars", nthreads = 4L),
  split = \(x, d, ...) stringfish::sf_split(subject = x, split = d, nthreads = 4L),
  assign = \(x, i, e) stringfish::sf_assign(x = x, i = i, e = e)
)

sf$c("a", "b", "c")
sf$p(c("a", "b", "c"))
sf$gsub("a", "a", "b")
sf$grepl("a", "a")


random_hcpcs <- \(n, l) {

  h <- stringfish::convert_to_sf(
    collapse::funique(
      northstar::search_descriptions()$hcpcs_code))

  stringfish::sf_substr(
    sample(h, size = n),
    start = 1,
    stop = l,
    nthreads = 4L)
}

random_hcpcs_vec2 <- \(n = 10) {

  c(
    sample(
      c(LETTERS[
        stringfish::sf_grepl(
          LETTERS, "[^DINOW-Z]", nthreads = 4L)], 0:9),
      size = sample.int(5, 1)),
    codex::random_hcpcs(n = n, l = 2),
    codex::random_hcpcs(n = n, l = 3),
    codex::random_hcpcs(n = n, l = 4),
    codex::random_hcpcs(n = n, l = 5)
  )

}

# Returns length of vector `hcpcs` that match regular expression `rx`
vlen <- \(hcpcs, rx) {

  cheapr::vector_length(
    vctrs::vec_slice(
      hcpcs, stringfish::sf_grepl(hcpcs, rx)
      )
    )

}

# Returns character at position `i`
takei <- \(x, i = 1) stringfish::sf_substr(x, start = i, stop = i, nthreads = 4L)

empty <- \(x) vctrs::vec_is_empty(x)

# Detects if a character vector contains ONE uppercase letter
grabaz <- \(x) { x[stringfish::sf_grepl(x, "[A-Z]{1}", nthreads = 4L)] }

# Splits a character vector into a list of character vectors
chop <- \(v, g) vctrs::vec_chop(v, sizes = vctrs::vec_run_sizes(g))

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
