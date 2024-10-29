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

random_hcpcs_vec <- \(n = 10) {

  nn <- if (n >= 10) 10 else n

  c(
    sample(
      c(LETTERS[
        stringfish::sf_grepl(
          LETTERS, "[^DINOW-Z]", nthreads = 4L)], 0:9),
      size = nn),
    random_hcpcs(n = n, l = 2),
    random_hcpcs(n = n, l = 3),
    random_hcpcs(n = n, l = 4),
    random_hcpcs(n = n, l = 5)
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
takei <- \(x, i = 1) { stringfish::sf_substr(x, start = i, stop = i, nthreads = 4L) }

empty <- \(x) { vctrs::vec_is_empty(x) }

# Detects if a character vector contains ONE uppercase letter
grabaz <- \(x) { x[stringfish::sf_grepl(x, "[A-Z]{1}", nthreads = 4L)] }

# Splits a character vector into a list of character vectors
chop <- \(v, g) { vctrs::vec_chop(v, sizes = vctrs::vec_run_sizes(g)) }

# Shortens `x` to length of `y` then
# Returns characters in `x` that are not in `y`
contrast <- \(x, y, negate = FALSE) {

  end <- collapse::funique(
    collapse::vlengths(y, use.names = FALSE)
  )

  ifelse(
    negate,
    x[stringfish::sf_substr(x, start = 1, stop = end, nthreads = 4L) %in% y],
    x[!stringfish::sf_substr(x, start = 1, stop = end, nthreads = 4L) %in% y]
  )
}
