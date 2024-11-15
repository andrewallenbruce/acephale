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


rx <- list(
  All              = "^[A-CEGHJ-MP-V0-9][0-9]{3}[AFMTU0-9]$",
  "-Level I"       = "^[0-9]{4}[AFMTU0-9]$",
  "--Category I"   = "^[0-9]{4}[AMU0-9]$",
  "--Category II"  = "^[0-9]{4}F$",
  "--Category III" = "^[0-9]{4}T$",
  "-Level II"      = "^[A-CEGHJ-MP-V][0-9]{4}$"
)

greys <- c("black", "grey20", "grey50", "grey80", "grey90", "grey20")

len <- \(rx) cheapr::vector_length(vctrs::vec_slice(hcpcs, sf_detect(hcpcs, rx)))

data.frame(
  row.names = names(rx),
  Count = map_int(delist(rx), len),
  Regex = delist(rx)) |>
  hl(greys, cols = 1) |>
  knit_print.emphatic()


end_chr <- sf_convert(sf_extract(hcpcs, "[A-Z]$"))

message("HCPCS Ending with Letter:")
print_ls(as.list(table(sf_extract(sf_sub(end_chr, 5, 5), "[A-Z]"))))
message("Total: ", length(end_chr))

start_chr <- sf_convert(sf_extract(hcpcs, "^[A-Z]"))

message("HCPCS Beginning with Letter:", sep = "\n")
print_ls(as.list(table(sf_extract(sf_sub(start_chr, 1, 1), "[A-Z]"))))
message("Total:", length(start_chr))


# CLI ---------------------------------------------------------------------

box_chars <- function(ansi = TRUE) {

  orange <- crayon::make_style("orange")

  if (ansi) {
    list(
      "h"  = "\u2500",         # - horizontal
      "hd" = "\u2504",         # - horizontal dotted
      "v"  = "\u2502",         # | vertical
      "vd" = "\u250A",         # | vertical dotted
      "l"  = "\u2514",         # \ leaf
      "j"  = "\u251C",         # + junction
      "n"  = orange("\u2588")  # X node
    )
  } else {
    list(
      "h"  = "-",
      "hd" = "-", # Just use normal chars for dotted
      "v"  = "|",
      "vd" = "|",
      "l"  = "\\",
      "j"  = "+",
      "n"  = orange("o")
    )
  }
}

grey <- function(...) {
  crayon::make_style(grDevices::grey(0.5), grey = TRUE)(...)
}

# string -----------------------------------------------------------------

str_dup <- function(x, n) {

  vapply(
    n,
    function(i)
      paste0(
        rep(x, i),
        collapse = ""),
    character(1))
}

str_indent <- function(x, first, rest) {

  if (length(x) == 0) {

    character()

  } else if (length(x) == 1) {

    paste0(first, x)

  } else {

    c(
      paste0(first, x[[1]]),
      paste0(rest, x[-1L])
    )

  }
}

str_truncate <- function(x, n) {

  too_long <- nchar(x, type = "width") > n

  x[too_long] <- paste0(substr(x[too_long], 1, n - 3), "...")

  x
}

print_line <- function(x, ...) {

  cat(paste(x, "\n", collapse = ""), sep = "")

  invisible(x)

}

cat_line <- function(...) {

  cat(paste0(..., "\n", collapse = ""))

}


cutoff <- \(x) stringr::str_trunc(string = x, width = 40, side = "right")
wrap   <- \(x) stringr::str_wrap(string = x, width = 60, exdent = 5, whitespace_only = FALSE)

view <- \(x) {
  idx  <- seq_along(x)
  len  <- vlen(x)
  tree <- unname(purrr::map_vec(x, paste0, collapse = " ")) |> purrr::map_vec(cutoff)

  glue::glue("{format(idx)}: ",
             "{format(len)}|",
             "--[{if (empty(tree)) NULL else format(tree, justify = 'none')}]")
}

view_remove <- \(x) {

  len  <- length(x)
  tree <- unname(paste0(x, collapse = " ")) |> purrr::map_vec(cutoff)

  glue::glue("Removed ",
             "{format(len, justify = 'left')}|",
             "--[{if (empty(tree)) NULL else format(tree, justify = 'none')}]")
}

split_lengths <- function(x, verbose = FALSE) {

  stopifnot(is.character(x))

  x <- sf_remove(x, "\\*|\\s") |>
    unique_narm() |>
    stringr::str_sort()

  l <- vlen(x)

  out <- list(
    x1 = x[l == 1],
    x2 = x[l == 2],
    x3 = x[l == 3],
    x4 = x[l == 4],
    x5 = x[l == 5])

  if (verbose) {

    cli::cat_boxx(
      label = view(out),
      header = glue::glue("{cli::symbol$stop} Split By Lengths"),
      align = "left",
      padding = c(0, 1, 0, 1))

    return(invisible(out))

  } else {
    return(out)
  }
}

remove_redundant <- function(x, verbose = FALSE) {

  .c(x1, x2, x3, x4, x5) %=% x

  out <- list(
    x1 = x1,
    x2 = rr(x2, x1),
    x3 = rr(x3, x1) |> rr(x2),
    x4 = rr(x4, x1) |> rr(x2) |> rr(x3),
    x5 = rr(x5, x1) |> rr(x2) |> rr(x3) |> rr(x4)
  )

  if (verbose) {

    cli::cat_boxx(
      label = view(out),
      header = glue::glue("{strrep(cli::symbol$stop, 2)} Remove Redundancies"),
      align = "left",
      padding = c(0, 1, 0, 1))

    return(invisible(out))

  } else {
    return(out)
  }
}
