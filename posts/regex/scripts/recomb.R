
sf_extract(hcpcs, "^L(5[69]|80)[0-9]{2}$")

red2test <- sf_collapse(
  c(
  "^0[2-5][A-Z0-9]{3}$",
  "^15[A-Z0-9]{3}$",
  "^2[027][A-Z0-9]{3}$",
  "^3[0-24-6][A-Z0-9]{3}$",
  "^5[0458][A-Z0-9]{3}$",
  "^6[134][A-Z0-9]{3}$",
  "^76[A-Z0-9]{3}$",
  "^9[25][A-Z0-9]{3}$",
  "^C[79][A-Z0-9]{3}$",
  "^E0[A-Z0-9]{3}$",
  "^G0[A-Z0-9]{3}$",
  "^S2[A-Z0-9]{3}$"),
  collapse = "|")

red2test2 <- paste0("^(", sf_collapse(list("0[2-5]", "15", "2[027]", "3[0-24-6]", "5[0458]", "6[134]", "76", "9[25]", "C[79]", "E0", "G0", "S2") |> purrr::list_c(), collapse = "|"), ")[A-Z0-9]{3}$")

length(sf_extract(hcpcs, red2test))

length(sf_extract(hcpcs, red2test2))

# red2()
red2_out <- list("0[2-5]", "15", "2[027]", "3[0-24-6]", "5[0458]", "6[134]", "76", "9[25]", "C[79]", "E0", "G0", "S2") |> purrr::list_c()

# red3()
list(
  list("02[1267]", "03[23]", "04[24]", "050"),
  list("157"),
  list("209", "225", "271"),
  list("308", "312", "326", "34[78]", "355", "362"),
  list("50[35]", "544", "55[23]", "58[5-9]"),
  list("61[02]", "630", "64[46]"),
  list("765"),
  list("92[0-2]", "958"),
  list("C75", "C97"),
  list("E06"),
  list("G0[24]"),
  list("S23")
) |>
  purrr::list_flatten() |>
  purrr::list_c()


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
