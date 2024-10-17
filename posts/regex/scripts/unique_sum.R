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
