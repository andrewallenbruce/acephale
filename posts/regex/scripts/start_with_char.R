group_4_num <- function(x) {

  map(x, function(x) {

    idx <- data.table(
      code = x,
      grp1 = sf_sub(x, 1, 2),
      grp2 = sf_sub(x, 1, 3),
      a1 = take_at(x),
      a2 = take_at(x, 2),
      a3 = take_at(x, 3),
      a4 = take_at(x, 4),
      i1 = groupid(take_at(x))) |>
      fgroup_by(a1) |>
      fmutate(i2 = groupid(a2)) |>
      fgroup_by(a1, a2) |>
      fmutate(i3 = groupid(a3)) |>
      fgroup_by(a1, a2, a3) |>
      fmutate(i4 = groupid(a4)) |>
      fungroup()

    lone <- fcount(idx, i1, add = TRUE) |>
      fsubset((i2 + i3 + i4 + N) == 4)

    last <- fsubset(idx, code %!in% lone[["code"]])

    last <- join(
      fcount(last, grp1, a3),
      fcount(last, grp1, name = "G"), on = "grp1", verbose = 0) |>
      fsubset(N == G) |>
      join(last, on = c("grp1", "a3"), how = "right", verbose = 0) |>
      fsubset(not_na(N))

    rest <- fsubset(idx, code %!in% c(lone[["code"]], last[["code"]]))

    c(if (empty(lone)) NULL else list(lone[["code"]]),
      if (empty(last)) NULL else gchop(last[["code"]], last[["grp1"]]),
      if (empty(rest)) NULL else gchop(rest[["code"]], rest[["grp2"]]))
  })
}

group_4_chr <- function(x) {

  x <- c("C751", "C970", "G020", "G040")

  map(x, function(x) {

    idx <- data.table(
      code = x,
      grp = sf_sub(x, 2, 3),
      a1 = take_at(x, 2),
      a2 = take_at(x, 3),
      a3 = take_at(x, 4),
      i1 = groupid(take_at(x, 2))
    ) |>
      fgroup_by(a1) |>
      fmutate(i2 = groupid(a2)) |>
      fgroup_by(a1, a2) |>
      fmutate(i3 = groupid(a3)) |>
      fungroup()

    lone <- fcount(idx, i1, add = TRUE) |>
      fsubset((i2 + i3 + N) == 3)

    last <- fsubset(idx, code %!in% lone[["code"]])

    last <- join(
      fcount(last, a1, a2),
      fcount(last, a1, name = "G"),
      on = "a1",
      verbose = 0
    ) |>
      fsubset(N == G) |>
      join(last,
           on = c("a1", "a2"),
           how = "right",
           verbose = 0) |>
      fsubset(not_na(N))

    rest <- fsubset(idx, code %!in% c(lone[["code"]], last[["code"]]))

    c(
      if (empty(lone))
        NULL
      else
        list(lone[["code"]]),
      if (empty(last))
        NULL
      else
        gchop(last[["code"]], last[["a1"]]),
      if (empty(rest))
        NULL
      else
        gchop(rest[["code"]], rest[["grp"]])
    )
  })
}

group_4 <- function(x) {

  x <- getelem(x, "x4")

  if (empty(x)) return(x)

  x_chr <- map(x, \(x) sf_extract(x, "^[A-Z]")) |> compact()

  x_num <- map(x, \(x) sf_extract(x, "^[0-9]")) |> compact()

  c(
    if (empty(x_chr)) NULL else group_4_chr(x_chr),
    if (empty(x_num)) NULL else group_4_num(x_num)
  )
}

c("C751", "C970", "G020", "G040") |>
  split_lengths() |>
  remove_redundant() |>
  split_first() |>
  group_4()
