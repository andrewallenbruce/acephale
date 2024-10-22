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
