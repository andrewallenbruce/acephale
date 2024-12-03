#' NPPES Other Identifier Type
#'
#' @param x `<chr>` vector of `otid` types
#'
#' @returns `<chr>` vector of `otid` descriptions
#'
#' @examples
#' other_id_type(as.character(0:9))
#'
#' @autoglobal
#'
#' @family NPPES Decoders
#'
#' @export
other_id_type <- function(x) {

  kit::nswitch(
    x = x,
    "01", "Other",
    "02", "Medicare UPIN",
    "04", "Medicare ID-Type Unspecified",
    "05", "Medicaid",
    "06", "Medicare Oscar-Certification",
    "07", "Medicare NSC",
    "08", "Medicare PIN",
    default = NA_character_
  )
}

#' NPPES Other Name Type
#'
#' @param x `<chr>` vector; `other_last_type` or `other_org_type`
#'
#' @returns `<chr>` vector of `other_*_type` descriptions
#'
#' @examples
#' other_id_type(as.character(1:5))
#'
#' @autoglobal
#'
#' @family NPPES Decoders
#'
#' @export
other_name_type <- function(x) {

  kit::nswitch(
    x = x,
    "1", "Former Name",
    "2", "Professional Name",
    "3", "Doing Business As",
    "4", "Former Legal Business Name",
    "5", "Other Name",
    default = NA_character_
  )
}
