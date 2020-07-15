#' Consistent and informative error
#' @importFrom glue glue
#' @importFrom rlang abort
#'
#' @param arg The name of the argument not being correct
#' @param must Description of what it should be
#' @param not Description of what it should NOT be
#'
#' Taken from https://adv-r.hadley.nz/conditions.html#signalling-conditions
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not
  )
}