#' Consistent and informative error
#' @importFrom rlang abort
#' @param arg The name of the argument not being correct
#' @param must Description of what it should be
#' @param not Description of what it should NOT be
#'
#' Taken from https://adv-r.hadley.nz/conditions.html#signalling-conditions
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- paste0("`", arg, "` must ", must)
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- paste0(msg, "; not ", not)
  }

  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not
  )
}


get_func_args <- function(FUN, data, ...) {
  args <- list(...)

  parameters_FUN <- attr(FUN, "parameters")
  if (!is.null(parameters_FUN)) {
    f_params <- attr(FUN, "parameters")()
    args <- args[f_params[f_params %in% names(args)]]
  }

  args <- append(args, list(data = data))
  return(args)
}



