#' Parse a character date into R
#'
#' Expected format is "YYYY-MM-DD"
#' @importFrom readr parse_date stop_for_problems
parse_date <- function(x, format="", tz="UTC"){

  # if x is already one of these three classes, it is already parsed
  if(any(c("POSIXct","numeric", "Date") %in% class(x)))
    return(x)
  # if it's a character, we need to parse it
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if(is.character(x)){
    out <- readr::parse_date(x, format=format, locale = readr::locale(tz = tz))
    readr::stop_for_problems(out)
    return(out)
  }
  stop("Unexpected type for x")
}


#' Parse a time date into R
#'
#' Expected format is "HH:MM:SS"
#' @importFrom readr parse_time stop_for_problems
parse_time <- function(x, format="", tz="UTC"){

  x <-  as.character(x)
  if("difftime" %in% class(x))
    return(x)
  if(is.character(x)){
    out <- readr::parse_time(x, format=format, locale = readr::locale(tz = tz))
    readr::stop_for_problems(out)
    return(out)
  }
  stop("Unexpected type for x")
}
