#' Checks if the expected columns are all in a given character vector. Error otherwise
#' @param expected_colnames the colnames that should be in a dt
#' @param df the dataframe in which the columns are expected
#' @noRd
check_columns <- function(expected_colnames, df){
  cols <- colnames(df)
  col_not_found <- expected_colnames[!expected_colnames %in% cols]
  if(length(col_not_found) > 0)
    stop("The following columns are needed, but not found: ", col_not_found)
}
