#' Take the mode of a distribution
#' Given a numeric input, find the number that is the most common
#' Taken from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
#' @param x Numeric vector
#' @return Mode of the vector. If several (equally frequent) modes are found, return the first one
Mode <- function(x) {
  ux <- unique(x)
  # find how many times each element in x is available in ux
  # sort by first appearance in x
  # i.e. the first element of tabulation counts how many times
  # the first unique entry in x is available in x
  tab <- tabulate(match(x, ux))
  # return the value of ux in the position of the first most common value
  return(ux[tab == max(tab)])
}

#' Apply the mode filter to one column only
#' @param data dataframe
#' @param column Name of column to be filtered
#' @param window_width Number of datapoints the filter looks at when sweeping the data (~kernel size)
#' @return Filtered data
#' @importFrom zoo rollapply
Mode_filter_one_column <- function(data, column, window_width=10) {

  id <- target_column_forbidden_name <- . <- NULL

  data$target_column_forbidden_name <- data[, column]
  data <- data[, .(target_column_forbidden_name := zoo::rollapply(target_column_forbidden_name, FUN = Mode, width = window_width, partial = T)
    ), by = id]

  data[, column] <- data$target_column_forbidden_name
  data[target_column_forbidden_name := NULL]
  return(data)
}


#' Filter noise in the data by taking the most common value
#' This is useful to remove outlier data produced due to (sparse) mistracking results
#' Plese note if mistracking is too frequent, this filter is not useful
#' @param data A dataframe with some column(s) that need(s) to be filtered
#' @param columns Character vector of column names in data.
#' Error is raised if a passed column name is not available in the data
#' @param ... Additional arguments to Mode_filter_one_column
#' @return Filtered data
Mode_filter <- function(data, columns, ...) {

  stopifnot(all(columns %in% colnames(data)))

  for (col in columns) {
     data <- Mode_filter_one_column(data, col, ...)
  }

  return(data)

}
