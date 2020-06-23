#' Merge annotation function results into a single dataframe
#'
#' This function merges the output of two different annotation functions for the same dataset,
#' represented by x and y, in such a way that x gains all the columns in y not present in x
#' The merge is performed using the t column
merge_annotations <- function(x, y) {
  columns_x <- colnames(x)
  data <- merge(x, y[, c("t", setdiff(colnames(y), columns_x)), with = F], by = "t")
  return(data)
}
