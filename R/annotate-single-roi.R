#' Annotate data from a single animal using an annotation function
#'
#' @param data A raw i.e. one row per frame behavr table
#' @param FUN A function describing how to aggregate the data into
#' non overlapping and consecutive time_windows of a fixed length, defined on their time_window_length arg.
#' It must:
#' \itemize{
#' \item Generate at least one new column where each value is a summary statistic for the corresponding time window
#' }

#' @param ... Additional arguments to FUN
annotate_one_func <- function(data, FUN, ...) {

  region_id <- path <- id <- datetime <- NULL

  # Run the annotation function on the data loaded from SQL
  # The return value is a behavr table with data for a single fly
  # The metadata in this table is a one row table
  # with a single column called
  # containing the id of the fly,
  # as taken from the id column in data
  data_annotated <- FUN(data, ...)

  # Check if the annotation is null or has 0 rows (in either case, considered empty)
  is_empty <- is.null(data_annotated)
  if (!is_empty) is_empty <- nrow(data_annotated) == 0

  # If it is empty, emit a warning
  # set the corresponding entry to NULL
  # and go on to the next iteration
  # TODO NULL entries are ignored? How
  if (is_empty) {
    warning("No data in ROI ", region_id, " after running FUN, from FILE ", path, ". Skipping")
    return(NULL)
  } else {
    data.table::setkey(data, id)
    return(data_annotated)
  }
}


#' Automatically annotate a raw single animal behavioral dataset
#'
#' Given a raw behavr table and a list of annotating functions
#' return a list of tables with one resulting table per input function
#'
#' @details All functions should summarise using the same windows
#' out should contain a single id. If you need to analyze data for several flies
#' call `annotate_single_roi` like this: `data[, annotate(.SD, FUN, progress), by = "id"]`
#' or call `scopr::annotate_all`. This is needed because annotate_single_roi
#' is NOT id-aware (because it expects a single animal). As such, the result is NOT a behavr table!
#' @inheritParams annotate_one_func
#' @param FUN A function or list of functions
#' @seealso annotate_one_func
#' @return A behavr table with all summary statistics in different columns
#' @import data.table
#' @importFrom purrr map map
#' @importFrom behavr merge_behavr
#' @export
annotate_single_roi <- function(data, FUN=NULL, ...) {

  ## Annotate one single animal (ROI)
  ## ----
  # Declare a list to store the annotations
  # produced by each passed FUN
  annotations <- list()

  # Annotation functions are available
  if (!is.null(FUN)) {
    # If FUN is a function then make it a list of a single function
    # This way a program designed to work with a list of functions
    # also works when there is only 1 and the user does not wrap it around list()
    if (is.function(FUN)) {FUN <- list(FUN)}
    annotations <- lapply(FUN, function(x) annotate_one_func(data=data, FUN=x, ...))

    # if no annotation FUN is passed
  } else {
    annotations[[1]] <- data
  }

  # merge all the annotations into a single data.table
  data_annotated <- Reduce(behavr::merge_behavr, annotations)
  return(data_annotated)
}


#' Annotate a behavr table, possibly with more than one id (animal)
#'
#' @import data.table
#' @importFrom behavr setmeta
#' @param data A multi animal behavr table to be pre analyzed or annotated
#' @param ... Additional arguments to annotae_single_roi
#' @export
annotate_all <- function(data, ...) {

  id <- NULL

  metadata <- data[, meta = T]
  data <- data[, annotate_single_roi(.SD, ...), by = eval(data.table::key(data))]
  data.table::setkey(data, id)
  behavr::setmeta(data, metadata)
  return(data)
}
