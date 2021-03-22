#' Automatically annotate a raw single animal behavioral dataset
#'
#' Given a raw behavr table and a list of annotating functions
#' return a list of tables with one resulting table per input function
#'
#' @details All functions should summarise using the same windows
#' out should contain a single id. If you need to analyze data for several flies
#' call `annotate` like this: `data[, annotate(.SD, FUN, progress), by = "id"]`
#' or call `fslscopr::annotate_all`. This is needed because annotate
#' is NOT id-aware. As such, the result is NOT a behavr table!
#'
#' @param out A raw i.e. one row per frame behavr table
#' @param FUN A function or a list of functions describing how to aggregate the data into
#' non overlapping and consecutive time_windows of a fixed length. It should generate at least one
#' new column where each value is a summary statistic for the corresponding time window
#' @param updateProgress A function to update a shiny::Progress instance
#' @return A behavr table with all summary statistics in different columns
#' @importFrom purrr map imap
#' @importFrom data.table setkeyv
#' @export
annotate <- function(out, FUN=NULL, updateProgress=NULL, region_id = NULL, path = NULL, ...) {
  ## Annotate
  ## ----
  # Declare a list to store the annotations
  # produced by each passed FUN
  annotations <- list()

  if (is.function(updateProgress)) {
    updateProgress(detail = sprintf("Annotating %s", unique(out$id)))
  }
  annotate_enclosed <- function(func, i) {
    # fun_name <- as.character(substitute(FU))
    message(sprintf('Running annotation function #%d', region_id))
    # Run the annotation function on the data loaded from SQL
    # The return value is a behavr table with data for a single fly
    # The metadata in this table is a one row table
    # with a single column called
    # containing the id of the fly,
    # as taken from the id column in out
    out_annotated <- func(out, ...)

    # Check if the annotation is empty
    is_empty <- is.null(out_annotated)
    if (!is_empty) is_empty <- nrow(out_annotated) == 0

    # If it is empty, emit a warning
    # set the corresponding entry to NULL
    # and go on to the next iteration
    # TODO NULL entries are ignored? How
    if (is_empty) {
      warning(sprintf("No data in ROI %i after running FUN, from FILE %s. Skipping", region_id, path))
      return(NULL)
      # If it is not empty, set the id and t columns as keys
      # and store it in the right slot
    } else {
      if ("id" %in% colnames(out_annotated)) data.table::setkey(out_annotated, id)

      # message(sprintf('Done with annotation function %d', i))
    }
    return(out_annotated)
  }


  # Annotation functions are available
  if (!is.null(FUN)) {
    # If FUN is a function then make it a list of a single function
    # This way a program designed to work with a list of functions
    # also works when there is only 1 and the user does not wrap it around list()
    if (is.function(FUN)) {FUN <- list(FUN)}
    annotations <- purrr::imap(FUN, ~annotate_enclosed(.x, .y))

    # if no annotation FUN is passed
  } else {
    annotations[[1]] <- out
  }

  # merge all the annotations into a single data.table
  out <- Reduce(fslbehavr::merge_behavr, annotations)

  return(out)

}


#' @importFrom data.table setkey
#' @importFrom fslbehavr setmeta
#' @export
annotate_all <- function(data, ...) {

  metadata <- data[, meta = T]
  data <- data[, annotate(.SD, ...), by = eval(data.table::key(data))]
  data.table::setkey(data, id)
  fslbehavr::setmeta(data, metadata)
  data
}
