#' Load data from a SQLite database for one ROI and preanalyze it

#' We obtain data from one ROI and optionaly preanalyse it, by applying FUN.
#' This function is run on single individual data
#' The actual work is done by parse_single_roi_wrapped. This function just does the following things before calling parse_single_roi_wrapped:
#' \itemize{
#' \item{Figure out which columns need to be queried from the SQLite file}
#' \item{Report information messages if the user wants to}
#' \item{Set up a memoised cache}
#' \item{Confirm the passed input leads to a sqlite file (the file ends in db) and check its size. The size is used later on to check whether the cache is invalid or not}
#' }
#' Very importantly, after calling parse_single_roi_wrapped, the passed metadata input is bound to the loaded data into a behavr table
#' @param data one row data.table corresponding to a row of the original (and then linked) metadata
#' @param verbose boolean, if TRUE, all information messages are printed on the console. The loading process is silent otherwise
#' @param cache character, path to a folder in the filesystem where cache files should be saved or searched to speed up future reloads of the same data
#' @inheritParams annotate_single_roi
#' @inheritParams read_single_roi
#' @inheritParams parse_single_roi_wrapped
#' @param ... Additional arguments to FUN
#' @importFrom memoise cache_filesystem
#' @return A behavr table containing the loaded and pre analyzed data
#' @seealso \url{https://github.com/shaliulab/behavr}
parse_single_roi <- function(data,
                             min_time = 0,
                             max_time = +Inf,
                             reference_hour = NULL,
                             columns = NULL,
                             cache=NULL,
                             verbose = FALSE,
                             FUN = NULL,
                             updateProgress_load = NULL,
                             updateProgress_annotate = NULL,
                             ...){

  roi_idx <- NULL
  id <- data$id
  region_id <- data$region_id
  path <- data$file_info[[1]]$path

  # save the name of the columns the FUN annotation
  # method needs by running its needed_columns
  # attribute if it is defined
  if (is.null(columns) & !is.null(FUN)) {
    needed_columns <- attr(FUN, "needed_columns")
    if (!is.null(needed_columns))
      columns <- needed_columns(...)
  }



  # Check the path leads to a sqlite3 file
  if (tools::file_ext(path) != "db")
    stop(sprintf("Unsuported file extention in %s", path))


  # Compute the filesize of the sqlite3 file
  # TODO Verify this is done to check whether the cache is invalid
  fs = file.info(path)["size"]

  # If the cache defined, wrap the analysis in the memoise function
  # to cache the results for faster reloading
  if (!is.null(cache)) {
    db <- memoise::cache_filesystem(cache, algo = "md5")
    parse_single_roi_wrapped_memo <- memoise::memoise(parse_single_roi_wrapped, cache = db)
  } else {
    # otherwise the "cached" version is just the original function
    parse_single_roi_wrapped_memo <- parse_single_roi_wrapped
  }


  # Call the analysis function, cached or not
  out <- parse_single_roi_wrapped_memo(
    id,
    region_id,
    path,
    min_time,
    max_time,
    reference_hour,
    columns,
    file_size = fs,
    verbose=verbose,
    FUN,
    updateProgress_load=updateProgress_load,
    updateProgress_annotate=updateProgress_annotate,
    ...
  )

  if (!is.null(out))
    behavr::setbehavr(out, data)
  return(out)
}


#' Load data from a sqlite database and preanalyze (annotate) it with FUN
#'
#' Loading is done by read_single_roi. Annotation is done by annotate_single_roi
#'
#' @param FUN, function or list, a function or list of functions that processes the data loaded from the SQLite file in some meaningful way
#' @inheritParams read_single_roi
#' @import data.table
#' @importFrom behavr setbehavr
parse_single_roi_wrapped <- function(id, region_id, path,
                                     min_time = 0,
                                     max_time = +Inf,
                                     reference_hour = NULL,
                                     columns = NULL,
                                     file_size = 0,
                                     verbose = FALSE,
                                     FUN = NULL,
                                     updateProgress_load = NULL,
                                     updateProgress_annotate = NULL,
                                     ...
){

  # TODO The file_size is not used

  ## Read data for a single ROI (fly) from the SQLite file
  ## ----
  time_stamp <- NULL

  # if verbose, log some information to the user so he knows
  # a new fly is being loaded (progress tracking)
  if (verbose) {
    info_message <- sprintf("Loading ROI number %i from:\n\t%s\n", region_id, path)
    message(info_message)
    # additionally, if a progress bar is available, update it as well
    # TODO The message is getting updated but the value is not

  }

  out <- read_single_roi(path,
                         region_id = region_id,
                         min_time = min_time,
                         max_time = max_time,
                         reference_hour = reference_hour,
                         columns = columns,
                         time_stamp = time_stamp
  )

  if (is.function(updateProgress_load)) {
     info_message <- sprintf("Loaded ROI number %i from:\n\t%s\n", region_id, path)
   updateProgress_load(info_message)
  }


  ## ----
  ## Check whether any data could be loaded or not
  if(is.null(out) || nrow(out) == 0){
    warning("No data in ROI ", region_id, " from FILE ", path, " Skipping")
    return(NULL)
  }

  ## ----
  ## Create the id column and put it in the first position
  # Get a vector with the original column names
  old_cols <- data.table::copy(names(out))
  # Create a new column called id
  # and set its value to the content
  # of the variable with the same name
  # This variable is passed to parse_single_roi via
  # the data argument, which is a row in the linked metadata
  out[,id := id] # column = value of id (will be equal for all rows)

  # Make id the first column
  data.table::setcolorder(out, c("id", old_cols))

  ## ----
  ## Set the key of the data.table to the newly created id column
  ## and use it to make a behavr table by attaching a temporary metadata
  ## that just contains id
  data.table::setkeyv(out, "id")
  met <- data.table::data.table(id = id, key = "id")
  behavr::setbehavr(out, met)


  ## ----
  ## Preanalyze or annotate the loaded data

  if (verbose) {
  info_message <- paste0('Running annotation function for ', path, ' ', region_id)
  message(info_message)
  }

  annot <- annotate_single_roi(out, FUN, ...)

  if (is.function(updateProgress_load)) {
      info_message <- paste0('Ran annotation function for ', path, ' ', region_id)
      updateProgress_load(detail = info_message)
  }

  return(annot)
}
