# for memoisation
# we obtain data from one ROI and optionaly preanalyse it, by applying FUN.
# this function is run on single individual data
#' @param data one row data.table corresponding to a row of the original (and then linked) metadata
#' @importFrom rlang list2
parse_single_roi <- function(data,
                             min_time = 0,
                             max_time = +Inf,
                             reference_hour = NULL,
                             verbose = TRUE,
                             columns = NULL,
                             cache=NULL,
                             FUN = NULL,
                             updateProgress = NULL,
                             ...){

  roi_idx = NULL
  id <- data$id
  region_id <- data$region_id
  path <- data$file_info[[1]]$path

  # save the name of the columns the FUN annotation
  # method needs by running its needed_columns
  # attribute if it is defined
  if(is.null(columns) & !is.null(FUN)){
    needed_columns <- attr(FUN, "needed_columns")
    if(!is.null(needed_columns))
      columns <- needed_columns(...)
  }

  columns <- c(columns, rlang::list2(...)$extra_columns)

  # if verbose, log some information to the user so he knows
  # a new fly is being loaded (progress tracking)
  if(verbose) {
    info_message <- sprintf("Loading ROI number %i from:\n\t%s\n", region_id, path)
    message(info_message)
    # additionally, if a progress bar is available, update it as well
    # TODO The message is getting update but the value is not
    #if (requireNamespace("shiny", quietly = TRUE) & !is.null(progress)) {
    if (is.function(updateProgress)) {
      updateProgress(detail = info_message)
      #progress$set(value = data$fly_count / total_count, detail = info_message_complete)
    }
  }

  # Check the path leads to a sqlite3 file
  if(tools::file_ext(path) != "db")
    stop(sprintf("Unsuported file extention in %s", path))


  # Compute the filesize of the sqlite3 file
  # TODO Verify this is done to check whether the cache is invalid
  fs = file.info(path)["size"]

  # If the cache defined, wrap the analysis in the memoise function
  # to cache the results for faster reloading
  if(!is.null(cache)){
    db <- memoise::cache_filesystem(cache, algo="md5")
    parse_single_roi_wrapped_memo <- memoise::memoise(parse_single_roi_wrapped, cache = db)
  } else{
    # otherwise the "cached" version is just the original function
    parse_single_roi_wrapped_memo <- parse_single_roi_wrapped
  }


  # Call the analysis function, cached or not

  out <- parse_single_roi_wrapped_memo(
    # a unique identifier for a fly
    id,
    # an integer indicating the position of the fly in the monitor
    region_id,
    # absolute path to the sqlite3 file (dbfile)
    path,
    # min and max time to be loaded
    # can be more than 24 i.e. span more than one ZT cycle
    min_time,
    max_time,
    # time when ZT0 happens in the same timezone as the ethoscope timezone
    reference_hour,
    # what columns to fetch
    # TODO Why not getting columns inside this function
    columns,

    file_size = fs,
    FUN,
    ...
  )

  # TODO Verify this
  # The return value is a data table i.e not a behavr table with a metadata
  if ("id" %in% colnames(out)) data.table::setkey(out, id)
  if(!is.null(out))
    fslbehavr::setbehavr(out, data)

  return(out)

}


#'
parse_single_roi_wrapped <- function(id, region_id, path,
                                     min_time = 0,
                                     max_time = +Inf,
                                     reference_hour = NULL,
                                     columns = NULL,
                                     file_size = 0,
                                     FUN = NULL,
                                     ...
){



  ## Read data for a single ROI (fly) from the SQLite file
  ## ----
  time_stamp = NULL
  message(sprintf("I am calling read single roi with region id %d", region_id))

  out <- read_single_roi(path,
                         region_id = region_id,
                         min_time = min_time,
                         max_time = max_time,
                         reference_hour = reference_hour,
                         columns = columns,
                         time_stamp = time_stamp
  )


  if(is.null(out) || nrow(out) == 0){
    warning(sprintf("No data in ROI %i, from FILE %s. Skipping", region_id, path))
    return(NULL)
  }

  ## ----
  ## Generate the id column of the result of read_single_roi
  ## and make it a behavr table by attaching a temporary metadata
  ## that just contains id
  ## ----
  # Get a vector with the original column names
  old_cols <- data.table::copy(names(out))
  # Create a new column called id
  # and set its value to the content
  # of the variable with the same name
  # This variable is passed to parse_single_roi via
  # the data argument, which is a row in the linked metadata
  out[,id := id]

  # Make id the first column
  data.table::setcolorder(out, c("id", old_cols))
  # Make id as key of the data table
  data.table::setkeyv(out, "id")

  # Create a behavr table with a metadata table
  # that contains just the id and where id is the key
  # TODO This is so

  met <- data.table::data.table(id = id, key = "id")
  fslbehavr::setbehavr(out, met)
  # ----
  annot <- annotate(out, FUN, ...)
  return(annot)
}
