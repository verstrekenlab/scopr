#' Try to connect to a sqlite 3 database
#' If an error is encountered, report it in a friendly-way
#' @import RSQLite
#' @noRd
dbConnectFriendly <- function(file, flags=RSQLite::SQLITE_RO, ...) {

  con <- tryCatch({
    con <- RSQLite::dbConnect(RSQLite::SQLite(), file, flags=flags, ...)
  }, error = function(e) {
    warning(e)
    stop("Sorry. R attempted to open the SQLite file but it cannot. Possible causes are: the path does not lead to a sqlite file or the file is corrupted. Check error trace to learn more.")
  })
  return(con)
}




#' Parse the json data extracted from the METADATA - selected_options of a dbfile
#' into an R list
#' @param metadata list produced by reading into R the result of the SELECT * FROM METADATA statement
#' and putting each row into an element of the list, where the field is the list element's name and the value is the list element's value
get_selected_options <- function(metadata) {

  selected_options <- metadata$selected_options %>%
    gsub(x = ., pattern = "'", replacement = '"') %>%
    gsub(x = ., pattern = "<class ", replacement = "") %>%
    gsub(x=., pattern = ">", replacement = "") %>%
    gsub(x=., pattern = "\\(\\)", replacement = '""') %>%
    jsonlite::parse_json(json = .)
  return(selected_options)
}

#' @import RSQLite
#' @export
sqlite <- function(file, statement, flags=RSQLite::SQLITE_RO) {

  message("Opening connection")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file, flags = flags)

  error <- FALSE
  dt <- tryCatch({
    message("Executing query")
    RSQLite::dbGetQuery(conn = con, statement = statement)
  }, error = function(e) {
    warning("Could not execute SQL query successfully")
    message(e)
    error <<- TRUE
  })

  message("Closing connection")
  RSQLite::dbDisconnect(con)
  return(dt)
}



#' Load the METADATA table in an ethoscope sqlite3 file
#' @param path Path to the dbfile
#' @export
get_metadata <- function(path) {

  metadata <- as.data.table(sqlite(path, "SELECT * FROM METADATA;"))
  value <- as.list(metadata$value)
  names(value) <- metadata$field
  metadata <- value
  metadata$selected_options <- get_selected_options(metadata)
  metadata$date_time <- as.numeric(metadata$date_time)
  metadata$frame_width <- as.numeric(metadata$frame_width)
  metadata$frame_height <- as.numeric(metadata$frame_height)

  return(metadata)
}


#' Query a database for interactor date range start / end
#' @param meta_row A row of metadata
#' @return Integer vector of length 2 stating the milliseconds since experiment start
#' until onset and end of sleep deprivation treatment in the animal captured by the metadata
#' @export
load_sd_daterange <- function(meta_row, from_zt0=TRUE) {

  # TODO Because fortify does not work well, file_info is still a list
  # (even if each element just has length 1)

  metadata <- get_metadata(sapply(meta_row$file_info, function(x) x$path))
  date_range <- metadata$selected_options$interactor$kwargs$date_range
  if (is.null(date_range)) {
    timestamps <- c(-1, -1)

  } else {
    timestamps <- strsplit(date_range, split = "  ") %>% lapply(., function(x) {
      x %>% as.POSIXct(tz = "GMT") %>% as.numeric
    }) %>% unlist
  }

  date_time <- metadata$date_time
  experiment_info <- list(date_time = as.POSIXct(metadata$date_time, origin = "1970-01-01", tz = "GMT"))

  ms_after_ref <- scopr::get_ms_after_ref(experiment_info, meta_row$reference_hour)

  timestamps <- timestamps - date_time
  if (from_zt0) {
    timestamps <- timestamps + (ms_after_ref / 1e3)
  }
  return(timestamps)
}


#' Load ethoscope data with a set of annotation parameters for different intervals
#'
#' @param ... Arguments to load_ethoscope
#' @param intervals Named list where every element is the start and end time in seconds since experiment start of an interval
#' The name of the interval should be interval_X, where X is the interval identifier
#' @seealso load_ethoscope
#' @export
load_ethoscope_multiplex <- function(..., intervals=NULL) {

  dt <- load_ethoscope(...)
  dt$interval <- "default"

  args <- list(...)
  if ("metadata" %in% args) {
    metadata <- args$metadata
  } else {
    metadata <- args[[1]]
  }


  if (!is.null(intervals)) {
    dt_patches <- lapply(1:length(intervals), function(i) {
      interv <- intervals[[i]]
      interv_name <- names(intervals)[i]
      args <- list(...)
      annotation_arg_names <- args$FUN %>% sapply(., function(fun) attr(fun, "parameters")()) %>% unique
      default_interval_args <- args[annotation_arg_names[annotation_arg_names %in% names(args)]]
      names(args)

      annotation_arg_index <- unlist(lapply(annotation_arg_names, function(arg_name) {
        grep(pattern = arg_name, x = names(args))
      })
      )
      non_annotation_args <- args[-annotation_arg_index]
      annotation_args <- args[annotation_arg_index]
      annotation_args <- annotation_args[!names(annotation_args) %in% annotation_arg_names]
      annotation_args <- annotation_args[grep(pattern = interv_name, x = names(annotation_args))]
      names(annotation_args) <- sapply(names(annotation_args), function(arg_name) {
        gsub(pattern = paste0("_", interv_name), replacement = "", x = arg_name)
      })


      # if only some of the parameters are passed for a specific interval
      # use the defaults for the remaining parameters
      # the default is defined by
      final_annotation_args <- default_interval_args
      for (arg_name in names(annotation_args)) {
        final_annotation_args[[arg_name]] <- annotation_args[[arg_name]]
      }

      args <- append(
        append(non_annotation_args, final_annotation_args),
        list(
          min_time = interv[1], max_time = interv[2]
        )
      )

      dt <- do.call(load_ethoscope, args)
      dt$interval <- interv_name
      dt
    })

    # remove data with alternative annotation
    for (interv in intervals) {
      dt <- dt[t < interv[1] | t > interv[2], ]
    }

    dt <- Reduce(behavr::rbind_behavr, append(list(dt), dt_patches))
    metadata <- behavr::meta(dt)
    metadata <- metadata[!duplicated(behavr::simplify_behavr(metadata, meta=F)), ]
    dt <- dt[order(rank(id), t)]
    setkey(dt, id)
    behavr::setmeta(dt, metadata)
  }
  return(dt)
}
