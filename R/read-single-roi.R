#' Can I read the sqlite3 database, or is it locked
#' @param FILE Path to sqlite3 database file
#' @importFrom RSQLite dbConnect dbDisconnect
#' @return Boolean stating whether the METADATA table can be read or not
database_is_available <- function(FILE) {
  tryCatch({
    # create the RSQLite connection
    con <- RSQLite::dbConnect(RSQLite::SQLite(), FILE, flags = RSQLite::SQLITE_RO)
    # try reading a table that should ALWAYS be in the dbfiles
    tryCatch({
      metadata <- data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM METADATA;"))
      ok <- all(colnames(metadata) == c("field", "value"))
      return(ok)
    }, finally = function() {
      RSQLite::dbDisconnect(con)
    })

  }, error = function(e) {
    FALSE
  })
}

#' Read data from a single ROI in a SQLite file
#'
#' This function performs the following actions
#' \begin{itemize}
#' \item{Verify the desired columns are available, by checking the VAR_MAP table, which describes the ROI_X tables.
#'   If the VAR_MAP is corrupted, a warning is emitted and a default VAR_MAP is loaded from scopr internal dataset
#' }
#' \item{Verify the desired ROI is available, by checking the ROI_MAP table}
#' \item{Load the behavioral data for region_id from table ROI_region_id (e.g. ROI_1, ROI_20, ...)}
#' \item{Adjust the time loaded from the sqlite file if reference_hour is passed. The loaded t is 0 at experiment start. The adjusted time is 0 at reference_hour}
#' \item{If the start happened > 12 hours since ZT0 (i.e. in the D phase for a standard 12h LD experiment) ..}
#' \item{If the start happened < 12 hours since ZT0 (i.e. in the L phase for a standard 12h LD experiment) the difference in between is added so t=0 becomes ZT0, and not the experiment start}
#' \item{Moreover, the unit is switched from ms to s}
#' \item{The data is filtered, so only data not inferred from previous positions is kept. Alternatively, if the has_interacted column is available and is 1, the data is also kept, even if inferred}
#' \end{itemize}
#' @param FILE character, absolute path to a sqlite3 file (ending in .db typically)
#' @param region_id numeric, position of the animal's roi in the ethoscope's layout. Should be 1-20 for the standard 20-animal arena
#' @param min_time numeric, time in seconds from the start of the experiment after which the data is loaded into R
#' @param max_time numeric, time in seconds from the start of the experiment after which the data is not loaded into R
#' @param reference_hour integer, hour of the day in GMT TZ when the incubator turned the light on. If the reference hour did not happen yet on the loading day,
#' a whole extra day is prepended to the data, because the time series always starts with an L phase (even if this L phase is fully NA)
#' @param columns Columns to be loaded from the dbfile. Checked against the VAR_MAP for availability
#' @param time_stamp Used to memoise the function
#' @return 
read_single_roi <- function(FILE,
                             region_id,
                             min_time = 0,
                             max_time = +Inf,
                             reference_hour = NULL,
                             columns = NULL,
                             time_stamp = NULL # only used for memoisation
){

  roi_idx = var_name = rois_idx = id = w = h = functional_type = sql_type = is_inferred = has_interacted = NULL

  ## 1 Read experiment metadata
  ## ----
  # read the metadata table from the result file
  # into a named list

  experiment_info <- experiment_info(FILE)

  if (min_time >= max_time)
    stop("min_time can only be lower than max_time!")

  ## ----

  ## 2 Connect to the sqlite3 file
  ## ----
  con <- RSQLite::dbConnect(RSQLite::SQLite(), FILE, flags = RSQLite::SQLITE_RO)

  tryCatch({

    ## 2.1 Read VAR_MAP
    ## ----
    #
    # Load the var map into a data table
    # We get a data.table with columns var_name sql_type functional_type
    # * var_name: the name of the variable as it will appear
    # in the data.table returned to the user
    # * sql_type: the SQL data type (INT, SMALLINT, BOOLEAN, ...)
    # * funcional_type: a one word description of the type of variable
    # that gives information about its constraints
    # i.e. distance (cannot be negative), angle (cannot be more than 2pi),
    # bool, interaction, relative_distance_1e6, ...
    var_map <- data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM VAR_MAP"))
    if(nrow(var_map) < 8) {
      warning("
            Expected number of rows in VAR_MAP is less than expected (8).
            Probably the table is not complete because its creation was interrupted.
            I will use a default VAR_MAP, which is always identical to the original VAR_MAP.
            You can dismiss this message if your data is loaded correctly!
      ")
      var_map <- as.data.table(fslscopr:::var_map)
    }

    data.table::setkey(var_map, var_name)
    # NOTE the var_map is loaded so
    # 1. the program can cross
    #    the columns requested by the user
    #    with the columns available in the data
    #    and act accordingly
    # 2. to learn the functional type of the columns
    #    and this way know how to present the data to the user
    #    i.e. normalize distances to the roi_width,
    #    make boolean characters actual R booleans, etc
    ## ----

    ## 2.2 Build the part of the SQL query that will
    ## fetch the requested columns
    ## ----
    # If no columns are explicitly requested, fetch everything
    if(is.null(columns)){
      selected_cols <- "*"
      # If they are, add is_inferred and t (time)
      # and make the data SQL compatible
    } else {
      # Always add is_inferred to the list of returned columns
      # regarded of whether it is requested
      if("is_inferred" %in% var_map$var_name)
        columns <- unique(c(columns, "is_inferred"))

      # Warn the user that some of the requested columns are not available
      if(any(!columns %in% var_map$var_name)) {
        message(sprintf('Requested columns %s', paste0(columns, collapse = ', ')))
        stop(sprintf("Some of the requested columns are NOT available. Available columns are: %s",
                     paste(var_map$var_name, collapse = " ")))
      }

      # TODO Why not adding t above with is_inferred?
      # Generate a string using the requested columns
      # + is_inferred + t by collapsing them with ,+space
      # that can be used to build a SQL query
      selected_cols = paste(unique(c("t", columns)), collapse=", ")

      # TODO Document why we need to subset the var_map
      var_map <- var_map[columns]
      data.table::setkey(var_map, var_name)
    }
    ## ----

    ## 2.6 Read ROI_MAP
    ## ----
    # Load the current ROI properties
    # roi_idx roi_value   x   y   w  h
    # x, and y are referenced to the top left corner i.e. Python OpenCV way
    roi_map <- data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM ROI_MAP"))
    roi_row <- roi_map[roi_idx == region_id]

    if(nrow(roi_row) == 0 ){
      warning(sprintf("ROI %i does not exist, skipping", region_id))
      return(NULL)
    }

    ## ----


    ## 2.3 Build the part of the SQL query that will
    ## filter the data using min_time and max_time
    ## ----
    # Prepare the time filter given
    # the user provided min_time and max_time
    if(max_time == Inf)
      # No filter
      max_time_condition <- ""
    else
      # t needs to be less than max_time in ms
      max_time_condition <-  sprintf("AND t < %e", max_time * 1000)

    # we always filter for the min_time and if it's 0
    # that's equivalent to no filtering
    min_time <- min_time * 1000
    ## ----

    ## 2.4 Load the behavioral data into R
    ## ----
    # TODO filter here is inferred
    # SELECT
    # the selected columns
    # from the ROI table given by the region_id
    # with the user provided time filters
    sql_query <- sprintf("SELECT %s FROM ROI_%i WHERE t >= %e %s",
                         selected_cols, region_id,
                         min_time, max_time_condition )


    result <- RSQLite::dbGetQuery(con, sql_query)

    ## ----
    # TODO here, use setDT!!
    # FIXME however, bottleneck is sqlite 10times slower than reading equivalent csv!!!!
    roi_dt <- data.table::setDT(result)

    # remove the id column if it is available
    if("id" %in% colnames(roi_dt))
      roi_dt[, id := NULL]
    ## ----

    # Use these properties and the knowledge in
    # var_map to normalize distances
    roi_width <- max(c(roi_row[, w], roi_row[, h]))
    for(var_n in var_map$var_name){
      if(var_map[var_n, functional_type] == "distance"){
        roi_dt[, (var_n) := get(var_n) / roi_width]
      }
      if(var_map[var_n, sql_type] == "BOOLEAN"){
        roi_dt[, (var_n) := as.logical(get(var_n))]
      }
    }


    ## ----
    ## 2.5 Adjust the t column by
    ## * Aligning it to ZT0 if provided via the reference_hour argument
    ## * Converting it to seconds

    # 1. change the reference t0 from the start of the experiment
    # to the user provided reference_hour if available
    # the start of the experiment is recorded in experiment_info$date_time
    # see experiment_info() for more information
    # 2. convert from ms to s
    if(!is.null(reference_hour)){
      # get the start time recorded in the dbfile
      # in a format() compatible format
      p <- experiment_info$date_time
      # get a timestamp in hours since the beginning of the day
      # i.e %Y-%m-%d 10:30:12 becomes 10 + 0.5 + 1/300 hours
      hour_start <- as.numeric(format(p, "%H")) +
                    as.numeric(format(p, "%M")) / 60 +
                    as.numeric(format(p, "%S")) / 3600

      # compute how many hours ahead of ZT0 was the experiment_start
      h_after_ref <- (hour_start - reference_hour) %% 24
      # convert to ms
      ms_after_ref <- h_after_ref * 3600 * 1000
      # add that amount to the t column so it becomes aligned with ZT
      # t will reflect the time since ZT0 and NOT since the experiment start
      # convert to seconds
      message(sprintf("Adding %d ms to t column of fly %s", ms_after_ref, id))

      roi_dt[, t := (t + ms_after_ref) / 1e3 ]
    }
    else{
      # if no reference_hour available, assume they are already aligned
      # i.e. do nothing
      # convert to seconds
      roi_dt[, t := t / 1e3]
    }
    ## ----


    ## 2.7 Default filtering of data
    ## ----
    # Keep rows where is_inferred is False or has_interacted is True
    # NOTE is_inferred is True when a fly is not detected in a frame
    # In that case, the last position where it was found is recorded instead
    # The AdaptiveBGModel has this behavior for 30 seconds.
    # Beyond that interval, no position is recorded i.e.
    # it does not assume the fly is still there anymore
    # In any case, we do not use them in the vanilla scopr workflow
    if(all(c("is_inferred", "has_interacted") %in% colnames(roi_dt))){
      # we keep infered cols if interaction happened!
      roi_dt <- roi_dt[is_inferred == F | has_interacted == T]
    }
    else if("is_inferred" %in% colnames(roi_dt))
      roi_dt <- roi_dt[is_inferred == F]
        roi_dt[, is_inferred := NULL]
    ## End of tryCatch: return the data.table
    return(roi_dt)
  },
  # Close the SQLite connection
  finally = { RSQLite::dbDisconnect(con) }
  )
}
