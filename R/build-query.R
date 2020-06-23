#' @noRd
#' @import data.table
build_query <- function(result_dir,
                        query = NULL,
                        # use_cached = FALSE,
                        index_file = NULL
                        ) {
  data = .SD = .N = time = n = machine_name = datetime = path = NULL

  # Create a data table displaying all the sqlite3 files available in the database
  # at result_dir
  # columns: machine_id, machine_name, datetime, date, time, path
  files_info <- list_result_files(result_dir, index_file)
  mykey <- data.table::key(files_info)

  # Subset this table by keeping only the last (.N)
  # entry of each group with same key
  # The key is tipically a combination of date, time and machine_name
  # TODO Not sure why this is necessary
  unique_fi <- files_info[, .SD[.N], by = mykey]

  key <- c("date", "time","machine_name")

  if(!is.null(query)){
    q <- data.table::copy(data.table::as.data.table(query))

    # Make columns date and machine_name required
    check_columns(c("date","machine_name"), q)

    message("parsing date and time")
    if(!"time" %in% colnames(q))
      q[, time := NA_character_]
    # Transform date from a character column
    # to lubridate::Date columm
    # TODO Why is it needed?
    q[, date := parse_date(date, tz = "UTC")]
    q[, time := parse_time(time, tz = "UTC")]
    data.table::setkeyv(q, key)
  }

  # Retrieve the files for which time was not specified (NA)
  message("processing rows without time")

  q_no_time <- q[is.na(time)]
  q_no_time[, time := NULL]
  unique_fi_last_of_day <- data.table::copy(unique_fi)
  # n becomes the number of duplicated dates for each query (.N)
  unique_fi_last_of_day[, n := .N, by=c("date", "machine_name")]
  # keep the last entry for each combination of date and machine_name
  unique_fi_last_of_day <- unique(
    unique_fi_last_of_day,
    by = c("date", "machine_name"),
    fromLast = TRUE
  )


  message("... removing duplicates i.e. no 2 db files will be from same ethoscope and day.
  If more than 1 is found, we keep the last. A warning will be emitted in that case")

  # Display duplicated experiments on same date
  # Merge the report of the local database (unique_fi_last_of_day)
  # with the user query (q_no_time)
  duplicated_queries <- unique_fi_last_of_day[q_no_time, on=c("date", "machine_name")][n>1]
  duplicated_queries <- unique(duplicated_queries, by=c("date", "machine_name"))
  if(nrow(duplicated_queries) > 0){
    for( i in 1:nrow(duplicated_queries)){
      str <- "Several files (%i) in machine %s and date %s.
      Keeping last file (%s). Use a `time` column if this is not intended.}"
      str <- sprintf(str,duplicated_queries[i, n],
                     duplicated_queries[i, machine_name],
                     duplicated_queries[i, as.character(date)],
                     duplicated_queries[i, as.character(datetime)])
      warning(str)
    }
  }

  # we don't need "n" anymore
  unique_fi_last_of_day[,n:=NULL]

  # Link user query with local database using date and machine_name
  out_no_time <-  unique_fi_last_of_day[q_no_time, on=c("date", "machine_name")]


  message("processing rows with time")
  # now, we process the query row with time
  q_time <- q[!is.na(time)]
  out_time <- unique_fi[q_time]
  out <- rbind(out_time, out_no_time)
  out <- out[,.SD, by=c("date", "machine_name")]

  nas <- out[,is.na(path)]
    if(any(nas)){
      out_nas <- out[nas,]

      for(i in 1:nrow(out_nas)){
        warning(sprintf("No result for machine_name == %s, date == %s and time == %s. Omiting query",
                        out_nas[i,machine_name],
                        out_nas[i,date],
                        out_nas[i,time]
                        ))
      }
    }
    out[, date := NULL]
    out[, time := NULL]
    #out[, i.time := NULL]
    # Remove rows containing NAs
    # FIXME: This will remove lines where a metadata column is empty
    # even though it maybe absolutely non required to link the query and the database
    # e.g. an empty comment column.
    nrow_before <- nrow(out)
    out <- na.omit(out)
    nrow_after <- nrow(out)
    if (nrow_before != nrow_after) {

      warning(sprintf(
        "%s rows have been na.omitted by build_query(). Is it intended?",
        nrow_before - nrow_after
        )
      )

      }
    data.table::setkeyv(out, colnames(out))
    out
}

