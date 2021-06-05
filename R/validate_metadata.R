#' Verify the pased metadata is compliant
#'
#' @param metadata A data.table describing an experiment
#' It must contain the columns region_id, machine_name and date_time
#' It can contain the column reference_hour to provide a difference ZT for groups of flies
#' It can contain any other column that is not used in the analysis
#' It must NOT contain columns with NA values, as that will make scopr ignore the row
#' date_time must follow the format %YYYY-%MM-%DD_%HH-%MM-%SS
#' @export
validate_metadata <- function(metadata) {

  # make sure the required colums are available
  required_columns <- c("region_id", "machine_name", "date", "reference_hour")
  invalid <- !(required_columns %in% colnames(metadata))

  if (any(invalid)) {
    stop(abort_bad_argument(
      arg = "metadata",
      must = sprintf("contain columns %s", paste(required_columns, collapse = " "))
    ))
  }

  # make sure the reference hour is numeric
  if (!is.numeric(metadata$reference_hour)) {
    stop(abort_bad_argument("reference_hour", "be of class numeric", sprintf("not %s", class(metadata$reference_hour))))
  }


  # validate the date column
  date <- metadata$date
  date_valid <- purrr::map_chr(date, function(x) {tryCatch({
    parse_date(x)
  }, warning = function(e) {NA})})


  invalid <- (is.na(date_valid))

  if (sum(invalid) != 0)  {
    stop(
      abort_bad_argument(
        "date",
        sprintf(
          "follow format YYYY-MM-DD. Rows # %s dont follow it",
          paste(which(invalid), collapse = ", # ")
        )
      )
    )
  }

   metadata$date <- as.character(metadata$date)

   # if time is available, validate it too
  if ("time" %in% colnames(metadata)) {

    time <- metadata$time

    time_valid <- purrr::map_chr(time, function(x) {tryCatch({
      parse_time(x)
    }, error = function(e) {NA})})

    invalid <- (is.na(time_valid))

    if (sum(invalid) != 0)  {
      stop(
        abort_bad_argument(
          "time",
          sprintf(
            "follow format HH:MM:SS. Rows # %s dont follow it",
            paste(which(invalid), collapse = ", # ")
          )
        )
      )
    }
  }

  # check duplicates
  if (any(duplicated(metadata))) {
    warning(sprintf(
      "Rows # %s of metadata are duplicated",
      paste(which(duplicated(metadata)))
    ))
  }

  if (any(is.na(metadata))) {
    column_ids <- unique(which(is.na(metadata), arr.ind = TRUE)[, 2])
    row_ids <- unique(which(is.na(metadata), arr.ind = TRUE)[, 1])
    na_columns <- colnames(metadata)[column_ids]

    stop(
      abort_bad_argument(
        paste(na_columns, collapse = ", "),
        sprintf(
          "columns in rows %s in your metadata contain NA (not assigned) values.
          This is not allowed by rethomics, please either remove this column or give a dummy value like NOTASSIGNED",
          paste(row_ids, collapse = ", # ")
        )
      )
    )
  }


  invisible(TRUE)
}

