keep_valid_db_files <- function(db_files, result_dir) {
    fields <- strsplit(db_files,"/")
    # keep files stored following the expected structure
    # all files should be under a 3 folder structure:
    # machine_id/machine_name/date_time/file.db
    # which gives 4 fieds
    valid_files <- sapply(fields,length) == 4
    db_files <- db_files[valid_files]

    invalids = fields[!valid_files]
    if(length(invalids) > 0){
      warning("There are some invalid files:")

      for(i in 1:length(invalids)){
        warning(paste(invalids[[i]]),sep='/')
      }
    }

    if(length(db_files) == 0){
      stop(sprintf("No .db files detected in the directory '%s'. Ensure it is not empty.", result_dir))
    }
    return(valid_files)
}



#' @noRd
#' @importFrom stringr str_split
#' @import data.table
parse_datetime <- function(x){
    match <- stringr::str_split(x, "_", simplify=TRUE)
    d <- parse_date(match[,1])
    t <- parse_time(match[,2],format="%H-%M-%S")
    data.table::data.table(date=d, time = t)
}


#' List all available result files
#'
#' This function discovers all ethoscope result files and put them in a [data.table::data.table].
#' This is useful to figure out when and which experiments were performed.
#'
#' @param result_dir the root directory where all data are saved, or the path to a remote directory.
#' @param index_file the name of an index_file, in `result_dir` (needed for loading remote data).
#' @return a [data.table::data.table].
#' Each row is a single experimental file, and columns describe details such as its `path`, start `date` and `time`,
#' and the name and id of the ethoscope used.
#' @seealso
#' * [load_ethoscope] -- to load the actual data
#' * [experiment_info] -- to show the metadata of a specific experiment
#' @export
list_result_files <- function(result_dir, index_file=NULL){
  path <- datetime <- NULL


  # the key identifies a unique ethoscope start in a given database
  key <- c("date", "time","machine_name")

  if(!is.null(index_file)){
    index_file <- paste(result_dir, index_file, sep="/")
    args <- list(header=FALSE, verbose=FALSE, showProgress=FALSE)
    if (substr(index_file, 1, 4) == "http") {
      args <- append(list(input = index_file), args)
    } else {
      args <- append(list(file = index_file), args)
    }
    tryCatch({dt_all_files  <- do.call(data.table::fread, args)},
             error = function(e) stop(sprintf("Could not find index file: %s",
                                         index_file)))

    all_db_files <- grep(".*\\.db", dt_all_files$V1, value = TRUE)
  }

  else{
    # this is the call that yields all the dbfiles available in the database
    all_db_files <- list.files(result_dir, recursive=T, pattern="*\\.db$")
  }


  # keep only files that follow the right structure
  valid_files <- keep_valid_db_files(all_db_files, result_dir)
  fields <- strsplit(all_db_files[valid_files], split="/")

  # build a meta dataset (files_info)
  files_info <- data.table::as.data.table(do.call("rbind",fields))
  data.table::setnames(files_info, c("machine_id", "machine_name", "datetime","file"))
  files_info[, file := NULL]
  files_info <- cbind(files_info, parse_datetime(files_info$datetime))
  files_info[, datetime := as.POSIXct(datetime, "%Y-%m-%d_%H-%M-%S", tz="UTC")]
  files_info[, path := paste(result_dir, all_db_files, sep="/")]
  data.table::setkeyv(files_info, key)
  return(files_info)
}
