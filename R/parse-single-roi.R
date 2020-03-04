# for memoisation
# we obtain data from one ROI and optionaly preanalyse it, by applying FUN.
parse_single_roi <- function(data,
                        min_time = 0,
                        max_time = +Inf,
                        reference_hour = NULL,
                        verbose = TRUE,
                        columns = NULL,
                        cache=NULL,
                        FUN = NULL,
                        FUN_filter = NULL,
                        progress = NULL,
                        total_count,
                        feather_interface = FALSE,
                        rds_interface = FALSE,
                        ...){
  roi_idx = NULL
  id <- data$id
  region_id <- data$region_id
  path <- data$file_info[[1]]$path

  # we get the columns to get from the method used itself
  if(is.null(columns) & !is.null(FUN)){
    needed_columns <- attr(FUN, "needed_columns")
    if(!is.null(needed_columns))
      columns <- needed_columns(...)
  }
  if(verbose) {
    info_message <- sprintf("Loading ROI number %i",region_id)

    info_message_complete <- sprintf("%s from:\n\t%s\n", info_message, path)
    cat(info_message_complete)
    if (requireNamespace("shiny", quietly = TRUE) & !is.null(progress)) {
      progress$set(value=data$fly_count/total_count, detail=info_message_complete)
    }
  }

  if(tools::file_ext(path) != "db")
    stop(sprintf("Unsuported file extention in %s",path))

  fs = file.info(path)["size"]

  if(!is.null(cache)){
    db <- memoise::cache_filesystem(cache, algo="md5")
    parse_single_roi_wrapped_memo <- memoise::memoise(parse_single_roi_wrapped, cache=db)
  }
  else{
    parse_single_roi_wrapped_memo <- parse_single_roi_wrapped
  }

  out <- parse_single_roi_wrapped_memo( id,
                                 region_id,
                                 path,
                                 min_time,
                                 max_time,
                                 reference_hour,
                                 columns,
                                 file_size= fs,
                                 FUN,
                                 FUN_filter,
                                 feather_interface = feather_interface,
                                 rds_interface = rds_interface,
                                 ...
                                 )
  if(!is.null(out))
    fslbehavr::setbehavr(out, data)



  out

}



parse_single_roi_wrapped <- function(id, region_id,path,
                                     min_time = 0,
                                     max_time = +Inf,
                                     reference_hour = NULL,
                                     columns = NULL,
                                     file_size=0,
                                     FUN = NULL,
                                     FUN_filter = NULL,
                                     feather_interface = FALSE,
                                     rds_interface = FALSE,
                                     ...
                                     ){
  time_stamp = NULL
  out <- read_single_roi(path,
                         region_id=region_id,
                         min_time = min_time,
                         max_time = max_time,
                         reference_hour = reference_hour,
                         columns=columns,
                         time_stamp = time_stamp,
                         feather_interface = feather_interface,
                         rds_interface = rds_interface
                         )

  if(is.null(out) || nrow(out) == 0){
    warning(sprintf("No data in ROI %i, from FILE %s. Skipping",region_id, path))
    return(NULL)
  }


  #id <- as.factor(sprintf("%02d|%s",region_id,experiment_id))

  old_cols <- data.table::copy(names(out))
  out[,id := id]
  data.table::setcolorder(out,c("id", old_cols))
  data.table::setkeyv(out, "id")
  met <- data.table::data.table(id = id, key="id")
  fslbehavr::setbehavr(out, met)

  if(!is.null(FUN_filter))
    out <- FUN_filter(out, ...)

  if(!is.null(FUN)){

    list_of_outs <- list()
    i <- 1
    for (FU in FUN) {

      # fun_name <- as.character(substitute(FU))
      logging::logdebug(glue::glue('Running annotation function #{i}'))
      # browser()
      out_annotated <- FU(out,...)

      is_empty <- is.null(out_annotated)
      if(!is_empty)
        is_empty <- nrow(out_annotated) == 0
      if(is_empty){
        warning(sprintf("No data in ROI %i after running FUN, from FILE %s. Skipping",region_id, path))
        # return(NULL)
        list_of_outs[[i]] <- NULL
      } else {
        setkey(out_annotated, 'id', 't')
        list_of_outs[[i]] <- out_annotated
      }
      logging::logdebug(glue::glue('Done with annotation function #{i}'))
      i <- i + 1
    }
  }

  metadata <- fslbehavr::meta(list_of_outs[[1]])
  # browser()
    if(length(list_of_outs) == 1) {
	  out <- list_of_outs[[1]]
  } else {
    out <- Reduce(function(x, y) merge(x = x, y = y),list(as.data.table(list_of_outs[[1]]), as.data.table(list_of_outs[[2]])))

    if(length(list_of_outs) > 2) {
      for(i in 3:length(list_of_outs)) {
        out <- Reduce(function(x, y) merge(x = x, y = y),list(as.data.table(out), as.data.table(list_of_outs[[i]])))
      }
    }
  }
  
  setkey(out, 'id')
  fslbehavr::setmeta(out, metadata)
  # out <- out[!duplicated(out),]
  return(out)


}
