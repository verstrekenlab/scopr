#' This function will be executed for every entry in q_l
#' i.e. once for every file
#' It uses one and only one core
#' @param q Metadata subset with animals from the same experiment
#' @param ... Additional arguments to load_row
load_data_single_core <- function(q, ...){

  # Make the metadata compatible with lapply
  # by making it a list of rows
  l_rows <- lapply(1:nrow(q), function(i) { q[i,] })

  # call parse_single_roi with l_rows combined with the arguments
  # parsed from the metadata file
  l_dt <- lapply(l_rows, function(x) load_row(row=x, ...))

  # restore a behavr table from a list of behavr tables
  # each element is the behavr table of a single fly
  res <- behavr::bind_behavr_list(l_dt)
  return(res)
}


#' Load and preanalyze all entries from one dbfile
#' Several cores can be used (if ncores>1)
#' @param q_l List of metadata subsets where every subset represents animals from the same experiment i.e. contained in the same dbfile
#' @param ncores number of cores to use for optional parallel processing (experimental).
#' @param ... Extra arguments to be passed to load_row
#' @seealso load_row
#' @return behavr table
load_data <- function(q_l, ncores=1, ...) {

  if (ncores == 1) {
    l_dt <- lapply(1:length(q_l), function(i) load_data_single_core(q_l[[i]], ...))
  } else{
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("`parallel` package needed for ncores > 1.
           Please install it.",
           call. = FALSE)
    }
    l_dt <- parallel::mclapply(1:length(q_l), function(i) load_data_single_core(q_l[[i]], ...), mc.cores = ncores)
  }
  return(l_dt)
}

check_arg_list_for_dups <- function(arg_list) {
  dups <- duplicated(names(arg_list))
  if (any(dups)) {
    duplicate_args <- names(arg_list)[dups]
    arg_list <- arg_list[!dups]
    for (d in duplicate_args) {
      warning("Argument ", d, " was passed more than once. I will use value ", arg_list[d])
    }
  }
  return (arg_list)
}



#' Call parse_single_roi with sensible arguments

#' Prepare a list of arguments to pass to parse_single_roi
#' using arguments explicitly declared by the user or taken from the metadata
#' @param row Metadata for one animal only
#' @param min_time load only data > `min_time` (in seconds).
#' This time is *relative to the start of the experiment (not ZT0)*.
#' @param max_time load only data < `max_time` (in seconds).
#' This time is *relative to the start of the experiment (not ZT0)*.
#' @param reference_hour hour, in the day, to use as ZT0 reference.
#' When unspecified, time will be relative to the start of the experiment.
#' @param cache the name of a local directory to cache results for faster subsequent data loading.
#' @param intervals Named list where every element is the start and end time in seconds since experiment start of an interval
#' The name of the interval should be interval_X, where X is the interval identifier
#' @param verbose whether to print progress (a logical)
#' @param columns optional vector of columns to be selected from the db file.
#' Time (t) is always implicitly selected.
#' When `NULL` and if `FUN` is set, columns can be retrieved automatically (from the attributes of `FUN`).
#' @param FUN function (optional) to transform the data from each individual
#' immediately after is has been loaded.
#' @param map_arg  OPTIONAL a list to map `FUN` arguments to metavariables values. See details
#' @details `map_arg` is a list of the form `list(fun_arg = "metavariable")`.
#' When provided, `FUN` will set specific arguments (`fun_arg`) to the value of a (quoted) metavariable.
#' @param ... Extra arguments to be passed to `FUN`
#' @family load_ethoscope
#'
load_row <- function(row,
                     min_time = 0,
                     max_time = Inf,
                     reference_hour = NULL,
                     verbose = TRUE,
                     columns = NULL,
                     cache = NULL,
                     intervals = NULL,
                     FUN = NULL,
                     map_arg = NULL,
                     ...
){

  # initialize a list of arguments to parse_single_roi
  arg_list <- list(row,
                   min_time = min_time,
                   max_time = max_time,
                   verbose = verbose,
                   columns = columns,
                   cache = cache,
                   intervals = intervals,
                   FUN = FUN,
                   ...
  )

  # if reference_hour is NA, the user
  # wants to get the reference_hour from the metadata

  if (!is.null(reference_hour)) {
    if (is.na(reference_hour)) map_arg <- c(map_arg, reference_hour = "reference_hour")
    # if it is not NA, use the reference_hour argument passed to load_ethoscope
  } else if (is.null(reference_hour)) {
    arg_list <- c(arg_list, reference_hour = NULL)
  }  else {
    arg_list <- c(arg_list, reference_hour = reference_hour)
  }

  # create an additional list by parsing the elements in map_arg
  # and mapping them to the right column in row
  #
  # for each key-val pair in map_arg
  # assign to column key the value val

  arg_val <- lapply(map_arg, function(x)row[,eval(parse(text=x))])
  arg_list <- c(arg_list, arg_val)
  arg_list <- check_arg_list_for_dups(arg_list)

  PRESET_INTERVAL <- list(
    SD = function(row) {c(load_sd_daterange(row, from_zt0 = FALSE), load_sd_daterange(row, from_zt0 = TRUE))}
  )

  interval_columns <- grep(pattern = "interval_", x = colnames(row), value = TRUE)



  if (is.null(intervals) & length(interval_columns) != 0) {
    intervals <- lapply(1:length(interval_columns), function(i) {
      interv <- row[[interval_columns[i]]]
      # interv_name <- gsub(pattern = "interval_", replacement = "", x = interval_columns[i])
    })
    names(intervals) <- interval_columns
  } else if (is.null(intervals) & length(interval_columns) == 0) {
    intervals <- list()
  }

  intervals <- append(list(default = c(0, Inf)), intervals)

  intervals <- lapply(intervals, function(interv) {
    # print(interv)
    if (class(interv) == "character" && grepl(pattern = ";", x = interv)) {
      interv <- strsplit(interv, split = ";") %>% unlist
    } else if (class(interv) == "character" && interv %in% names(PRESET_INTERVAL)) {
    interv <- PRESET_INTERVAL[[interv]](row)
    } else {
    interv
    }
  })


  # print(intervals)

  dt_patches <- lapply(1:length(intervals), function(i) {
    interv <- intervals[[i]]
    interv_name <- names(intervals)[i] %>% gsub(pattern = "interval_", replacement = "", x = .)
    args <- arg_list
    annotation_arg_names <- args$FUN %>% sapply(., function(fun) attr(fun, "parameters")()) %>% unique %>% as.vector
    default_interval_args <- args[annotation_arg_names[annotation_arg_names %in% names(args)]]

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

    if (names(non_annotation_args)[1] == "" & "list" %in% class(non_annotation_args[1])) {
      names(non_annotation_args)[1] <- "data"
    }
    non_annotation_args <- non_annotation_args[setdiff(names(non_annotation_args), c("min_time", "max_time"))]

    args <- append(
      append(non_annotation_args, final_annotation_args),
      list(
        min_time = interv[1], max_time = interv[2]
      )
    )

    # call parse_single_roi with this combined list of arguments
    # parse single roi will
    # * load the data into R
    # * preanalyze / annotate it
    out <- do.call(parse_single_roi, args)
    out$interval <- interv_name
    out
  })

  names(dt_patches) <- names(intervals)

  patches <- dt_patches[setdiff(names(dt_patches), "default")]

  patches_dt <- Reduce(behavr::rbind_behavr, patches)
  dt <- dt_patches[["default"]]
  if (length(patches) > 0) {
     for (i in 1:length(patches)) {
       interv <- intervals[[names(patches)[i]]]
       dt <- dt[t < interv[3] | t > interv[4], ]
     }
  }

  out <- behavr::rbind_behavr(dt, patches_dt)
  behavr::setmeta(out, behavr::meta(dt))
  out
}


#' Load data from ethoscope result files
#'
#' This function is used to import behavioral data generated
#' by the [ethoscope platform](http://gilestrolab.github.io/ethoscope/).
#' That is it loads multiple `.db` files into a single `R` [behavr::behavr] table.
#'
#' @param metadata [data.table::data.table] used to load data (see detail)
#' @param ... extra arguments to be passed to load_data
#' @seealso load_data
#' @seealso load_row
#'
#' @return A [behavr] table.
#' In addition to the metadata, it contains the data, with the columns:
#' * `id` -- autogenerated unique identifier, one per animal
#' * `t` -- time (s)
#' * Several variables recorded by ethoscopes (position, angle, width/height and others), or computed by `FUN`.
#' Distance units (e.g. xy position, height/width) are expressed as a fraction of the width of the ROI they originate from.
#' @details
#' the linked metadata should be generated using [link_ethoscope_metadata].
#' @examples
#' dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
#' data(region_id_metadata)
#' metadata <- link_ethoscope_metadata(region_id_metadata, dir)
#' print(metadata)
#'
#' # Default data loading
#' dt <- load_ethoscope(metadata)
#' dt
#'
#' # We use reference hour to set zt0 to 09:00 GMT
#' dt <- load_ethoscope(metadata, reference_hour=9)
#' dt
#'
#' # Only load x and y positions
#' dt <- load_ethoscope(metadata, columns=c("x", "y"), reference_hour=9)
#' dt
#' # apply function whilst loading the data
#' dt <- load_ethoscope(metadata, reference_hour=9, FUN=head)
#' dt
#'
#'@seealso
#' * [behavr::behavr] -- to understand the output format
#' * [experiment_info] -- to show information about a file/experiment
#' * [list_result_files] -- to list available files
#' @references
#' * [behavr tutorial](https://rethomics.github.io/behavr.html) -- how to work with the obtained [behavr] table
#' @family load_ethoscope
#' @inheritParams load_row

#' @export
load_ethoscope <- function(metadata,
                           min_time = 0,
                           max_time = Inf,
                           reference_hour = NULL,
                           ncores = 1,
                           verbose = TRUE,
                           columns = NULL,
                           cache = NULL,
                           FUN = NULL,
                           map_arg = NULL,
                           callback = NULL,
                           ...){

  file_info <- NULL

  metadata$fly_count <- 1:nrow(metadata)


  meta_fun <- list()
  data_fun <- list()

  if (!is.null(FUN)) {
    if (is.function(FUN))
      FUN <- list(FUN)
  }

  for (func in FUN) {
    if (is.function(attr(func, "use_meta")) && isTRUE(attr(func, "use_meta")())) {
      meta_fun <- append(meta_fun, list(func))
    } else {
      data_fun <- append(data_fun, list(func))
    }
  }

  if (length(data_fun) == 0) data_fun <- NULL

  # Split the metadata into a list where every element is a subset
  # where every row has the same path
  experiment_id <- metadata[, sapply(file_info, function(x) x$path)]
  q_l <- split(metadata, experiment_id)
  for (q in q_l) data.table::setkeyv(q, data.table::key(metadata))

  # Call load_entries_from_one_file in parallel or unithreaded
  # depending on the value of ncores

  l_dt <- load_data(q_l,
                    min_time = min_time,
                    max_time = max_time,
                    reference_hour = reference_hour,
                    ncores = ncores,
                    verbose = verbose,
                    columns = columns,
                    cache = cache,
                    FUN = data_fun,
                    map_arg = map_arg,
                    callback = callback,
                    ...)

  dt <- behavr::bind_behavr_list(l_dt)

  # browser()
  if (length(meta_fun) != 0) {
    annotations <- lapply(meta_fun, function(func) {
      args <- get_func_args(func, dt, ...)
      do.call(func, args)
    })
    dt_meta <- Reduce(behavr::merge_behavr_all, annotations)
    dt <- behavr::merge_behavr_all(dt, dt_meta)
  }

  # Get rid of temporary data containers not needed anymore
  # Force R to garbage collect, making memory available
  rm(l_dt)
  gc()
  return(dt)
}
