#' @noRd
is.writable <- function(path) {
  # code <- system(paste0("touch ", path, " 2> /dev/null"))
  # if (code == 0)
  #   writable <- TRUE
  # else
  #   writable <- FALSE
  code <- file.access(names = path, mode=2)
  if (code == 0) writable <- TRUE
  else writable <- FALSE
  return(writable)
}


#' Configuration of scopr
#'
#' Load and update the configuration of the program
#'
#' Anywhere in scopr `scoprConfiguration` can be instantiated so
#' default values for hardware defined parameters can be set dynamically
#' via a config file
#'
#' @name scoprConfiguration
#' @importFrom R6 R6Class
#' @importFrom rjson fromJSON toJSON
#' @method initialize something
#' @field content List of configuration items
#' @export
#' @field config_file Default configuration file path
scoprConfiguration <- R6::R6Class(classname = "scoprConfiguration", public = list(

  content = list(),
  config_file = "",

  #' Initialize a configuration instance
  #' Called by running `scoprConfiguration$new()`
  #' @param config_file Path to the default configuration file
  initialize = function(config_file = config_files) {

    content <- list(debug = FALSE, ncores = 1, stop_backups=FALSE, reference_hour_required=TRUE, testing=FALSE)
    content$folders <- list(
      "results" = list(
        "path" = "/ethoscope_data/results",
        "description" = "A path to a folder containing an ethoscope database of sqlite3 files"
      ),
      "cache" = list(
        "path" = "/ethoscope_data/cache",
        "description" = "A path to a folder containing rds files for fast reloading of data loaded in a previous run.
        The files here are generated automatically everytime a new fly is loaded with rethomics.
        A separate file is created for each fly"
      )
    )

    self$config_file <- "/etc/scopr.conf"
    self$content <- content
    self$load(self$config_file)
  },

  toggle = function(property) {
    self$content[[property]] <- ! self$content[[property]]
    self$save(self$config_file)
  },

  #' Make sure all folders that scopr needs are writable
  #' if they are not, recreate them as needed in the HOME folder
  #' @param config_file Configuration file path
  verify = function(config_file = NULL) {

    content <- self$content
    old_content <- content
    folders <- content$folders

    # check if folder is writable
    for (i in 1:length(folders)) {
      f <- folders[[i]]
      if (!is.writable(path = dirname(f$path))) {
        writable_f <- file.path(Sys.getenv("HOME"), ".scopr", f$path)
        content$folders[[i]]$path <- writable_f
      } else {
        writable_f <- f$path
      }

      # create folder if does not exist
      if (!dir.exists(writable_f)) dir.create(writable_f, recursive = TRUE)
    }
    self$content <- content
    self$save(self$config_file)

    if (!identical(self$content, old_content))
      self$save(self$config_file)
  },

  #' Save the configuration in to a config_file
  #'
  #' Save the configuration stored in self$content
  #' If the passed config_file is null, use the instance's default
  #' @param config_file Configuration file path
  save = function(config_file) {
    json <- rjson::toJSON(self$content)

    perm <- file.access(config_file, mode=2)
    if (perm != 0)
      stop(paste0("No writing permission for config file: ", config_file))


    if(self$content$debug) message(paste0("Saving ", config_file))
    write(x = json, file = config_file)
  },

  #' Load a a configuration from a config_file
  #' If the passed config_file is null, use the instance's default
  #' If the passed file does not exist create it with the conf in the default file
  #' If it exists, load its contents and update the configuration
  #' @param config_file Configuration file path
  load = function(config_file) {

    # if the config file is not does not exist or if it is empty
    if (!file.exists(config_file) | file.size(config_file) == 0)
      self$save(config_file)
    else {
      if(self$content$debug) message(paste0("Loading ", config_file))
      json <- rjson::fromJSON(file = config_file)
      self$content <- modifyList(self$content, json)
    }

    return(self$content)
  }
))
