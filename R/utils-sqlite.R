#' @import RSQLite
#' @noRd
dbConnectSafe <- function(file) {

  tryCatch({
    con <- RSQLite::dbConnect(RSQLite::SQLite(), file, flags=RSQLite::SQLITE_RO)
  }, error = function(e) {
    warning(e)
    stop("Sorry. R attempted to open the SQLite file but it cannot. Possible causes are: the path does not lead to a sqlite file or the file is corrupted. Check error trace to learn more.")
  })
}
