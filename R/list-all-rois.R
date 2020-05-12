#' List the ROIs avalable in an ethoscope result file
#' @param FILE the name of the input file
#' @return an integer vector
#' @noRd
#' @importFrom logging logerror
#' @importFrom glue glue
list_all_rois <- function(FILE){
  roi_idx = NULL
  tryCatch({
    con <- tryCatch({
      RSQLite::dbConnect(RSQLite::SQLite(), FILE, flags=RSQLite::SQLITE_RO)
    }, error = function(e) {
      logging::loginfo(glue::glue("I cant establish a connection with FILE={FILE}"))
      logging::loginfo("This is the line causing trouble")
      logging::loginfo(glue::glue("RSQLite::dbConnect(RSQLite::SQLite(), FILE, flags=RSQLite::SQLITE_RO)"))
      logging::loginfo("Does it exist? Do I have reading permission? See the error log below")
      logging::logerror(e)
    })
    roi_map <- data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM ROI_MAP"))
  }, error = function(e) {
    browser()
  }, finally = {
    RSQLite::dbDisconnect(con)
  }
  )
  data.table::setkey(roi_map, roi_idx)
  available_rois  <- as.integer(roi_map[ ,roi_idx])
  return(available_rois)
}
