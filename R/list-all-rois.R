#' List the ROIs avalable in an ethoscope result file
#' @param FILE the name of the input file
#' @return an integer vector
#' @noRd
#' @importFrom logging logerror
#' @importFrom glue glue
list_all_rois <- function(FILE){
  roi_idx = NULL
  tryCatch({
    tryCatch({
      con <- RSQLite::dbConnect(RSQLite::SQLite(), FILE, flags=RSQLite::SQLITE_RO)
    }, error = function(e) {
      logging::logerror(glue::glue("I cant establish a connection with file {FILE}"))
      logging::logerror("Does it exist? Do I have reading permission?")
      logging::logerror(e)
    })
    roi_map <- data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM ROI_MAP"))
  },
  finally = {
    RSQLite::dbDisconnect(con)
  }
  )
  data.table::setkey(roi_map, roi_idx)
  available_rois  <- as.integer(roi_map[ ,roi_idx])
  return(available_rois)
}
