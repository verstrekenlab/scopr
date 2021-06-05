#' List the ROIs avalable in an ethoscope result file
#' @param FILE the name of the input file
#' @return an integer vector
#' @import data.table 
#' @import RSQLite
#' @noRd
list_all_rois <- function(FILE){
  roi_idx = NULL
  tryCatch({
    con <- dbConnectSafe(FILE)
    roi_map <- data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM ROI_MAP"))
  }, finally = {
    RSQLite::dbDisconnect(con)
  }
  )
  data.table::setkey(roi_map, roi_idx)
  available_rois  <- as.integer(roi_map[ ,roi_idx])
  return(available_rois)
}
