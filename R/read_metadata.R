#' Load an ethoscope metadata .csv into R
#' 
#' With this function you can pass a metadata file:
#' * with comments (lines starting with #)
#' * ZT0 OR reference_hour can be passed to state the time in GMT TZ when the lights turned on
#' @importFrom data.table as.data.table 
#' @return data.table with loaded metadata (not validated)
read_metadata <- function(metadata_path) {

  metadata <- read.table(metadata_path, sep=",", comment.char="#", header=TRUE, row.names=F)
  metadata <- data.table::as.data.table(metadata)

  # change the column zt0 to reference_hour if available
  if ((!"reference_hour" %in% colnames(metadata)) & ("ZT0" %in% colnames(metadata))) {
    metadata$reference_hour <- metadata$ZT0
    metadata$ZT0 <- NULL
  }
  return(metadata)
}
