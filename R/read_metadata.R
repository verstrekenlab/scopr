#' Load a metadata .csv into R and perform basic validation
#'
#' @importFrom data.table fread
#' @importFrom magrittr `%>%`
#' @export
read_metadata <- function(metadata_path) {

  metadata <- tryCatch(
    data.table::fread(cmd = paste0("grep -v '#' ", metadata_path)),
    error = function(e) {
      message(e)
      stop_bad_argument(what = "metadata_path", "cannot be read with fread(). Check it is not malformed. For instance, make sure all rows have same number of columns i.e. same number of commas")
    })

  # change the column zt0 to reference_hour if available
  if ((!"reference_hour" %in% colnames(metadata)) & ("ZT0" %in% colnames(metadata))) {
    colnames(metadata) <- colnames(metadata) %>% gsub(
      pattern = "ZT0",
      x = colnames(metadata),
      replacement =  "reference_hour"
    )
  }

  return(metadata)
}
