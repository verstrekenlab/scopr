#' Verify the pased metadata is compliant
#'
#' @param metadata A data.table describing an experiment
#' It must contain the columns region_id, machine_name and date_time
#' It can contain the column reference_hour to provide a difference ZT for groups of flies
#' It can contain any other column that is not used in the analysis
#' It must NOT contain columns with NA values, as that will make fslscopr ignore the row
#' date_time must follow the format %YYYY-%MM-%DD_%HH-%MM-%SS
#' @export
validate_metadata <- function(metadata) {

  # TODO Add ethoscope metadata validation

  return(TRUE)
}
