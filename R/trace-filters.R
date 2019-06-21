#' Take the mode of a distribution
Mode <- function(x) {
  ux <- unique(x)
  tabulation <- tabulate(match(x, ux))
  if(max(tabulation) > length(x) / 2) {
    return(ux[which.max(tabulation)])
  }
  else
    return(x[1])

}

#' @export
Mode_filter <- function(data, window_width=10) {

  if("y" %in% colnames(data)) {
    data <- data[, `:=`(x = rollapply(x, FUN = Mode, width = window_width, partial = T),
                        y = rollapply(y, FUN = Mode, width = window_width, partial = T)
    ), by = id]



  } else {
    data <- data[, `:=`(x = rollapply(x, FUN = Mode, width = window_width, partial = T)),
                 by = id]
  }

  null_distance_log10x1000 <- round(1000*log10(3e-4))
  data[which(x[-1] - x[-(.N)] == 0), xy_dist_log10x1000 := null_distance_log10x1000]

  return(data)

}
