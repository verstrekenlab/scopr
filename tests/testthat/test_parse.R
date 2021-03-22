# library(data.table)
# metadata <- fread("/1TB/Cloud/Lab/Gitlab/fslretho/inst/extdata/ethoscope_metadata/metadata_test.csv")
#
# metadata <- fslscopr::link_ethoscope_metadata(metadata, result_dir = "/ethoscope_data/results/")
#
# d <- out
#
# interaction_number <- function(data) {
#   # t_round is included in the columns because it is in the by arg
#   d <- copy(data)
#   wrapped <- function(d) {
#     d2 <- copy(d)
#     d2[, t_round := floor(t / 10)]
#     d_small <- d2[, .(interaction_count = sum(has_interacted)),  by = 't_round']
#     d_small[, t := t_round * 10]
#     d_small[, t_round := NULL]
#     d2 <- d2[d_small, on = c("t"), roll = T]
#     d2[, t_round := NULL]
#     return(d2)
#   }
#
#   data <- data[, wrapped(.SD), key=key(data)]
#   return(data)
# }
#
# attr(interaction_number, "needed_columns") <- function(...) {
#   c("has_interacted")
# }
#
#
# dt <- load_ethoscope(metadata = metadata[1:2,], verbose = T, reference_hour = NULL, FUN = list(interaction_number))
# dt <- load_ethoscope(metadata = metadata[1:2,], verbose = T, reference_hour = NULL, FUN = list(velocity_avg))
# dt <- load_ethoscope(metadata = metadata[1:2,], verbose = T, reference_hour = NULL, FUN = list(fslsleepr::sleep_annotation))
# dt <- load_ethoscope(metadata = metadata[1:2,], verbose = T, reference_hour = NULL, FUN = list(
#   movement_detector,
#   fslsleepr::sleep_annotation
#   ))
