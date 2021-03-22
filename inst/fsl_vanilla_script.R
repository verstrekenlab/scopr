  library(data.table)
  library(fslggetho)
  library(fslsleepr)
  library(fslscopr)
  library(rlang)

  #metadata <- fread("/1TB/Cloud/Lab/Gitlab/fslretho/inst/extdata/ethoscope_metadata/metadata_test.csv")
  metadata <- fread("/1TB/Cloud/Lab/fslretho_notebooks/metadata/aoj_metadata - 2020-05-09_rebound_014.csv")
  head(metadata)

  metadata <- link_ethoscope_metadata(x = metadata, result_dir = "/ethoscope_data/results/")
  metadata$test <- 0.003
  #metadata$test <- 0.005

  DT <- load_ethoscope(
    metadata = metadata,
    verbose=TRUE,
    cache = NULL,
    ncores = 4,
    FUN = sleep_annotation,
    map_arg = list(velocity_correction_coef = "test"),
    extra_columns = c("frame_count")
  )


ggetho(DT, aes(y=asleep)) +
  stat_pop_etho() +
  stat_ld_annotations()

ggplot(data = dt, aes(x = max_velocity)) +
  geom_histogram(bins=300) +
  scale_x_continuous(limits = c(0,5))

ggplot(data = dt, aes(x = t, y = max_velocity)) +
  geom_point() +
  scale_x_hours()
