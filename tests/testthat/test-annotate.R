context("annotate")

dummy_annot <- function(data, time_window_length = 5000) {
  # compute the maximum xy_dist_log10x1000 in intervals of 5000 seconds
  data <- data[, t_round := floor(t / time_window_length) * time_window_length]
  setkey(data, t_round)
  d_small <- data[, .(max_distance = max(xy_dist_log10x1000)), by = key(data)]
  setnames(d_small, 't_round', 't')

  data <- data[d_small, on = 't', roll = TRUE]
  data <- data[, t_round := NULL]
  return(data)
}

test_that("annotate works without annotation functions", {
  # it just returns the input untouched
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db", sep = "/")

  data <- fslscopr:::dt[xmv(region_id) == 1,]
  data[, id := NULL]
  dt <- annotate(out = data)
  expect_equal(dt, data)

})

test_that("annotate can handle an annotation function", {

  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db", sep = "/")

  data <- fslscopr:::dt[xmv(region_id) == 1,]
  data[, id := NULL]

  dt <- annotate(out = data, FUN = list(dummy_annot))

  expect_is(dt, class = c('behavr', 'data.table', 'data.frame'))
  expect_gt(nrow(data), nrow(dt))
  expect_true("max_distance" %in% colnames(dt))

})
