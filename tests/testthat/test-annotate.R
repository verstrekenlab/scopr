context("annotate_single_roi")

dummy_annot <- function(data, time_window_length = 2) {
  # compute the maximum xy_dist_log10x1000 in intervals of 5000 seconds
  data <- data[, t_round := floor(t / time_window_length) * time_window_length]
  setkey(data, t_round)
  d_small <- data[, .(max_distance = max(xy_dist_log10x1000)), by = key(data)]
  setnames(d_small, 't_round', 't')

  data <- data[d_small, on = 't', roll = TRUE]
  data[, t_round := NULL]
  data[ , xy_dist_log10x1000 := NULL]

  return(data)
}

test_that("annotate_single_roi works without annotation functions", {

  data <- scopr:::toy_ethoscope_data()
  dt <- annotate_single_roi(data = data)
  expect_equal(dt, data)
})

test_that("annotate_single_roi can handle an annotation function", {

  data <- scopr:::toy_ethoscope_data()[t < 6,]
  dt <- annotate_single_roi(data = data, FUN = list(dummy_annot))

  expect_is(dt, class = c('behavr', 'data.table', 'data.frame'))
  expect_gt(nrow(data), nrow(dt))
  expect_true("max_distance" %in% colnames(dt))
  expect_true(all(dt$max_distance == c(-3760, -3950, -3196)))
})
