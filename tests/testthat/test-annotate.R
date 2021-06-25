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

  data <- behavr:::toy_ethoscope_data()
  dt <- annotate_single_roi(data = data)
  expect_equal(dt, data)
})

test_that("annotate_single_roi can handle an annotation function without attributes", {

  setattr(dummy_annot, "parameters", function() {"time_window_length"})
  setattr(dummy_annot, "variables", function() {"xy_dist_log10x1000"})

  data <- behavr:::toy_ethoscope_data()[t < 6,]
  dt <- annotate_single_roi(data = data, FUN = list(dummy_annot))

  expect_is(dt, class = c('behavr', 'data.table', 'data.frame'))
  expect_gt(nrow(data), nrow(dt))
  expect_true("max_distance" %in% colnames(dt))
  expect_true(all(dt$max_distance == c(-3760, -3950, -3196)))
})

test_that("annotate_single_roi can handle an annotation function", {
  data <- behavr:::toy_ethoscope_data()[t < 6,]
  dt <- annotate_single_roi(data = data, FUN = list(dummy_annot))

  expect_is(dt, class = c('behavr', 'data.table', 'data.frame'))
  expect_gt(nrow(data), nrow(dt))
  expect_true("max_distance" %in% colnames(dt))
  expect_true(all(dt$max_distance == c(-3760, -3950, -3196)))
})



test_that("annotate_all can handle several animals", {

  data1 <- behavr:::toy_ethoscope_data()[t < 6,]
  data2 <- behavr:::toy_ethoscope_data()[t < 6,]

  metadata2 <- behavr::meta(data2)
  metadata2$id <- "toy_data_2"

  data2$t <- rev(data2$t)
  data2 <- data2[order(t), ]

  data2$id <- "toy_data_2"
  data.table::setkey(data2, id)
  data.table::setkey(metadata2, id)
  behavr::setmeta(data2, metadata2)

  data <- behavr::rbind_behavr(data1, data2)

  dt <- scopr::annotate_all(data = data, FUN = list(dummy_annot))

  expect_true(all(dt$max_distance == c(-3760, -3950, -3196, -3196, -3950, -3760)))
})

test_that("annotate_all can be passed programmatic arguments", {
  data <- behavr:::toy_ethoscope_data()[t < 6,]
  arguments <- list(FUN = dummy_annot)
  dt <- do.call(scopr::annotate_all, append(arguments, list(data=data)))
  expect_true(all(dt$max_distance == c(-3760, -3950, -3196)))
})
