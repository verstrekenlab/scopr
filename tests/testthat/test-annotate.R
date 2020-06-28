context("annotate")

test_that("annotate can handle an annotation function", {

  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db", sep= "/")

  dummy_annot <- function(data, time_window_length = 5000) {
    data <- data[, t_round := floor(t / time_window_length) * time_window_length]
    setkeyv(data, c('id', 't_round'))
    d_small <- data[, .(max_distance = max(xy_dist_log10x1000)), by=key(data)]
    setkeyv(d_small, 'id')
    setnames(d_small, 't_round', 't')

    data <- data[d_small, on=c('id', 't'), roll=TRUE]
    data <- data[, t_round := NULL]
    return(data)
  }

  dt <- annotate(out = copy(fslscopr:::dt), FUN = list(dummy_annot))

  expect_is(dt, class = c('behavr', 'data.table', 'data.frame'))
  expect_gt(nrow(fslscopr:::dt), nrow(dt))

})
