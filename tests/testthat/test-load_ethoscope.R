context("load_ethoscope")

dummy_fun <- function(data, velocity_correction_coef = 0.001) {
  data$velocity_correction_coef <- velocity_correction_coef
  message("velocity_correction_coef = ", velocity_correction_coef)
  return(data)
}


test_that("query ethoscopes functions", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")

  metadata <- data.table(machine_name = c("E_014"),
                         date = c("2016-01-25"),
                         time = c("21:46:14"),
                         test=c(1)
  )
  query <- link_ethoscope_metadata(metadata,dir)
  dt <- load_ethoscope(query, verbose=F)
  expect_equal(nrow(dt[meta=TRUE]), 20)
})

test_that("query ethoscopes works", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  metadata <- data.table(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
                      )
  query <- link_ethoscope_metadata(metadata,dir)
  dt <- load_ethoscope(query, verbose=F)
  expect_equal(nrow(dt[meta=TRUE]), 60)
})


test_that("query ethoscopes works with multiple cores", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
  )
  query <- link_ethoscope_metadata(query, dir)
  dt <- load_ethoscope(query, verbose=F)


  if(.Platform$OS.type == "unix") {
    dt_m <- load_ethoscope(query, ncores=2, verbose=F)
    expect_identical(dt_m, dt)
  } else {
    message("skipping test, ncores >1 not supported on windows")
  }
})


test_that("load_ethoscope can take a FUN", {

  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(region_id = 1:2,
                      machine_name = "E_014",
                      date = "2016-01-25",
                      time = "21:46:14"
  )

  query <- link_ethoscope_metadata(query, dir)

  dt <- load_ethoscope(
    query, verbose = F,
    FUN = dummy_fun
  )

  expect_true("velocity_correction_coef" %in% colnames(dt))

})


test_that("FUN parameters can be passed through load_ethoscope", {

  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(region_id = 1:2,
                      machine_name = "E_014",
                      date = "2016-01-25",
                      time = "21:46:14"
  )

  query <- link_ethoscope_metadata(query, dir)

  cnd <- rlang::catch_cnd({
    load_ethoscope(
      query, verbose = F,
      FUN = dummy_fun
    )
  }, classes="message")

  expect_equal(cnd$message, "velocity_correction_coef = 0.001\n")


    cnd <- rlang::catch_cnd({
    load_ethoscope(
      query, verbose = F,
      FUN = dummy_fun,
      velocity_correction_coef = 0.004
    )
  }, classes="message")

  expect_equal(cnd$message, "velocity_correction_coef = 0.004\n")
})


test_that("several FUN can be placed in the same load_ethoscope call", {

  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(region_id = 1:2,
                      machine_name = "E_014",
                      date = "2016-01-25",
                      time = "21:46:14"
  )
  query <- link_ethoscope_metadata(query, dir)
  foo <- function(data, ...){behavr::bin_apply_all(data,y = x, ...)}
  attr(foo, "needed_columns") <- function(){
    "x"
  }

  bar <- function(data, ...){behavr::bin_apply_all(data, y = y, ...)}
  attr(bar, "needed_columns") <- function(){
    "y"
  }

  dt <- load_ethoscope(query, verbose = F, FUN = list(foo, bar), x_bin_length = mins(120))

  expect_true(all(dt[1:2, round(x, digits = 1)] == c(0.4, 0.6)))
  expect_true(all(c("x", "y") %in% colnames(dt)))
})



test_that("callbacks work as expected", {

  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")

  metadata <- data.table(region_id = 1,
                         machine_name = c("E_014"),
                         date = c("2016-01-25"),
                         time = c("21:46:14"),
                         test=c(1)
  )[1,]

  callback <- function(detail) {
    # I need to emit a warning just to be able to distinguish it
    # from other messages which unfortunately catch_cnd catches
    # first, ignoring this one as a consequence
    message("callback successful")
  }


  query <- link_ethoscope_metadata(metadata,dir)
  cnd <- rlang::catch_cnd(dt_raw <- load_ethoscope(query, verbose=F, callback=callback), class="message")
  expect_equal(cnd$message, "callback successful\n")
})


not_so_dummy_fun <- function(data, multiplier=1, divider=1) {
  data$x <- data$x * multiplier
  data$y <- data$y / divider
  return(data)
}
attr(not_so_dummy_fun, "parameters") <- function() {
  c("multiplier", "divider")
}
attr(not_so_dummy_fun, "needed_columns") <- function() {
  c("x", "y")
}


test_that("load_ethoscope_multiplex works", {
  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(region_id = 1:2,
                      machine_name = "E_014",
                      date = "2016-01-25",
                      time = "21:46:14"
  )

  query <- link_ethoscope_metadata(query, dir)

  dt1 <- load_ethoscope_multiplex(
    query, verbose = F,
    FUN = not_so_dummy_fun,
  )

  dt2 <- load_ethoscope_multiplex(
    query, verbose = F,
    FUN = not_so_dummy_fun,
    multiplier = 1,
    divider = 1,
    multiplier_sd = 2,
    divider_sd = 2,
    intervals = list(sd = c(1e4, 2e4))
  )

  expect_true(all(dt1$t == dt2$t))
  expect_true(all(c("default", "sd") %in% dt2$interval))
  expect_true(all(dt2[interval == "sd", x] == dt1[dt2$interval == "sd", x] * 2))
  expect_true(all(dt2[interval == "sd", y] == dt1[dt2$interval == "sd", y] / 2))

  dt3 <- load_ethoscope_multiplex(
    query, verbose = F,
    FUN = not_so_dummy_fun,
    multiplier = 1,
    divider = 1,
    divider_sd = 2,
    intervals = list(sd = c(1e4, 2e4))
  )
  expect_true(all(dt1[interval == "sd", x] == dt3[interval == "sd", x]))
})

