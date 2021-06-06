context("load_ethoscope")

dummy_fun <- function(data, velocity_correction_coef = 0.001) {
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

test_that("several annotations can be placed in the same call", {

  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
  )
  query <- link_ethoscope_metadata(query, dir)
  foo <- function(d){behavr::bin_apply_all(d,y = x)}
  attr(foo, "needed_columns") <- function(){
    "x"
  }

  bar <- function(d){behavr::bin_apply_all(d, z = x)}
  attr(foo, "needed_columns") <- function(){
    "x"
  }

  dt <- load_ethoscope(query, verbose = F, FUN = list(foo, bar))
})


test_that("different coefficients can be passed through load_ethoscope", {

  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")
  query <- data.frame(machine_name = c("E_014", "E_014","E_029"),
                      date = c("2016-01-25", "2016-02-17","2016-01-25"),
                      time = c("21:46:14", NA, NA),
                      test=c(1,2,3)
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


test_that("callbacks work as expected", {

  dir <- paste0(scopr_example_dir(), "/ethoscope_results/")

  metadata <- data.table(machine_name = c("E_014"),
                         date = c("2016-01-25"),
                         time = c("21:46:14"),
                         test=c(1)
  )[1,]

  updateProgress_load <- function(detail) {
    message("load callback successful")
  }
  updateProgress_annotate <- function(detail) {
    # I need to emit a warning just to be able to distinguish it
    # from other messages which unfortunately catch_cnd catches
    # first, ignoring this one as a consequence
    warning("annotate callback successful")
  }


  query <- link_ethoscope_metadata(metadata,dir)
  cnd <- rlang::catch_cnd(dt_raw <- load_ethoscope(query, verbose=F, updateProgress_load=updateProgress_load), class="message")
  expect_equal(cnd$message, "load callback successful\n")

  cnd <- rlang::catch_cnd(dt <- suppressMessages(load_ethoscope(
    query, verbose=F,
    FUN=dummy_fun,
    updateProgress_annotate=updateProgress_annotate
  )), class="warning")

  expect_equal(cnd$message, "annotate callback successful")

})

