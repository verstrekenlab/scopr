context("parse_single_roi")


test_that("parse_single_roi works in normal conditions", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep="/")
  data <- data.table::data.table(id="xxx", region_id=1, file_info=list(list(path=test_file)), key="id")

  a <- parse_single_roi(data, verbose = F)
  a <- parse_single_roi(data, FUN = function(d){fslbehavr::bin_apply_all(d, y = x)}, verbose = F)

  expect_true(all(a[,id] == "xxx"))
  expect_s3_class(a, "behavr")
  expect_identical(a[meta=T], data)
})


test_that("parse_single_roi works with memosiation", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep="/")
  cache <- tempfile("scopr_test_cache")

  data <- data.table::data.table(id="xxx", region_id=1, file_info=list(list(path=test_file)), key="id")
  a <- fslscopr::parse_single_roi(data, verbose=F)
  b <- fslscopr::parse_single_roi(data, cache = cache, verbose = F)
  c <- fslscopr::parse_single_roi(data, cache = cache, verbose = F)

  expect_identical(a, b)
  expect_true(all(c == b) & identical(c[meta=T], b[meta=T]))
})



test_that("parse_single_roi works with autocolumn finding", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep="/")
  data <- data.table::data.table(id="xxx", region_id=1, file_info=list(list(path=test_file)), key="id")

  foo <- function(d){fslbehavr::bin_apply_all(d,y = x)}
  attr(foo, "needed_columns") <- function(){
    "x"
  }
  foo
  a <- fslscopr::parse_single_roi(data, FUN= foo, verbose = F)
  a[meta=T]

  attr(foo, "needed_columns") <- function(...){
    "www"
  }

  expect_error(fslscopr::parse_single_roi(data, FUN= foo, verbose=F))

})

pick_first <- function(d) {
  # t_round is included in the columns because it is in the by arg
  d2 <- copy(d)
  d2[, t_round := floor(t / 100)]
  d2 <- d2[, .SD[1,],  by = 't_round']
  return(d2)
}


test_that("custom annotation functions can be passed", {
  dir <- scopr_example_dir()
  test_file <- paste(dir, "ethoscope_results/029/E_029/2016-01-25_21-14-55/2016-01-25_21-14-55_029.db",sep = "/")
  data <- data.table::data.table(id = "xxx", region_id = 1, file_info = list(list(path = test_file)), key = "id")

  foo <- function(d){fslbehavr::bin_apply_all(d, y = x)}
  attr(foo, "needed_columns") <- function(){
    "x"
  }
  foo
  a <- fslscopr::parse_single_roi(data, FUN = foo, verbose = F)
  a[meta = T]

  attr(foo, "needed_columns") <- function(...){
    "www"
  }

  expect_error(fslscopr::parse_single_roi(data, FUN= foo, verbose=F))
})
