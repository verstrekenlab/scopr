context("configuration")


test_that("configuration can be toggled", {

  conf <- scopr::scoprConfiguration$new()
  conf$content$dummy <- F
  conf$toggle("dummy")
  testthat::expect_true(conf$content$dummy)
  conf$toggle("dummy")
  testthat::expect_false(conf$content$dummy)
  conf$content$dummy <- NULL
  conf$save(conf$config_file)
})
