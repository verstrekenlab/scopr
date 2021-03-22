context("read_metadata")

test_that("read_metadata validates valid metadata", {

  expect_error(read_metadata(metadata_path = "static_files/bad_header.csv"))
  metadata <- read_metadata(metadata_path = "static_files/good_metadata.csv")
  expect_identical(metadata, data.table::fread("static_files/good_metadata.csv"))
})
