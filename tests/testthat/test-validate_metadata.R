context("validate_metadata")

test_that("validate_metadata validates valid metadata", {

  # standard metadata
  metadata <- data.table(region_id = 1:10, machine_name = "ETHOSCOPE_001", date = "2020-07-05", reference_hour = 12)
  expect_true(validate_metadata(metadata))

  # passing a non standard ethoscope name
  # this is ok but we need to verify linking does not return an empty data.table then
  metadata <- data.table(region_id = 1:10, machine_name = "ETHOSCOPE_01", date = "2020-07-05", reference_hour = 12)
  expect_true(validate_metadata(metadata))

  # with a time column
  metadata <- data.table(region_id = 1:10, machine_name = "ETHOSCOPE_01", date = "2020-07-05", time = "23:10:54", reference_hour = 12)
  expect_true(validate_metadata(metadata))

  # without region id
  metadata <- data.table(machine_name = "ETHOSCOPE_01", date = "2020-07-05", time = "23:10:54", reference_hour = 12)
  expect_true(validate_metadata(metadata))

  # alternative formats that the parser can handle
  metadata <- data.table(machine_name = "ETHOSCOPE_01", date = "2020/07/05", time = "8:00:00", reference_hour = 12)
  expect_true(validate_metadata(metadata))

})

test_that("validate_metadta rejects invalid metadata", {

  # missing key columns
  metadata <- data.table(region_id = 1:10, date = "2020-07-05", reference_hour = 12)
  cnd <- rlang::catch_cnd(validate_metadata(metadata))
  expect_equal(cnd$message, "`metadata` must contain columns machine_name date reference_hour")

  metadata <- data.table(region_id = 1:10, reference_hour = 12)
  cnd <- rlang::catch_cnd(validate_metadata(metadata))
  expect_equal(cnd$message, "`metadata` must contain columns machine_name date reference_hour")

  metadata <- data.table(region_id = 1:10, genotype = "A")
  cnd <- rlang::catch_cnd(validate_metadata(metadata))
  expect_equal(cnd$message, "`metadata` must contain columns machine_name date reference_hour")

  metadata <- data.table(region_id = 1:10, reference_hour = "1", date = "2020-05-07", machine_name = "ETHOSCOPE_001")
  cnd <- rlang::catch_cnd(validate_metadata(metadata))
  expect_equal(cnd$message, "`reference_hour` must be of class numeric; not character.")

  metadata <- data.table(region_id = 1:10, genotype = "A")
  cnd <- rlang::catch_cnd(validate_metadata(metadata))
  expect_equal(cnd$message, "`metadata` must contain columns machine_name date reference_hour")


  # with wrong date format
  metadata <- data.table(region_id = 1:10, machine_name = "ETHOSCOPE_001", date = "2020-07-5", reference_hour = 12)
  cnd <- rlang::catch_cnd(validate_metadata(metadata), classes = "error")
  expect_equal(substr(cnd$message, 1, 36), "`date` must follow format YYYY-MM-DD")

  # with wrong time format
  metadata <- data.table(region_id = 1:10, machine_name = "ETHOSCOPE_001", date = "2020-07-05", time = "8-00-00", reference_hour = 12)
  cnd <- rlang::catch_cnd(validate_metadata(metadata), classes = "error")
  expect_equal(substr(cnd$message, 1, 34), "`time` must follow format HH:MM:SS")


})
