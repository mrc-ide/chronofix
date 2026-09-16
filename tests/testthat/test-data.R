
test_that("chronofix_prepare_data prepares data correctly", {
  toy <- toy_data()
  data <- toy$data$observed_data
  
  prepared_data <- chronofix_prepare_data(data)
  expect_true(inherits((prepared_data), "chronofix_data"))
  
  expect_equal(attr(prepared_data, "id"), "id")
  expect_equal(attr(prepared_data, "group"), "group")
})


test_that("chronofix_prepare_data prepares data correctly with custom names", {
  toy <- toy_data()
  data <- toy$data$observed_data
  names(data)[names(data) == "id"] <- "number"
  names(data)[names(data) == "group"] <- "set"
  
  prepared_data <- chronofix_prepare_data(data, id = "number", group = "set")
  expect_true(inherits((prepared_data), "chronofix_data"))
  
  expect_equal(attr(prepared_data, "id"), "number")
  expect_equal(attr(prepared_data, "group"), "set")
})


test_that("chronofix_prepare_data correctly flags missing columns", {
  toy <- toy_data()
  data <- toy$data$observed_data
  
  # missing id column in data
  data_no_id <- data
  data_no_id$id <- NULL
  expect_error(
    chronofix_prepare_data(data_no_id),
    "Did not find column 'id' in 'data'"
  )
  
  # NA ids
  data_na_id <- data
  data_na_id$id[3] <- NA
  
  expect_error(
    chronofix_prepare_data(data_na_id),
    "cannot contain missing values"
  )
  
  # duplicated ids
  data_duplicate_id <- data
  data_duplicate_id$id[2] <- data_duplicate_id$id[1]
  
  expect_error(
    chronofix_prepare_data(data_duplicate_id),
    "must contain unique values"
  )
})
