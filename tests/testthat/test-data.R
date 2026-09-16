
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
  
  prepared_data2 <- chronofix_prepare_data(prepared_data)
  expect_identical(prepared_data, prepared_data2)
  
  data_no_id <- data
  data_no_id$number <- NULL
  expect_error(
    chronofix_prepare_data(data_no_id, id = "number", group = "set"),
    "Did not find column `number` in `data`"
  )
  
  data_no_group <- data
  data_no_group$set <- NULL
  expect_error(
    chronofix_prepare_data(data_no_group, id = "number", group = "set"),
    "Did not find column `set` in `data`"
  )
  
})


test_that("chronofix_prepare_data requires data.frame input", {
  expect_error(
    chronofix_prepare_data(NULL),
    "Expected `data` to be a 'data.frame' object"
  )
  
  expect_error(
    chronofix_prepare_data(data.frame()),
    "Expected `data` to have at least one row"
  )
})


test_that("chronofix_prepare_data correctly flags missing columns", {
  toy <- toy_data()
  data <- toy$data$observed_data
  
  # missing id column in data
  data_no_id <- data
  data_no_id$id <- NULL
  expect_error(
    chronofix_prepare_data(data_no_id),
    "Did not find column `id` in `data`"
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
