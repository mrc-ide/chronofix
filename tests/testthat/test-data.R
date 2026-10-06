
test_that("chronofix_prepare_data prepares data correctly", {
  toy <- toy_data()
  data <- toy$data$observed_data
  
  prepared_data <- chronofix_prepare_data(data)
  expect_true(inherits((prepared_data), "chronofix_data"))
  
  expect_equal(attr(prepared_data, "id"), "id")
  expect_equal(attr(prepared_data, "group"), "group")
  expect_equal(attr(prepared_data, "format"), "%Y-%m-%d")
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
  
  # only one event column
  data_only_onset <- data[, c("id", "group", "onset")]
  expect_error(
    chronofix_prepare_data(data_only_onset),
    "Expected `data` to have at least two columns in addition to"
  )
})


test_that("chronofix_prepare_data correctly flags all-NA rows", {
  toy <- toy_data()
  data <- toy$data$observed_data
  
  # individual has all NA dates
  data_all_na <- data
  event_cols <- setdiff(names(data), c("id", "group"))
  data_all_na[1, event_cols] <- NA # row 1 dates set to all NA
  expect_error(
    data_all_na <- chronofix_prepare_data(data_all_na),
    "cannot have `NA` for all event dates"
  )
})


test_that("chronofix_prepare_data prepares data correctly with specified
          date format", {
  format <- "%d/%m/%Y"
  toy <- toy_data(format = format)
  data <- toy$data$observed_data
  
  prepared_data <- chronofix_prepare_data(data, format = format)
  expect_true(inherits((prepared_data), "chronofix_data"))
  
  expect_equal(attr(prepared_data, "id"), "id")
  expect_equal(attr(prepared_data, "group"), "group")
  expect_equal(attr(prepared_data, "format"), format)
})


test_that("chronofix_prepare_data correctly flags incorrect date formats", {
  toy <- toy_data(format = "%d/%m/%Y")
  data <- toy$data$observed_data
  
  expect_error(chronofix_prepare_data(data),
               'All dates not matching declared format "%Y-%m-%d"')
  
  data$onset[1:10] <- format(as.Date(data$onset[1:10], format = "%d/%m/%Y"),
                             format = "%Y-%m-%d")
  expect_error(chronofix_prepare_data(data, format = "%d/%m/%Y"),
               'Some dates not matching declared format "%d/%m/%Y"')
})
