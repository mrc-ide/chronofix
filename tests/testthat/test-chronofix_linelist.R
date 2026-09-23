
test_that("chronofix_linelist requires samples to be chronofix_mcmc_samples", {
  expect_error(
    chronofix_linelist(
      samples = NULL
      ),
    "Expected 'samples' to be a 'chronofix_mcmc_samples' object",
    fixed = TRUE
  )
})


test_that("chronofix_linelist requires error_thresholds in the right format", {
  mock_samples <- make_mock_samples()
  
  expect_error(
    chronofix_linelist(
      samples = mock_samples,
      error_thresholds = 1,
    ),
    "Expected 'error_thresholds' to be a numeric vector of length 3",
    fixed = TRUE
  )
  
  expect_error(
    chronofix_linelist(
      samples = mock_samples,
      error_thresholds = c(0, 0.5, 2),
    ),
    "Expected all values in 'error_thresholds' to be in the range [0, 1]",
    fixed = TRUE
  )
  
  expect_error(
    chronofix_linelist(
      samples = mock_samples,
      error_thresholds = c(-1, 0.5, 0.95),
    ),
    "Expected all values in 'error_thresholds' to be in the range [0, 1]",
    fixed = TRUE
  )
  
  expect_error(
    chronofix_linelist(
      samples = mock_samples,
      error_thresholds = c(0.05, 0.95, 0.5),
    ),
    "Expected values in 'error_thresholds' to be strictly increasing",
    fixed = TRUE
  )
})


test_that("chronofix_linelist rejects unsupported output formats", {
  mock_samples <- make_mock_samples()
  
  expect_error(
    chronofix_linelist(
      samples = mock_samples,
      format = "pdf"
    ),
    "The 'format' argument must be either 'xlsx' or 'csv'.",
    fixed = TRUE
  )
})


test_that("chronofix_linelist hides p_error columns by default", {
  mock_samples <- make_mock_samples()
  
  result <- suppressMessages({
    chronofix_linelist(
      samples = mock_samples,
      format = "csv",
      filename = tempfile(fileext = ".csv")
    )
  })
  
  expect_false(any(grepl("_p_error$", names(result))))
})


test_that("chronofix_linelist includes p_error columns when requested", {
  mock_samples <- make_mock_samples()
  
  result <- suppressMessages({
    chronofix_linelist(
      samples = mock_samples,
      format = "csv",
      filename = tempfile(fileext = ".csv"),
      show_p_error = TRUE
    )
  })
  
  expect_true(all(
    c(
      "onset_p_error",
      "hospitalisation_p_error",
      "report_p_error",
      "death_p_error",
      "discharge_p_error"
    ) %in% names(result)
  ))
})


test_that("chronofix_linelist distinguishes imputed missing from structural missing", {
  mock_samples <- make_mock_samples()
  
  i <- which(mock_samples$data$group == "community-alive")[1]
  report_event <- 3
  
  mock_samples$augmented_data$estimated_dates[i, report_event, ] <- 
    date_to_int("2025-01-12") + runif(1)
  mock_samples$augmented_data$error_indicators[i, report_event, ] <- NA
  
  result <- suppressMessages({
    chronofix_linelist(
      samples = mock_samples,
      format = "csv",
      filename = tempfile(fileext = ".csv"),
      show_p_error = TRUE
    )
  })
  
  expect_equal(result$report[i], as.Date("2025-01-12"))
  expect_true(is.na(result$report_p_error[i]))
})


test_that("chronofix_linelist writes xlsx output", {
  mock_samples <- make_mock_samples()
  path <- tempfile(fileext = ".xlsx")
  
  result <- suppressMessages({
    chronofix_linelist(
      samples = mock_samples,
      format = "xlsx",
      filename = path
    )
  })
  
  expect_true(file.exists(path))
  expect_false(any(grepl("_p_error$", names(result))))
})


test_that("chronofix_linelist creates expected worksheets in xlsx output", {
  mock_samples <- make_mock_samples()
  path <- tempfile(fileext = ".xlsx")
  
  suppressMessages({
    chronofix_linelist(
      samples = mock_samples, 
      format = "xlsx", 
      filename = path
    )
  })
  
  sheet_names <- openxlsx::getSheetNames(path)
  
  expect_equal(sheet_names, c("Reconstructed Dates", "Legend"))
})


test_that("chronofix_linelist flags filename and fileext mismatch", {
  mock_samples <- make_mock_samples()
  path <- tempfile(fileext = ".xlsx")
  
  expect_error(
    result <-
    chronofix_linelist(
      samples = mock_samples,
      format = "csv",
      filename = path),
    "Extension mismatch: 'filename' has extension '.xlsx' but 'format' is set to 'csv'.",
    fixed = TRUE
  )
  
  path <- tempfile(fileext = ".csv")
  
  expect_error(
    result <-
      chronofix_linelist(
        samples = mock_samples,
        format = "xlsx",
        filename = path),
    "Extension mismatch: 'filename' has extension '.csv' but 'format' is set to 'xlsx'.",
    fixed = TRUE
  )
  
})


test_that("chronofix_linelist uses default filename when filename is NULL", {
  mock_samples <- make_mock_samples()
  
  # CSV default
  suppressMessages({
    chronofix_linelist(
      samples = mock_samples, 
      format = "csv", 
      filename = NULL
    )
  })
  expect_true(file.exists("chronofix_linelist.csv"))
  unlink("chronofix_linelist.csv")
  
  # XLSX default
  suppressMessages({
    chronofix_linelist(
      samples = mock_samples, 
      format = "xlsx", 
      filename = NULL
    )
  })
  expect_true(file.exists("chronofix_linelist.xlsx"))
  unlink("chronofix_linelist.xlsx")
})


test_that("chronofix_linelist replicates custom id and group column names", {
  mock_samples <- make_mock_samples(id = "number", group = "set")
  
  result <- suppressMessages({
    chronofix_linelist(
      samples = mock_samples)
  })
  unlink("chronofix_linelist.xlsx")
  
  expect_equal(names(result)[1:2], c("number", "set"))
})


test_that("chronofix_linelist_status_matrix treats threshold value as Error", {
  mode_dates_num <- matrix(rep(20000, 6), ncol = 1)
  prob_error <- matrix(c(0.04, 0.06, 0.49, 0.51, 0.94, 0.96), ncol = 1)
  
  status <- chronofix_linelist_status_matrix(
    mode_dates_num = mode_dates_num,
    prob_error = prob_error,
    error_thresholds = c(0.05, 0.5, 0.95)
  )
  
  expect_equal(
    as.vector(status),
    c("Correct", 
      "Possible Error", 
      "Possible Error", 
      "Likely Error",
      "Likely Error", 
      "Highly Likely Error")
  )
})


test_that("chronofix_linelist_status_matrix classifies date statuses correctly", {
  mode_dates_num <- matrix(
    c(
      NA, # structurally missing
      20000, # imputed missing
      20000, # highly likely error
      20000, # likely error
      20000, # possible error
      20000 # correct
    ),
    ncol = 1
  )
  
  prob_error <- matrix(
    c(
      NA, # structurally missing
      NA, # imputed missing
      1, # highly likely error
      0.75, # likely error
      0.25, # possible error
      0 # correct
    ),
    ncol = 1
  )
  
  status <- chronofix_linelist_status_matrix(
    mode_dates_num = mode_dates_num,
    prob_error = prob_error,
    error_thresholds = c(0.05, 0.5, 0.95)
  )
  
  expect_equal(
    as.vector(status),
    c(
      "Structurally Missing",
      "Imputed Missing",
      "Highly Likely Error",
      "Likely Error",
      "Possible Error",
      "Correct"
    )
  )
})


test_that("chronofix_style_mapper returns correct style keys for known statuses", {
  expect_equal(chronofix_style_mapper("Structurally Missing"), "style_structural")
  expect_equal(chronofix_style_mapper("Imputed Missing"), "style_imputed")
  expect_equal(chronofix_style_mapper("Highly Likely Error"), "style_highly_likely_error")
  expect_equal(chronofix_style_mapper("Likely Error"), "style_likely_error")
  expect_equal(chronofix_style_mapper("Possible Error"), "style_possible_error")
})


test_that("chronofix_style_mapper returns NA for unstyled statuses", {
  expect_true(is.na(chronofix_style_mapper("Correct")))
})


test_that("chronofix_style_mapper handles NA and unexpected inputs safely", {
  expect_true(is.na(chronofix_style_mapper(NA_character_)))
  expect_true(is.na(chronofix_style_mapper("Unknown Status")))
})
