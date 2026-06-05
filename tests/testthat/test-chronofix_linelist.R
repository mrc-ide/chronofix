library(testthat)

test_that("chronofix_linelist requires mcmc_output", {
  mock <- make_mock_data()
  
  expect_error(
    chronofix_linelist(observed_data = mock$observed),
    "'mcmc_output' is missing.",
    fixed = TRUE
  )
})

test_that("chronofix_linelist requires observed_data", {
  mock <- make_mock_data()
  
  expect_error(
    chronofix_linelist(mcmc_output = mock$mcmc),
    "'observed_data' is missing.",
    fixed = TRUE
  )
})

test_that("chronofix_linelist requires group column in observed_data", {
  mock <- make_mock_data()
  observed_no_group <- mock$observed[, setdiff(names(mock$observed), "group")]
  
  expect_error(
    chronofix_linelist(
      mcmc_output = mock$mcmc,
      observed_data = observed_no_group,
      format = "csv",
      filename = tempfile(fileext = ".csv")
    ),
    "No 'group' column found in observed_data",
    fixed = TRUE
  )
})

test_that("chronofix_linelist rejects unsupported output formats", {
  mock <- make_mock_data()
  
  expect_error(
    chronofix_linelist(
      mcmc_output = mock$mcmc,
      observed_data = mock$observed,
      format = "pdf"
    ),
    "The 'format' argument must be either 'xlsx' or 'csv'.",
    fixed = TRUE
  )
})

test_that("chronofix_linelist checks observed event columns match MCMC event dimension", {
  mock <- make_mock_data()
  
  observed_extra_event <- mock$observed
  observed_extra_event$extra_event <- as.Date("2025-02-01")
  
  expect_error(
    chronofix_linelist(
      mcmc_output = mock$mcmc,
      observed_data = observed_extra_event,
      format = "csv",
      filename = tempfile(fileext = ".csv")
    ),
    "Only 'id', 'group' and event date columns can be supplied as observed data",
    fixed = TRUE
  )
})

test_that("chronofix_linelist hides p_error columns by default", {
  mock <- make_mock_data()
  
  result <- suppressMessages({
    chronofix_linelist(
      mcmc_output = mock$mcmc,
      observed_data = mock$observed,
      format = "csv",
      filename = tempfile(fileext = ".csv")
    )
  })
  
  expect_false(any(grepl("_p_error$", names(result))))
})

test_that("chronofix_linelist includes p_error columns when requested", {
  mock <- make_mock_data()
  
  result <- suppressMessages({
    chronofix_linelist(
      mcmc_output = mock$mcmc,
      observed_data = mock$observed,
      format = "csv",
      filename = tempfile(fileext = ".csv"),
      show_p_error = TRUE
    )
  })
  
  expect_true(all(
    c(
      "Onset_p_error",
      "Hospitalisation_p_error",
      "Report_p_error",
      "Death_p_error",
      "Discharge_p_error"
    ) %in% names(result)
  ))
})

test_that("chronofix_linelist distinguishes imputed missing from structural missing", {
  mock <- make_mock_data()
  
  i <- which(mock$observed$group == "community-alive")[1]
  report_event <- 3
  
  estimated_dates <- mock$mcmc$data$estimated_dates
  dim(estimated_dates) <- dim(mock$mcmc$data$error_indicators)
  
  estimated_dates[i, report_event, , 1] <- "2025-01-12"
  mock$mcmc$data$error_indicators[i, report_event, , 1] <- NA
  
  mock$mcmc$data$estimated_dates <- as.vector(estimated_dates)
  
  result <- suppressMessages({
    chronofix_linelist(
      mcmc_output = mock$mcmc,
      observed_data = mock$observed,
      format = "csv",
      filename = tempfile(fileext = ".csv"),
      show_p_error = TRUE
    )
  })
  
  expect_equal(result$Report[i], as.Date("2025-01-12"))
  expect_true(is.na(result$Report_p_error[i]))
})

test_that("chronofix_linelist writes xlsx output", {
  mock <- make_mock_data()
  path <- tempfile(fileext = ".xlsx")
  
  result <- suppressMessages({
    chronofix_linelist(
      mcmc_output = mock$mcmc,
      observed_data = mock$observed,
      format = "xlsx",
      filename = path
    )
  })
  
  expect_true(file.exists(path))
  expect_false(any(grepl("_p_error$", names(result))))
})

test_that("chronofix_linelist creates expected worksheets in xlsx output", {
  mock <- make_mock_data()
  path <- tempfile(fileext = ".xlsx")
  
  suppressMessages({
    chronofix_linelist(
      mcmc_output = mock$mcmc, 
      observed_data = mock$observed, 
      format = "xlsx", 
      filename = path
    )
  })
  
  sheet_names <- openxlsx::getSheetNames(path)
  
  expect_equal(sheet_names, c("Reconstructed Dates", "Legend"))
})

test_that("chronofix_linelist flags filename and fileext mismatch", {
  mock <- make_mock_data()
  path <- tempfile(fileext = ".xlsx")
  
  expect_error(
    result <-
    chronofix_linelist(
      mcmc_output = mock$mcmc,
      observed_data = mock$observed,
      format = "csv",
      filename = path),
    "Extension mismatch: 'filename' has extension '.xlsx' but 'format' is set to 'csv'.",
    fixed = TRUE
  )
  
  path <- tempfile(fileext = ".csv")
  
  expect_error(
    result <-
      chronofix_linelist(
        mcmc_output = mock$mcmc,
        observed_data = mock$observed,
        format = "xlsx",
        filename = path),
    "Extension mismatch: 'filename' has extension '.csv' but 'format' is set to 'xlsx'.",
    fixed = TRUE
  )
  
})

test_that("chronofix_linelist uses default filename when filename is NULL", {
  mock <- make_mock_data()
  
  # CSV default
  suppressMessages({
    chronofix_linelist(
      mcmc_output = mock$mcmc, 
      observed_data = mock$observed, 
      format = "csv", 
      filename = NULL
    )
  })
  expect_true(file.exists("chronofix_linelist.csv"))
  unlink("chronofix_linelist.csv")
  
  # XLSX default
  suppressMessages({
    chronofix_linelist(
      mcmc_output = mock$mcmc, 
      observed_data = mock$observed, 
      format = "xlsx", 
      filename = NULL
    )
  })
  expect_true(file.exists("chronofix_linelist.xlsx"))
  unlink("chronofix_linelist.xlsx")
})

test_that("chronofix_linelist capitalises event names in the returned dataframe", {
  mock <- make_mock_data()
  
  result <- suppressMessages({
    chronofix_linelist(
      mcmc_output = mock$mcmc,
      observed_data = mock$observed,
      format = "csv",
      filename = tempfile(fileext = ".csv")
    )
  })
  
  raw_names <- setdiff(colnames(mock$observed), c("id", "group"))
  expected_names <- paste0(toupper(substr(raw_names, 1, 1)), substring(raw_names, 2))
  
  expect_true(all(expected_names %in% colnames(result)))
})

test_that("chronofix_linelist_status_matrix treats threshold value as Error", {
  median_dates_num <- matrix(c(20000, 20000), ncol = 1)
  prob_error <- matrix(c(0.5, 0.499), ncol = 1)
  
  status <- chronofix_linelist_status_matrix(
    median_dates_num = median_dates_num,
    prob_error = prob_error,
    error_threshold = 0.5
  )
  
  expect_equal(
    as.vector(status),
    c("Error", "Potential Error")
  )
})

test_that("chronofix_linelist_status_matrix classifies date statuses correctly", {
  median_dates_num <- matrix(
    c(
      NA, # structurally missing
      20000, # imputed missing
      20000, # error
      20000, # potential error
      20000 # correct
    ),
    ncol = 1
  )
  
  prob_error <- matrix(
    c(
      NA, # structurally missing
      NA, # imputed missing
      0.75, # error
      0.25, # potential error
      0 # correct
    ),
    ncol = 1
  )
  
  status <- chronofix_linelist_status_matrix(
    median_dates_num = median_dates_num,
    prob_error = prob_error,
    error_threshold = 0.5
  )
  
  expect_equal(
    as.vector(status),
    c(
      "Structurally Missing",
      "Imputed Missing",
      "Error",
      "Potential Error",
      "Correct"
    )
  )
})

test_that("chronofix_style_mapper returns correct style keys for known statuses", {
  expect_equal(chronofix_style_mapper("Structurally Missing"), "style_structural")
  expect_equal(chronofix_style_mapper("Imputed Missing"), "style_imputed")
  expect_equal(chronofix_style_mapper("Error"), "style_error")
  expect_equal(chronofix_style_mapper("Potential Error"), "style_potential")
})

test_that("chronofix_style_mapper returns NA for unstyled statuses", {
  expect_true(is.na(chronofix_style_mapper("Correct")))
})

test_that("chronofix_style_mapper handles NA and unexpected inputs safely", {
  expect_true(is.na(chronofix_style_mapper(NA_character_)))
  expect_true(is.na(chronofix_style_mapper("Unknown Status")))
})
