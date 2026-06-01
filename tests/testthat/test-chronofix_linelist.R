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
