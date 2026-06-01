library(testthat)

make_mock_data <- function() {
  n_per_group <- 3
  groups <- c("community-alive", "hospitalised-alive", "community-dead")
  n_ind <- n_per_group * length(groups)
  n_evt <- 5
  n_iter <- 4
  n_chains <- 1
  
  observed_data <- data.frame(
    id = seq_len(n_ind),
    group = rep(groups, each = n_per_group),
    onset = as.Date(rep("2025-01-01", n_ind)),
    hospitalisation = as.Date(rep(NA, n_ind)),
    report = as.Date(rep("2025-01-10", n_ind)),
    death = as.Date(rep(NA, n_ind)),
    discharge = as.Date(rep(NA, n_ind))
  )
  
  observed_data$hospitalisation[observed_data$group == "hospitalised-alive"] <-
    as.Date("2025-01-05")
  observed_data$discharge[observed_data$group == "hospitalised-alive"] <-
    as.Date("2025-01-15")
  observed_data$death[observed_data$group == "community-dead"] <-
    as.Date("2025-01-20")
  
  estimated_dates <- array(
    as.Date(NA_character_),
    dim = c(n_ind, n_evt, n_iter, n_chains)
  )
  
  error_indicators <- array(
    NA,
    dim = c(n_ind, n_evt, n_iter, n_chains)
  )
  
  for (i in seq_len(n_ind)) {
    allowed_events <- switch(
      observed_data$group[i],
      "community-alive" = c(1, 3),
      "hospitalised-alive" = c(1, 2, 3, 5),
      "community-dead" = c(1, 3, 4)
    )
    
    for (e in allowed_events) {
      estimated_dates[i, e, , 1] <- as.character(as.Date("2025-01-01") + e + seq_len(n_iter))
      error_indicators[i, e, , 1] <- c(FALSE, FALSE, TRUE, TRUE)
    }
  }
  
  list(
    observed = observed_data,
    mcmc = list(
      data = list(
        estimated_dates = as.character(as.vector(estimated_dates)),
        error_indicators = error_indicators
      )
    )
  )
}

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
