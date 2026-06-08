test_that("chronofix_get_delays returns posterior delay summaries", {
  mock <- make_delay_summary_mock()
  
  result <- chronofix_get_delays(
    mcmc_output = mock$mcmc_output,
    delay_map = mock$delay_map
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  
  expect_equal(
    names(result),
    c("Group",
      "Delay",
      "Distribution",
      "Delay_Median",
      "Delay_Lower_95_CrI",
      "Delay_Upper_95_CrI",
      "CV_Median",
      "CV_Lower_95_CrI",
      "CV_Upper_95_CrI")
  )
  
  expect_equal(result$Group[1], "Community Alive")
  expect_equal(result$Group[2], "Hospitalised Alive, Hospitalised Dead")
  
  expect_equal(result$Delay[1], "Onset to Report")
  expect_equal(result$Delay[2], "Hospitalisation to Discharge")
  
  expect_equal(result$Distribution, c("Gamma", "Log-Normal"))
  
  expect_equal(result$Delay_Median, c(3, 30))
  expect_equal(result$CV_Median, c(0.3, 3))
})

test_that("chronofix_get_delays calculates rounded 95% credible intervals", {
  mock <- make_delay_summary_mock()
  
  result <- chronofix_get_delays(
    mcmc_output = mock$mcmc_output,
    delay_map = mock$delay_map
  )
  
  expected_mean1 <- round(
    stats::quantile(c(1, 2, 3, 4, 5),
                    probs = c(0.025, 0.5, 0.975),
                    names = FALSE),
    3
  )
  expected_cv1 <- round(
    stats::quantile(c(0.1, 0.2, 0.3, 0.4, 0.5),
                    probs = c(0.025, 0.5, 0.975),
                    names = FALSE),
    3
  )
  
  expect_equal(result$Delay_Lower_95_CrI[1], expected_mean1[1])
  expect_equal(result$Delay_Median[1], expected_mean1[2])
  expect_equal(result$Delay_Upper_95_CrI[1], expected_mean1[3])
  
  expect_equal(result$CV_Lower_95_CrI[1], expected_cv1[1])
  expect_equal(result$CV_Median[1], expected_cv1[2])
  expect_equal(result$CV_Upper_95_CrI[1], expected_cv1[3])
})

test_that("chronofix_get_delays requires mcmc_output", {
  mock <- make_delay_summary_mock()
  
  expect_error(
    chronofix_get_delays(delay_map = mock$delay_map),
    "'mcmc_output' is missing.",
    fixed = TRUE
  )
})

test_that("chronofix_get_delays requires delay_map", {
  mock <- make_delay_summary_mock()
  
  expect_error(
    chronofix_get_delays(mcmc_output = mock$mcmc_output),
    "'delay_map' is missing.",
    fixed = TRUE
  )
})

test_that("chronofix_get_delays requires pars in mcmc_output", {
  mock <- make_delay_summary_mock()
  
  expect_error(
    chronofix_get_delays(
      mcmc_output = list(),
      delay_map = mock$delay_map
    ),
    "'mcmc_output' must contain a 'pars' array.",
    fixed = TRUE
  )
})

test_that("chronofix_get_delays requires parameter names", {
  mock <- make_delay_summary_mock()
  dimnames(mock$mcmc_output$pars)[[1]] <- NULL
  
  expect_error(
    chronofix_get_delays(
      mcmc_output = mock$mcmc_output,
      delay_map = mock$delay_map
    ),
    "'mcmc_output$pars' must have parameter names in the first dimension.",
    fixed = TRUE
  )
})

test_that("chronofix_get_delays requires delay_map columns", {
  mock <- make_delay_summary_mock()
  delay_map <- mock$delay_map[, setdiff(names(mock$delay_map), "distribution")]
  
  expect_error(
    chronofix_get_delays(
      mcmc_output = mock$mcmc_output,
      delay_map = delay_map
    ),
    "'delay_map' is missing required column(s): distribution",
    fixed = TRUE
  )
})

test_that("chronofix_get_delays requires delay parameters for each delay", {
  mock <- make_delay_summary_mock()
  
  mock$mcmc_output$pars <- mock$mcmc_output$pars[
    c("delay_mean1", "delay_cv1", "delay_mean2"), , , drop = FALSE
  ]
  
  expect_error(
    chronofix_get_delays(
      mcmc_output = mock$mcmc_output,
      delay_map = mock$delay_map
    ),
    "Missing parameter(s) in 'mcmc_output$pars': delay_cv2",
    fixed = TRUE
  )
})

test_that("chronofix_get_delays handles NAs in MCMC output safely", {
  mock <- make_delay_summary_mock()
  mock$mcmc_output$pars["delay_mean1", 1, 1] <- NA
  
  result <- chronofix_get_delays(mock$mcmc_output, mock$delay_map)
  
  expect_false(is.na(result$Delay_Median[1]))
  # median of c(NA, 2, 3, 4, 5) should be 3.5
  expect_equal(result$Delay_Median[1], 3.5) 
})
