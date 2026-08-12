test_that("chronofix_get_delays securely calls validation helper", {
  expect_error(chronofix_get_delays(), "is missing")
})

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
  
  # calculated expected from mock data
  expected_gamma_cv <- round(1 / sqrt(6), 3) # median of 1/sqrt(c(2, 4, 6, 8, 10))
  expected_ln_mean <- round(exp(3 + (1 / 3) / 2), 3)
  expected_ln_cv <- round(sqrt(exp(1 / 3) - 1), 3)
  
  expect_equal(result$Delay_Median, c(3, expected_ln_mean))
  expect_equal(result$CV_Median, c(expected_gamma_cv, expected_ln_cv))
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
    stats::quantile(1 / sqrt(c(2, 4, 6, 8, 10)),
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

test_that("chronofix_get_delays handles NAs in MCMC output safely", {
  mock <- make_delay_summary_mock()
  mock$mcmc_output$pars["delay_mean1", 1, 1] <- NA
  
  result <- chronofix_get_delays(mock$mcmc_output, mock$delay_map)
  
  expect_false(is.na(result$Delay_Median[1]))
  # median of c(NA, 2, 3, 4, 5) should be 3.5
  expect_equal(result$Delay_Median[1], 3.5) 
})
