test_that("Can run mcmc", {
  set.seed(1)
  toy <- toy_data()
  data <- toy$data$observed_data
  delay_map <- toy$delay_map
  
  hyperparameters <- chronofix_hyperparameters()
  initial <- chronofix_mcmc_initial()
  control <- chronofix_mcmc_control(n_steps = 50, n_chains = 3)
  
  samples <- chronofix_mcmc(data, delay_map, hyperparameters, initial, control)
  
  expect_true(inherits(samples, "chronofix_mcmc_samples"))
  
  ## 2 parameters per delay plus prob_error
  expect_equal(dim(samples$pars), c(2 * nrow(delay_map) + 1, 150))
  ## 40 individuals x 5 dates x 150 samples (3 chains of 50 samples)
  expect_equal(dim(samples$data$error_indicators), c(40, 5, 150))
  expect_equal(dim(samples$data$estimated_dates), c(40, 5, 150))
})


test_that("Can run mcmc with cascade sampling", {
  set.seed(1)
  toy <- toy_data()
  data <- toy$data$observed_data
  delay_map <- toy$delay_map
  
  hyperparameters <- chronofix_hyperparameters()
  initial <- chronofix_mcmc_initial()
  control <- chronofix_mcmc_control(n_steps = 50, n_chains = 3,
                                    cascade_sampling = TRUE)
  
  samples <- chronofix_mcmc(data, delay_map, hyperparameters, initial, control)
  
  expect_true(inherits(samples, "chronofix_mcmc_samples"))
  
  ## 2 parameters per delay plus prob_error
  expect_equal(dim(samples$pars), c(2 * nrow(delay_map) + 1, 150))
  ## 40 individuals x 5 dates x 150 samples (3 chains of 50 samples)
  expect_equal(dim(samples$data$error_indicators), c(40, 5, 150))
  expect_equal(dim(samples$data$estimated_dates), c(40, 5, 150))
})


test_that("Can run mcmc with single group and single delay", {
  set.seed(1)
  toy <- toy_data(single_group = "community-alive")
  data <- toy$data$observed_data
  delay_map <- toy$delay_map
  
  hyperparameters <- chronofix_hyperparameters()
  initial <- chronofix_mcmc_initial()
  control <- chronofix_mcmc_control(n_steps = 50, n_chains = 3,
                                    cascade_sampling = TRUE)
  
  samples <- chronofix_mcmc(data, delay_map, hyperparameters, initial, control)
  
  expect_true(inherits(samples, "chronofix_mcmc_samples"))
  
  ## 2 parameters per delay plus prob_error
  expect_equal(dim(samples$pars), c(2 * nrow(delay_map) + 1, 150))
  ## 10 individuals x 2 dates x 150 samples (3 chains of 50 samples)
  expect_equal(dim(samples$data$error_indicators), c(10, 2, 150))
  expect_equal(dim(samples$data$estimated_dates), c(10, 2, 150))
})

