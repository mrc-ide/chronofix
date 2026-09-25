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
  expect_equal(dim(samples$augmented_data$error_indicators), c(40, 5, 150))
  expect_equal(dim(samples$augmented_data$estimated_dates), c(40, 5, 150))
})


test_that("Can run mcmc with single group and single delay", {
  set.seed(1)
  toy <- toy_data(single_group = "community-alive")
  data <- toy$data$observed_data
  delay_map <- toy$delay_map
  
  hyperparameters <- chronofix_hyperparameters()
  initial <- chronofix_mcmc_initial()
  control <- chronofix_mcmc_control(n_steps = 50, n_chains = 3)
  
  samples <- chronofix_mcmc(data, delay_map, hyperparameters, initial, control)
  
  expect_true(inherits(samples, "chronofix_mcmc_samples"))
  
  ## 2 parameters per delay plus prob_error
  expect_equal(dim(samples$pars), c(2 * nrow(delay_map) + 1, 150))
  ## 10 individuals x 2 dates x 150 samples (3 chains of 50 samples)
  expect_equal(dim(samples$augmented_data$error_indicators), c(10, 2, 150))
  expect_equal(dim(samples$augmented_data$estimated_dates), c(10, 2, 150))
})


test_that("Can run mcmc with data prepared with chronofix_prepare_data", {
  set.seed(1)
  toy <- toy_data()
  data <- toy$data$observed_data
  delay_map <- toy$delay_map
  
  hyperparameters <- chronofix_hyperparameters()
  initial <- chronofix_mcmc_initial()
  control <- chronofix_mcmc_control(n_steps = 50, n_chains = 3)
  
  ## First run with the data with default id and group column names
  set.seed(1)
  samples <- chronofix_mcmc(data, delay_map, hyperparameters, initial, control)
  
  ## Now change the id and group column names
  names(data)[names(data) == "id"] <- "number"
  names(data)[names(data) == "group"] <- "set"
  
  ## Error produced without running chronofix_prepare_data
  expect_error(
    chronofix_mcmc(data, delay_map, hyperparameters, initial, control),
    "Did not find column `id` in `data`",
    fixed = TRUE)
  
  data <- chronofix_prepare_data(data, id = "number", group = "set")
  set.seed(1)
  samples2 <- chronofix_mcmc(data, delay_map, hyperparameters, initial, control)
  
  ## will be identical except for data
  samples$data <- NULL
  samples2$data <- NULL
  expect_identical(samples, samples2)
})
