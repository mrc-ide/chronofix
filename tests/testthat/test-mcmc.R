test_that("Can run mcmc", {
  set.seed(1)
  control <- chronofix_mcmc_control(n_steps = 50, n_chains = 3)
  
  model <- toy_model(control = control)$model
  initial <- chronofix_mcmc_initial(model)
  
  sampler <- chronofix_sampler(control)
  samples <- chronofix_mcmc_run(model, sampler, initial, control)
  
  expect_equal(dim(samples$pars), c(length(model$parameters), 150))
  ## 40 individuals x 5 dates x 150 samples (3 chains of 50 samples)
  expect_equal(dim(samples$data$error_indicators), c(40, 5, 150))
  expect_equal(dim(samples$data$estimated_dates), c(40, 5, 150))
})


test_that("Can run mcmc with cascade sampling", {
  set.seed(1)
  control <- chronofix_mcmc_control(n_steps = 50, n_chains = 3,
                                    cascade_sampling = TRUE)
  
  model <- toy_model(control = control)$model
  initial <- chronofix_mcmc_initial(model)
  
  sampler <- chronofix_sampler(control)
  samples <- chronofix_mcmc_run(model, sampler, initial, control)
  
  expect_equal(dim(samples$pars), c(length(model$parameters), 150))
  ## 40 individuals x 5 dates x 150 samples (3 chains of 50 samples)
  expect_equal(dim(samples$data$error_indicators), c(40, 5, 150))
  expect_equal(dim(samples$data$estimated_dates), c(40, 5, 150))
})


test_that("Can run mcmc with single group and single delay", {
  set.seed(1)
  control <- chronofix_mcmc_control(n_steps = 50, n_chains = 3,
                                    cascade_sampling = TRUE)
  
  model <- toy_model(control = control, single_group = "community-alive")$model
  initial <- chronofix_mcmc_initial(model)
  
  sampler <- chronofix_sampler(control)
  samples <- chronofix_mcmc_run(model, sampler, initial, control)
  
  expect_equal(dim(samples$pars), c(length(model$parameters), 150))
  ## 10 individuals x 2 dates x 150 samples (3 chains of 50 samples)
  expect_equal(dim(samples$data$error_indicators), c(10, 2, 150))
  expect_equal(dim(samples$data$estimated_dates), c(10, 2, 150))
})

