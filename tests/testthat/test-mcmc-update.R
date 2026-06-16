test_that("update_prob_error_parameters works correctly", {
  
  ## error_indicators - here there are 
  error_indicators <- data.frame(date1 = c(TRUE, FALSE, NA, FALSE, TRUE),
                                 date2 = c(NA, NA, FALSE, FALSE, TRUE),
                                 date3 = c(TRUE, TRUE, FALSE, FALSE, FALSE),
                                 date4 = c(TRUE, FALSE, FALSE, NA, TRUE))
  hyperparameters <- list(prob_error_shape1 = 2,
                          prob_error_shape2 = 3)
  
  beta_pars <- update_prob_error_parameters(error_indicators, hyperparameters)
  
  a <- sum(error_indicators, na.rm = TRUE) + hyperparameters$prob_error_shape1
  b <- sum(!error_indicators, na.rm = TRUE) + hyperparameters$prob_error_shape2
  
  expect_identical(beta_pars, list(shape1 = a, shape2 = b))
})

test_that("update_prob_error updates correctly", {
  
  model <- toy_model()$model
  
  parameters <- model$parameters
  pars <- numeric(length(parameters))
  pars[parameters == "prob_error"] <- 0.1
  pars[grepl("delay", parameters)] <- 1
  
  rng <- monty::monty_rng_create(1L)
  
  augmented_data <- model$augmented_data_update(pars, rng)
  attr(pars, "data") <- augmented_data$data
  state_chain <- list(pars = pars,
                      density = augmented_data$density)
  
  new_state_chain <- update_prob_error(state_chain, model, rng)
  
  ## check the correct parameter is updated
  i <- which(parameters == "prob_error")
  expect_true(new_state_chain$pars[i] != state_chain$pars[i])
  expect_equal(new_state_chain$pars[-i], state_chain$pars[-i])
  
  
  ## edge cases
  ## No non-errors (FALSE), shape2 = 0 must result in 1
  augmented_data <- model$data_packer$unpack(attr(state_chain$pars, "data"))
  augmented_data$error_indicators[, ] <- TRUE
  attr(state_chain$pars, "data") <- model$data_packer$pack(augmented_data)
  model$hyperparameters <- list(prob_error_shape1 = 2,
                                prob_error_shape2 = 0)
  new_state_chain <- update_prob_error(state_chain, model, rng)
  expect_equal(new_state_chain$pars[i], 1)
  
  ## No errors (TRUE), shape1 = 0 must result in 0
  augmented_data <- model$data_packer$unpack(attr(state_chain$pars, "data"))
  augmented_data$error_indicators[, ] <- FALSE
  attr(state_chain$pars, "data") <- model$data_packer$pack(augmented_data)
  model$hyperparameters <- list(prob_error_shape1 = 0,
                                prob_error_shape2 = 3)
  new_state_chain <- update_prob_error(state_chain, model, rng)
  expect_equal(new_state_chain$pars[i], 0)
})


test_that("update gamma mean works correctly", {
  
  toy <- toy_model()
  
  model <- toy$model
  
  estimated_dates <- toy$data$true_data
  estimated_dates$id <- NULL
  estimated_dates$group <- NULL
  
  delay_values <- estimated_dates$report - estimated_dates$onset
  
  prior_shape <- 2
  prior_rate <- 0.5
  
  hyperparameters <- 
    chronofix_hyperparameters(gamma_mean_prior_shape = prior_shape,
                              gamma_mean_prior_rate = prior_rate)
  
  shape <- 3
  
  sample_pars <- 
    update_gamma_mean_parameters(shape, delay_values, hyperparameters)
  
  cmp_shape <- shape * length(delay_values) + prior_shape
  expect_equal(sample_pars$shape, cmp_shape)
  cmp_rate <- shape * sum(delay_values) + prior_rate
  expect_equal(sample_pars$rate, cmp_rate)
  
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  mean <- update_gamma_mean(shape, delay_values, hyperparameters, rng)
  cmp_mean <- 1 / monty::monty_random_gamma_rate(sample_pars$shape, 
                                                 sample_pars$rate, rng1)
  expect_equal(mean, cmp_mean)
})


test_that("update gamma shape works correctly", {
  
  set.seed(1)
  
  toy <- toy_model()
  
  model <- toy$model
  
  estimated_dates <- toy$data$true_data
  estimated_dates$id <- NULL
  estimated_dates$group <- NULL
  
  delay_values <- estimated_dates$report - estimated_dates$onset
  
  prior_shape <- 2
  prior_rate <- 0.5
  
  hyperparameters <- 
    chronofix_hyperparameters(gamma_shape_prior_shape = prior_shape,
                              gamma_shape_prior_rate = prior_rate)
  
  mean <- 10
  
  sample_pars <- 
    update_gamma_shape_parameters(mean, delay_values, hyperparameters)
  
  expect_equal(sample_pars$shape, 22.57924)
  expect_equal(sample_pars$rate, 1.9647787)
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  shape <- 3
  
  shape_new <- update_gamma_shape(shape, mean, delay_values,
                                  hyperparameters, rng)
  
  cmp_shape_new <- 
    monty::monty_random_gamma_rate(sample_pars$shape, sample_pars$rate, rng1)
  accept_prob <-
    calc_gamma_shape_accept_prob(cmp_shape_new, shape, mean, sample_pars,
                                 delay_values, hyperparameters)
  expect_gt(accept_prob, 0)
  expect_equal(shape_new, cmp_shape_new)
  
  log_like_ratio <- 
    sum(log_density_delay(delay_values, list(shape = shape_new, mean = mean), 
                          "gamma")) -
    sum(log_density_delay(delay_values, list(shape = shape, mean = mean), 
                          "gamma"))
  log_prior_ratio <- 
    dgamma(shape_new, prior_shape, rate = prior_rate, log = TRUE) -
    dgamma(shape, prior_shape, rate = prior_rate, log = TRUE)
  
  log_prop_ratio <- 
    dgamma(shape_new, sample_pars$shape, rate = sample_pars$rate, log = TRUE) -
    dgamma(shape, sample_pars$shape, rate = sample_pars$rate, log = TRUE)
  
  cmp_accept_prob <- log_like_ratio + log_prior_ratio - log_prop_ratio
  expect_equal(accept_prob, cmp_accept_prob)
})
