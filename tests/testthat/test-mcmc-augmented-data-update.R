test_that("sampling order calculated correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  ## Expected behaviour: the result should feature the indexes in the
  ## first argument to_resample, we resample non-errors (FALSE) first and then
  ## errors or missing (TRUE/NA) - order within these is determined by
  ## the order in to_resample and whether or not errors or missing dates have
  ## delay-connected dates to use for sampling
  expect_equal(
    calc_batch_sampling_order(c(1, 3), c(FALSE, NA, TRUE, NA, NA),
                              model_info$is_date_connected[, , 1]),
    c(1, 3))
  expect_equal(
    calc_batch_sampling_order(c(1, 3), c(TRUE, NA, FALSE, NA, NA),
                              model_info$is_date_connected[, , 1]),
    c(3, 1))
  expect_equal(
    calc_batch_sampling_order(c(1, 4, 3), c(TRUE, NA, FALSE, FALSE, NA),
                              model_info$is_date_connected[, , 2]),
    c(4, 3, 1))
  expect_equal(
    calc_batch_sampling_order(c(1, 4, 3), c(FALSE, NA, TRUE, TRUE, NA),
                              model_info$is_date_connected[, , 2]),
    c(1, 4, 3))
  expect_equal(
    calc_batch_sampling_order(c(1, 4, 3), c(TRUE, NA, FALSE, NA, NA),
                              model_info$is_date_connected[, , 2]), 
    c(3, 1, 4))
  expect_equal(
    calc_batch_sampling_order(c(5, 3, 2, 1), c(TRUE, NA, FALSE, NA, TRUE),
                              model_info$is_date_connected[, , 3]), 
    c(3, 1, 2, 5))
  expect_equal(
    calc_batch_sampling_order(c(5, 3, 2, 1), c(FALSE, NA, TRUE, NA, FALSE),
                              model_info$is_date_connected[, , 3]),
    c(5, 1, 3, 2))
  expect_equal(
    calc_batch_sampling_order(c(5, 3, 2, 1), c(TRUE, FALSE, FALSE, NA, NA),
                              model_info$is_date_connected[, , 3]), 
    c(3, 2, 5, 1))
})


test_that("cascade resampling order calculated correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  # group 2: onset(1) to report(3) and onset(1) to death(4)
  # 3 and 4 NOT directly connected
  
  event_order <- model_info$event_order[[2]]
  is_date_connected <- model_info$is_date_connected[, , 2]
  
  ## anchor = 3
  ## all correct, no cascade
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(FALSE, NA, FALSE, FALSE, NA),
                                is_date_connected),
    3)
  
  ## anchor = 3
  ## 3 (correct) only connected to a correct date, no cascade
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(FALSE, NA, FALSE, NA, NA),
                                is_date_connected),
    3)
  
  ## anchor = 3
  ## 3 (correct) --> 1 (erroneous) --> 4 (missing)
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(TRUE, NA, FALSE, NA, NA),
                                is_date_connected),
    c(3, 1, 4))
  
  ## same as above but 1 is missing instead (makes no difference)
  ## anchor = 3
  ## 3 (correct) --> 1 (missing) --> 4 (missing)
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(NA, NA, FALSE, NA, NA),
                                is_date_connected),
    c(3, 1, 4))
  
  ## anchor = 3
  ## 3 (correct) --> 1 (erroneous)
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(TRUE, NA, FALSE, FALSE, NA),
                                is_date_connected),
    c(3, 1))
  
  ## anchor = 3
  ## 3 (erroneous) cannot cascade as not connected to a correct date
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(NA, NA, TRUE, NA, NA),
                                is_date_connected),
    3)
  
  ## same as above but 1 is correct (makes no difference)
  ## anchor = 3
  ## 3 (erroneous) cannot cascade
  expect_equal(
    calc_cascade_sampling_order(3, event_order, c(FALSE, NA, TRUE, NA, NA),
                                is_date_connected),
    3)
  
  
  ## anchor = 1
  ## 1 (correct) --> 3 (erroneous) and 1 --> 4 (missing)
  ## event order determines order in which we do those
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, NA, TRUE, NA, NA),
                                is_date_connected),
    c(1, 3, 4))
  
  ## anchor = 1
  ## 1 (correct) --> 3 (erroneous)
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, NA, TRUE, FALSE, NA),
                                is_date_connected),
    c(1, 3))
  
  ## anchor = 1
  ## 1 (erroneous), no cascade as all connected dates corret
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, FALSE, FALSE, NA),
                                is_date_connected),
    1)
  
  ## anchor = 1
  ## 1 (erroneous) cannot cascade as not connected to correct date
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, TRUE, NA, NA),
                                is_date_connected),
    1)
  
  ## anchor = 1
  ## 1 (erroneous) --> 4 (erroneous)
  ## can cascade as 1 is connected to correct date 3
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, FALSE, NA, NA),
                                is_date_connected),
    c(1, 4))
  
  
  # group 4: onset(1) to report(3) and onset(1) to hospitalisation (2) then
  #          hospitalisation (2) to death (4)
  
  event_order <- model_info$event_order[[4]]
  is_date_connected <- model_info$is_date_connected[, , 4]
  
  ## anchor = 1
  ## all correct, no cascade
  expect_equal(
    calc_cascade_sampling_order(1, event_order, 
                                c(FALSE, FALSE, FALSE, FALSE, NA),
                                is_date_connected),
    1)
  
  ## anchor = 1
  ## 1 (correct) only connected to correct dates, no cascade
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, FALSE, FALSE, NA, NA),
                                is_date_connected),
    1)
  
  ## anchor = 1
  ## 1 (correct) --> 2 (erroneous) and 1 --> 3 (missing)
  ## then 2 --> 4 (missing)
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, TRUE, NA, NA, NA),
                                is_date_connected),
    c(1, 2, 3, 4))
  
  ## anchor = 1
  ## 1 (correct) --> 2 (erroneous) --> 4 (missing)
  ## no cascade to 3 (correct) 
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, TRUE, FALSE, NA, NA),
                                is_date_connected),
    c(1, 2, 4))
  
  ## anchor = 1
  ## 1 (correct) --> 3 (erroneous)
  ## no cascade to 2 (correct) 
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(FALSE, FALSE, TRUE, NA, NA),
                                is_date_connected),
    c(1, 3))
  
  ## anchor = 1
  ## cannot cascade as 1 (erroneous) not connected to a correct date
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, TRUE, FALSE, NA),
                                is_date_connected),
    1)
  
  ## anchor = 1
  ## 1 (erroneous) --> 2 (erroneous) --> 4 (missing)
  ## can cascade as 1 (erroneous) connected to correct date 3
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, NA, FALSE, TRUE, NA),
                                is_date_connected),
    c(1, 2, 4))
  
  ## anchor = 1
  ## 1 (erroneous) --> 3 (missing)
  ## can cascade as 1 (erroneous) connected to correct date 2
  expect_equal(
    calc_cascade_sampling_order(1, event_order, c(TRUE, FALSE, NA, TRUE, NA),
                                is_date_connected),
    c(1, 3))
  
  ## anchor = 2
  ## 2 (correct) --> 1 (erroneous) and 2 (correct) --> 4 (missing)
  ## then 1 (erroneous) --> 3 (missing)
  ## can cascade as 1 (erroneous) connected to correct date 2
  expect_equal(
    calc_cascade_sampling_order(2, event_order, c(TRUE, FALSE, NA, NA, NA),
                                is_date_connected),
    c(2, 1, 4, 3))
  
  
  ## anchor = 2
  ## 2 (erroneous) --> 1 (erroneous) --> 3 (missing)
  ## can cascade as 2 (erroneous) connected to correct date 4
  expect_equal(
    calc_cascade_sampling_order(2, event_order, c(TRUE, TRUE, NA, FALSE, NA),
                                is_date_connected),
    c(2, 1, 3))
  
  ## anchor = 2
  ## 2 (erroneous) --> 4 (erroneous)
  ## can cascade as 2 (erroneous) connected to correct date 1
  expect_equal(
    calc_cascade_sampling_order(2, event_order, c(FALSE, TRUE, NA, TRUE, NA),
                                is_date_connected),
    c(2, 4))
  
})


test_that("updating estimated dates skipped correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1) 
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  control <- mcmc_control(prob_update_estimated_dates = 1)
  
  ## group = 2, i = 2 - no hospitalisation for this group so no update
  group <- 2
  i <- 2
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    update_estimated_dates1(i, augmented_data, observed_dates, group,
                            prob_error, model_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## group = 4, i = 5 - no discharge for this group so no update
  group <- 4
  i <- 5
  observed_dates <- c(10, 5, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  augmented_data_new <- 
    update_estimated_dates1(i, augmented_data, observed_dates, group,
                            prob_error, model_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## Try to update i = 4 for group 4, but with prob_update_estimated_dates = 0
  ## there should be no update, and a single random draw
  i <- 4
  control <- mcmc_control(prob_update_estimated_dates = 0)
  augmented_data_new <- 
    update_estimated_dates1(i, augmented_data, observed_dates, group,
                            prob_error, model_info, date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
})


test_that("updating error indicators skipped correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  control <- mcmc_control(prob_update_error_indicators = 1)
  
  ## group = 2, i = 2 - missing date so no update
  group <- 2
  i <- 2
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    update_error_indicators1(i, augmented_data, observed_dates, group,
                             prob_error, model_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## group = 4, i = 4 - missing date so no update
  group <- 4
  i <- 4
  observed_dates <- c(10, 5, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  augmented_data_new <- 
    update_error_indicators1(i, augmented_data, observed_dates, group,
                             prob_error, model_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## Try to update i = 3 for group 4, but with prob_update_error_indicators = 0
  ## there should be no update, and a single random draw
  i <- 3
  control <- mcmc_control(prob_update_error_indicators = 0)
  augmented_data_new <- 
    update_error_indicators1(i, augmented_data, observed_dates, group,
                             prob_error, model_info, date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
})


test_that("swap error indicators skipped correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  control <- mcmc_control(prob_error_swap = 1)
  
  ## group = 2, both dates FALSE so no update
  group <- 2
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 68.1, NA),
                         error_indicators = c(NA, NA, FALSE, FALSE, NA))
  augmented_data_new <- 
    swap_error_indicators(augmented_data, observed_dates, group,
                          prob_error, model_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## group = 4, no TRUE (only FALSE or missing) so no update
  group <- 4
  i <- 4
  observed_dates <- c(10, 15, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, FALSE, FALSE, NA, NA))
  augmented_data_new <- 
    swap_error_indicators(augmented_data, observed_dates, group,
                          prob_error, model_info, date_range, control, rng)
  ## augmented_data and rng should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  
  ## Try to update individual with mixed errors, but with
  ## prob_swap_error_indicators = 0 there should be no update, and a single
  ## random draw
  augmented_data <- list(estimated_dates = c(10.3, 20.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  expect_true(has_mixed_errors(augmented_data$error_indicators))
  control <- mcmc_control(prob_error_swap = 0)
  augmented_data_new <- 
    swap_error_indicators(augmented_data, observed_dates, group,
                          prob_error, model_info, date_range, control, rng)
  ## augmented_data should be unchanged
  expect_equal(augmented_data, augmented_data_new)
  ## rng should have gone through a single random_real draw so let's do the same
  ## to rng1 and check they match
  x <- monty::monty_random_real(rng1)
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
})


test_that("estimated dates proposed correctly", {
  
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  model_info$delay_mean <- c(5, 8, 3, 4, 7, 10)
  model_info$delay_cv <- c(0.5, 0.3, 0.2, 0.7, 0.6, 0.9)
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  ## group 2, propose new (correct) report date
  group <- 2
  to_update <- 3
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    propose_estimated_dates(to_update, augmented_data, observed_dates, group,
                            model_info, rng)
  expect_equal(augmented_data$error_indicators,
               augmented_data_new$error_indicators)
  expect_equal(augmented_data$estimated_dates[-to_update],
               augmented_data_new$estimated_dates[-to_update])
  expect_equal(augmented_data_new$estimated_dates[to_update],
               observed_dates[to_update] + monty::monty_random_real(rng1))
  
  
  ## group 2, propose new (error) death date
  ## based on delay 2, onset (date 1) to death (date 4)
  group <- 2
  to_update <- 4
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  augmented_data_new <- 
    propose_estimated_dates(to_update, augmented_data, observed_dates, group,
                            model_info, rng)
  expect_equal(augmented_data$error_indicators,
               augmented_data_new$error_indicators)
  expect_equal(augmented_data$estimated_dates[-to_update],
               augmented_data_new$estimated_dates[-to_update])
  expect_equal(augmented_data_new$estimated_dates[to_update],
               sample_from_delay(to_update, augmented_data_new,
                                 group, model_info, rng1))
  
  
  ## group 2, propose all dates, swapping errors
  group <- 2
  
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, TRUE, FALSE, NA))
  sampling_order <-
    calc_batch_sampling_order(model_info$event_order[[group]],
                              augmented_data$error_indicators,
                              model_info$is_date_connected[, , group])
  augmented_data_new <- 
    propose_estimated_dates(sampling_order, augmented_data, observed_dates,
                            group, model_info, rng)
  expect_equal(augmented_data_new$error_indicators, 
               augmented_data$error_indicators)
  cmp <- list(estimated_dates = rep(NA, 5),
              error_indicators = augmented_data$error_indicators)
  ## date 4 will be sampled based on observed date
  cmp$estimated_dates[4] <- observed_dates[4] + monty::monty_random_real(rng1)
  ## will then sample date 1 (connected to date 4) and then date 3
  cmp$estimated_dates[1] <- 
    sample_from_delay(1, cmp, group, model_info, rng1)
  cmp$estimated_dates[3] <- 
    sample_from_delay(3, cmp, group, model_info, rng1)
  expect_equal(augmented_data_new$estimated_dates, cmp$estimated_dates)
  
})


test_that("proposal density calculated correctly", {
  
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  model_info$delay_mean <- c(5, 8, 3, 4, 7, 10)
  model_info$delay_cv <- c(0.5, 0.3, 0.2, 0.7, 0.6, 0.9)
  
  params <- 
    mapply(function(x, y, z) unlist(convert_to_distribution_params(x, y, z)),
           model_info$delay_mean, model_info$delay_cv,
           model_info$delay_distribution)
  
  # group 2, 3 dates
  group <- 2
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  ## group 2, updated correct report date
  ## proposal log-density should be zero
  updated <- 3
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), 0)
  
  ## group 2, updated error death date
  ## proposal based on delay 2 (gamma) onset (date 1) to death (date 4)
  updated <- 4
  d <- dgamma(augmented_data$estimated_dates[4] - 
                augmented_data$estimated_dates[1],
              shape = params[1, 2], rate = params[2, 2], log = TRUE)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  ## group 2, updated missing onset date 
  ## based on delay 1 (gamma), onset (date 1) to report (date 3)
  ## but not delay 2 (gamma), onset (date 1) to death (date 4)
  ## because date 3 is correct and date 4 is not
  updated <- 1
  d <- dgamma(augmented_data$estimated_dates[3] - 
                augmented_data$estimated_dates[1],
              shape = params[1, 1], rate = params[2, 1], log = TRUE)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  ## group 2, updated all dates
  ## report (date 3) is correct so has no impact for proposing this
  ## then onset is proposed based on delay 1 (gamma), 
  ##               onset (date 1) to report (date 3)
  ## then death is proposed based on delay 2 (gamma),
  ##               onset (date 1) to death (date 4)
  sampling_order <-
    calc_batch_sampling_order(model_info$event_order[[group]],
                              augmented_data$error_indicators,
                              model_info$is_date_connected[, , group])
  d <- sum(dgamma(augmented_data$estimated_dates[c(3, 4)] - 
                    augmented_data$estimated_dates[1],
                  shape = params[1, c(1, 2)], rate = params[2, c(1, 2)],
                  log = TRUE))
  expect_equal(calc_proposal_density(
    sampling_order, augmented_data, group, model_info), d)
  
  ## group 2, updated missing onset date 
  ## based on delay 1 (gamma), onset (date 1) to report (date 3)
  ## and delay 2 (gamma), onset (date 1) to death (date 4)
  ## the two delays are equally likely to be used as dates 3 and 4 both correct
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, FALSE, NA))
  updated <- 1
  d <- log(sum(dgamma(augmented_data$estimated_dates[c(3, 4)] - 
                        augmented_data$estimated_dates[1],
                      shape = params[1, c(1, 2)], 
                      rate = params[2, c(1, 2)]))) - log(2)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  ## group 2, updated missing onset date 
  ## based on delay 1 (gamma), onset (date 1) to report (date 3)
  ## and delay 2 (gamma), onset (date 1) to death (date 4)
  ## the two delays are equally likely to be used as dates 3 and 4 both error
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, FALSE, NA))
  updated <- 1
  d <- log(sum(dgamma(augmented_data$estimated_dates[c(3, 4)] - 
                        augmented_data$estimated_dates[1],
                      shape = params[1, c(1, 2)], 
                      rate = params[2, c(1, 2)]))) - log(2)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  
  # group 4, 3 dates
  group <- 4
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  
  ## group 4, updated correct onset date
  ## proposal log-density should be zero
  updated <- 1
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), 0)
  
  ## group 4, updated correct report date
  ## proposal log-density should be zero
  updated <- 3
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), 0)
  
  ## group 4, updated error hospitalisation date 
  ## based on delay 5 (log-normal), onset (date 1) to hospitalisation (date 2)
  ## but not delay 6 (log-normal), hospitalisation (date 2) to death (date 4)
  ## because date 1 is correct and date 4 is missing
  updated <- 2
  d <- dlnorm(augmented_data$estimated_dates[2] - 
                augmented_data$estimated_dates[1],
              meanlog = params[1, 5], sdlog = params[2, 5], log = TRUE)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  ## group 4, updated missing death date 
  ## based on delay 6 (log-normal), hospitalisation (date 2) to death (date 4)
  updated <- 4
  d <- dlnorm(augmented_data$estimated_dates[4] - 
                augmented_data$estimated_dates[2],
              meanlog = params[1, 6], sdlog = params[2, 6], log = TRUE)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  ## group 4, updated all dates
  ## onset (date 1) and report (date 3) correct so no impact for proposing
  ## then hospitalisation is proposed based on delay 5, onset (date 1) to
  ##    hospitalisation (date 2)
  ## then death is proposed based on delay 6 (log-normal), hospitalisation
  ##    (date 2) to death (date 4)
  updated <- c(1, 2, 3, 4)
  d <- sum(dlnorm(augmented_data$estimated_dates[c(2, 4)] - 
                    augmented_data$estimated_dates[c(1, 2)],
                  meanlog = params[1, c(5, 6)], sdlog = params[2, c(5, 6)],
                  log = TRUE))
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  
  ## group 4, updated error hospitalisation date 
  ## based on delay 5 (log-normal), onset (date 1) to hospitalisation (date 2)
  ## and delay 6 (log-normal), hospitalisation (date 2) to death (date 4)
  ## the two delays are equally likely to be used
  ## because both date 1 and 4 are correct
  updated <- 2
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, FALSE, NA))
  d <- log(sum(dlnorm(augmented_data$estimated_dates[c(2, 4)] - 
                        augmented_data$estimated_dates[c(1, 2)],
                      meanlog = params[1, c(5, 6)], 
                      sdlog = params[2, c(5, 6)]))) - log(2)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  
  ## group 4, updated error hospitalisation date 
  ## based on delay 5 (log-normal), onset (date 1) to hospitalisation (date 2)
  ## and delay 6 (log-normal), hospitalisation (date 2) to death (date 4)
  ## the two delays are equally likely to be used
  ## because neither date 1 nor 4 are correct
  updated <- 2
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(TRUE, TRUE, FALSE, NA, NA))
  d <- log(sum(dlnorm(augmented_data$estimated_dates[c(2, 4)] - 
                        augmented_data$estimated_dates[c(1, 2)],
                      meanlog = params[1, c(5, 6)], 
                      sdlog = params[2, c(5, 6)]))) - log(2)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
})


test_that("acceptance probability calculated correctly", {
  
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  model_info$delay_mean <- c(5, 8, 3, 4, 7, 10)
  model_info$delay_cv <- c(0.5, 0.3, 0.2, 0.7, 0.6, 0.9)
  prob_error <- 0.05
  
  date_range <- c(0, 101)
  
  ## separate function for calculating acceptance probability
  calc_accept <- function(sampling_order, sampling_order_reverse,
                          augmented_data_new, augmented_data, group) {
    ll_delays_current <- 
      datefixer_log_likelihood_delays1(augmented_data$estimated_dates,
                                       model_info$delay_mean, 
                                       model_info$delay_cv,
                                       model_info$delay_from, 
                                       model_info$delay_to,
                                       model_info$delay_distribution,
                                       model_info$is_delay_in_group[, group])
    ll_delays_new <- 
      datefixer_log_likelihood_delays1(augmented_data_new$estimated_dates,
                                       model_info$delay_mean, 
                                       model_info$delay_cv,
                                       model_info$delay_from, 
                                       model_info$delay_to,
                                       model_info$delay_distribution,
                                       model_info$is_delay_in_group[, group])
    
    ll_errors_current <-
      datefixer_log_likelihood_errors(prob_error, 
                                      augmented_data$error_indicators,
                                      date_range)
    ll_errors_new <-
      datefixer_log_likelihood_errors(prob_error, 
                                      augmented_data_new$error_indicators,
                                      date_range)
    
    ratio_ll_delays <- sum(ll_delays_new) - sum(ll_delays_current)
    ratio_ll_errors <- ll_errors_new - ll_errors_current
    ratio_post <- ratio_ll_delays + ratio_ll_errors
    
    prop_current <- 
      calc_proposal_density(sampling_order_reverse, augmented_data,
                            group, model_info)
    prop_new <- 
      calc_proposal_density(sampling_order, augmented_data_new,
                            group, model_info)
    ratio_prop <- prop_current - prop_new
    
    ratio_post + ratio_prop
  }
  
  # group 2
  group <- 2
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  ## updating error death (date 4) but matching observed date so auto-reject
  sampling_order <- 4
  sampling_order_reverse <- 4
  augmented_data_new <- list(estimated_dates = c(20.5, NA, 40.2, 68.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    -Inf)
  
  ## updating death (date 4) but outside date range so auto-reject
  sampling_order <- 4
  sampling_order_reverse <- 4
  augmented_data_new <- list(estimated_dates = c(20.5, NA, 40.2, 150.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    -Inf)
  
  
  ## updating onset (date 1) but outside date range so auto-reject
  sampling_order <- 1
  sampling_order_reverse <- 1
  augmented_data_new <- list(estimated_dates = c(-20.5, NA, 40.2, 50.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    -Inf)
  
  ## updating onset (date 1) but negative delay resulting so auto-reject
  sampling_order <- 1
  sampling_order_reverse <- 1
  augmented_data_new <- list(estimated_dates = c(43.5, NA, 40.2, 50.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    -Inf)
  
  ## updating onset (date 1)
  sampling_order <- 1
  sampling_order_reverse <- 1
  augmented_data_new <- list(estimated_dates = c(10.5, NA, 40.2, 50.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    calc_accept(sampling_order, sampling_order_reverse,
                augmented_data_new, augmented_data, group))
  
  
  ## updating death (date 4), switching to correct
  sampling_order <- 4
  sampling_order_reverse <- 4
  augmented_data_new <- list(estimated_dates = c(20.5, NA, 40.2, 68.1, NA),
                             error_indicators = c(NA, NA, FALSE, FALSE, NA))
  expect_equal(
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    calc_accept(sampling_order, sampling_order_reverse,
                augmented_data_new, augmented_data, group))
  
  ## updating all dates, swapping errors
  updated <- c(1, 3, 4)
  augmented_data_new <- list(estimated_dates = c(10.5, NA, 50.2, 68.1, NA),
                             error_indicators = c(NA, NA, TRUE, FALSE, NA))
  sampling_order <-
    calc_batch_sampling_order(model_info$event_order[[group]],
                              augmented_data_new$error_indicators,
                              model_info$is_date_connected[, , group])
  sampling_order_reverse <-
    calc_batch_sampling_order(model_info$event_order[[group]],
                              augmented_data$error_indicators,
                              model_info$is_date_connected[, , group])
  expect_equal(
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    calc_accept(sampling_order, sampling_order_reverse,
                augmented_data_new, augmented_data, group))
  
  
  ## group 4
  group <- 4
  observed_dates <- c(10, 5, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  
  ## updating all dates, swapping errors
  augmented_data_new <- list(estimated_dates = c(2.3, 5.2, 33.2, 40.1, NA),
                             error_indicators = c(TRUE, FALSE, TRUE, NA, NA))
  sampling_order <-
    calc_batch_sampling_order(model_info$event_order[[group]],
                              augmented_data_new$error_indicators,
                              model_info$is_date_connected[, , group])
  sampling_order_reverse <-
    calc_batch_sampling_order(model_info$event_order[[group]],
                              augmented_data$error_indicators,
                              model_info$is_date_connected[, , group])
  expect_equal(
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    calc_accept(sampling_order, sampling_order_reverse,
                augmented_data_new, augmented_data, group))
})

test_that("has_mixed_errors returns TRUE only when both TRUE and FALSE 
          present", {
  # Only non-errors
  expect_false(has_mixed_errors(c(FALSE, FALSE)))
  expect_false(has_mixed_errors(c(FALSE, FALSE, FALSE)))
  
  # Only errors
  expect_false(has_mixed_errors(c(TRUE, TRUE)))
  
  # Only missing
  expect_false(has_mixed_errors(c(NA, NA)))
  
  # Non-errors and missing (no TRUE)
  expect_false(has_mixed_errors(c(FALSE, NA)))
  expect_false(has_mixed_errors(c(FALSE, FALSE, NA)))
  
  # Errors and missing (no FALSE)
  expect_false(has_mixed_errors(c(TRUE, NA)))
  expect_false(has_mixed_errors(c(TRUE, TRUE, NA)))
  
  # Mixed: both FALSE and TRUE present (NA irrelevant)
  expect_true(has_mixed_errors(c(FALSE, TRUE)))
  expect_true(has_mixed_errors(c(FALSE, TRUE, NA)))
  expect_true(has_mixed_errors(c(TRUE, FALSE, FALSE)))
  expect_true(has_mixed_errors(c(NA, FALSE, TRUE, NA)))
})

