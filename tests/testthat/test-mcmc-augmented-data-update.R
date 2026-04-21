test_that("batch resampling order calculated correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  ## Expected behaviour: the result should feature the indexes in the
  ## first argument to_resample, we resample non-errors (FALSE) first and then
  ## errors or missing (TRUE/NA) - order within these is determined by
  ## the order in to_resample and whether or not errors or missing dates have
  ## delay-connected dates to use for sampling
  expect_equal(
    calc_batch_resampling_order(c(1, 3), c(FALSE, NA, TRUE, NA, NA),
                               model_info$is_date_connected[, , 1]), c(1, 3))
  expect_equal(
    calc_batch_resampling_order(c(1, 3), c(TRUE, NA, FALSE, NA, NA),
                          model_info$is_date_connected[, , 1]), c(3, 1))
  expect_equal(
    calc_batch_resampling_order(c(1, 4, 3), c(TRUE, NA, FALSE, FALSE, NA),
                          model_info$is_date_connected[, , 2]), c(4, 3, 1))
  expect_equal(
    calc_batch_resampling_order(c(1, 4, 3), c(FALSE, NA, TRUE, TRUE, NA),
                          model_info$is_date_connected[, , 2]), c(1, 4, 3))
  expect_equal(
    calc_batch_resampling_order(c(1, 4, 3), c(TRUE, NA, FALSE, NA, NA),
                          model_info$is_date_connected[, , 2]), c(3, 1, 4))
  expect_equal(
    calc_batch_resampling_order(c(5, 3, 2, 1), c(TRUE, NA, FALSE, NA, TRUE),
                          model_info$is_date_connected[, , 3]), c(3, 1, 2, 5))
  expect_equal(
    calc_batch_resampling_order(c(5, 3, 2, 1), c(FALSE, NA, TRUE, NA, FALSE),
                          model_info$is_date_connected[, , 3]), c(5, 1, 3, 2))
  expect_equal(
    calc_batch_resampling_order(c(5, 3, 2, 1), c(TRUE, FALSE, FALSE, NA, NA),
                          model_info$is_date_connected[, , 3]), c(3, 2, 5, 1))
})

test_that("cascade resampling order calculated correctly", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  # group 2: onset(1) to report(3) and onset(1) to death(4)
  # 3 and 4 NOT directly connected
  
  ## anchor = 3
  ## 3 --> 1 --> 4
  expect_equal(
    calc_cascade_resampling_order(c(3, 1, 4),
                                  model_info$is_date_connected[, , 2]),
    c(3, 1, 4))
  
  ## same dates, different input order
  expect_equal(
    calc_cascade_resampling_order(c(3, 4, 1),
                                  model_info$is_date_connected[, , 2]),
    c(3, 1, 4))
  
  ## anchor = 4
  ## 4 --> 1 --> 3
  expect_equal(
    calc_cascade_resampling_order(c(4, 3, 1),
                                  model_info$is_date_connected[, , 2]),
    c(4, 1, 3))
  
  ## anchor = 1
  ## both 3 and 4 connected to 1, currently input order breaks the tie
  expect_equal(
    calc_cascade_resampling_order(c(1, 3, 4),
                                  model_info$is_date_connected[, , 2]),
    c(1, 3, 4))
  expect_equal(
    calc_cascade_resampling_order(c(1, 4, 3),
                                  model_info$is_date_connected[, , 2]),
    c(1, 4, 3))
  
  # group 4: onset(1) to hosp(2) and hosp(2) to death(4)
  # 1 and 4 NOT directly connected
  
  ## anchor = 1
  ## 1 --> 2 --> 4
  expect_equal(
    calc_cascade_resampling_order(c(1, 4, 2),
                                  model_info$is_date_connected[, , 4]),
    c(1, 2, 4))
  
  ## anchor = 4
  ## 4 --> 2 --> 1
  expect_equal(
    calc_cascade_resampling_order(c(4, 1, 2),
                                  model_info$is_date_connected[, , 4]),
    c(4, 2, 1))
  
  ## anchor = 2
  ## both 1 and 4 connected to 2, currently input order breaks the tie
  expect_equal(
    calc_cascade_resampling_order(c(2, 4, 1),
                                  model_info$is_date_connected[, , 4]),
    c(2, 4, 1))
  expect_equal(
    calc_cascade_resampling_order(c(2, 1, 4),
                                  model_info$is_date_connected[, , 4]),
    c(2, 1, 4))
})

test_that("updating error indicator cascades to connected dates", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  model_info$delay_mean <- c(5, 8, 3, 4, 7, 10)
  model_info$delay_cv <- c(0.5, 0.3, 0.2, 0.7, 0.6, 0.9)
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  control <- mcmc_control(prob_update_error_indicators = 1)
  
  ## group 2, flip correct report (date 3) to error (FALSE --> TRUE)
  ## onset (date 1) is missing and connected --> resampled
  ## death (date 4) is error and connected via date 1 --> resampled
  group <- 2
  i <- 3
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  rng  <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  augmented_data_new <-
    update_error_indicators1(i, augmented_data, observed_dates, group,
                             prob_error, model_info, date_range, control, rng)
  
  x <- monty::monty_random_real(rng1)
  proposed <- propose_estimated_dates(i, augmented_data, observed_dates, group,
                                      model_info, rng1, update_errors = TRUE)
  augmented_data_proposed <- proposed$augmented_data
  updated_dates <- proposed$updated
  
  # check proposal
  expect_true(augmented_data_proposed$error_indicators[i])
  expect_true(augmented_data_proposed$estimated_dates[i] !=
                augmented_data$estimated_dates[i])
  expect_true(all(c(1, 4) %in% updated_dates))
  
  accept_prob <- calc_accept_prob(updated_dates, augmented_data_proposed,
                                  augmented_data, observed_dates, group,
                                  prob_error, model_info, date_range)
  accept <- log(monty::monty_random_real(rng1)) < accept_prob
  
  if (accept) {
    expect_equal(augmented_data_new, augmented_data_proposed)
    expect_true(augmented_data_new$error_indicators[3]) # error flipped
  } else {
    expect_equal(augmented_data_new, augmented_data)
    expect_false(augmented_data_new$error_indicators[3])
  }
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  ## group 2, flip error death (date 4) to correct (TRUE --> FALSE)
  ## onset (date 1) is missing and connected --> resampled
  group <- 2
  i <- 4
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates  = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  augmented_data_new <-
    update_error_indicators1(i, augmented_data, observed_dates, group,
                             prob_error, model_info, date_range, control, rng)
  
  x <- monty::monty_random_real(rng1)
  proposed <- propose_estimated_dates(i, augmented_data, observed_dates, group,
                                      model_info, rng1, update_errors = TRUE)
  augmented_data_proposed <- proposed$augmented_data
  updated_dates <- proposed$updated
  
  # check proposal
  expect_false(augmented_data_proposed$error_indicators[i])
  expect_equal(floor(augmented_data_proposed$estimated_dates[i]), observed_dates[i])
  expect_true(1 %in% updated_dates)
  
  accept_prob <- calc_accept_prob(updated_dates, augmented_data_proposed,
                                  augmented_data, observed_dates, group,
                                  prob_error, model_info, date_range)
  accept <- log(monty::monty_random_real(rng1)) < accept_prob
  
  if (accept) {
    expect_equal(augmented_data_new, augmented_data_proposed)
    expect_false(augmented_data_new$error_indicators[4]) # error flipped
  } else {
    expect_equal(augmented_data_new, augmented_data)
    expect_true(augmented_data_new$error_indicators[4])
  }
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  ## group 4, flip error hospitalisation (date 2) to correct (TRUE --> FALSE)
  ## death (date 4) is missing and connected --> resampled
  group <- 4
  i <- 2
  observed_dates <- c(10, 5, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  augmented_data_new <-
    update_error_indicators1(i, augmented_data, observed_dates, group,
                             prob_error, model_info, date_range, control, rng)
  
  x <- monty::monty_random_real(rng1)
  proposed <- propose_estimated_dates(i, augmented_data, observed_dates, group,
                                      model_info, rng1, update_errors = TRUE)
  augmented_data_proposed <- proposed$augmented_data
  updated_dates <- proposed$updated
  
  # check proposal
  expect_false(augmented_data_proposed$error_indicators[i])
  expect_equal(floor(augmented_data_proposed$estimated_dates[i]), observed_dates[i])
  expect_true(4 %in% updated_dates) # death date 4 is missing so should update
  expect_false(1 %in% updated_dates) # onset date 1 is correct (no update)
  
  accept_prob <- calc_accept_prob(updated_dates, augmented_data_proposed,
                                  augmented_data, observed_dates, group,
                                  prob_error, model_info, date_range)
  accept <- log(monty::monty_random_real(rng1)) < accept_prob
  
  if (accept) {
    expect_equal(augmented_data_new, augmented_data_proposed)
    expect_false(augmented_data_new$error_indicators[2])
  } else {
    expect_equal(augmented_data_new, augmented_data)
    expect_true(augmented_data_new$error_indicators[2])
  }
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
})

test_that("updating estimated date cascades to connected missing/erroneous dates", {
  delay_map <- toy_model()$delay_map
  dates <- c("onset", "hospitalisation", "report", "death", "discharge")
  model_info <- make_model_info(delay_map, dates)
  
  model_info$delay_mean <- c(5, 8, 3, 4, 7, 10)
  model_info$delay_cv <- c(0.5, 0.3, 0.2, 0.7, 0.6, 0.9)
  
  date_range <- c(0, 500)
  prob_error <- 0.05
  control <- mcmc_control(prob_update_estimated_dates = 1)
  
  ## group 2, update correct report (date 3)
  ## onset (date 1) is missing and connected to date 3 --> date 1 updated next
  ## after date 1 is updated --> can update incorrect date 4
  group <- 2
  i <- 3
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  augmented_data_new <- 
    update_estimated_dates1(i, augmented_data, observed_dates, group,
                            prob_error, model_info, date_range, control, rng)
  
  ## manually replicate expected behaviour with rng1
  x <- monty::monty_random_real(rng1)
  
  proposed <- propose_estimated_dates(i, augmented_data, observed_dates, group,
                                      model_info, rng1, FALSE)
  
  augmented_data_proposed <- proposed$augmented_data
  updated_dates <- proposed$updated
  
  accept_prob <- calc_accept_prob(updated_dates, augmented_data_proposed,
                                  augmented_data, observed_dates, group,
                                  prob_error, model_info, date_range)
  accept <- log(monty::monty_random_real(rng1)) < accept_prob
  
  if (accept) {
    expect_equal(augmented_data_new, augmented_data_proposed)
  } else {
    expect_equal(augmented_data_new, augmented_data)
  }
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  ## group 4, update correct onset (date 1)
  ## hosp (date 2) is erroneous and connected --> date 2 updated next
  ## date 4 connected and missing --> date 4 updated next
  group <- 4
  i <- 1
  observed_dates <- c(10, 5, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  augmented_data_new <- 
    update_estimated_dates1(i, augmented_data, observed_dates, group,
                            prob_error, model_info, date_range, control, rng)
  
  x <- monty::monty_random_real(rng1)
  proposed <- propose_estimated_dates(i, augmented_data, observed_dates, group,
                                      model_info, rng1, FALSE)
  
  augmented_data_proposed <- proposed$augmented_data
  updated_dates <- proposed$updated
  
  accept_prob <- calc_accept_prob(updated_dates, augmented_data_proposed,
                                  augmented_data, observed_dates, group,
                                  prob_error, model_info, date_range)
  
  accept <- log(monty::monty_random_real(rng1)) < accept_prob
  
  if (accept) {
    expect_equal(augmented_data_new, augmented_data_proposed)
  } else {
    expect_equal(augmented_data_new, augmented_data)
  }
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
  
  ## group 4, update correct report (date 3)
  ## no connected missing/erroneous dates so only date 3 to update
  group <- 4
  i <- 3
  observed_dates <- c(10, 5, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, FALSE, FALSE, NA, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  augmented_data_new <- 
    update_estimated_dates1(i, augmented_data, observed_dates, group,
                            prob_error, model_info, date_range, control, rng)
  
  x <- monty::monty_random_real(rng1)
  proposed <- propose_estimated_dates(i, augmented_data, observed_dates, group,
                                      model_info, rng1, FALSE)
  
  augmented_data_proposed <- proposed$augmented_data
  updated_dates <- proposed$updated
  
  accept_prob <- calc_accept_prob(updated_dates, augmented_data_proposed,
                                  augmented_data, observed_dates, group,
                                  prob_error, model_info, date_range)
  accept <- log(monty::monty_random_real(rng1)) < accept_prob
  
  if (accept) {
    expect_equal(augmented_data_new, augmented_data_proposed)
  } else {
    expect_equal(augmented_data_new, augmented_data)
  }
  expect_equal(monty::monty_rng_state(rng), monty::monty_rng_state(rng1))
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

test_that("has_mixed_errors returns TRUE only when both TRUE and FALSE present", {
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
  
  ## group 2, propose new (correct) report date (date 3)
  ## update date 1 (onset) using date 3
  ## update date 4 using date 1
  group <- 2
  i <- 3
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  proposed <- propose_estimated_dates(i, augmented_data, observed_dates, group,
                                      model_info, rng, FALSE)
  
  proposed_error_indicators <- proposed$augmented_data$error_indicators
  proposed_estimated_dates <- proposed$augmented_data$estimated_dates
  proposed_updates <- proposed$updated
  
  expect_equal(augmented_data$error_indicators,
               proposed_error_indicators)
  expect_equal(augmented_data$estimated_dates[-proposed_updates],
               proposed_estimated_dates[-proposed_updates])
  expect_equal(proposed_estimated_dates[3],
               observed_dates[3] + monty::monty_random_real(rng1))
  expect_equal(proposed_updates, c(3, 1, 4))
  
  ## group 2, propose new (error) death date (4)
  ## updated death (4) used to re-estimate missing onset (1)
  ## report (3) is connected to 1 but it is correct, so no update
  group <- 2
  i <- 4
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  proposed <- 
    propose_estimated_dates(i, augmented_data, observed_dates, group,
                            model_info, rng, FALSE)
  
  proposed_error_indicators <- proposed$augmented_data$error_indicators
  proposed_estimated_dates <- proposed$augmented_data$estimated_dates
  proposed_updates <- proposed$updated
  
  expect_equal(augmented_data$error_indicators,
               proposed_error_indicators)
  expect_equal(augmented_data$estimated_dates[-proposed_updates],
               proposed_estimated_dates[-proposed_updates])
  expect_equal(proposed_estimated_dates[4],
               sample_from_delay(4, augmented_data$estimated_dates,
                                 augmented_data$error_indicators, group,
                                 model_info, rng1))
  expect_equal(proposed_updates, c(4, 1))
  
  
  ## group 2, propose new (missing) onset date (1)
  ## report (3) is correct so not updated
  ## death (4) is error and is updated
  group <- 2
  i <- 1
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  proposed <- 
    propose_estimated_dates(i, augmented_data, observed_dates, group,
                            model_info, rng, FALSE)
  
  proposed_error_indicators <- proposed$augmented_data$error_indicators
  proposed_estimated_dates <- proposed$augmented_data$estimated_dates
  proposed_updates <- proposed$updated
  
  expect_equal(augmented_data$error_indicators,
               proposed_error_indicators)
  expect_equal(augmented_data$estimated_dates[-proposed_updates],
               proposed_estimated_dates[-proposed_updates])
  expect_equal(proposed_estimated_dates[1],
               sample_from_delay(1, proposed_estimated_dates,
                                 proposed_error_indicators,
                                 group, model_info, rng1))
  expect_equal(proposed_estimated_dates[4],
               sample_from_delay(4, proposed_estimated_dates,
                                 proposed_error_indicators,
                                 group, model_info, rng1))
  
  
  ## group 2, propose new report date (3), going from correct to error
  ## update missing onset date (1) then update erroneous death date (4)
  group <- 2
  i <- 3
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  proposed <- 
    propose_estimated_dates(i, augmented_data, observed_dates, group,
                            model_info, rng, update_errors = TRUE)
  
  proposed_error_indicators <- proposed$augmented_data$error_indicators
  proposed_estimated_dates <- proposed$augmented_data$estimated_dates
  proposed_updates <- proposed$updated
  
  expect_equal(augmented_data$error_indicators[-proposed_updates],
               proposed_error_indicators[-proposed_updates])
  expect_equal(proposed_updates, c(3, 1, 4))
  expect_equal(proposed_error_indicators, c(NA, NA, TRUE, TRUE, NA))
  expect_equal(augmented_data$estimated_dates[-proposed_updates],
               proposed_estimated_dates[-proposed_updates])
  expect_equal(proposed_estimated_dates[3],
               sample_from_delay(3, augmented_data$estimated_dates,
                                 augmented_data$error_indicators,
                                 group, model_info, rng1))
  
  ## group 2, propose new death date (4), going from error to correct
  ## then update missing onset date (1)
  ## no need to update other date as it is correct
  group <- 2
  i <- 4
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  proposed <- 
    propose_estimated_dates(i, augmented_data, observed_dates, group,
                            model_info, rng, update_errors = TRUE)
  
  proposed_error_indicators <- proposed$augmented_data$error_indicators
  proposed_estimated_dates <- proposed$augmented_data$estimated_dates
  proposed_updates <- proposed$updated
  
  expect_equal(augmented_data$error_indicators[-proposed_updates],
               proposed_error_indicators[-proposed_updates])
  expect_equal(proposed_updates, c(4, 1))
  expect_equal(proposed_error_indicators[proposed_updates], c(FALSE, NA))
  expect_equal(augmented_data$estimated_dates[-proposed_updates],
               proposed_estimated_dates[-proposed_updates])
  expect_equal(proposed_estimated_dates[4],
               observed_dates[4] + monty::monty_random_real(rng1))
  
  
  ## group 2, propose all dates, swapping errors
  group <- 2
  to_update <- c(3, 1, 4)
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  rng <- monty::monty_rng_create(seed = 1)
  rng1 <- monty::monty_rng_create(seed = 1)
  
  proposed <- 
    propose_estimated_dates(to_update, augmented_data, observed_dates, group,
                            model_info, rng, update_errors = TRUE)
  
  proposed_error_indicators <- proposed$augmented_data$error_indicators
  proposed_estimated_dates <- proposed$augmented_data$estimated_dates
  proposed_updates <- proposed$updated
  
  expect_equal(proposed_error_indicators, c(NA, NA, TRUE, FALSE, NA))
  expect_equal(proposed_updates, c(4, 1, 3))
  
  estimated_dates <- rep(NA, 5)
  ## date 4 will be sampled based on observed date
  estimated_dates[4] <- observed_dates[4] + monty::monty_random_real(rng1)
  ## will then sample date 1 (connected to date 4) and then date 3
  estimated_dates[1] <- 
    sample_from_delay(1, estimated_dates, c(NA, NA, TRUE, FALSE, NA),
                      group, model_info, rng1)
  estimated_dates[3] <- 
    sample_from_delay(3, estimated_dates, c(NA, NA, TRUE, FALSE, NA),
                      group, model_info, rng1)
  expect_equal(proposed_estimated_dates, estimated_dates)
  
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
  ## report (date 3) is correct so delay 1 prioritised over delay 2
  updated <- 1
  d <- dgamma(augmented_data$estimated_dates[3] - augmented_data$estimated_dates[1],
              shape = params[1, 1], rate = params[2, 1], log = TRUE)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  ## group 2, updated all dates
  ## report (date 3) is correct, no density contribution
  ## onset (date 1): only delay 1 available at time of sampling (death not yet
  ##   available); report (3) is correct so no prioritisation change needed
  ## death (date 4): only delay 2 available (onset now available); onset (1) is
  ##   missing so no prioritisation
  updated <- c(1, 3, 4)
  d <- sum(dgamma(augmented_data$estimated_dates[c(3, 4)] - 
                    augmented_data$estimated_dates[1],
                  shape = params[1, c(1, 2)], rate = params[2, c(1, 2)],
                  log = TRUE))
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info, is_batch = TRUE), d)
  
  
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
  ## onset (date 1) is correct and death (date 4) is missing so only
  ## delay 5 (onset to hosp) is prioritised
  updated <- 2
  d <- dlnorm(augmented_data$estimated_dates[2] - augmented_data$estimated_dates[1],
              meanlog = params[1, 5], sdlog = params[2, 5], log = TRUE)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  ## group 4, updated error hospitalisation date with both connected dates correct
  ## onset (date 1) and death (date 4) both correct so delays 5 and 6 both
  ## prioritised -- uniform mixture over the two correct delays
  augmented_data_both_correct <- list(
    estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
    error_indicators = c(FALSE, TRUE, FALSE, FALSE, NA))
  updated <- 2
  d <- log(sum(dlnorm(augmented_data_both_correct$estimated_dates[c(2, 4)] - 
                        augmented_data_both_correct$estimated_dates[c(1, 2)],
                      meanlog = params[1, c(5, 6)], 
                      sdlog = params[2, c(5, 6)]))) - log(2)
  expect_equal(
    calc_proposal_density(updated, augmented_data_both_correct, group,
                          model_info), d)
  
  ## group 4, updated missing death date
  ## hosp (date 2) is erroneous so no prioritisation
  ## based on delay 6 (log-normal), hospitalisation (date 2) to death (date 4)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  updated <- 4
  d <- dlnorm(augmented_data$estimated_dates[4] - 
                augmented_data$estimated_dates[2],
              meanlog = params[1, 6], sdlog = params[2, 6], log = TRUE)
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info), d)
  
  ## group 4, updated all dates
  ## onset (date 1) and report (date 3) correct, no density contribution
  ## hosp (date 2): only delay 5 available at time of sampling (death not yet
  ##   available); onset (1) correct so prioritised, same result
  ## death (date 4): only delay 6 available (hosp now available); hosp (2) is
  ##   erroneous so no prioritisation
  updated <- c(1, 2, 3, 4)
  d <- sum(dlnorm(augmented_data$estimated_dates[c(2, 4)] - 
                    augmented_data$estimated_dates[c(1, 2)],
                  meanlog = params[1, c(5, 6)], sdlog = params[2, c(5, 6)],
                  log = TRUE))
  expect_equal(
    calc_proposal_density(updated, augmented_data, group, model_info, is_batch = TRUE), d)
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
  calc_accept <- function(updated, augmented_data_new, augmented_data, group,
                          is_batch = FALSE) {
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
      calc_proposal_density(updated, augmented_data, group, model_info, is_batch)
    prop_new <- 
      calc_proposal_density(updated, augmented_data_new, group, model_info, is_batch)
    ratio_prop <- prop_current - prop_new
    
    ratio_post + ratio_prop
  }
  
  # group 2
  group <- 2
  observed_dates <- c(NA, NA, 40, 68, NA)
  augmented_data <- list(estimated_dates = c(20.5, NA, 40.2, 50.1, NA),
                         error_indicators = c(NA, NA, FALSE, TRUE, NA))
  
  ## updating error death (date 4) but matching observed date so auto-reject
  updated <- 4
  augmented_data_new <- list(estimated_dates = c(20.5, NA, 40.2, 68.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(updated, augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    -Inf)
  
  ## updating death (date 4) but outside date range so auto-reject
  updated <- 4
  augmented_data_new <- list(estimated_dates = c(20.5, NA, 40.2, 150.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(updated, augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    -Inf)
  
  
  ## updating onset (date 1) but outside date range so auto-reject
  updated <- 1
  augmented_data_new <- list(estimated_dates = c(-20.5, NA, 40.2, 50.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(updated, augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    -Inf)
  
  ## updating onset (date 1) but negative delay resulting so auto-reject
  updated <- 1
  augmented_data_new <- list(estimated_dates = c(43.5, NA, 40.2, 50.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(updated, augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    -Inf)
  
  ## updating onset (date 1)
  updated <- 1
  augmented_data_new <- list(estimated_dates = c(10.5, NA, 40.2, 50.1, NA),
                             error_indicators = c(NA, NA, FALSE, TRUE, NA))
  expect_equal(
    calc_accept_prob(updated, augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    calc_accept(updated, augmented_data_new, augmented_data, group))
  
  
  ## updating death (date 4), switching to correct
  updated <- 4
  augmented_data_new <- list(estimated_dates = c(20.5, NA, 40.2, 68.1, NA),
                             error_indicators = c(NA, NA, FALSE, FALSE, NA))
  expect_equal(
    calc_accept_prob(updated, augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range),
    calc_accept(updated, augmented_data_new, augmented_data, group))
  
  ## updating all dates, swapping errors
  updated <- c(1, 3, 4)
  augmented_data_new <- list(estimated_dates = c(10.5, NA, 50.2, 68.1, NA),
                             error_indicators = c(NA, NA, TRUE, FALSE, NA))
  expect_equal(
    calc_accept_prob(updated, augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range, is_batch = TRUE),
    calc_accept(updated, augmented_data_new, augmented_data, group,
                is_batch = TRUE))
  
  
  ## group 4
  group <- 4
  observed_dates <- c(10, 5, 30, NA, NA)
  augmented_data <- list(estimated_dates = c(10.3, 15.4, 30.2, 40.1, NA),
                         error_indicators = c(FALSE, TRUE, FALSE, NA, NA))
  
  ## updating all dates, swapping errors
  updated <- c(1, 2, 3, 4)
  augmented_data_new <- list(estimated_dates = c(2.3, 5.2, 33.2, 40.1, NA),
                             error_indicators = c(TRUE, FALSE, TRUE, NA, NA))
  expect_equal(
    calc_accept_prob(updated, augmented_data_new, augmented_data,
                     observed_dates, group, prob_error, model_info,
                     date_range, is_batch = TRUE),
    calc_accept(updated, augmented_data_new, augmented_data, group, is_batch = TRUE))
})
