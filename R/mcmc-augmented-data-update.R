update_augmented_data <- function(augmented_data, observed_dates, pars, groups,
                                  model_info, date_range, control, rng) {

  n_delays <- length(model_info$delay_from)
  
  delay_pars <- unpack_delay_pars(pars, model_info$delay_distribution)
  prob_error <- pars[["prob_error"]]

  for (i in seq_len(nrow(observed_dates))) {
    augmented_data_i <- lapply(augmented_data, function(x) x[i, ])
    augmented_data_i <- 
      update_augmented_data1(augmented_data_i, observed_dates[i, ], groups[i],
                             prob_error, delay_pars, model_info, date_range,
                             control, rng)
    augmented_data$estimated_dates[i, ] <- augmented_data_i$estimated_dates
    augmented_data$error_indicators[i, ] <- augmented_data_i$error_indicators
  }
  
  augmented_data
}


# Updating the augmented data for one individual
update_augmented_data1 <- function(augmented_data, observed_dates, group,
                                   prob_error, delay_pars, model_info,
                                   date_range, control, rng) {

  augmented_data <-
    update_estimated_dates(augmented_data, observed_dates, group, prob_error,
                           delay_pars, model_info, date_range, control, rng)
  
  augmented_data <-
    update_error_indicators(augmented_data, observed_dates, group, prob_error,
                            delay_pars, model_info, date_range, control, rng)
  
  augmented_data <-
    swap_error_indicators(augmented_data, observed_dates, group, prob_error,
                          delay_pars, model_info, date_range, control, rng)

  augmented_data
}


# Updating all the relevant estimated dates for one individual
update_estimated_dates <- function(augmented_data, observed_dates, group,
                                   prob_error, delay_pars, model_info,
                                   date_range, control, rng) {

  for (i in seq_along(observed_dates)) {
    augmented_data <- 
      update_estimated_dates1(i, augmented_data, observed_dates, group,
                              prob_error, delay_pars, model_info,
                              date_range, control, rng)
  }
  
  augmented_data
}


# Updating one of the estimated dates for an individual
update_estimated_dates1 <- function(i, augmented_data, observed_dates, group,
                                    prob_error, delay_pars, model_info,
                                    date_range, control, rng) {
  
  ## we check if date i is in the given group
  ## if FALSE, no update
  ## if TRUE, update with probability prob_update_estimated_dates
  group_info <- model_info$group_info[[group]]
  update <- group_info$is_date_in_group[i] &&
    monty::monty_random_real(rng) < control$prob_update_estimated_dates
  if (!update) {
    return(augmented_data)
  }
  
  if (control$cascade_sampling) {
    sampling_order <- 
      calc_cascade_sampling_order(i, group_info$event_order,
                                  augmented_data$error_indicators,
                                  group_info$is_date_connected,
                                  group_info$shortest_paths)
  } else {
    sampling_order <- i
  }
  sampling_order_reverse <- sampling_order
  
  augmented_data_new <- 
    propose_estimated_dates(sampling_order, augmented_data, observed_dates,
                            group, delay_pars, model_info, date_range, rng)
  
  accept_prob <-
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data, observed_dates,
                     group, prob_error, delay_pars, model_info, date_range)

  accept <- log(monty::monty_random_real(rng)) < accept_prob
  if (accept) {
    augmented_data <- augmented_data_new
  }
  
  augmented_data
}


# Updating all the relevant error indicators (and corresponding estimated dates)
# for one individual
update_error_indicators <- function(augmented_data, observed_dates, group,
                                    prob_error, delay_pars, model_info,
                                    date_range, control, rng) {
  
  for (i in seq_along(observed_dates)) {
    augmented_data <- 
      update_error_indicators1(i, augmented_data, observed_dates, group,
                               prob_error, delay_pars, model_info, date_range,
                               control, rng)
  }
  
  augmented_data
}


# Updating one of the error indicators (and corresponding estimated date) for an
# individual
update_error_indicators1 <- function(i, augmented_data, observed_dates, group,
                                     prob_error, delay_pars, model_info,
                                     date_range, control, rng) {
  
  ## we check if error indicator is non-NA (so date is non-missing)
  ## if FALSE, no update
  ## if TRUE, update with probability prob_update_error_indicators
  update <- !is.na(augmented_data$error_indicators[i]) &&
    monty::monty_random_real(rng) < control$prob_update_error_indicators
  if (!update) {
    return(augmented_data)
  }
  
  group_info <- model_info$group_info[[group]]
  
  augmented_data_new <- change_error_indicators(augmented_data, i)
  if (control$cascade_sampling) {
    sampling_order <- 
      calc_cascade_sampling_order(i, group_info$event_order,
                                  augmented_data_new$error_indicators,
                                  group_info$is_date_connected,
                                  group_info$shortest_paths)
    sampling_order_reverse <- 
      calc_cascade_sampling_order(i, group_info$event_order,
                                  augmented_data$error_indicators,
                                  group_info$is_date_connected,
                                  group_info$shortest_paths)
  } else {
    sampling_order <- i
    sampling_order_reverse <- i
  }
  
  augmented_data_new <- 
    propose_estimated_dates(sampling_order, augmented_data_new, observed_dates, 
                            group, delay_pars, model_info, date_range, rng)
  
  accept_prob <-
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data, observed_dates,
                     group, prob_error, delay_pars, model_info, date_range)

  accept <- log(monty::monty_random_real(rng)) < accept_prob
  if (accept) {
    augmented_data <- augmented_data_new
  }
  
  augmented_data
}


# Sample new date using randomly selected delay
sample_from_delay <- function(i, augmented_data, group, delay_pars, model_info,
                              date_range, rng) {
  group_info <- model_info$group_info[[group]]
  is_date_connected <- group_info$is_date_connected[i, ]
  delay_connecting_dates <- group_info$delay_connecting_dates[i, ]
  is_date_available <- !is.na(augmented_data$estimated_dates)
  
  delays_for_sampling <- 
    delay_connecting_dates[is_date_available & is_date_connected]
  
  ## Filter to only delays where the other date is available (needed for swap)
  if (length(delays_for_sampling) == 0) {
    ## not possible to sample from a delay so sample a random date
    d <- as.numeric(date_range)
    proposed_date <- monty::monty_random_uniform(d[1L], d[2L], rng)
  } else {
    ## If it is involved in several delays, randomly select one
    delay_idx <- if (length(delays_for_sampling) == 1) 1 else 
      ceiling(length(delays_for_sampling) * monty::monty_random_real(rng))
    selected_delay <- delays_for_sampling[delay_idx]
    
    delay_from <- model_info$delay_from[selected_delay]
    
    ## Is date i the 'from' or 'to' in this delay
    is_from <- (i == delay_from)
    
    if (is_from) {
      ## proposed date = other_date - delay
      ## so delay = other_date - proposed_date
      sign <- -1
      ## Find the other date in this date pair
      delay_to <- model_info$delay_to[selected_delay]
      other_date <- augmented_data$estimated_dates[delay_to]
    } else {
      ## proposed date = other_date + delay  
      ## so delay = proposed_date - other_date
      sign <- 1
      ## Find the other date in this date pair
      other_date <- augmented_data$estimated_dates[delay_from]
    }
    
    ## Sample a delay from the marginal posterior
    pars <- delay_pars[[selected_delay]]
    distribution <- model_info$delay_distribution[selected_delay]
    
    sampled_delay <- sample_from_delay1(pars, distribution, rng)
    
    ## Calculate proposed date based on the sampled delay
    proposed_date <- other_date + sign * sampled_delay
  }
  
  
  proposed_date
  
}


sample_from_delay1 <- function(pars, distribution, rng) {
  
  if (distribution == "gamma") {
    rate <- pars$shape / pars$mean
    x <- monty::monty_random_gamma_rate(pars$shape, rate, rng)
  } else if (distribution == "log-normal") {
    x <- monty::monty_random_log_normal(pars$meanlog, 
                                        1 / sqrt(pars$precisionlog), rng)
  } 
  
  x
}


# propose new estimated dates for date indices in to_update
propose_estimated_dates <- function(sampling_order, augmented_data,
                                    observed_dates, group, delay_pars,
                                    model_info, date_range, rng) {
  
  augmented_data$estimated_dates[sampling_order] <- NA
  
  for (i in sampling_order) {
    if (isFALSE(augmented_data$error_indicators[i])) {
      augmented_data$estimated_dates[i] <-
        observed_dates[i] + monty::monty_random_real(rng)
    } else {
      augmented_data$estimated_dates[i] <-
        sample_from_delay(i, augmented_data, group, delay_pars, 
                          model_info, date_range, rng)
    }
  }
  
  augmented_data
}


## calculate the (log) acceptance probability for updating augmented_data to
## augmented_data_new where updated is the indices of the updated date(s)
calc_accept_prob <- function(sampling_order, sampling_order_reverse,
                             augmented_data_new, augmented_data,
                             observed_dates, group, prob_error, delay_pars,
                             model_info, date_range) {
  
  is_delay_in_group <- model_info$group_info[[group]]$is_delay_in_group

  ## are error indicators TRUE with estimated date matching observed date
  incompatible_error_and_date <-
    !is.na(augmented_data_new$error_indicators[sampling_order]) &
    augmented_data_new$error_indicators[sampling_order] == TRUE &
    (floor(augmented_data_new$estimated_dates[sampling_order]) == 
       observed_dates[sampling_order])
  ## are estimated dates outside the date range 
  date_outside_range <- 
    augmented_data_new$estimated_dates[sampling_order] < date_range[1] |
    augmented_data_new$estimated_dates[sampling_order] >= date_range[2]
  reject <- any(incompatible_error_and_date) || any(date_outside_range)
  if (reject) {
    return(-Inf)
  }
  
  ## new delays log likelihood
  ll_delays_new <- chronofix_log_likelihood_delays1(
    augmented_data_new$estimated_dates, delay_pars, model_info$delay_from,
    model_info$delay_to, model_info$delay_distribution, is_delay_in_group)
  
  if (any(is.infinite(ll_delays_new))) {
    ## Covering two cases here:
    ## 1. a proposed delay is negative so we want to auto-reject
    ## 2. we haveended up in a situation that such a small delay has been drawn
    ##    that when recalculated from the dates it is essentially 0 and 
    ##    distribution has infinite density at 0 (CV > 1). Let's reject for
    ##    the moment
    return(-Inf)
  }
  
  ## current delays log likelihood
  ll_delays_current <- chronofix_log_likelihood_delays1(
    augmented_data$estimated_dates, delay_pars, model_info$delay_from,
    model_info$delay_to, model_info$delay_distribution, is_delay_in_group)
  
  ratio_ll_delays <- sum(ll_delays_new) - sum(ll_delays_current)
  
  if (identical(augmented_data$error_indicators, 
                augmented_data_new$error_indicators)) {
    ## can skip errors log likelihood calculation
    ratio_ll_errors <- 0 
  } else {
    ## current errors log likelihood
    ll_errors_current <- chronofix_log_likelihood_errors(
      prob_error, augmented_data$error_indicators, date_range)
    ## new errors log likelihood
    ll_errors_new <- chronofix_log_likelihood_errors(
      prob_error, augmented_data_new$error_indicators, date_range)
    
    ratio_ll_errors <- ll_errors_new - ll_errors_current
  }
  
  ratio_post <- ratio_ll_delays + ratio_ll_errors

  ## No need to calculate proposal correction if ratio_post is -Inf
  if (ratio_post == -Inf) {
    return(-Inf)
  }
  
  prop_current <- 
    calc_proposal_density(sampling_order_reverse, augmented_data, group, 
                          delay_pars, model_info, date_range)
  prop_new <- calc_proposal_density(sampling_order, augmented_data_new,
                                    group, delay_pars, model_info, date_range)
  ratio_prop <- prop_current - prop_new
  
  ratio_post + ratio_prop
}


calc_proposal_density <- function(sampling_order, augmented_data,
                                  group, delay_pars, model_info, date_range) {
  group_info <- model_info$group_info[[group]]
  is_date_in_delay <- group_info$is_date_in_delay
  is_date_in_group <- group_info$is_date_in_group
  is_date_connected <- group_info$is_date_connected
  delay_connecting_dates <- group_info$delay_connecting_dates
  
  is_resampled <- 
    seq_along(augmented_data$error_indicators) %in% sampling_order
  is_date_available <- is_date_in_group & !is_resampled
  
  d <- rep(0, length(sampling_order))
  
  for (j in seq_along(sampling_order)) {
    
    i <- sampling_order[j]
    
    ## if non-error (FALSE) - proposal is uniform over one day so log-density 
    ## is 0, hence only need to calculate for error (TRUE) or missing (NA)
    
    if (!isFALSE(augmented_data$error_indicators[i])) {
      ## which dates were available for sampling
      delays_for_sampling <- 
        delay_connecting_dates[i, is_date_available & is_date_connected[i, ]]
      
      if (length(delays_for_sampling) == 0) {
        ## no connected date available so just sampled from date range
        d[j] <- -log(date_range[2L] - date_range[1L])
      } else {
        ## error or missing - proposal is based on delay(s)
        delay_pars_sample <- delay_pars[delays_for_sampling]
        delay_distribution <- 
          model_info$delay_distribution[delays_for_sampling]
        delay_from <- model_info$delay_from[delays_for_sampling]
        delay_to <- model_info$delay_to[delays_for_sampling]
        delay_values <- augmented_data$estimated_dates[delay_to] - 
          augmented_data$estimated_dates[delay_from]
        
        if (length(delays_for_sampling) == 1) {
          ## single delay involving date i
          d[j] <- log_density_delay(delay_values, delay_pars_sample[[1]],
                                    delay_distribution)
        } else {
          ## multiple delays involving date i, so delay selected at random
          d[j] <- log(sum(exp(mapply(log_density_delay, delay_values, 
                                     delay_pars_sample, delay_distribution)))) - 
            log(length(delays_for_sampling))
        }
      }
      
    }
    
    is_date_available[i] <- TRUE
  }
  
  sum(d)
}


## Swap -----------------------------------------------------------------------

# Check for individuals with at least one error and non-error (exclude missing)
has_mixed_errors <- function(error_indicators) {
  length(unique(error_indicators[!is.na(error_indicators)])) == 2
}


# Swap error indicators for one eligible individual
swap_error_indicators <- function(augmented_data, observed_dates, group,
                                  prob_error, delay_pars, model_info,
                                  date_range, control, rng) {

  ## we check if individual has mixed errors (at least one error and non-error)
  ## if FALSE, no update
  ## if TRUE, update with probability prob_error_swap
  update <- has_mixed_errors(augmented_data$error_indicators) &&
    monty::monty_random_real(rng) < control$prob_error_swap
  if (!update) {
    return(augmented_data)
  } 
  
  group_info <- model_info$group_info[[group]]
  event_order <- group_info$event_order
  
  augmented_data_new <- change_error_indicators(augmented_data, event_order)
  
  sampling_order <- 
    calc_batch_sampling_order(event_order, augmented_data_new$error_indicators,
                              group_info$is_date_connected)
  
  sampling_order_reverse <- 
    calc_batch_sampling_order(event_order, augmented_data$error_indicators,
                              group_info$is_date_connected)

  # systematically sample new errors and missing dates based on new non-errors
  augmented_data_new <- 
    propose_estimated_dates(sampling_order, augmented_data_new, observed_dates, 
                            group, delay_pars, model_info, date_range, rng)

  accept_prob <-
    calc_accept_prob(sampling_order, sampling_order_reverse,
                     augmented_data_new, augmented_data, observed_dates,
                     group, prob_error, delay_pars, model_info, date_range)
  
  accept <- log(monty::monty_random_real(rng)) < accept_prob
  if (accept) {
    augmented_data <- augmented_data_new
  }

  augmented_data

}


calc_batch_sampling_order <- function(to_resample, error_indicators,
                                      is_date_connected) {
  
  if (length(to_resample) == 1) {
    return(to_resample)
  }
  
  ## resample non-errors first
  err_ind <- error_indicators[to_resample]
  is_non_error <- !err_ind & !is.na(err_ind)
  sampling_order <- to_resample[is_non_error]
  
  remaining_to_resample <- to_resample[!is_non_error]
  
  if (length(remaining_to_resample) > 0) {
    
    while (length(remaining_to_resample) > 1) {
      # Find all dates connected to available dates
      is_connected <- 
        rowSums(is_date_connected[remaining_to_resample, sampling_order,
                                  drop = FALSE]) > 0
      connected_dates <- remaining_to_resample[is_connected]
      
      # Earliest connected event according to sampling_order
      earliest_idx <- which(remaining_to_resample %in% connected_dates)[1]
      date_to_sample <- remaining_to_resample[earliest_idx]
      
      # Update resampling order and remove from remaining
      sampling_order <- c(sampling_order, date_to_sample)
      remaining_to_resample <- remaining_to_resample[-earliest_idx]
    }
    
    sampling_order <- c(sampling_order, remaining_to_resample)
  }
  
  sampling_order
}


calc_cascade_sampling_order <- function(i, event_order,
                                        error_indicators, is_date_connected,
                                        shortest_paths) {
  
  ## Check if there are any correct dates, if not then we do not cascade
  is_possible_anchor <- visFALSE(error_indicators[event_order])
  
  if (is_possible_anchor[event_order == i]) {
    ## Date i is correct so use itself as an anchor
    sampling_order <- i
  } else {
    has_anchor <- any(is_date_connected[i, event_order] & is_possible_anchor)
    if (has_anchor) {
      ## Date is connected to an anchor
      sampling_order <- i
    } else {
      possible_anchors <- event_order[is_possible_anchor]
      shortest_path_lengths <- 
        vnapply(shortest_paths[[i]][possible_anchors], length)
      if (all(shortest_path_lengths == 0)) {
        ## No path to an anchor so cascade from i
        sampling_order <- i
      } else {
        possible_anchors <- possible_anchors[shortest_path_lengths > 0]
        shortest_path_lengths <- 
          shortest_path_lengths[shortest_path_lengths > 0]
        anchor <- possible_anchors[which.min(shortest_path_lengths)]
        sampling_order <- rev(shortest_paths[[i]][[anchor]])[-1L]
      }
    }
  }
  
  ## cascade_candidates are dates not already in sampling_order that are
  ## missing or erroneous
  cascade_candidates <- event_order[!(event_order %in% sampling_order)]
  is_cascade_candidate <- !visFALSE(error_indicators[cascade_candidates])
  cascade_candidates <- cascade_candidates[is_cascade_candidate]
  
  while (length(cascade_candidates) > 0) {
    ## identify which of cascade_candidates are connected to dates in
    ## sampling_order
    is_cascadable <- rowSums(
      is_date_connected[cascade_candidates, sampling_order, drop = FALSE]) > 0
    to_cascade <- cascade_candidates[is_cascadable]
    if (length(to_cascade) == 0) {
      ## none connected so return
      return(sampling_order)
    } else {
      ## add connected cascade candidates to sampling_order and remove
      ## from cascade candidates
      sampling_order <- c(sampling_order, to_cascade)
      cascade_candidates <- cascade_candidates[!is_cascadable]
    }
  }
  
  sampling_order
}


change_error_indicators <- function(augmented_data, i) {
  augmented_data$error_indicators[i] <- !augmented_data$error_indicators[i]
  augmented_data
}
