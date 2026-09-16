##' Run MCMC
##'
##' @title Run MCMC
##'
##' @param data Observed data
##'
##' @param delay_map Delays information
##' 
##' @param hyperparameters List of hyperparameters
##'
##' @param initial Initial values
##' 
##' @param control List of control parameters
##'
##' @return Output
##'
##' @export
chronofix_mcmc <- function(data,
                           delay_map,
                           hyperparameters = chronofix_hyperparameters(),
                           initial = chronofix_mcmc_initial(),
                           control = chronofix_mcmc_control()) {
  
  data <- chronofix_prepare_data(data)
  
  model <- chronofix_model(data, delay_map, hyperparameters, control)
  sampler <- chronofix_sampler(control)
  
  pars_initial <- mcmc_pars_initialise(model, initial)
  
  parameters <- model$parameters
  
  runner <- 
    if (control$parallel) monty::monty_runner_callr(control$n_workers) else
      monty::monty_runner_serial()
  
  samples <- monty::monty_sample(model, sampler, control$n_steps,
                                 initial = pars_initial,
                                 n_chains = control$n_chains, runner = runner,
                                 burnin = control$burnin, 
                                 thinning_factor = control$thinning_factor,
                                 save_full_chains = TRUE,
                                 flatten_chains = TRUE)
  
  ## Unpack augmented data
  samples$data <- unpack_augmented_data(samples$data, model$data_packer)
  id <- attr(data, "id")
  group <- attr(data, "group")
  rownames(samples$data$estimated_dates) <- data[[id]]
  colnames(samples$data$estimated_dates) <- setdiff(names(data), c(id, group))
  rownames(samples$data$error_indicators) <- data[[id]]
  colnames(samples$data$error_indicators) <- setdiff(names(data), c(id, group))
  
  class(samples) <- c(class(samples), "chronofix_mcmc_samples")
  
  samples
  
}

##' Create control parameters
##'
##' @title Create control parameters
##'
##' @param n_steps The number of steps to run in each MCMC chain
##' 
##' @param burnin The number of steps at the beginning of each chain to discard
##'   as burnin
##'
##' @param thinning_factor A thinning factor applied to the chains. If given,
##'   every`thinning_factor`'th step is retained 
##'
##' @param n_chains The number of chains to run
##' 
##' @param parallel Logical, indicating whether or not to run chains in parallel
##'
##' @param n_workers Number of workers to use for parallelisation
##'
##' @param lower_quantile Lower quantile used for initialisation of true dates
##' 
##' @param upper_quantile Upper quantile used for initialisation of true dates
##' 
##' @param earliest_possible_date The earliest possible date (in YYYY-MM-DD 
##'   format). Dates will not be estimated as occurring earlier than this date.
##'   If `NULL` (the default), then it will be determined by the 
##'   earliest date in the data and `date_buffer`.
##'   
##' @param latest_possible_date The latest possible date (in YYYY-MM-DD 
##'   format). Dates will not be estimated as occurring later than this date.
##'   If `NULL` (the default), then it will be determined by the latest
##'   date in the data and `date_buffer`.
##'   
##' @param date_buffer The date buffer in terms of days to determine the
##'   earliest and/or latest possible dates from the data. If 
##'   `earliest_possible_date` is not specified then the earliest possible date
##'   will be taken as `date_buffer` days before the earliest date in the data.
##'   Similarly, if `latest_possible_date` is not specified then the latest
##'   possible date will be taken as `date_buffer` days after the latest date in
##'   the data.
##' 
##' @param prob_update_estimated_dates The probability of proposing an update
##'   to each estimated date at each iteration in the MCMC
##'
##' @param prob_update_error_indicators The probability of proposing an update
##'   to each error indicator (with the corresponding estimated date updated
##'   accordingly) at each iteration in the MCMC
##'   
##' @param prob_error_swap The probability of proposing to swap all errors to
##' non-errors and vice versa (excluding missing dates) for individuals with at
##' least one error and non-error at each iteration in the MCMC
##' 
##' @param cascade_sampling Logical, indicating whether or not to use cascade
##'   sampling for estimated dates when updating an error indicator
##'
##' @return List of control parameters
##'
##' @export
chronofix_mcmc_control <- function(n_steps = 1000,
                                   burnin = 0,
                                   thinning_factor = 1,
                                   n_chains = 1,
                                   parallel = FALSE,
                                   n_workers = 1,
                                   lower_quantile = 0.01,
                                   upper_quantile = 0.99,
                                   earliest_possible_date = NULL,
                                   latest_possible_date = NULL,
                                   date_buffer = 30,
                                   prob_update_estimated_dates = 0.1,
                                   prob_update_error_indicators = 0.1,
                                   prob_error_swap = 1,
                                   cascade_sampling = FALSE) {
  
  list(n_steps = n_steps,
       burnin = burnin,
       thinning_factor = thinning_factor,
       n_chains = n_chains,
       parallel = parallel,
       n_workers = n_workers,
       lower_quantile = lower_quantile,
       upper_quantile = upper_quantile,
       earliest_possible_date = earliest_possible_date,
       latest_possible_date = latest_possible_date,
       date_buffer = date_buffer,
       prob_update_estimated_dates = prob_update_estimated_dates,
       prob_update_error_indicators = prob_update_error_indicators,
       prob_error_swap = prob_error_swap,
       cascade_sampling = cascade_sampling)
}

##' Create initial parameter values
##'
##' @title Create initial parameter values
##'
##' @param initial_delay_shape The initial value for the shape parameter of
##'   gamma-distributed delays
##' 
##' @param initial_delay_mean The initial value for the mean of
##'   gamma-distributed delays
##'
##' @param initial_delay_meanlog The initial value for the mean on the log-scale
##'   of log-normal-distributed delays
##'
##' @param initial_delay_precisionlog The initial value for the precision on the
##'  log-scale of log-normal-distributed delays
##'
##' @param initial_prob_error The initial value for the probability of error  
##' 
##' @return List of initial parameter values
##'
##' @export
chronofix_mcmc_initial <- function(initial_delay_shape = 1,
                                   initial_delay_mean = 5,
                                   initial_delay_meanlog = 1,
                                   initial_delay_precisionlog = 1,
                                   initial_prob_error = 0.1) {
  
  list(initial_delay_shape = initial_delay_shape,
       initial_delay_mean = initial_delay_mean,
       initial_delay_meanlog = initial_delay_meanlog,
       initial_delay_precisionlog = initial_delay_precisionlog,
       initial_prob_error = initial_prob_error)
}


mcmc_pars_initialise <- function(model, initial) {
  
  pars_initial <- numeric(length(model$parameters))
  pars_initial[model$parameters == "prob_error"] <- initial$initial_prob_error
  pars_initial[endsWith(model$parameters, "shape")] <- 
    initial$initial_delay_shape
  pars_initial[endsWith(model$parameters, "mean")] <- initial$initial_delay_mean
  pars_initial[endsWith(model$parameters, "meanlog")] <- 
    initial$initial_delay_meanlog
  pars_initial[endsWith(model$parameters, "precisionlog")] <- 
    initial$initial_delay_precisionlog
  
  pars_initial
}