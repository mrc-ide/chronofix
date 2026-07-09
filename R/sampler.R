#' chronofix sampler
#' @param control Object produced by `chronofix_mcmc_control()`
#' @return A monty sampler configured for the chronofix model.
#' @export
chronofix_sampler <- function(control) {
  monty::monty_sampler(
    "chronofix sampler",
    "chronofix sampler",
    control,
    chronofix_sampler_initialise,
    chronofix_sampler_step,
    chronofix_sampler_state_dump,
    chronofix_sampler_state_combine,
    chronofix_sampler_state_restore,
    chronofix_sampler_state_details,
    properties = monty::monty_sampler_properties(allow_augmented_data = TRUE))
}


chronofix_sampler_initialise <- function(state_chain, control, model, rng) {
  state <- new.env(parent = emptyenv())
  
  augmented_data <- model$data_packer$unpack(state_chain$data)
  
  array_initial <- 0 * augmented_data$estimated_dates
  
  state$update_estimated_dates$attempts <- array_initial
  state$update_estimated_dates$accepts <- array_initial
  
  state$update_error_indicators$attempts <- array_initial
  state$update_error_indicators$accepts <- array_initial
  
  state$swap_error_indicators$attempts <- rep(0, nrow(array_initial))
  state$swap_error_indicators$accepts <- rep(0, nrow(array_initial))
  
  state
}


chronofix_sampler_step <- function(state_chain, state_sampler, control, 
                                   model, rng) {
  
  state_chain <- update_pars_delay(state_chain, control, model, rng)
  
  state_chain <- update_prob_error(state_chain, model, rng)
  
  state_chain 
}


chronofix_sampler_state_dump <- function(state, control) {
  as.list(state)
}


chronofix_sampler_state_restore <- function(chain_id, state_chain,
                                            state_sampler, control,
                                            model) {
  state_sampler
}


chronofix_sampler_state_details <- function(state, control) {
  state
}


chronofix_sampler_state_combine <- function(state, control) {
  join <- function(name, ...) {
    list_nm <- lapply(state, "[[", name)
    list(attempts = abind::abind(lapply(list_nm, "[[", "attempts"), ...),
         accepts = abind::abind(arrays = lapply(list_nm, "[[", "accepts"), ...))
  }
  
  list(swap_error_indicators = join("swap_error_indicators", along = 2),
       update_error_indicators = join("update_error_indicators", along = 3),
       update_estimated_dates = join("update_estimated_dates", along = 3))
}
