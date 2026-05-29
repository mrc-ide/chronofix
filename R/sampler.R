#' chronofix sampler
#' @param control Object produced by `mcmc_control()`
#' @return A monty sampler configured for the chronofix model.
#' @export
chronofix_sampler <- function(control) {
  monty::monty_sampler(
    "chronofix sampler",
    "chronofix sampler",
    control,
    chronofix_sampler_initialise,
    chronofix_sampler_step,
    properties = monty::monty_sampler_properties(allow_augmented_data = TRUE))
}


chronofix_sampler_initialise <- function(state_chain, control, model, rng) {
  return(NULL)
}


chronofix_sampler_step <- function(state_chain, state_sampler, control, 
                                   model, rng) {
  
  state_chain <- update_pars_delay(state_chain, control, model, rng)
  
  state_chain <- update_prob_error(state_chain, model, rng)
  
  state_chain 
}
