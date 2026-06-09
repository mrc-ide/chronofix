#' Validate inputs for delay summary and plotting functions
#'
#' @param mcmc_output Output list from `chronofix_mcmc_run()`.
#' @param delay_map The delay map used for the model setup.
#'
#' @importFrom cli cli_abort
#' @noRd
validate_delay_inputs <- function(mcmc_output, delay_map) {
  
  if (missing(mcmc_output)) {
    cli::cli_abort(c(
      "x" = "{.arg mcmc_output} is missing.",
      "i" = "Please provide the output list from {.fn chronofix_mcmc_run}."
    ))
  }
  if (missing(delay_map)) {
    cli::cli_abort(c(
      "x" = "{.arg delay_map} is missing.",
      "i" = "Please provide the delay map used for the model setup."
    ))
  }
  if (is.null(mcmc_output$pars)) {
    cli::cli_abort(c(
      "x" = "{.arg mcmc_output} must contain a {.field pars} array."
    ))
  }
  
  param_names <- dimnames(mcmc_output$pars)[[1]]
  
  if (is.null(param_names)) {
    cli::cli_abort(c(
      "x" = "Parameter names are missing from the {.code mcmc_output$pars} array.",
      "i" = "Ensure the array has row names corresponding to your model parameters."
    ))
  }
  
  required_cols <- c("group", "from", "to", "distribution")
  missing_cols <- setdiff(required_cols, names(delay_map))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg delay_map} is missing required column{?s}: {.val {missing_cols}}."
    ))
  }
  
  # check for all required delay parameters
  n_delays <- nrow(delay_map)
  required_pars <- c(paste0("delay_mean", seq_len(n_delays)), 
                     paste0("delay_cv", seq_len(n_delays)))
  
  missing_pars <- setdiff(required_pars, param_names)
  if (length(missing_pars) > 0) {
    cli::cli_abort(c(
      "x" = "Missing parameter{?s} in {.code mcmc_output$pars}: {.val {missing_pars}}."
    ))
  }
  
  invisible(TRUE)
}
