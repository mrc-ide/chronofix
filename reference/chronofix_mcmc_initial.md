# Create initial parameter values

Create initial parameter values

## Usage

``` r
chronofix_mcmc_initial(
  initial_delay_shape = 1,
  initial_delay_mean = 5,
  initial_delay_meanlog = 1,
  initial_delay_precisionlog = 1,
  initial_prob_error = 0.1
)
```

## Arguments

- initial_delay_shape:

  The initial value for the shape parameter of gamma-distributed delays

- initial_delay_mean:

  The initial value for the mean of gamma-distributed delays

- initial_delay_meanlog:

  The initial value for the mean on the log-scale of
  log-normal-distributed delays

- initial_delay_precisionlog:

  The initial value for the precision on the log-scale of
  log-normal-distributed delays

- initial_prob_error:

  The initial value for the probability of error

## Value

List of initial parameter values
