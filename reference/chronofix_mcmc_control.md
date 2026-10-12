# Create control parameters

Create control parameters

## Usage

``` r
chronofix_mcmc_control(
  n_steps = 1000,
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
  prob_update_estimated_dates = 1,
  prob_update_error_indicators = 1,
  prob_error_swap = 1
)
```

## Arguments

- n_steps:

  The number of steps to run in each MCMC chain

- burnin:

  The number of steps at the beginning of each chain to discard as
  burnin

- thinning_factor:

  A thinning factor applied to the chains. If given,
  every`thinning_factor`'th step is retained

- n_chains:

  The number of chains to run

- parallel:

  Logical, indicating whether or not to run chains in parallel

- n_workers:

  Number of workers to use for parallelisation

- lower_quantile:

  Lower quantile used for initialisation of true dates

- upper_quantile:

  Upper quantile used for initialisation of true dates

- earliest_possible_date:

  The earliest possible date (in YYYY-MM-DD format). Dates will not be
  estimated as occurring earlier than this date. If `NULL` (the
  default), then it will be determined by the earliest date in the data
  and `date_buffer`.

- latest_possible_date:

  The latest possible date (in YYYY-MM-DD format). Dates will not be
  estimated as occurring later than this date. If `NULL` (the default),
  then it will be determined by the latest date in the data and
  `date_buffer`.

- date_buffer:

  The date buffer in terms of days to determine the earliest and/or
  latest possible dates from the data. If `earliest_possible_date` is
  not specified then the earliest possible date will be taken as
  `date_buffer` days before the earliest date in the data. Similarly, if
  `latest_possible_date` is not specified then the latest possible date
  will be taken as `date_buffer` days after the latest date in the data.

- prob_update_estimated_dates:

  The probability of proposing an update to each estimated date at each
  iteration in the MCMC

- prob_update_error_indicators:

  The probability of proposing an update to each error indicator (with
  the corresponding estimated date updated accordingly) at each
  iteration in the MCMC

- prob_error_swap:

  The probability of proposing to swap all errors to non-errors and vice
  versa (excluding missing dates) for individuals with at least one
  error and non-error at each iteration in the MCMC

## Value

List of control parameters
