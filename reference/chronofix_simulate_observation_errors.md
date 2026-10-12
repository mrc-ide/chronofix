# Add observation errors

Simulate observed data incorporating observation error from true data

## Usage

``` r
chronofix_simulate_observation_errors(
  true_data,
  error_params,
  date_range,
  format = "%Y-%m-%d"
)
```

## Arguments

- true_data:

  Dataframe containing the true, unobserved dates.

- error_params:

  A list containing `prop_missing_data` and `prob_error`.

- date_range:

  A vector of two integer dates for the simulation range.

- format:

  A character string declaring the date format. The default is
  `"%Y-%m-%d"` (e.g. `"2015-06-21"`). For guidance on formats see
  [`help(strptime)`](https://rdrr.io/r/base/strptime.html)
