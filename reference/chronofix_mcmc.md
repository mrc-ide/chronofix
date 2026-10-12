# Run MCMC

Run MCMC

## Usage

``` r
chronofix_mcmc(
  data,
  delay_map,
  hyperparameters = chronofix_hyperparameters(),
  initial = chronofix_mcmc_initial(),
  control = chronofix_mcmc_control()
)
```

## Arguments

- data:

  Observed data

- delay_map:

  Delays information

- hyperparameters:

  List of hyperparameters

- initial:

  Initial values

- control:

  List of control parameters

## Value

Output
