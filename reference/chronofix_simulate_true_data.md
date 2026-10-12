# Simulate true data

Simulate the true, unobserved dates for individuals

## Usage

``` r
chronofix_simulate_true_data(n_per_group, group_names, delay_info, date_range)
```

## Arguments

- n_per_group:

  Vector of number of individuals to simulate in each group.

- group_names:

  A character or numeric vector of names for the groups being simulated.

- delay_info:

  A data frame that defines the delays between events, and their
  distributions and parameters. It must contain the columns `from`
  (character), `to` (character), `group` (list of numeric or character
  group IDs), `distribution` (character), `mean` (numeric) and `cv`
  (numeric)

- date_range:

  A vector of two integer dates for the simulation range.
