# Prepare data

Prepare data for use with `chronofix`. You do not have to use this
function if you name your data.frame with our standard column names
(i.e., `id` column containing individual IDs) as it will be called
within other functions directly. However, you can use this to validate
your data separately or to use different column names than the defaults.

## Usage

``` r
chronofix_prepare_data(data, format = "%Y-%m-%d", id = NULL, group = NULL)
```

## Arguments

- data:

  A data.frame containing IDs and dates. By default we expect a column
  `id` (or one with the name given as the argument `id`) and two or more
  date columns.

- format:

  A character string declaring the date format. The default is
  `"%Y-%m-%d"` (e.g. `"2015-06-21"`). For guidance on formats see
  [`help(strptime)`](https://rdrr.io/r/base/strptime.html)

- id:

  Optional name of a column within `data` to use for unique individual
  identifiers.

- group:

  Optional name of a column within `data` to use for groups

## Value

A data.frame, with the addition of the class attribute `chronofix_data`;
once created you should not modify this object.
