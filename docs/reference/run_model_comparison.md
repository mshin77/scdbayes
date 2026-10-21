# Model comparison via LOO-CV

Model comparison via LOO-CV

## Usage

``` r
run_model_comparison(
  data,
  model_formulas,
  family = "gaussian",
  iter = 1000,
  chains = 2,
  existing_model = NULL
)
```

## Arguments

- data:

  Data frame for model fitting.

- model_formulas:

  Named list of formula strings to compare.

- family:

  Model family (default `"gaussian"`).

- iter:

  Number of sampling iterations.

- chains:

  Number of MCMC chains.

- existing_model:

  An existing `brmsfit` object to include in comparison.

## Value

A list with LOO comparison table, fitted models, and LOO objects, or
`NULL` if fewer than two models succeed.

## Examples

``` r
if (interactive()) {
dat <- simulate_scd_data()
run_model_comparison(dat, build_comparison_models("outcome", "phase", "case"))
}
```
