# Convergence diagnostics for Bayesian models

Convergence diagnostics for Bayesian models

## Usage

``` r
compute_convergence_diagnostics(model, data = NULL)
```

## Arguments

- model:

  A brmsfit model object.

- data:

  Optional data frame for posterior predictive checks.

## Value

An scd_diagnostics list with convergence metrics.

## Examples

``` r
if (interactive()) {
fit <- brms::brm(outcome ~ phase + (1 | case), data = simulate_scd_data())
compute_convergence_diagnostics(fit)
}
```
