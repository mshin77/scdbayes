# Posterior predictive replicate datasets

Draws replicate outcomes the way
[`posterior_predictive_features()`](https://mshin77.github.io/scdbayes/reference/posterior_predictive_features.md)
does, for use in any graphical or numerical posterior predictive check:
an observation-level random effect (one level per fitted row) is redrawn
rather than conditioned on, `mode = "new"` also draws new case effects,
and replicate values are `NA` wherever the observed outcome is missing.
A latent [`ar()`](https://rdrr.io/r/stats/ar.html) model is predicted on
its fitted rows.

## Usage

``` r
ppc_replicates(model, data = NULL, mode = c("same", "new"), n_draws = 100L)
```

## Arguments

- model:

  A fitted `brmsfit`.

- data:

  Data holding the outcome, case, phase and (optional) time columns;
  defaults to the model's data. Rows with a missing outcome are allowed:
  replicate values are set to `NA` there.

- mode:

  `"same"` replicates for the observed cases; `"new"` draws new case
  effects from the population posterior (`allow_new_levels = TRUE`) to
  assess generalization. In both modes an observation-level random
  effect (one level per fitted row) is redrawn rather than conditioned
  on, and replicate values are set to `NA` wherever the observed outcome
  is missing so both sides use the same rows.

- n_draws:

  Number of replicate datasets.

## Value

A numeric matrix with `n_draws` rows and one column per row of `data`.

## Examples

``` r
if (interactive()) {
  fit <- run_scd_analysis(simulate_scd_data())$model
  yrep <- ppc_replicates(fit, n_draws = 50)
  bayesplot::ppc_dens_overlay(fit$data$outcome, yrep)
}
```
