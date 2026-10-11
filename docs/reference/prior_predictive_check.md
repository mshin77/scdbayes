# Prior predictive check for a fitted SCD model

Resamples the model from the prior alone and compares the implied
outcome distribution to the observed data range, the prior-predictive
step of the Bayesian workflow. A prior-predictive interval that fails to
span the observed data signals a mis-scaled prior.

## Usage

``` r
prior_predictive_check(model, data = NULL, ndraws = 100)
```

## Arguments

- model:

  A fitted `brmsfit`.

- data:

  Data holding the outcome column; defaults to the model's data. A
  different frame must carry every model term, since the replicates are
  drawn on it so that both sides use the same rows.

- ndraws:

  Prior-predictive draws summarized.

## Value

A list with the prior-predictive median and central 99\\ the observed
outcome range, `covers_observed`, and `note` (`NULL`, or how an
[`ar()`](https://rdrr.io/r/stats/ar.html) term conditions the
predictions); `NULL` if the model cannot be sampled from the prior (for
example under improper priors).

## Details

With a Gaussian [`ar()`](https://rdrr.io/r/stats/ar.html) term, brms
predicts each occasion conditional on lagged residuals computed from the
observed outcomes, so prior predictions are pulled toward the observed
series and the coverage check is optimistic; the returned `note` says
so.

## Examples

``` r
if (interactive()) {
fit <- run_scd_analysis(simulate_scd_data())$model
prior_predictive_check(fit)
}
```
