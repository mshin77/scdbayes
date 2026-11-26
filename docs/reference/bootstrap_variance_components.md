# Posterior variance components and ICCs

Summarizes the residual variance, the random-intercept variance of each
grouping factor, and the intraclass correlations from the posterior
draws of a fitted model. For each draw, \\ICC_g = \tau_g^2 / (\sum_h
\tau_h^2 + \sigma^2)\\, where \\\tau_g\\ is the intercept SD
(`sd_<g>__Intercept`) of grouping factor \\g\\ and \\\sigma^2\\ the
residual response variance; the residual enters the denominator once.
The intervals are posterior quantiles, so they carry the full posterior
uncertainty of every variance component.

## Usage

``` r
bootstrap_variance_components(model, n_boot = 200)
```

## Arguments

- model:

  A `brmsfit` object.

- n_boot:

  Positive finite integer number of posterior draws summarized (evenly
  spaced over the chains; all draws when fewer are available).

## Value

A list with `summary` (a matrix with rows `mean`, `sd`, `ci_lower.2.5%`,
`ci_upper.97.5%` and one column per quantity: `sigma2`, each grouping
factor's intercept variance, and `iccs.<group>`), `n_successful` (draws
used), and `n_attempted` (`n_boot`); `NULL` for a non-`brmsfit` or when
fewer than 10 draws are available.

## Details

The name is kept for compatibility. It previously refit the model to
posterior-predictive replicates, which double counted the residual and
held the case effects at their estimates, so its intervals were too
narrow. A parametric bootstrap of a Bayesian multilevel SCD model is
unnecessary: the posterior draws already give the distribution of each
component. For families without a residual SD (`sigma`), such as Poisson
or negative binomial, or for a model with `sigma` predictors, the ICCs
are `NA` with a message, since a level-1 variance on the response scale
is not a single parameter there. The reported `sigma2` includes the
Student-t factor `nu/(nu - 2)`. Natural-residual stationary AR(1) models
include the factor `1/(1 - ar^2)`. A covariance-form latent Gaussian
AR(1) adds `sderr^2/(1 - ar^2)` to the independent response variance
rather than rescaling its `sigma^2`. Conditional latent serial models
have occasion-dependent boundary variances and return `NA`; no
interior-occasion ICC is substituted. Constant response variances are
supported for identity-link Gaussian, Student-t, and skew-normal
families only. ICCs are undefined if any retained response SD is
nonfinite or negative, if any retained Student-t draw has `nu <= 2`, if
the fitted serial specification cannot establish stationary AR(1), or if
the family or link prevents a constant residual response variance.
Random-slope models report intercept-reference ratios, not correlations
at arbitrary sessions or covariate values.

## Examples

``` r
if (interactive()) {
fit <- brms::brm(outcome ~ phase + (1 | case), data = simulate_scd_data())
bootstrap_variance_components(fit, n_boot = 1000)
}
```
