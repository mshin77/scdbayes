# Posterior summary of variance components

Reports posterior mean, SD, and credible interval for each random-effect
standard deviation (`sd_*`), each random-effect correlation (`cor_*`),
and each auxiliary distributional parameter (`sigma`, `nu`, `phi`,
`shape`, `alpha`).

## Usage

``` r
summarize_variance_components(model, ci_level = 0.95)
```

## Arguments

- model:

  A fitted `brmsfit`.

- ci_level:

  Credible interval width (default 0.95).

## Value

A data frame with columns `parameter`, `type` (one of
`"random_effect_sd"`, `"random_effect_cor"`, `"auxiliary"`), `mean`,
`sd`, `lower`, `upper`.

## Examples

``` r
if (interactive()) {
  dat <- simulate_scd_data()
  fit <- brms::brm(outcome ~ phase + (1 + phase | case),
                   data = dat, refresh = 0)
  summarize_variance_components(fit)
}
```
