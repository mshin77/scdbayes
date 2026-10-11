# Run sensitivity analysis with different priors and outlier handling

Run sensitivity analysis with different priors and outlier handling

## Usage

``` r
run_sensitivity_analysis(
  data = NULL,
  base_model_formula = NULL,
  vary_priors = TRUE,
  vary_outliers = TRUE,
  family = "gaussian",
  priors = NULL,
  iter = 1000,
  chains = 2,
  model = NULL
)
```

## Arguments

- data:

  A data frame; with `model`, defaults to `model$data`.

- base_model_formula:

  Formula string or formula object for the base model; ignored when
  `model` is supplied.

- vary_priors:

  Logical; test different prior widths.

- vary_outliers:

  Logical; add a Student-t refit.

- family:

  brms family name of the base model (default `"gaussian"`).

- priors:

  Optional priors of the fitted base model, as a `brmsprior` or as the
  list returned by
  [`assemble_brms_priors()`](https://mshin77.github.io/scdbayes/reference/assemble_brms_priors.md);
  prior arms replace only its treatment-coefficient prior (or its SD
  priors for `prior_sd_small`).

- iter, chains:

  Sampler settings for each refit.

- model:

  Optional fitted base `brmsfit`. Each arm then refits it with
  `update(model, prior = ...)` (and `family = student()` for the outlier
  arm), which keeps its full
  [`brms::bf()`](https://paulbuerkner.com/brms/reference/brmsformula.html)
  formula, including distributional submodels such as `sigma ~ phase`;
  its formula, family, and user-set priors replace `base_model_formula`,
  `family`, and `priors`. Without `model`, each arm is fitted from
  `data` and the mean formula only.

## Value

A named list of
[`brms::fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html)
matrices, one per arm (`prior_default`, `prior_narrow`, `prior_diffuse`,
`prior_sd_small`, `student_t`), with attributes `treatment_sd` (the
default treatment-prior SD `s` of the arms) and `treatment_coef` (the
targeted coefficient).

## Details

The prior arms refit the model with the treatment-coefficient prior at
the adaptive default SD (see
[`generate_adaptive_priors()`](https://mshin77.github.io/scdbayes/reference/generate_adaptive_priors.md)),
at half of it (narrow), and at twice it (diffuse), each centered on 0
and targeted at the treatment coefficient. For a model with group-level
terms the `prior_sd_small` arm keeps the base priors but sets every
group-level SD prior to `student_t(3, 0, s / sqrt(10))`, where `s` is
the default SD scale: a tenfold smaller variance scale, for the small
individual-variation case where the variance priors drive the
sensitivity. The outlier arm keeps every observation and refits with a
Student-t likelihood, whose heavy tails downweight outlying points
instead of deleting them; it runs only for Gaussian models, since a
Student-t base fit already has that likelihood.

## Examples

``` r
if (interactive()) {
dat <- process_scd_phases(simulate_scd_data(), list(), "AB")
run_sensitivity_analysis(dat, "outcome ~ time_A + level_AB + trend_AB + (1 | case)")
fit <- brms::brm(brms::bf(outcome ~ time_A + level_AB + trend_AB + (1 | case),
                          sigma ~ phase), data = dat, refresh = 0)
run_sensitivity_analysis(model = fit)
}
```
