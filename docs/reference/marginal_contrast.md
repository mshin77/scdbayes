# Population-averaged counterfactual contrast at a common session

For each posterior draw, predicts the expected outcome of `n_new` new
cases drawn from the fitted random-effect distribution at follow-up
session \\B\\, once with treatment introduced just after session \\A\\
and once without treatment, and averages over the new cases. The result
is the population-averaged (marginal) treatment contrast at a single
session, in contrast to the typical-case (conditional) contrasts of
[`summarize_fixed_effect_decisions()`](https://mshin77.github.io/scdbayes/reference/summarize_fixed_effect_decisions.md)
and
[`compute_phase_contrast()`](https://mshin77.github.io/scdbayes/reference/compute_phase_contrast.md),
which hold random effects at zero. On a log or logit link the two differ
whenever random effects are present, because the mean of \\\exp(\cdot)\\
over cases is not \\\exp(\cdot)\\ at the mean case.

## Usage

``` r
marginal_contrast(
  model,
  data = NULL,
  time_var = NULL,
  case_var = NULL,
  time_A = NULL,
  time_B = NULL,
  n_new = 200,
  ndraws = 500,
  intervention_goal = c("increase", "decrease"),
  ab_rule = c("scdhlm", "shortest_treatment"),
  duration = NULL
)
```

## Arguments

- model:

  A fitted `brmsfit` from an AB or multiple-baseline design.

- data:

  The processed data the model was fitted to (with `level_AB` and
  `time_A`); defaults to `model$data`.

- time_var:

  Time column; defaults to
  [`find_time_var()`](https://mshin77.github.io/scdbayes/reference/find_time_var.md),
  and `time_A` is used when neither is present.

- case_var:

  Case column; defaults to
  [`find_case_var()`](https://mshin77.github.io/scdbayes/reference/find_case_var.md).

- time_A, time_B:

  Last pre-treatment session \\A\\ and follow-up session \\B\\ on the
  time scale of `time_var`; `NULL` uses the defaults in Details.

- n_new:

  Number of new cases averaged per draw.

- ndraws:

  Number of posterior draws.

- intervention_goal:

  `"increase"` or `"decrease"`; sets the direction counted by
  `prob_improvement`.

- ab_rule:

  `"scdhlm"` or `"shortest_treatment"`; the default rule for \\B\\ (see
  Details).

- duration:

  Optional \\B - A\\ in sessions; overrides `ab_rule`.

## Value

A list with

- `summary`:

  Data frame with columns `quantity`, `median`, `lower`, `upper` (2.5\\
  quantiles) for `mean_untreated`, `mean_treated`, `difference` (treated
  minus untreated), `ratio` (treated over untreated; log links only),
  and `prob_improvement`, the posterior probability that the difference
  lies in the `intervention_goal` direction (given in `median`, with
  `NA` bounds).

- `draws`:

  Data frame with one row per draw: `draw`, `mean_untreated`,
  `mean_treated`, `difference`, and `ratio` (log links only).

- `n_draws`:

  Number of finite draws the summary uses.

- `time_A`, `time_B`:

  The sessions used.

## Details

The treated row uses the package's AB and multiple-baseline coding (see
[`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md)):
`time_A = B - T_A`, with \\T_A\\ the earliest first baseline session,
`level_AB = 1`, and `trend_AB = B - T_B`, with \\T_B\\ the first
observed session after \\A\\ (the treatment onset). The untreated row
has `time_A = B - T_A`, `level_AB = 0`, and `trend_AB = 0`. A `time_B`
column is `B - T_B` in both arms, as
[`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md)
codes it on every row. Maintenance and generalization terms are 0. Each
new case borrows one fitted row drawn at random, shared by its two arms,
so predictors outside the design coding average over their observed
distribution instead of being fixed at one row. A phase column (for
example in `sigma ~ phase`) takes a baseline-role label in the untreated
arm and an intervention-role label in the treated arm. Every grouping
factor of the model, including an observation-level random effect, is
relabeled to a new level per new case, so
[`brms::posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html)
draws it with `allow_new_levels = TRUE`,
`sample_new_levels = "gaussian"`, and `re_formula = NULL`; both arms of
a new case share its draw.

**Autocorrelation.** Predictions use `incl_autocor = FALSE`: the AR(1)
residual has mean 0, so the expected outcome of a new case does not
depend on lagged residuals, and the outcome values of the template rows
never enter. On a log link with latent AR(1) residuals (non-Gaussian
families in brms), covariance mode (`cov = TRUE`) uses stationary latent
variance \\\sigma_e^2 / (1 - \phi^2)\\, with `sderr` = \\\sigma_e\\.
Conditional mode (`cov = FALSE`) instead adds an independent latent
residual and its lagged contribution: \\z_t + \phi z\_{t-1}\\, with
independent \\z_t\\ of variance \\\sigma_e^2\\. Its interior-session
variance is \\\sigma_e^2 (1 + \phi^2)\\; at the initial session, the
absent lag contributes zero and variance is \\\sigma_e^2\\. The
follow-up contrast uses the interior-session variance. Each draw is
multiplied by the exponential of half the mode-specific variance.
Unknown modes, higher AR orders, and MA terms are rejected rather than
assigned this AR(1) correction. Other non-identity links hold the latent
residual at 0 and warn. Draws with a non-finite contrast are `NA` in
`draws`, excluded from `summary`, counted by `n_draws`, and reported in
a warning.

**Times.** \\A\\ is the last session of the shortest baseline across
tiers. With `ab_rule = "scdhlm"` (default) \\B - A\\ is the treatment
duration of the tier(s) with that shortest baseline, as in
[`scdhlm::default_times()`](https://jepusto.github.io/scdhlm/reference/default_times.html);
with `"shortest_treatment"` it is the shortest treatment duration across
tiers. Tiers without both baseline and treatment sessions are left out.
`duration` sets \\B - A\\ directly, for a common benchmark duration
across studies; `time_B` overrides both. Only AB and multiple-baseline
(multiple-probe) fits carrying `level_AB`, `trend_AB`, and `time_A` are
supported.

No established population-averaged estimand for single-case designs
exists yet; this function follows the general counterfactual
marginalization over new cases.

## Examples

``` r
if (interactive()) {
  d <- process_scd_phases(simulate_scd_data(n_cases = 4), list(), "AB")
  fit <- build_model(d, "outcome ~ time_A + level_AB + trend_AB + (1 + level_AB | case)",
                     family = "poisson")
  marginal_contrast(fit, d)$summary
}
```
