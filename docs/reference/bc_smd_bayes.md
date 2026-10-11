# Bayesian design-comparable between-case standardized mean difference

Fits one study's multiple-baseline (multiple-probe) or
treatment-reversal data with a Gaussian piecewise multilevel model in
brms and returns the posterior of the design-comparable BC-SMD, computed
draw by draw as \\\delta\_{AB} = p'\beta / \sqrt{r'\theta}\\.

## Usage

``` r
bc_smd_bayes(
  data,
  design = detect_design_type(data),
  time_A = NULL,
  time_B = NULL,
  trend = c("none", "fixed", "random"),
  ar = TRUE,
  family = "gaussian",
  priors = NULL,
  iter = 2000,
  chains = 4,
  outcome_var = "outcome",
  intervention_goal = c("increase", "decrease"),
  ab_rule = c("scdhlm", "shortest_treatment"),
  duration = NULL,
  random_effect = TRUE,
  ...
)
```

## Arguments

- data:

  One study's data with case, phase, time and outcome columns.

- design:

  `"multiple_baseline"`, `"multiple_probe"`, or `"reversal"`; defaults
  to
  [`detect_design_type()`](https://mshin77.github.io/scdbayes/reference/detect_design_type.md).

- time_A, time_B:

  Sessions \\A\\ and \\B\\; `NULL` uses the defaults in Details.

- trend:

  `"none"`, `"fixed"`, or `"random"` time trend.

- ar:

  Logical; AR(1) residuals within series.

- family:

  Must be `"gaussian"`.

- priors:

  brms priors; `NULL` uses the package defaults scaled to the outcome SD
  plus the bounded AR(1) prior.

- iter, chains:

  Sampler settings.

- outcome_var:

  Outcome column.

- intervention_goal:

  `"increase"` or `"decrease"`; with `"decrease"` the sign of
  \\\delta\_{AB}\\ is reversed.

- ab_rule:

  `"scdhlm"` or `"shortest_treatment"`; the default rule for \\B\\ (see
  Details).

- duration:

  Optional \\B - A\\ in sessions; overrides `ab_rule`.

- random_effect:

  Logical; let the treatment effect vary across cases (default `TRUE`).
  `FALSE` fits a common-effect model, useful as a sensitivity check.

- ...:

  Passed to
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

## Value

A list with `summary` (one-row data frame: `estimate` (posterior
median), `sd`, `lower`, `upper` (95\\ credible bounds)), `draws`
(numeric vector of \\\delta\_{AB}\\ draws), `n_nonfinite`, `time_A`,
`time_B`, `n_cases` (participants), `n_series`, `design`, and `model`
(the `brmsfit`).

## Details

**Effect size.** \\\delta\_{AB}\\ is the model-implied difference
between the average outcome at follow-up session \\B\\ when treatment is
introduced just after session \\A\\ and the average outcome at \\B\\
without treatment, divided by the standard deviation of the outcome at
\\B\\ without treatment. With `intervention_goal = "decrease"` the draws
are multiplied by \\-1\\, so a positive \\\delta\_{AB}\\ always means
improvement, the orientation
[`meta_bayes()`](https://mshin77.github.io/scdbayes/reference/meta_bayes.md)
expects.

**Model.** Rows with a baseline-role or intervention-role phase are kept
(maintenance and generalization rows are dropped); the tiers of
[`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md)
are the series. For multiple-baseline data the fixed part uses the
package's AB coding (see
[`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md)):
`level_AB` for `trend = "none"`, plus `time_A` and `trend_AB` for
`"fixed"` and a series-varying `time_A` for `"random"`; `time_A` counts
sessions from the earliest session across all series, one origin as in
scdhlm. Series carry a random intercept, and by default
(`random_effect = TRUE`) the treatment effect varies too,
`(1 + level_AB | case)`, matching the one-stage default; this leaves the
form of \\\delta\_{AB}\\ unchanged. For reversal data a single
`treatment` indicator (intervention role vs baseline role) is the
effect; `"fixed"` and `"random"` add only the baseline time trend
`time_A`. With `ar = TRUE` the residuals follow a stationary AR(1)
process within series with a `uniform(-1, 1)` prior bounded to \\\[-1,
1\]\\.

**Participants and series.** When a participant carries several series
(a replicated multiple baseline across behaviors or settings) the model
adds a participant level, `outer`, above the series; in a clustered
multiple baseline (a cluster column with two or more clusters) `outer`
is the cluster. The outer level carries an intercept and, with
`random_effect = TRUE`, a varying treatment effect,
`(1 + level_AB | outer)` (`(1 + treatment | outer)` for reversal data);
`random_effect = FALSE` keeps `(1 | outer)`. Both intercept variances
enter \\r'\theta\\. Data with both a cluster column and several series
per participant stop. At least three participants are required; series
of one participant do not count separately.

**Numerator \\p'\beta\\.** The `level_AB` (or `treatment`) coefficient
plus, for multiple-baseline models with a trend, the `trend_AB`
coefficient times \\B - T_B\\, where \\T_B\\ is the first observed
session after \\A\\, matching the coding of `trend_AB`.

**Denominator \\r'\theta\\.** Per draw, the untreated between-series
variance at \\B\\, \\\tau_0^2 + 2\tau\_{02}(B - T_A) + \tau_2^2 (B -
T_A)^2\\ (the last two terms only with random trends; \\T_A\\ is the
earliest first baseline session), the participant or cluster intercept
variance when present, and the within-series variance. brms
parameterizes AR(1) residuals by the innovation SD `sigma`, so the
stationary residual variance is \\\sigma^2 / (1 - \phi^2)\\, the level-1
variance.

**Times.** \\A\\ is the last session of the shortest baseline across
series. `ab_rule = "shortest_treatment"` sets \\B\\ from the shortest
treatment duration across cases;
[`scdhlm::default_times()`](https://jepusto.github.io/scdhlm/reference/default_times.html)
instead adds to \\A\\ the treatment duration of the case(s) with that
shortest baseline. With `ab_rule = "scdhlm"` (default) \\B\\ follows
scdhlm, so a comparison with its REML estimate is like for like.
`duration` sets \\B - A\\ directly, for a common benchmark duration
across the studies of a meta-analysis; `time_B` overrides both. For
reversal data both times refer to the first baseline phase and the first
treatment phase: \\B\\ never passes the end of the first treatment
phase. With `trend = "none"` and no random trend, \\\delta\_{AB}\\ does
not depend on \\A\\ or \\B\\.

**Uncertainty.** The REML estimator applies a small-sample correction
\\J\\; here the posterior of \\\delta\_{AB}\\ carries the uncertainty in
every parameter, so no \\J\\ correction is applied. Draws that are not
finite (for example from \\\|\phi\| = 1\\) are dropped from the summary
and counted in `n_nonfinite`.

The BC-SMD rests on Gaussian errors, so only `family = "gaussian"` is
accepted; count or proportion outcomes go through the one-stage GLMM
path
([`run_scd_analysis()`](https://mshin77.github.io/scdbayes/reference/run_scd_analysis.md),
[`marginal_contrast()`](https://mshin77.github.io/scdbayes/reference/marginal_contrast.md)).
scdbayes does not compute the within-case SMD, which is biased under
autocorrelation.

## Examples

``` r
if (interactive()) {
  d <- simulate_scd_data(n_cases = 4)
  bc_smd_bayes(d, design = "multiple_baseline", trend = "fixed")$summary
}
```
