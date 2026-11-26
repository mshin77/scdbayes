# Posterior predictive checks on single-case visual-analysis features

Computes visual-analysis summary statistics on the observed data and on
posterior-predictive replicate datasets, then returns the posterior
predictive p-value for each statistic.

## Usage

``` r
posterior_predictive_features(
  model,
  data = NULL,
  statistics = .ppc_statistics_all,
  n_draws = 500L,
  mode = c("same", "new"),
  ar_method = c("gls", "moment"),
  pool = c("case_means", "all_obs"),
  design = NULL
)
```

## Arguments

- model:

  A fitted `brmsfit`.

- data:

  Data holding the outcome, case, phase and (optional) time columns;
  defaults to the model's data. Rows with a missing outcome are allowed:
  replicate values are set to `NA` there.

- statistics:

  Character vector selecting which statistics to compute (`"immediacy"`
  selects all five time points).

- n_draws:

  Number of posterior-predictive replicate datasets (default 500). The
  p-value is a proportion over these draws, so its Monte Carlo error is
  `sqrt(p * (1 - p) / n_draws)`: about 0.007 at the 0.025 flag boundary
  with 500 draws and about 0.022 with 50, large enough at small
  `n_draws` that a value near the boundary can flag by chance.

- mode:

  `"same"` replicates for the observed cases; `"new"` draws new case
  effects from the population posterior (`allow_new_levels = TRUE`) to
  assess generalization. In both modes an observation-level random
  effect (one level per fitted row) is redrawn rather than conditioned
  on, and replicate values are set to `NA` wherever the observed outcome
  is missing so both sides use the same rows.

- ar_method:

  `"gls"` (default) or `"moment"` estimator for the autocorrelation
  statistic (see Details). `"gls"` needs the nlme package and falls back
  to `"moment"` with a message when it is not installed; it fits one
  model per replicate, so it is slower.

- pool:

  `"case_means"` (default) or `"all_obs"`: how the pooled level and
  absence rows combine tiers (see Details).

- design:

  Design the model was fit with, as returned by
  [`detect_design_type`](https://mshin77.github.io/scdbayes/reference/detect_design_type.md).
  Only `"alternating"` changes the phase structure: conditions are
  pooled instead of being split into numbered runs. `NULL` (default)
  re-derives it from `data`, which can disagree with the design the
  model was fit with.

## Value

A list with `summary` (one row per statistic/tier/phase, with columns
`statistic`, `case` (the tier label, or `"pooled"`), `phase` (phase
occurrence such as `"B1"`), `observed`, `p_value`, `p_tie` (share of
replicates equal to the observed statistic), and `flag`; the pooled
`initial` row is left out when it duplicates the pooled `level` row, see
Details) and `draws` (long data frame of replicate values for density
plots), and `note` (`NULL`, or how replicates were conditioned for an
[`ar()`](https://rdrr.io/r/stats/ar.html) model and which requested
statistics were not computed and why). Empty data frames with a `note`
if the case, phase, or outcome column is missing; an error if `model` is
not a `brmsfit` or brms cannot produce posterior draws.

## Details

Statistics are computed per tier (each combination of the grouping
columns of
[`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md),
so a case identifier repeated across studies or a case measured on
several behaviors gives separate series) and per phase occurrence within
the tier: A1, B1, A2, B2, M1 (phase labels with a known role, see
[`phase_role()`](https://mshin77.github.io/scdbayes/reference/phase_role.md),
take the role letters; other labels keep their own). Each statistic is
computed in each phase for each participant and pooled across
participants. In an alternating-treatments design every session of a
condition forms one phase.

- `initial`: first baseline observation, compared with the replicate
  value at that session. When the model formula has no time term (a
  stable baseline level), every session of the initial baseline phase
  has the same predictive distribution, so each observation of that
  phase is compared with the replicate first session: the p-value
  averages the per-observation p-values and `observed` is the phase
  mean. The pooled row compares the mean of these tier phase means with
  the same mean computed on each replicate; under `pool = "case_means"`
  with `level` requested this equals the pooled level row of that
  baseline phase and is dropped. A variable in a fixed, random-effect,
  or smooth term counts as a time term when its name contains the time
  column name or starts with `time`, `session`, or `trend` (e.g.
  `session_c`). An [`ar()`](https://rdrr.io/r/stats/ar.html), `ma()`, or
  `arma()` term also counts.

- `level`, `trend`, `variability`, `absence`: within-phase mean, OLS
  slope, SD, and proportion of outcomes \\\le 0\\. Pooled rows average
  the tier means (level) and tier proportions (absence) with equal
  weight per tier under the default `pool = "case_means"`, or use all
  observations of the phase under `pool = "all_obs"`; the two differ
  only when tiers have unequal phase lengths. Trend pools the mean of
  the tier slopes and variability the within-tier pooled SD. Absence is
  computed for every family: for a continuous family it exposes a model
  that predicts values \\\le 0\\ the data never show (for example a
  Normal model of a rate).

- `immediacy_t1` to `immediacy_t5`: time-specific treatment effects
  (TSTE) at each of the first five intervention time points, the
  observation minus the prediction of the regression line fitted to the
  preceding baseline phase, one row per adjacent
  baseline-to-intervention transition and a pooled mean per time point.
  Withdrawal transitions are not used.

- `nonoverlap`: Tau between each pair of adjacent phases, the mean of
  the signs of all later-minus-earlier differences. Pairs are onsets
  (baseline to intervention, e.g. A1-B1, A2-B2) and, in reversal
  designs, withdrawals (intervention to baseline, e.g. B1-A2), reported
  as separate phase contrasts. The row's `phase` is the later phase of
  the pair, so `"A2"` is the B1-A2 withdrawal contrast.

- `autocorrelation`: one Level-1 lag-1 estimate pooled across tiers.
  `ar_method = "gls"` (default) fits a generalized least squares model
  with [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) by REML:
  an intercept and a slope for each phase occurrence within tier
  (`Case:Phase`, A1, B1, A2, B2) with a common AR(1) correlation over
  sessions within tier, started at 0. `ar_method = "moment"` is a faster
  moment estimate from time-ordered residuals after each phase's level
  and trend are removed. The same estimator is applied to observed and
  replicate data.

The role of each phase (baseline vs intervention) only pairs phases for
immediacy and non-overlap and colors the overlay plot. Immediacy and Tau
are reported as later minus earlier on every row, whatever the
intervention goal, so a decrease goal shows negative values. The
extremeness flag is two-sided, so the orientation does not change which
checks flag. The posterior predictive p-value is \\p = \Pr\[T(y^{rep})
\ge T(y)\]\\, the share of replicates whose statistic is at least the
observed one. Ties count fully, so a statistic equal to the observed
value in every replicate (e.g. absence = 0 throughout) gives \\p = 1\\
and flags. The share of ties, \\\Pr\[T(y^{rep}) = T(y)\]\\, is reported
as `p_tie`, so such a flag can be told apart from replicates that all
exceed the observed value. The flag is set when `p_value` is below 0.025
or above 0.975. Under a correct model p-values concentrate near 0.5
rather than spreading uniformly, so a flag is a strong signal and an
unflagged value is weak evidence of fit.

For a model with an [`ar()`](https://rdrr.io/r/stats/ar.html) term
fitted without `cov = TRUE`, brms predicts each occasion conditional on
the observed lagged residuals, so replicates track the observed series;
the autocorrelation check is then conservative and is best read
alongside the posterior of the `ar` coefficient. For non-Gaussian
families the [`ar()`](https://rdrr.io/r/stats/ar.html) term is a latent
residual that brms cannot predict for new data, so replicates are drawn
on the fitted rows conditional on the fitted latent residuals (and on
any observation-level effect), `mode = "new"` stops with an error, and
the returned `note` says so. Level and immediacy checks are then
conservative, while variability and autocorrelation replicates add
sampling noise on top of the fitted residuals and can flag without
misfit. With a Gaussian [`ar()`](https://rdrr.io/r/stats/ar.html) term,
`mode = "new"` still conditions each occasion on lagged residuals
computed from the observed outcomes and the new-case prediction, so
replicates are pulled toward the observed series; the `note` says so and
the generalization check is then conservative.

An observation-level random effect is redrawn: the `obs` levels are
relabeled and predicted with `allow_new_levels = TRUE`.

## Examples

``` r
if (interactive()) {
  fit <- run_scd_analysis(simulate_scd_data())$model
  posterior_predictive_features(fit, n_draws = 200)$summary
}
```
