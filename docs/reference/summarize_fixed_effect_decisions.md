# Posterior decision summary for fixed-effect coefficients

For each fixed-effect coefficient (`b_*`) in the model, returns
posterior mean, SD, credible interval, probability of direction (`pd`),
and optional probability of meaningful change and ROPE-probability.
Posteriors are on the link scale (Stan parameterization).

## Usage

``` r
summarize_fixed_effect_decisions(
  model,
  threshold = NULL,
  rope = NULL,
  intervention_goal = c("increase", "decrease"),
  ci_level = 0.95
)
```

## Arguments

- model:

  A fitted `brmsfit`.

- threshold:

  Numeric. A positive magnitude expressed on the improvement-oriented
  scale (i.e., after any `decrease` reflection). If supplied, the
  posterior probability that the effect meets or exceeds this magnitude
  in the goal direction is reported as `prob_meaningful_change`. It
  applies to level terms only: slope terms (`trend_*`, `time_*`) get
  `NA`, since a per-session slope is meaningful or not only relative to
  the number of sessions it is projected over.

- rope:

  Numeric length-2. ROPE bounds on the link scale, expressed on the
  native coefficient scale. For `intervention_goal = "decrease"` the
  bounds are reflected together with the draws, so an asymmetric ROPE
  stays attached to the same parameter region. If supplied, posterior
  probability inside the ROPE is reported as `rope_prob` for level
  terms; slope terms get `NA`.
  [`default_rope()`](https://mshin77.github.io/scdbayes/reference/default_rope.md)
  gives the default region.

- intervention_goal:

  `"increase"` (default) or `"decrease"`. For `"decrease"`, every
  reported coefficient is sign-flipped so that a positive value denotes
  improvement (full reflection onto an improvement-oriented scale); `pd`
  is unchanged and the ROPE is reflected with the draws. Reversal
  withdrawal steps (an intervention-role phase followed by a
  baseline-role phase, e.g. `level_B1A2`,
  `trend_Intervention1Baseline2`) remove the treatment, so they take the
  opposite reflection: under `"increase"` they are sign-flipped and
  under `"decrease"` they are not. A positive value on every row
  therefore reads as change in the direction the intervention predicts
  (improvement at onset, return toward baseline at withdrawal).
  Intercept and ordinal threshold terms (`b_Intercept`) and coefficients
  of distributional parameters (`b_sigma_*`, `b_nu_*`, `b_phi_*`,
  `b_shape_*`, `b_zi_*`, `b_hu_*`, `b_disc_*`, ...) are excluded from
  the table, so only treatment, trend, and moderator slopes on the mean
  are reported.

- ci_level:

  Credible interval width (default 0.95).

## Value

A data frame with one row per fixed-effect term and columns `term`,
`mean`, `sd`, `lower`, `upper`, `pd`, `prob_meaningful_change`,
`rope_prob`. For a log link on a count family, the response-scale
incidence-rate ratio columns `irr`, `irr_lower`, `irr_upper` are
attached; for a log-link Gamma (or Weibull/exponential) family, the
ratio of means `mean_ratio`; for a lognormal family, the ratio of
medians `median_ratio`; for a logit link, the odds-ratio columns `or`,
`or_lower`, `or_upper`. Each with its `_lower`/`_upper` bounds. These
ratios are [`exp()`](https://rdrr.io/r/base/Log.html) of the native
(unreflected) coefficient, so they carry the conventional definition
regardless of `intervention_goal`; only the `mean`/`lower`/`upper`
columns are reflected for direction. An identity link attaches nothing,
since link scale equals response scale. When a ratio is attached, a
`ratio_type` column reads `"conditional"`: on a nonlinear link, `exp(b)`
is the ratio for a typical case (all random effects at zero), not the
population-averaged (marginal) ratio, and the two differ when random
effects are present. Label these columns as typical-case ratios;
[`marginal_contrast()`](https://mshin77.github.io/scdbayes/reference/marginal_contrast.md)
gives the population-averaged ratio. In a hurdle or zero-inflated family
the `irr` columns are a ratio on the conditional count component only,
not on the marginal expectation, which also moves with the hurdle or
zero-inflation part.

## Examples

``` r
if (interactive()) {
  dat <- simulate_scd_data()
  fit <- brms::brm(outcome ~ phase + (1 | case), data = dat, refresh = 0)
  summarize_fixed_effect_decisions(fit, threshold = 0.5,
                                   rope = c(-0.1, 0.1))
}
```
