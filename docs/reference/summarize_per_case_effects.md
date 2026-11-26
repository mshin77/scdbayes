# Per-case posterior decision summary

For each unit and each fixed-effect term that has a corresponding
unit-level random effect, returns the unit-specific posterior (`b + u`)
summarized with decision quantities, on the link scale.

## Usage

``` r
summarize_per_case_effects(
  model,
  case_var = "case",
  threshold = NULL,
  rope = NULL,
  intervention_goal = c("increase", "decrease"),
  ci_level = 0.95
)
```

## Arguments

- model:

  A fitted `brmsfit`.

- case_var:

  Name of the case-level grouping column in the model (default
  `"case"`).

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

A data frame with columns `case` (the unit label), `term`, `mean`, `sd`,
`lower`, `upper`, `pd`, `prob_meaningful_change`, `rope_prob`, plus the
response-scale ratio columns of
[`summarize_fixed_effect_decisions()`](https://mshin77.github.io/scdbayes/reference/summarize_fixed_effect_decisions.md)
on a log or logit link. Those are `exp(b + u)`, the ratio conditional on
that case's random effects, and carry `ratio_type = "conditional"`; they
are not population-averaged ratios (see
[`marginal_contrast()`](https://mshin77.github.io/scdbayes/reference/marginal_contrast.md)).

## Details

The unit is the grouping factor that contains `case_var` and carries
random slopes on fixed-effect terms, the innermost one when several do:
with the default structure `(1 | case) + (1 + level_AB | case:behavior)`
the units are case-behavior tiers, labeled `"<case>_<behavior>"` as brms
labels them. Random effects of every enclosing grouping (`study`,
`study:case`, `case`) on the same term are added, so the unit posterior
is the full `b + u_study + u_case + u_tier`.

## Examples

``` r
if (interactive()) {
  dat <- simulate_scd_data()
  fit <- brms::brm(outcome ~ phase + (1 + phase | case),
                   data = dat, refresh = 0)
  summarize_per_case_effects(fit, threshold = 0.5)
}
```
