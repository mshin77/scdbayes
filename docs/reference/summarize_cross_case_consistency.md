# Posterior probability of consistent intervention direction across cases

For each fixed-effect term with a corresponding unit-level random
effect, returns the posterior probability that the unit-specific effect
(`b` plus the random effects of the unit and every enclosing grouping,
as in
[`summarize_per_case_effects()`](https://mshin77.github.io/scdbayes/reference/summarize_per_case_effects.md))
points in the expected direction for every unit, from the joint
posterior of the unit effects.

## Usage

``` r
summarize_cross_case_consistency(
  model,
  case_var = "case",
  ci_level = 0.95,
  intervention_goal = c("increase", "decrease")
)
```

## Arguments

- model:

  A fitted `brmsfit`.

- case_var:

  Name of the case-level grouping column in the model (default
  `"case"`).

- ci_level:

  Credible interval width (default 0.95).

- intervention_goal:

  `"increase"` (default) or `"decrease"`. The expected direction of a
  term is the goal direction, reversed for reversal withdrawal steps
  (see
  [`summarize_fixed_effect_decisions()`](https://mshin77.github.io/scdbayes/reference/summarize_fixed_effect_decisions.md)).

## Value

A data frame with columns `term`, `prob_all_positive`,
`prob_all_negative` (both on the native coefficient scale), and
`prob_consistent`, the probability that every case changes in the
expected direction (`prob_all_positive` when that direction is an
increase, `prob_all_negative` when it is a decrease).

## Examples

``` r
if (interactive()) {
  dat <- simulate_scd_data()
  fit <- brms::brm(outcome ~ phase + (1 + phase | case),
                   data = dat, refresh = 0)
  summarize_cross_case_consistency(fit)
}
```
