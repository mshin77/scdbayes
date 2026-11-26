# Posterior phase contrast on the response scale

Computes the posterior distribution of an end-of-phase contrast (e.g.,
phase B vs phase A) on the outcome's response scale using
[`brms::posterior_epred`](https://mc-stan.org/rstantools/reference/posterior_epred.html).
Supported for families with a scalar mean on the response scale
(gaussian, student, skew_normal, poisson, negbinomial,
zero_inflated_poisson, bernoulli, beta, Gamma). Ordinal and categorical
families (cumulative, sratio, cratio, acat, categorical) are **not**
supported, since a response-scale mean contrast is not well defined for
them; read those effects on the link scale via
[`summarize_fixed_effect_decisions()`](https://mshin77.github.io/scdbayes/reference/summarize_fixed_effect_decisions.md).
Designs with more than two phases (alternating-treatment,
changing-criterion) require explicit `from`/`to` labels.

## Usage

``` r
compute_phase_contrast(
  model,
  data = NULL,
  from = "A",
  to = "B",
  phase_var = "phase",
  case_var = "case",
  time_var = "time",
  at_session = c("last", "mean"),
  per_case = TRUE,
  threshold = NULL,
  rope = NULL,
  intervention_goal = c("increase", "decrease"),
  ci_level = 0.95
)
```

## Arguments

- model:

  A fitted `brmsfit`.

- data:

  Data frame; defaults to `model$data`.

- from:

  Character. Reference phase for the baseline side of the contrast
  (default `"A"`).

- to:

  Character. Reference phase for the treatment side of the contrast
  (default `"B"`).

- phase_var:

  Name of the phase column (default `"phase"`).

- case_var:

  Name of the case column (default `"case"`).

- time_var:

  Name of the time column (default `"time"`).

- at_session:

  `"last"` (last session of each phase, default) or `"mean"` (average
  across all sessions of each phase).

- per_case:

  Logical; include per-case contrasts (default `TRUE`).

- threshold:

  Numeric. A positive magnitude on the improvement-oriented scale (i.e.,
  after any `decrease` reflection). If supplied, posterior probability
  that the contrast meets or exceeds this clinically meaningful change
  is reported as `prob_meaningful_change`.

- rope:

  Numeric length-2. ROPE bounds on the response scale. For
  `intervention_goal = "decrease"` the bounds are reflected together
  with the contrast, so an asymmetric ROPE stays attached to the same
  region. If supplied, posterior probability inside the ROPE is reported
  as `rope_prob`.

- intervention_goal:

  `"increase"` (default) or `"decrease"`. When `"decrease"`, the
  contrast sign is flipped so positive values denote improvement, and
  the ROPE is reflected with it.

- ci_level:

  Credible interval width (default 0.95).

## Value

A list with:

- `population_contrast`:

  One-row data frame: posterior mean, SD, lower / upper credible bounds,
  probability of direction, optional `prob_meaningful_change` and
  `rope_prob`.

- `per_case_contrast`:

  Data frame of per-case contrasts with the same columns plus `case`.
  `NULL` if `per_case = FALSE`.

- `draws`:

  Numeric vector of posterior draws for the population contrast.

- `metadata`:

  Arguments recorded for downstream reporting.

## Details

Reference rows are drawn from the fitted data. With
`at_session = "last"`, the last session per case in each phase is used;
with `at_session = "mean"`, all sessions in each phase contribute and
are averaged per draw.

The population contrast is a typical-case contrast of observed rows: it
predicts with `re_formula = NA` (every random effect at zero) and
compares rows from the two phases, which sit at different sessions. On a
nonlinear link, the expected outcome at zero random effects is not the
population average, and the two sides differ in time as well as
condition.
[`marginal_contrast()`](https://mshin77.github.io/scdbayes/reference/marginal_contrast.md)
gives the population-averaged contrast at one common session, averaging
over new cases drawn from the random-effect distribution with and
without treatment. The per-case contrasts include each case's random
effects.

## Examples

``` r
if (interactive()) {
  dat <- simulate_scd_data()
  fit <- brms::brm(outcome ~ phase + (1 | case), data = dat, refresh = 0)
  compute_phase_contrast(fit, threshold = 5, rope = c(-1, 1))
}
```
