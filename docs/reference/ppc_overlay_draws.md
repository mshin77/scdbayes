# Posterior-predictive line draws for a spaghetti overlay

Returns the observed single-case series alongside a small number of
posterior-predictive replicate series, shaped for a line-graph overlay
colored by baseline vs intervention condition.

## Usage

``` r
ppc_overlay_draws(
  model,
  data = NULL,
  n_lines = 20L,
  mode = c("same", "new"),
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

- n_lines:

  Number of replicate series to return.

- mode:

  `"same"` replicates for the observed cases; `"new"` draws new case
  effects from the population posterior (`allow_new_levels = TRUE`) to
  assess generalization. In both modes an observation-level random
  effect (one level per fitted row) is redrawn rather than conditioned
  on, and replicate values are set to `NA` wherever the observed outcome
  is missing so both sides use the same rows.

- design:

  Design the model was fit with, as returned by
  [`detect_design_type`](https://mshin77.github.io/scdbayes/reference/detect_design_type.md).
  Only `"alternating"` changes the phase structure: conditions are
  pooled instead of being split into numbered runs. `NULL` (default)
  re-derives it from `data`, which can disagree with the design the
  model was fit with.

## Value

A list with `observed` and `replicates` data frames (columns `case`
(tier label), `time`, `condition`, `phase` (phase occurrence such as
`"A1"`), `outcome`, and `draw` for replicates); `NULL` if the case,
phase, or outcome column is missing, and an error if brms cannot produce
posterior draws.

## Examples

``` r
if (interactive()) {
  fit <- run_scd_analysis(simulate_scd_data())$model
  str(ppc_overlay_draws(fit, n_lines = 5))
}
```
