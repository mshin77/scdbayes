# Main analysis function

Main analysis function

## Usage

``` r
run_scd_analysis(data, options = list())
```

## Arguments

- data:

  Data frame containing SCD data with columns `case`, `phase`, and
  `outcome`.

- options:

  Named list of analysis options. Recognized entries: `formula` (model
  formula or string; defaults to a design-based piecewise formula),
  `scd_type` (design passed to
  [`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md);
  defaults to
  [`detect_design_type()`](https://mshin77.github.io/scdbayes/reference/detect_design_type.md)),
  `family` (brms family name, default `"gaussian"`; `"binomial"` needs a
  numeric trials column, see Details), `prior_type` (`"default"`,
  `"informative"`, `"meta_analytic"`, or `"sensitivity"`),
  `prior_effect_mean`/`prior_effect_sd` (for the meta-analytic prior),
  `intervention_goal` (`"increase"` or `"decrease"`, passed to the
  posterior decision summary), `expected_effect_size`, `quick_test`,
  `prior_predictive` (logical; when `TRUE` adds a prior predictive check
  to the diagnostics), and `exposure` (name of a positive exposure
  column for the `"poisson"`, `"negbinomial"`,
  `"zero_inflated_poisson"`, or `"zero_inflated_negbinomial"` family;
  adds `offset(log(<exposure>))` through
  [`add_exposure_offset()`](https://mshin77.github.io/scdbayes/reference/add_exposure_offset.md)).

## Value

An object of class `scd_analysis` containing data summary, power
estimate, fitted model, effect sizes, a posterior decision summary
(`decisions`), and diagnostics.

## Details

The decision summary reports a ROPE probability for level-change terms
only, with the region of
[`default_rope()`](https://mshin77.github.io/scdbayes/reference/default_rope.md).
Slope terms (`trend_*`, `time_*`) get `NA`: a per-session slope is
negligible or not only relative to the number of sessions it is
projected over.

Proportions of a known number of trials or intervals can be modeled with
`family = "binomial"` when the data carry a numeric `trials`,
`n_trials`, `opportunities`, or `intervals` column; the response is then
`outcome | trials(<col>)`. The beta family stops on outcomes at 0 or 1
and points to the (y \* (n - 1) + 0.5) / n squeeze or the binomial
family.

A formula with an [`ar()`](https://rdrr.io/r/stats/ar.html) term warns
when time differences within a series are not constant or when outcomes
are missing: brms lags by row order, while an AR(1) model assumes
equidistant occasions, and dropping missed sessions alters the series.

## Examples

``` r
if (interactive()) {
run_scd_analysis(simulate_scd_data())
}
```
