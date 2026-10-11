# Re-express the outcome in within-case residual-SD units

Scale harmonization for raw-data synthesis: fits an ordinary least
squares regression of the outcome on the fixed piecewise terms
separately for every tier (case, or case within study), then divides
that tier's outcome by the tier's residual standard deviation (root mean
squared error). The returned `<outcome>_std` column is in within-case
residual-SD units, so studies measured on different scales can enter one
multilevel model. This is scale harmonization, not an effect size: the
standardized coefficients are not standardized mean differences.
scdbayes does not compute the within-case SMD, which is biased under
autocorrelation. The design-comparable BC-SMD is estimated in a Bayesian
model
([`bc_smd_bayes()`](https://mshin77.github.io/scdbayes/reference/bc_smd_bayes.md)),
and studies are pooled with a Bayesian meta-analysis
([`meta_bayes()`](https://mshin77.github.io/scdbayes/reference/meta_bayes.md));
scdhlm/metafor scripts are optional frequentist comparisons.

## Usage

``` r
standardize_by_case(
  data,
  formula_fixed = NULL,
  outcome_var = "outcome",
  tiers = tier_vars(data)
)
```

## Arguments

- data:

  Data frame with the outcome and either a `phase` column or the
  piecewise columns named in `formula_fixed`.

- formula_fixed:

  Optional fixed-effects formula (formula object or string, with or
  without a left-hand side, e.g. `"~ time_A + level_AB + trend_AB"`).
  `NULL` uses the default fixed terms for the detected design. Only
  syntactic column names and
  [`is_safe_formula()`](https://mshin77.github.io/scdbayes/reference/is_safe_formula.md)
  syntax are accepted; function calls such as
  [`I()`](https://rdrr.io/r/base/AsIs.html) raise an error, and default
  terms skip non-syntactic names.

- outcome_var:

  Outcome column name.

- tiers:

  Columns whose combination defines one series; defaults to
  [`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md).

## Value

`data` with a numeric column `<outcome_var>_std`. The attribute
`"case_sigma"` holds one row per tier: the tier columns, `tier` (label),
`n` (non-missing observations), `p` (estimated coefficients), `sigma`
(residual SD, `NA` when not estimable), and `status`.

## Details

Each participant's scores are divided by the root mean squared error of
a participant-specific ordinary regression before the multilevel
meta-analysis. Standardizing adds error from estimating the scale
factor, so analyzing raw data is preferable when all cases share one
scale.

Caveat: the per-case OLS residual SD ignores autocorrelation.
Within-case standardized effect sizes are biased under autocorrelation,
with neither OLS nor multilevel synthesis performing adequately, and
raw-data synthesis of data standardized for each case may share the
problem. Use the transformation as a sensitivity analysis, not as the
primary synthesis metric.

The regression contains only fixed piecewise terms: no random effects,
no moderators, no autocorrelation. With `formula_fixed = NULL` the terms
are the package defaults for the design returned by
[`detect_design_type()`](https://mshin77.github.io/scdbayes/reference/detect_design_type.md)
after coding with
[`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md).
A supplied formula keeps its fixed terms and drops grouping terms such
as `(1 | case)` and [`ar()`](https://rdrr.io/r/stats/ar.html) terms.
Columns constant within a tier are aliased and do not count toward the
number of coefficients.

A tier is left `NA` (with a warning) when it has fewer than `p + 2`
non-missing observations, where `p` counts the estimated coefficients
including the intercept (so at least two residual degrees of freedom
remain), or when its residual SD is zero or not finite.

The transformation suits continuous or interval outcomes. For counts,
rates, and short ordinal scales the residual SD depends on the mean and
the result is approximate; there in particular, keep a count or ordinal
likelihood on the natural scale as the primary analysis.

## Examples

``` r
d <- simulate_scd_data(n_cases = 3, n_phase_a = 8, n_phase_b = 8)
out <- standardize_by_case(d)
attr(out, "case_sigma")
#>   case tier  n p    sigma status
#> 1   C1   C1 16 4 8.037677     ok
#> 2   C2   C2 16 4 7.817246     ok
#> 3   C3   C3 16 4 9.784271     ok
```
