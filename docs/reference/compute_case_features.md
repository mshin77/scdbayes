# Per-case descriptive features

Per-case descriptive features

## Usage

``` r
compute_case_features(
  data,
  outcome_var = "outcome",
  phase_var = "phase",
  case_var = "case",
  time_var = "time",
  intervention_goal = c("increase", "decrease")
)
```

## Arguments

- data:

  Data frame containing SCD data.

- outcome_var:

  Name of the outcome variable.

- phase_var:

  Name of the phase variable.

- case_var:

  Name of the case variable.

- time_var:

  Name of the time variable.

- intervention_goal:

  Direction of a desirable change: `"increase"` (default) or
  `"decrease"`. For `"decrease"`, `NAP` is reported as `1 - NAP` and
  `PND` counts phase-B points below the phase-A minimum.

## Value

A data frame of summary features with one row per tier (each combination
of the grouping columns of
[`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md),
so a case identifier repeated across studies or a case measured on
several behaviors gives separate rows), or `NULL` if requirements are
not met. `case` holds the case identifier and `tier` the full tier label
(e.g. `"S1 / P1 / reading"`). Within each tier, the `_A` features use
the first run of a baseline-role phase (see
[`phase_role()`](https://mshin77.github.io/scdbayes/reference/phase_role.md))
and the `_B` features the next intervention-role run, so labels such as
`"Baseline 1"` and later reversal phases do not change which phases are
compared. The nonoverlap descriptives `NAP` and `PND` are computed in
the direction given by `intervention_goal`.

## Examples

``` r
d <- simulate_scd_data(n_cases = 2, n_phase_a = 6, n_phase_b = 6)
compute_case_features(d)
#>   case tier   mean_A   mean_B      sd_A      sd_B    slope_A     slope_B
#> 1   C1   C1 46.64336 48.05030  9.124774  6.483815  0.3550227 -0.09015397
#> 2   C2   C2 57.23262 48.13144 10.194156 11.155911 -3.4646165 -1.01825202
#>        cv_A      cv_B   autocor_A  autocor_B mean_diff pooled_SD       NAP
#> 1 0.1956286 0.1349381 -0.18254586  0.1115179  1.406941  7.915218 0.5833333
#> 2 0.1781179 0.2317801 -0.09699311 -0.1550718 -9.101179 10.685859 0.2222222
#>         PND phase_length_ratio trend_accel_A trend_accel_B
#> 1 0.1666667                  1     2.6576965     -13.22389
#> 2 0.0000000                  1     0.4468388     -13.09183
```
