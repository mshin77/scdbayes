# Build candidate model formulas

A coarse factor-level screen (intercept-only, level change, level +
trend) for LOO comparison. It uses the raw `phase`/`time` columns and is
deliberately simpler than the piecewise `time_A + level_AB + trend_AB`
parameterization used for the reported models; it is not a substitute
for that family.

## Usage

``` r
build_comparison_models(
  outcome_var,
  phase_var,
  case_var,
  time_var = NULL,
  has_autocor = FALSE
)
```

## Arguments

- outcome_var:

  Name of the outcome variable.

- phase_var:

  Name of the phase variable.

- case_var:

  Name of the case variable.

- time_var:

  Name of the time variable (optional).

- has_autocor:

  Logical; whether autocorrelation features are present.

## Value

A named list of model formula strings.

## Examples

``` r
build_comparison_models("outcome", "phase", "case", time_var = "time")
#> $`Intercept only`
#> [1] "outcome ~ 1 + (1|case)"
#> 
#> $`Level change only`
#> [1] "outcome ~ phase + (1|case)"
#> 
#> $`Level + trend`
#> [1] "outcome ~ phase + time + (1|case)"
#> 
```
