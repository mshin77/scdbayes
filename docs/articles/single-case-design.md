# Single-Case Design

This guide shows a minimal call to
[`run_scd_analysis()`](https://mshin77.github.io/scdbayes/reference/run_scd_analysis.md).

## Data

Use one row per observation, with columns for case, time, phase, and
outcome. The following data are simulated.

``` r

library(scdbayes)

data <- simulate_scd_data(n_cases = 3, n_phase_a = 5, n_phase_b = 5,
                          effect_size = 1)
head(data[, c("case", "time", "phase", "outcome")], 4)
#>   case time phase  outcome
#> 1   C1    1     A 55.20589
#> 2   C1    2     A 39.20309
#> 3   C1    3     A 51.39238
#> 4   C1    4     A 49.15251
```

## Fit a model

``` r

result <- run_scd_analysis(data, options = list(family = "gaussian"))
result$diagnostics
```

Model fitting requires Stan and can take time. The result contains the
fitted model, summaries, and diagnostics. Inspect diagnostics before
interpreting results. See
[`?run_scd_analysis`](https://mshin77.github.io/scdbayes/reference/run_scd_analysis.md)
for arguments.
