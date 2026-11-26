# Validate SCD data

Validate SCD data

## Usage

``` r
validate_scd_data(data)
```

## Arguments

- data:

  A data frame to validate.

## Value

A list with valid, issues, warnings, recommendations, and diagnostics.

## Examples

``` r
d <- data.frame(
  case = rep(c("C1", "C2"), each = 10),
  phase = rep(c("A", "B"), 10),
  time = rep(1:10, 2),
  outcome = rnorm(20)
)
validate_scd_data(d)
#> $valid
#> [1] TRUE
#> 
#> $issues
#> list()
#> 
#> $warnings
#> $warnings[[1]]
#> [1] "Case C1 has phases with fewer than 3 observations"
#> 
#> $warnings[[2]]
#> [1] "Case C2 has phases with fewer than 3 observations"
#> 
#> $warnings[[3]]
#> [1] "Only 2 case(s). SCD meta-analysis typically requires at least 3 cases"
#> 
#> 
#> $recommendations
#> list()
#> 
#> $autocorr_cases
#> character(0)
#> 
#> $autocorr_warning
#> NULL
#> 
#> $trend_cases
#> character(0)
#> 
#> $trend_warning
#> NULL
#> 
#> $n_cases
#> [1] 2
#> 
#> $n_obs
#> [1] 20
#> 
#> $n_phases
#> [1] 2
#> 
```
