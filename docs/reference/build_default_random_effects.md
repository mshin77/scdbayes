# Random effects with explicit grouping columns

Builds the structure of
[`default_random_effects()`](https://mshin77.github.io/scdbayes/reference/default_random_effects.md)
from coded data with grouping columns that can be set explicitly; each
`NULL` falls back to the column
[`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md)
detects.

## Usage

``` r
build_default_random_effects(
  model_data,
  case_var = NULL,
  nest_var = NULL,
  study_var = NULL
)
```

## Arguments

- model_data:

  Coded data from
  [`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md).

- case_var, nest_var, study_var:

  Optional column names for the case-level grouping, the within-case
  grouping (e.g., behavior, skill, task), and the study-level grouping.

## Value

A character string representing the random effects formula, or `NULL`
when no case variable is found or the data hold a single series.

## Examples

``` r
d <- data.frame(
  case = rep(c("C1", "C2"), each = 10),
  phase = rep(c("A", "B"), 10),
  level_AB = rep(c(0, 1), 10),
  outcome = rnorm(20)
)
build_default_random_effects(d)
#> [1] "(1 + level_AB | case)"
```
