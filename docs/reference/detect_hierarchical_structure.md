# Detect hierarchical structure in data

Detect hierarchical structure in data

## Usage

``` r
detect_hierarchical_structure(data, max_levels = 5, method = "auto")
```

## Arguments

- data:

  A data frame containing outcome and potential grouping variables.

- max_levels:

  Integer specifying the maximum number of hierarchical levels to
  detect. Default is 5.

- method:

  Character string specifying the detection method. One of `"auto"`,
  `"variance"`, or `"information"`.

## Value

A list with components `levels`, `level_names`, `level_variables`,
`n_levels`, `units_per_level`, `nesting_structure`,
`cross_classification`, `recommendations`, and `icc_values`.

## Examples

``` r
d <- data.frame(
  case = rep(c("C1", "C2", "C3"), each = 8),
  phase = rep(c("A", "B"), 12),
  time = rep(1:8, 3),
  outcome = rnorm(24)
)
detect_hierarchical_structure(d)
#> $levels
#> $levels[[1]]
#> [1] "case"
#> 
#> 
#> $level_names
#> [1] "Level 2"
#> 
#> $level_variables
#> [1] "case"
#> 
#> $n_levels
#> [1] 1
#> 
#> $units_per_level
#> [1] 3
#> 
#> $nesting_structure
#> list()
#> 
#> $cross_classification
#> [1] FALSE
#> 
#> $recommendations
#> $recommendations$model_type
#> [1] "nested_random_effects"
#> 
#> $recommendations$estimation
#> [1] "bayesian"
#> 
#> $recommendations$estimation_message
#> [1] "Small number of units at higher levels ( 3 ). Bayesian estimation recommended."
#> 
#> 
#> $variance_based
#> $variance_based$case
#> $variance_based$case$variable
#> [1] "case"
#> 
#> $variance_based$case$level
#> [1] "level5"
#> 
#> $variance_based$case$between_variance
#> [1] 0.09516285
#> 
#> $variance_based$case$prop_variance
#> [1] 0.09053894
#> 
#> $variance_based$case$n_groups
#> [1] 3
#> 
#> 
#> 
#> $ic_based
#> $ic_based$case
#> $ic_based$case$variable
#> [1] "case"
#> 
#> $ic_based$case$n_groups
#> [1] 3
#> 
#> $ic_based$case$ic_score
#> [1] 3.534161
#> 
#> $ic_based$case$effective_sample_per_group
#> [1] 8
#> 
#> 
#> 
#> $crossed_factors
#> list()
#> 
#> $icc_values
#> $icc_values$case
#> [1] 0
#> 
#> 
```
