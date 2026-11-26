# Detect hierarchy levels in SCD data

Detect hierarchy levels in SCD data

## Usage

``` r
detect_hierarchy_levels(data)
```

## Arguments

- data:

  A data frame.

## Value

A list describing detected levels: n_levels, level variables, and unit
counts.

## Examples

``` r
d <- data.frame(case = rep(c("A", "B"), each = 5), time = rep(1:5, 2),
                outcome = rnorm(10))
detect_hierarchy_levels(d)
#> $n_levels
#> [1] 2
#> 
#> $level1_var
#> [1] "time"
#> 
#> $level2_var
#> [1] "case"
#> 
#> $level3_var
#> NULL
#> 
#> $level4_var
#> NULL
#> 
#> $structure
#> [1] "two_level"
#> 
#> $n_level1
#> [1] 10
#> 
#> $n_level2
#> [1] 2
#> 
```
