# Calculate meta-analytic summary statistics

Calculate meta-analytic summary statistics

## Usage

``` r
calculate_meta_summary(data, structure)
```

## Arguments

- data:

  A data frame containing effect size data.

- structure:

  A list describing the data structure, including grouping levels and
  variable names.

## Value

A list with elements `n_studies`, `n_effects`, `total_participants`
(distinct cases resolved via
[`find_case_var()`](https://mshin77.github.io/scdbayes/reference/find_case_var.md);
`NA` when no case column is found), and `effects_per_study`.

## Examples

``` r
d <- data.frame(
  study_id = rep(c("S1", "S2"), each = 10),
  participant = rep(1:4, 5),
  outcome = rnorm(20)
)
calculate_meta_summary(d, structure = list())
#> $n_studies
#> [1] 2
#> 
#> $n_effects
#> [1] 20
#> 
#> $total_participants
#> [1] 8
#> 
#> $effects_per_study
#> $effects_per_study$min
#> [1] 10
#> 
#> $effects_per_study$mean
#> [1] 10
#> 
#> $effects_per_study$median
#> [1] 10
#> 
#> $effects_per_study$max
#> [1] 10
#> 
#> 
```
