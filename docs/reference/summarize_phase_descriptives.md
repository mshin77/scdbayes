# Pre-model descriptive phase summary

Raw-scale descriptive summary of a two-phase contrast (typically phase A
vs B). Reports means, SDs, sample sizes per phase and the raw mean
difference on the response scale. No standardized effect sizes; use
[`compute_phase_contrast()`](https://mshin77.github.io/scdbayes/reference/compute_phase_contrast.md)
on a fitted brmsfit for posterior decision quantities.

## Usage

``` r
summarize_phase_descriptives(
  data,
  phase_var = "phase",
  outcome_var = "outcome"
)
```

## Arguments

- data:

  A data frame.

- phase_var:

  Phase column name.

- outcome_var:

  Outcome column name.

## Value

A list with `raw_difference`, `phase_a_mean`, `phase_b_mean`,
`phase_a_sd`, `phase_b_sd`, `phase_a_n`, `phase_b_n`. NA fields when
phases or outcome are not usable.

## Examples

``` r
summarize_phase_descriptives(simulate_scd_data(n_cases = 2))
#> $raw_difference
#> [1] 2.955497
#> 
#> $phase_a_mean
#> [1] 49.81406
#> 
#> $phase_b_mean
#> [1] 52.76956
#> 
#> $phase_a_sd
#> [1] 6.911064
#> 
#> $phase_b_sd
#> [1] 12.30167
#> 
#> $phase_a_n
#> [1] 10
#> 
#> $phase_b_n
#> [1] 10
#> 
```
