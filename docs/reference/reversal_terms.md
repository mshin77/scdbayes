# Reversal-design piecewise terms

Returns the first-run time terms followed by the per-transition step and
trend terms created by the reversal coder (e.g. `level_A1B1`,
`level_B1A2`, `level_A2B2`), in column order. Only first-run times enter
the model; the continuing `trend_*` terms carry every later slope. A
combined design whose tiers start in different phases carries one
first-run time per starting phase (e.g. `time_A1` and `time_B1`).

## Usage

``` r
reversal_terms(nm)
```

## Arguments

- nm:

  Column names of processed data.

## Value

Character vector of term names; empty when the data carry no reversal
coding.

## Examples

``` r
d <- data.frame(case = "P1", time = 1:12, outcome = rnorm(12),
                phase = rep(c("A", "B", "A", "B"), each = 3))
reversal_terms(names(process_scd_phases(d, list(), "reversal")))
#> [1] "time_A1"    "level_A1B1" "trend_A1B1" "level_B1A2" "trend_B1A2"
#> [6] "level_A2B2" "trend_A2B2"
```
