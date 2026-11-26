# Locate the phase column by common SCED name patterns

Matches `phase`/`condition`/`treatment_phase` (case-insensitive).

## Usage

``` r
find_phase_var(data)
```

## Arguments

- data:

  Data frame.

## Value

Column name, or `NULL` if no match.

## Examples

``` r
find_phase_var(data.frame(condition = c("A", "B"), outcome = c(1, 2)))
#> [1] "condition"
```
