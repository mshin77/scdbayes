# Identify the treatment coefficient of a piecewise SCD model

Picks the first numeric treatment-onset column among `level_AB`, the
reversal onset step, changing-criterion steps, alternating-treatment
dummies, `treatment`, and `phase_num`. When `formula_str` is supplied,
only columns that appear as formula terms qualify.

## Usage

``` r
treatment_coef(data, formula_str = NULL)
```

## Arguments

- data:

  Processed data frame.

- formula_str:

  Optional character formula string.

## Value

Column name, or `NULL` when none qualifies.

## Examples

``` r
treatment_coef(data.frame(level_AB = c(0, 1), outcome = c(1, 2)))
#> [1] "level_AB"
```
