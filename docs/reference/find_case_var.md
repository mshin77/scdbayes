# Locate the case-level grouping column by common SCED name patterns

Matches `case`/`participant`/`subject`/`id` (case-insensitive). Returns
the original column name.

## Usage

``` r
find_case_var(data)
```

## Arguments

- data:

  Data frame.

## Value

Column name, or `NULL` if no match.

## Examples

``` r
find_case_var(data.frame(participant = 1:3, outcome = rnorm(3)))
#> [1] "participant"
```
