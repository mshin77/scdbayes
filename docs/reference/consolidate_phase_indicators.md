# Consolidate binary phase indicator columns into a single phase column

Consolidate binary phase indicator columns into a single phase column

## Usage

``` r
consolidate_phase_indicators(data)
```

## Arguments

- data:

  A data frame with binary phase indicator columns (A1, B1, etc.).

## Value

The data frame with a single phase column.

## Examples

``` r
d <- data.frame(
  A1 = c(1, 1, 0, 0),
  B1 = c(0, 0, 1, 1),
  outcome = rnorm(4)
)
consolidate_phase_indicators(d)
#>   A1 B1     outcome phase
#> 1  1  0  1.05253854    A1
#> 2  1  0  0.67768364    A1
#> 3  0  1  0.03849955    B1
#> 4  0  1 -0.35638119    B1
```
