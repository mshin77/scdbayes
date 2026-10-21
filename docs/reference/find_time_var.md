# Locate the time column by common SCED name patterns

Matches `time`/`session`/`occasion`/`wave`/ `timepoint`/`observation`
(case-insensitive).

## Usage

``` r
find_time_var(data)
```

## Arguments

- data:

  Data frame.

## Value

Column name, or `NULL` if no match.

## Examples

``` r
find_time_var(data.frame(session = 1:5, outcome = rnorm(5)))
#> [1] "session"
```
