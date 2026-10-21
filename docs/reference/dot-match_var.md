# Match a column name (case-insensitive) against a pattern list

Ranked by position in `patterns`, not by column order, so a data frame
holding both `session` and `time` resolves to the pattern listed first
rather than whichever column happens to come first.

## Usage

``` r
.match_var(data, patterns)
```

## Arguments

- data:

  Data frame.

- patterns:

  Character vector of lowercase patterns, most specific first.

## Value

Original column name (preserving case), or `NULL`.
