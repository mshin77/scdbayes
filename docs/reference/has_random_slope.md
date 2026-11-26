# Detect a random slope in a model formula

Parses the formula and inspects every grouping term written as
`(lhs | g)` or `(lhs || g)`. A term carries a random slope when its
left-hand side names a variable; constant-only sides such as `1`,
`0 + 1`, or the brms ID form `(1 | ID | g)` do not.

## Usage

``` r
has_random_slope(formula_str)
```

## Arguments

- formula_str:

  Character formula string.

## Value

Logical scalar; `FALSE` when the string does not parse.

## Examples

``` r
has_random_slope("outcome ~ level_AB + (1 | case)")
#> [1] FALSE
has_random_slope("outcome ~ level_AB + (0 + level_AB || case)")
#> [1] TRUE
```
