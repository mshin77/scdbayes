# Check that a model formula uses only data columns and brms term syntax

Formula text typed into the app reaches `brms` and
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html), which
evaluate calls such as `I(q())`. This check accepts data columns, the
constants `0` and `1`, the operators `~ + : * | || / - (`, and the brms
helpers [`ar()`](https://rdrr.io/r/stats/ar.html) (named `time`, `gr`,
`cov` arguments), `gr()`, `trials()`, and the exposure offset
`offset(log(<column>))`; every other function call, string, `$`, `@`, or
`::` is rejected.

## Usage

``` r
is_safe_formula(x, allowed_vars)
```

## Arguments

- x:

  Formula string, either a full formula (`"y ~ x + (1 | case)"`) or a
  right-hand side.

- allowed_vars:

  Column names of the analysis data.

## Value

Logical scalar.

## Examples

``` r
is_safe_formula("y ~ level_AB + (1 | case)", c("y", "level_AB", "case"))
#> [1] TRUE
is_safe_formula("y ~ I(q())", "y")
#> [1] FALSE
```
