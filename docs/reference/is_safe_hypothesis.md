# Check that a hypothesis string uses only model parameters and arithmetic

[`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html)
evaluates its string as R code, so any function call typed into a
hypothesis box runs on the server. This check parses each side of a
single `=`, `==`, `<`, or `>` relation and accepts only parameter names,
numeric constants, and the operators `+ - * / ^ ( :`.

## Usage

``` r
is_safe_hypothesis(x, params)
```

## Arguments

- x:

  Hypothesis string such as `"level_AB > 0"`.

- params:

  Parameter names of the fitted model, for example
  `brms::variables(fit)`; names with and without the `b_` prefix are
  both accepted.

## Value

Logical scalar.

## Examples

``` r
is_safe_hypothesis("level_AB > 0", c("b_Intercept", "b_level_AB"))
#> [1] TRUE
is_safe_hypothesis("level_AB*0 + q() = 0", "b_level_AB")
#> [1] FALSE
```
