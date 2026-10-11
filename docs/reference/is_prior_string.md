# Check that a prior string is a single distribution call

Accepts one call to an allowlisted distribution whose arguments are all
finite numeric constants, such as `"normal(0, 1)"` or
`"student_t(3, 0, 2.5)"`. Statement separators, braces, `target`
increments, nested calls (`reject()`,
[`exp()`](https://rdrr.io/r/base/Log.html)), `constant()`, and symbols
are rejected. With `class`, the distribution must suit that parameter
class: location families for `b` and `Intercept`; `student_t`, `normal`,
`exponential`, `gamma`, `inv_gamma`, `cauchy` for scale classes (brms
adds the lower bound); `lkj` for `cor`; and `normal` or `uniform` for
`ar` and `ma`, whose location must lie inside \[-1, 1\] and, for
`uniform`, both bounds as well.

## Usage

``` r
is_prior_string(x, class = NULL)
```

## Arguments

- x:

  Prior string.

- class:

  Optional brms parameter class.

## Value

Logical scalar.

## Details

Each distribution takes its own number of arguments, and scale, degrees
of freedom, shape, rate, and `eta` arguments must be positive:
`normal(mu, sigma)`, `student_t(nu, mu, sigma)`, `cauchy(mu, sigma)`,
`skew_normal(xi, omega, alpha)`, `logistic(mu, sigma)`,
`double_exponential(mu, sigma)`, `exponential(rate)`, `gamma(a, b)`,
`inv_gamma(a, b)`, `lkj(eta)`, `lkj_corr_cholesky(eta)`, and
`uniform(a, b)` with `a < b`. `uniform` is accepted only with `class`
`"ar"` or `"ma"`; without `class` or for any other class it is rejected.
Numbers must be plain decimals (optionally with an exponent);
hexadecimal or `L`-suffixed literals, comments, and line breaks are
rejected.

## Examples

``` r
is_prior_string("normal(0, 1)")
#> [1] TRUE
is_prior_string("student_t(3, 0, 2.5)", class = "sd")
#> [1] TRUE
is_prior_string("normal(0, -1)")
#> [1] FALSE
is_prior_string("uniform(-1, 1)", class = "ar")
#> [1] TRUE
```
