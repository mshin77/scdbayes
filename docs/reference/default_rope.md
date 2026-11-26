# Default region of practical equivalence for treatment coefficients

On identity links (gaussian, student, skew_normal) the region is +/- 0.1
SD of the outcome, and on logit links +/- 0.1 \* pi / sqrt(3) (about
0.18), a tenth of the SD of the standard logistic distribution. On log
links the region is +/- log(1.1), a 10\\ All defaults are conventions,
not substantive thresholds; supply a context-specific ROPE when one is
known. Other links require explicit bounds on their own scale.

## Usage

``` r
default_rope(data, family = "gaussian", response = "outcome")
```

## Arguments

- data:

  Data frame with the response column.

- family:

  brms family name or family object.

- response:

  Response column name.

## Value

Numeric length-2 vector `c(lower, upper)` on the link scale.

## Examples

``` r
default_rope(simulate_scd_data(n_cases = 3), "gaussian")
#> [1] -1.120338  1.120338
default_rope(simulate_scd_data(n_cases = 3), "poisson")
#> [1] -0.09531018  0.09531018
```
