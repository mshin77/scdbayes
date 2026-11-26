# Add an exposure offset to a count-model formula

Appends `offset(log(<exposure>))` to a Poisson or negative binomial
formula (zero-inflated or not), so the coefficients describe the rate
per unit of exposure (session length, number of intervals,
opportunities) rather than the raw count. The exposure column must be
numeric, present on every row, and strictly positive.

## Usage

``` r
add_exposure_offset(formula_str, data, exposure, family)
```

## Arguments

- formula_str:

  Model formula as a string.

- data:

  Data holding the exposure column.

- exposure:

  Name of the exposure column, or `NULL` to return `formula_str`
  unchanged.

- family:

  brms family name; `"poisson"`, `"negbinomial"`,
  `"zero_inflated_poisson"`, or `"zero_inflated_negbinomial"`.

## Value

The formula string with the offset term appended (unchanged when it
already carries that offset); an error when the exposure column or
family is not usable, or when the formula already holds a different
offset.

## Examples

``` r
d <- data.frame(outcome = c(3, 5, 2), level_AB = c(0, 1, 1), minutes = c(10, 20, 15))
add_exposure_offset("outcome ~ level_AB", d, "minutes", "poisson")
#> [1] "outcome ~ level_AB + offset(log(minutes))"
```
