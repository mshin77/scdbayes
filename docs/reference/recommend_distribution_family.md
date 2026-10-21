# Recommend distribution family based on outcome data

Recommend distribution family based on outcome data

## Usage

``` r
recommend_distribution_family(outcome_data)
```

## Arguments

- outcome_data:

  A numeric vector of outcome values.

## Value

A list with recommended family, reasoning, and optional warning.

## Examples

``` r
recommend_distribution_family(c(0, 1, 0, 1, 1, 0))
#> $recommended
#> [1] "Bernoulli (binary)"
#> 
#> $reasoning
#> [1] "Binary outcome with values 0, 1. Use Bernoulli (logistic) link."
#> 
#> $warning
#> NULL
#> 
recommend_distribution_family(rnorm(50))
#> $recommended
#> [1] "Gaussian (continuous)"
#> 
#> $reasoning
#> [1] "Approximately normal distribution (skew: 0.26, n=50). Gaussian family is appropriate."
#> 
#> $warning
#> NULL
#> 
recommend_distribution_family(rpois(50, lambda = 3))
#> $recommended
#> [1] "Negative Binomial"
#> 
#> $reasoning
#> [1] "Count data (range: 0-7) with overdispersion (1.24). Negative binomial handles excess variance."
#> 
#> $warning
#> NULL
#> 
```
