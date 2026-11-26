# Analyze continuous and categorical covariates

Analyze continuous and categorical covariates

## Usage

``` r
analyze_covariates(data, structure)
```

## Arguments

- data:

  A data frame containing covariates to analyze.

- structure:

  A list describing the data structure, used to exclude grouping and
  phase variables from covariate detection.

## Value

A list with elements `continuous` and `categorical`, each containing
descriptive statistics for detected covariates.

## Examples

``` r
d <- data.frame(
  case = rep(c("C1", "C2"), each = 10),
  phase = rep(c("A", "B"), 10),
  age = rnorm(20, 8, 1),
  gender = sample(c("M", "F"), 20, replace = TRUE),
  outcome = rnorm(20)
)
analyze_covariates(d, structure = list(levels = list(case = "case")))
#> $continuous
#> $continuous$age
#>    n missing_pct mean   sd  min  q25 median  q75   max
#> 1 20           0 7.69 1.12 5.56 6.96   7.75 8.52 10.07
#> 
#> 
#> $categorical
#> $categorical$gender
#> $categorical$gender$levels
#> [1] "F" "M"
#> 
#> $categorical$gender$frequencies
#> [1] 12  8
#> 
#> $categorical$gender$proportions
#> 
#>  F  M 
#> 60 40 
#> 
#> 
#> 
```
