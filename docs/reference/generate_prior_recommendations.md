# Generate prior recommendations

Generate prior recommendations

## Usage

``` r
generate_prior_recommendations(data_chars, structure)
```

## Arguments

- data_chars:

  A list of data characteristics (from `analyze_data_characteristics`).

- structure:

  A list describing the hierarchical structure.

## Value

A list of recommendation strings covering sample size, distribution,
hierarchy, missing data, and overall prior strength.

## Examples

``` r
data_chars <- list(n_obs = 30, heavy_tailed = FALSE, missing_rate = 0.05)
generate_prior_recommendations(data_chars, list(n_levels = 2))
#> $sample_size
#> [1] "Small sample: run a prior predictive check and a prior sensitivity analysis before interpreting the posterior."
#> 
#> $overall
#> [1] "Prior strength: weakly informative, scaled to the outcome SD or the link scale."
#> 
```
