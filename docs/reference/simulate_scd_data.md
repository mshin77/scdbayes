# Simulate SCD data

Generates simulated single-case design data using a piecewise multilevel
model.

## Usage

``` r
simulate_scd_data(
  n_cases = 5,
  n_phase_a = 5,
  n_phase_b = 5,
  effect_size = 0,
  trend_effect = 0,
  autocorr = 0,
  baseline_mean = 50,
  baseline_sd = 10,
  baseline_trend = 0
)
```

## Arguments

- n_cases:

  Number of cases to simulate.

- n_phase_a:

  Number of time points in phase A.

- n_phase_b:

  Number of time points in phase B.

- effect_size:

  Standardized effect size for the phase change.

- trend_effect:

  Standardized trend effect after phase change.

- autocorr:

  Lag-1 autocorrelation coefficient.

- baseline_mean:

  Mean of the baseline phase.

- baseline_sd:

  Standard deviation of the baseline phase.

- baseline_trend:

  Linear trend in the baseline phase.

## Value

A data frame with columns `case`, `time`, `phase`, `phase_num`,
`time_center`, `time_since_change`, and `outcome`.

## Examples

``` r
simulate_scd_data(n_cases = 3, n_phase_a = 5, n_phase_b = 5,
                  effect_size = 1.0)
#>    case time phase phase_num time_center time_since_change  outcome
#> 1    C1    1     A         0           0                 0 57.19016
#> 2    C1    2     A         0           1                 0 43.01995
#> 3    C1    3     A         0           2                 0 31.05874
#> 4    C1    4     A         0           3                 0 50.76299
#> 5    C1    5     A         0           4                 0 58.75309
#> 6    C1    6     B         1           5                 0 64.53827
#> 7    C1    7     B         1           6                 1 51.49283
#> 8    C1    8     B         1           7                 2 65.66202
#> 9    C1    9     B         1           8                 3 71.52212
#> 10   C1   10     B         1           9                 4 52.43803
#> 11   C2    1     A         0           0                 0 45.10742
#> 12   C2    2     A         0           1                 0 38.33948
#> 13   C2    3     A         0           2                 0 45.20331
#> 14   C2    4     A         0           3                 0 51.15348
#> 15   C2    5     A         0           4                 0 32.31952
#> 16   C2    6     B         1           5                 0 45.92361
#> 17   C2    7     B         1           6                 1 67.09178
#> 18   C2    8     B         1           7                 2 47.59157
#> 19   C2    9     B         1           8                 3 56.31673
#> 20   C2   10     B         1           9                 4 64.62080
#> 21   C3    1     A         0           0                 0 46.77167
#> 22   C3    2     A         0           1                 0 37.12785
#> 23   C3    3     A         0           2                 0 39.69960
#> 24   C3    4     A         0           3                 0 65.14089
#> 25   C3    5     A         0           4                 0 53.46904
#> 26   C3    6     B         1           5                 0 77.79442
#> 27   C3    7     B         1           6                 1 63.86631
#> 28   C3    8     B         1           7                 2 50.81305
#> 29   C3    9     B         1           8                 3 44.15664
#> 30   C3   10     B         1           9                 4 59.15941
```
