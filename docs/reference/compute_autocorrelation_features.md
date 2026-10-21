# Lag-1 autocorrelation per phase per case

Computes lag-1 autocorrelation per phase per case as derived features.

## Usage

``` r
compute_autocorrelation_features(
  data,
  outcome_var = "outcome",
  phase_var = "phase",
  case_var = "case",
  time_var = "time"
)
```

## Arguments

- data:

  Data frame containing SCD data.

- outcome_var:

  Name of the outcome variable.

- phase_var:

  Name of the phase variable.

- case_var:

  Name of the case variable.

- time_var:

  Name of the time variable.

## Value

The input data frame with added `autocor_A` and `autocor_B` columns.

## Examples

``` r
d <- simulate_scd_data(n_cases = 2, n_phase_a = 5, n_phase_b = 5)
compute_autocorrelation_features(d)
#>    case time phase phase_num time_center time_since_change  outcome  autocor_A
#> 1    C1    1     A         0           0                 0 47.91117 -0.3853909
#> 2    C1    2     A         0           1                 0 36.00590 -0.3853909
#> 3    C1    3     A         0           2                 0 52.58537 -0.3853909
#> 4    C1    4     A         0           3                 0 45.58201 -0.3853909
#> 5    C1    5     A         0           4                 0 55.68600 -0.3853909
#> 6    C1    6     B         1           5                 0 71.26850  0.0000000
#> 7    C1    7     B         1           6                 1 54.24858  0.0000000
#> 8    C1    8     B         1           7                 2 33.15718  0.0000000
#> 9    C1    9     B         1           8                 3 52.49402  0.0000000
#> 10   C1   10     B         1           9                 4 60.72838  0.0000000
#> 11   C2    1     A         0           0                 0 70.39369 -0.2314364
#> 12   C2    2     A         0           1                 0 54.49454 -0.2314364
#> 13   C2    3     A         0           2                 0 63.91814 -0.2314364
#> 14   C2    4     A         0           3                 0 54.26567 -0.2314364
#> 15   C2    5     A         0           4                 0 51.07584 -0.2314364
#> 16   C2    6     B         1           5                 0 50.22295  0.0000000
#> 17   C2    7     B         1           6                 1 56.03611  0.0000000
#> 18   C2    8     B         1           7                 2 47.37349  0.0000000
#> 19   C2    9     B         1           8                 3 44.71736  0.0000000
#> 20   C2   10     B         1           9                 4 51.92149  0.0000000
#>      autocor_B
#> 1   0.00000000
#> 2   0.00000000
#> 3   0.00000000
#> 4   0.00000000
#> 5   0.00000000
#> 6   0.03669897
#> 7   0.03669897
#> 8   0.03669897
#> 9   0.03669897
#> 10  0.03669897
#> 11  0.00000000
#> 12  0.00000000
#> 13  0.00000000
#> 14  0.00000000
#> 15  0.00000000
#> 16 -0.14253150
#> 17 -0.14253150
#> 18 -0.14253150
#> 19 -0.14253150
#> 20 -0.14253150
```
