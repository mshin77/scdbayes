# Default random-effects structure of a single-case data set

Returns the random-effects part of the default piecewise model, the same
string the package adds to the fixed terms when no formula is supplied.
Raw data (with a phase column but no coded columns) are first coded with
[`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md)
for `design`.

## Usage

``` r
default_random_effects(data, design = detect_design_type(data))
```

## Arguments

- data:

  Raw or coded single-case data.

- design:

  Design passed to
  [`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md)
  when `data` are not coded yet; defaults to
  [`detect_design_type()`](https://mshin77.github.io/scdbayes/reference/detect_design_type.md).

## Value

Character string such as `"(1 | study) + (1 + level_AB | study:case)"`,
or `NULL` when the data hold a single series.

## Details

Level-2 units are the tiers of
[`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md):
the combination of study, cluster, case, and within-case nest (behavior,
setting) columns, so a case identifier repeated in two studies is two
units. With two or more tiers every level-change coefficient of the
coded design (reversal steps, criterion steps, alternating-treatment
dummies, or `level_AB`/`level_BM`/`level_BG`) varies across tiers;
omitting a random slope that exists inflates the Type I error rate with
few cases. Each outer grouping (study, study:cluster,
study:cluster:case) with more levels than the grouping above it and
fewer than the tiers gets a random intercept, so behaviors or settings
form a level between measurements and participants and cases sit within
clusters and studies. Study, cluster, and nest columns constant in the
data are dropped; the case column is kept, so the tier term always names
cases.

In phase designs the onset step is correlated with the tier intercept
and later steps enter as uncorrelated (`||`) terms, a reduction of the
full random-effects covariance matrix that keeps the number of
correlations small when units are few. Alternating-treatment dummies
have no onset, so all of them enter as uncorrelated terms next to an
intercept-only tier term; no condition is singled out.

## Examples

``` r
default_random_effects(simulate_scd_data(n_cases = 3))
#> [1] "(1 + level_AB | case)"
```
