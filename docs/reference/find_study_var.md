# Locate the study- / institution-level grouping column

Matches `study`/`study_id`/`site`/`school`/ `district`
(case-insensitive). Used for the highest grouping level (meta-analytic
study or institution).

## Usage

``` r
find_study_var(data)
```

## Arguments

- data:

  Data frame.

## Value

Column name, or `NULL` if no match.

## Examples

``` r
find_study_var(data.frame(study_id = c("S1", "S2"), outcome = c(1, 2)))
#> [1] "study_id"
```
