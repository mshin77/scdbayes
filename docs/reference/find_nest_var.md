# Detect a within-case nesting variable by name pattern then structure

Detect a within-case nesting variable by name pattern then structure

## Usage

``` r
find_nest_var(
  data,
  exclude = character(0),
  case_var = NULL,
  time_var = find_time_var(data)
)
```

## Arguments

- data:

  Data frame.

- exclude:

  Character vector of column names to skip (e.g., case, study, phase,
  time, outcome, piecewise terms).

- case_var:

  Optional case-grouping column. When supplied, a candidate must vary
  within at least one case; a column constant within every case is a
  between-case covariate and is rejected.

- time_var:

  Time column used by the structural fallback; defaults to
  `find_time_var(data)`.

## Value

Column name (length-1 character), or `NULL` if no candidate is found.

## Details

A column whose name matches a nest pattern (behavior, setting, target,
skill, ...) qualifies when it varies within a case. Any other
non-numeric column, or integer-coded column when a case column is known,
with 2-20 levels (criterion, trials, IOA, and coded piecewise columns
excluded) qualifies only when it splits concurrent series (time points
repeat within a case but not within a case-by-column cell) or when phase
onsets differ across its levels within a case, so a date or note column
never becomes a nest. Without a time column the structural fallback is
skipped.

## Examples

``` r
d <- data.frame(
  case = rep("Ian", 8),
  behavior = rep(c("greet", "share"), 4),
  outcome = rnorm(8)
)
find_nest_var(d, exclude = "case")
#> [1] "behavior"
```
