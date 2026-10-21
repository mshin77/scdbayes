# Grouping columns that define one data series (tier)

Returns the grouping columns present, ordered from outermost to
innermost: study
([`find_study_var()`](https://mshin77.github.io/scdbayes/reference/find_study_var.md)),
cluster
([`find_cluster_var()`](https://mshin77.github.io/scdbayes/reference/find_cluster_var.md)),
case
([`find_case_var()`](https://mshin77.github.io/scdbayes/reference/find_case_var.md)),
and a within-case nest variable such as behavior or setting
([`find_nest_var()`](https://mshin77.github.io/scdbayes/reference/find_nest_var.md)).
Each combination of these columns is one tier: one AB series whose
change points are coded at its own onset.

## Usage

``` r
tier_vars(data, time_var = find_time_var(data))
```

## Arguments

- data:

  Data frame.

- time_var:

  Time column used to decide whether a nest variable splits concurrent
  series; defaults to `find_time_var(data)`.

## Value

Character vector of column names (possibly empty).

## Details

A within-case nest variable is kept when time points repeat within the
study-cluster-case key, that is, when a case carries several
concurrently measured series, or, for a column named like a nest
(behavior, setting, skill, ...), when phase onsets differ across its
levels within a case (a multiple-baseline design across behaviors whose
sessions do not overlap in time). Behaviors or settings within a
participant form an additional level between measurements and
participants; cases are nested within studies, so a case identifier
repeated in two studies names two cases.

## Examples

``` r
d <- data.frame(study = rep(c("S1", "S2"), each = 4), case = "C1",
                time = rep(1:4, 2), outcome = rnorm(8))
tier_vars(d)
#> [1] "study" "case" 
```
