# Locate a mid-level cluster grouping column

Matches `cluster`/`therapist`/`teacher`/`classroom`/ `group`
(case-insensitive). Used for a grouping level above case but below
study/institution. Observer, rater, and assessor columns record
interobserver agreement rather than a design level, so they never match.

## Usage

``` r
find_cluster_var(data)
```

## Arguments

- data:

  Data frame.

## Value

Column name, or `NULL` if no match.

## Examples

``` r
find_cluster_var(data.frame(teacher = c("T1", "T2"), outcome = c(1, 2)))
#> [1] "teacher"
```
