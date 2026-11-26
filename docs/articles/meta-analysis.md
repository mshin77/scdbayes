# Meta-Analysis

This guide shows a minimal call to
[`run_scd_analysis()`](https://mshin77.github.io/scdbayes/reference/run_scd_analysis.md)
with multiple studies.

## Data

Combine observations in one table with study, case, time, phase, and
outcome columns. Assign distinct case identifiers across studies. Add
grouping or covariate columns as needed.

## Fit a model

Read a CSV file containing the required columns.

``` r

library(scdbayes)
data <- read.csv("data.csv")
result <- run_scd_analysis(data, options = list(family = "gaussian"))
result$diagnostics
```

The result contains the fitted model, summaries, and diagnostics.
Inspect diagnostics before interpreting results. See
[`?run_scd_analysis`](https://mshin77.github.io/scdbayes/reference/run_scd_analysis.md)
for arguments.
