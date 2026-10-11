# Evaluate dataset size against per-task server limits

Row and case thresholds sized for the shared 2 GB / 1 vCPU web instance;
typical SCD datasets (3-15 cases, under 1,000 rows) pass every gate.

## Usage

``` r
check_data_size(
  data,
  task = c("upload", "fit", "comparison", "sequential", "bootstrap")
)
```

## Arguments

- data:

  Data frame to evaluate.

- task:

  One of "upload", "fit", "comparison", "sequential", "bootstrap".

## Value

NULL when within limits; otherwise a list with `level` ("block" or
"warn") and `message`.
