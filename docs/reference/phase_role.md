# Design role of phase labels

Maps phase labels to their design role after lower-casing, trimming, and
stripping a trailing occurrence number together with an optional
separator, so `"Baseline 1"`, `"A_1"`, `"A-1"`, `"A.1"`, and `"A1"` all
read as baseline.

## Usage

``` r
phase_role(labels)
```

## Arguments

- labels:

  Character, factor, or numeric vector of phase labels.

## Value

Numeric vector of the same length: 0 for the baseline role (`baseline`,
`a`, `bl`, `bsl`, `pre`, `0`, `withdrawal`, `return`, `reversal`,
`control`, `wait`, `waitlist`, `wl`), 1 for intervention (`b`,
`treatment`, `treat`, `tx`, `trt`, `intervention`, `reinstatement`,
`post`, `1`), 2 for maintenance (`m`, `maintenance`, `2`), 3 for
generalization (`g`, `generalization`, `gen`, `3`), and `NA` for any
other label. `probe` has no fixed role: a probe can be a baseline,
maintenance, or generalization session.

## Examples

``` r
phase_role(c("Baseline 1", "Treatment 1", "A_2", "B-2", "M", 0, "Tx", "Probe"))
#> [1]  0  1  0  1  2  0  1 NA
```
