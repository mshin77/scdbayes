# Detect SCD design type

Phase sequences are built per tier (see
[`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md))
from time-ordered run lengths (`rle`), so repeated phases such as
A-B-A-B are preserved rather than collapsed to A-B, and a case
identifier repeated across studies is read as separate series.

## Usage

``` r
detect_design_type(data)
```

## Arguments

- data:

  A data frame with case and phase columns.

## Value

A character string: "ab", "reversal", "multiple_baseline",
"alternating", "changing_criterion", or "unknown".

## Details

Checks run in this order: alternating treatments (two or more conditions
alternating in short runs after an optional baseline phase), changing
criterion with a numeric criterion column that changes two or more
times, up or down, between adjacent intervention sessions of a tier (so
A-B-B-B-A-B with stepped criteria is not read as a reversal), reversal
(three or more runs alternating between baseline-role and
intervention-role phases, with Withdrawal, Return, Reversal, or a
numbered baseline in the baseline role and Reinstatement in the
intervention role), sequential non-recurring designs (A-B, A-B-M,
A-B1-B2-M, A-B-M-G with onsets that differ across tiers), and changing
criterion (four or more conditions after the first whose labels carry
the intervention role or no known role, such as B1/B2/B3, possibly
returning to an earlier criterion). Maintenance or generalization phases
rule out changing criterion: a single A-B-M-G series is `"ab"`.

## Examples

``` r
d <- data.frame(
  case = rep(c("C1", "C2"), each = 10),
  phase = rep(c("A", "B"), 10),
  time = rep(1:10, 2),
  outcome = rnorm(20)
)
detect_design_type(d)
#> [1] "alternating"
```
