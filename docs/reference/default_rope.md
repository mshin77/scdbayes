# Default region of practical equivalence for treatment coefficients

On identity links (gaussian, student, skew_normal) the region is +/- 0.1
pooled within-phase SD: the SD of observations around their
case-by-phase means, so the treatment effect itself does not widen the
region (the outcome SD is the fallback when it cannot be computed). On
log and logit links it is +/- log(1.1), a 10\\ multipliers are
conventions, not substantive thresholds; supply a context-specific ROPE
when one is known. Other links require explicit bounds on their own
scale.

## Usage

``` r
default_rope(data, family = "gaussian", response = "outcome")
```

## Arguments

- data:

  Processed data from
  [`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md)
  (pass these rather than raw data or `model$data`). Without a `phase`
  column, as in `model$data` of a fit on coded columns, phases are
  recovered from the coded step columns (`level_*`, `atd_*`,
  `treatment`): each distinct combination of their values within a tier
  is one phase.

- family:

  brms family name or family object.

- response:

  Response column name.

## Value

Numeric length-2 vector `c(lower, upper)` on the link scale.

## Details

Phases are grouped within tiers
([`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md)),
so a case identifier repeated across studies or behaviors does not pool
two series. Each run of a phase label within a tier is its own phase (A1
and A2 of a reversal design are separate), except in
alternating-treatments designs, where all sessions of a condition form
one phase.

## Examples

``` r
default_rope(simulate_scd_data(n_cases = 3), "gaussian")
#> [1] -1.098565  1.098565
default_rope(simulate_scd_data(n_cases = 3), "poisson")
#> [1] -0.09531018  0.09531018
```
