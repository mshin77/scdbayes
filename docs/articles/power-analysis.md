# Power Analysis

This guide shows a minimal call to
[`run_power_simulation()`](https://mshin77.github.io/scdbayes/reference/run_power_simulation.md).

## Run a simulation

``` r

library(scdbayes)
power <- run_power_simulation(
  n_sims = 50,
  n_cases = 3,
  n_phase_a = 5,
  n_phase_b = 5,
  effect_sizes = c(0, 0.5, 1),
  seed = 2026
)
power
```

This small example illustrates the call. Simulation runs require Stan
and can take substantial time because they fit Bayesian models.

## Review results

The result contains a row per effect size with power estimates and
simulation diagnostics. Check diagnostics before interpreting estimates.
See
[`?run_power_simulation`](https://mshin77.github.io/scdbayes/reference/run_power_simulation.md)
for arguments.
