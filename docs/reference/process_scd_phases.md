# Build piecewise regression coding for single-case design phases

Adds level, trend, and slope-change columns suitable for a piecewise
multilevel model with time-series intervention coding inside a
three-level meta-analytic structure. Every change point is coded within
each tier (the combination of the columns returned by
[`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md))
at that tier's own onset, using the actual time values, so unequally
spaced sessions and staggered onsets are honored.

## Usage

``` r
process_scd_phases(data, structure, scd_type = "reversal")
```

## Arguments

- data:

  A data frame with a phase column.

- structure:

  A list describing the hierarchical structure.

- scd_type:

  Design type: "reversal", "multiple_baseline", "multiple_probe",
  "alternating" (alias "alternating_treatment"), "changing_criterion",
  or "AB".

## Value

The data frame with piecewise regression columns added, rows in the
input order.

## Details

**AB, multiple baseline, multiple probe.** `time_A = t - T_A` on every
row, where `T_A` is the first baseline time of the tier, so the
intercept is the expected level at the start of baseline and `time_A`
carries the baseline trend through all phases. `level_AB` is 1 from the
first intervention time `T_B` on and `trend_AB = (t - T_B) * level_AB`,
so `level_AB` is the immediate change at `T_B` relative to the projected
baseline line and `trend_AB` the change in slope. Maintenance and
generalization phases add `level_BM`/`trend_BM` and
`level_BG`/`trend_BG` coded the same way from their own onsets: the
first maintenance-role time and the first generalization-role time, so
an A-B-G tier carries no maintenance step and `level_BG` is the change
at the generalization onset. `time_B`, `time_M`, `time_G` hold `t` minus
the corresponding onset on every row. Phases absent from a tier are
coded 0.

**Reversal (ABAB, BAB, ABA, ABABAB).** Labels with a known role (see
[`phase_role()`](https://mshin77.github.io/scdbayes/reference/phase_role.md),
including numeric codes 0/1/2/3 and numbered labels such as
`"Baseline 1"` or `"A_2"`) are renamed to the role letters A, B, M, G;
other labels keep their letters. Runs are then numbered by occurrence
within the tier (A1, B1, A2, B2, M1). The first-run time
(`time_<first run>`) is `t - T_1` on every row of the tier. Each
transition into run `k` adds a cumulative step
`level_<prev><next> = 1[t >= T_k]` (step coding) and a continuing slope
change `trend_<prev><next> = (t - T_k) * 1[t >= T_k]`. This is the
centered ABAB model: each level coefficient is the change between
adjacent phases at the first observation of the later phase, and each
trend coefficient the change in slope between adjacent phases. When
tiers start in different phases (an ABAB tier next to a BAB tier), every
first-run time column is kept (0 on the other tiers) and a warning notes
that the onset steps differ across tiers and are estimated separately.

**Changing criterion.** `time_A = t - T_A` on every row. Each criterion
subphase `j` (split on a numeric `criterion`, `crit`, `criterion_level`,
or `criterion_value` column when present, otherwise on phase-label runs)
adds a cumulative step `level_crit<j> = 1[t >= T_cj]` and a continuing
`trend_crit<j> = (t - T_cj) * level_crit<j>`, so each step compares
adjacent subphases. A return to an earlier criterion opens a new
subphase. A return to baseline after a criterion subphase adds the step
`level_ret<k> = 1[t >= T_rk]`. Without any baseline phase, `time_A` runs
from the first session of the tier and `level_crit1`/`trend_crit1` are
dropped with a warning, since they would equal the intercept and
`time_A`. The collapsed `treatment` (intervention vs baseline) column is
an intervention indicator; `time_treatment = (t - T_c1) * treatment` is
a centered variant of an uncentered session-number time variable.

**Alternating treatments.** The first baseline-role condition (else the
first condition) is the reference; every other condition, including a
second baseline-role label such as `"Control"`, gets a dummy
`atd_<condition>`. The `atd_` prefix keeps data columns such as
`treatment_fidelity` out of the condition terms. A general session trend
`time_session = t - T_1`, the session time centered at the first session
of the tier; centering at the first session only shifts the intercept.
Without it, a general trend loads on the condition dummies whenever
conditions are unevenly spread over time. Alternation blocks are counted
within each tier.

The time column is `structure$time_vars[1]` when supplied, otherwise
[`find_time_var()`](https://mshin77.github.io/scdbayes/reference/find_time_var.md)
(case-insensitive); with neither, sessions are numbered in row order
within each tier, with a warning that concurrence across tiers cannot be
assessed.

## Examples

``` r
d <- simulate_scd_data(n_cases = 2, n_phase_a = 5, n_phase_b = 5)
process_scd_phases(d, structure = list(), scd_type = "AB")
#>    case time phase phase_num time_center time_since_change  outcome
#> 1    C1    1     A         0           0                 0 39.43664
#> 2    C1    2     A         0           1                 0 51.98777
#> 3    C1    3     A         0           2                 0 56.50534
#> 4    C1    4     A         0           3                 0 53.43913
#> 5    C1    5     A         0           4                 0 64.77532
#> 6    C1    6     B         1           5                 0 50.72026
#> 7    C1    7     B         1           6                 1 71.26445
#> 8    C1    8     B         1           7                 2 35.23803
#> 9    C1    9     B         1           8                 3 54.07888
#> 10   C1   10     B         1           9                 4 63.93978
#> 11   C2    1     A         0           0                 0 53.60278
#> 12   C2    2     A         0           1                 0 56.54550
#> 13   C2    3     A         0           2                 0 60.52155
#> 14   C2    4     A         0           3                 0 30.20445
#> 15   C2    5     A         0           4                 0 62.08386
#> 16   C2    6     B         1           5                 0 48.30720
#> 17   C2    7     B         1           6                 1 52.95030
#> 18   C2    8     B         1           7                 2 62.66341
#> 19   C2    9     B         1           8                 3 38.64657
#> 20   C2   10     B         1           9                 4 38.68946
#>    phase_numeric time_A time_B level_AB trend_AB intervention_start
#> 1              0      0     -5        0        0                  6
#> 2              0      1     -4        0        0                  6
#> 3              0      2     -3        0        0                  6
#> 4              0      3     -2        0        0                  6
#> 5              0      4     -1        0        0                  6
#> 6              1      5      0        1        0                  6
#> 7              1      6      1        1        1                  6
#> 8              1      7      2        1        2                  6
#> 9              1      8      3        1        3                  6
#> 10             1      9      4        1        4                  6
#> 11             0      0     -5        0        0                  6
#> 12             0      1     -4        0        0                  6
#> 13             0      2     -3        0        0                  6
#> 14             0      3     -2        0        0                  6
#> 15             0      4     -1        0        0                  6
#> 16             1      5      0        1        0                  6
#> 17             1      6      1        1        1                  6
#> 18             1      7      2        1        2                  6
#> 19             1      8      3        1        3                  6
#> 20             1      9      4        1        4                  6
```
