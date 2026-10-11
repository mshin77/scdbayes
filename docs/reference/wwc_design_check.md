# WWC single-case design standards screening check

Screens the phase structure of single-case data against the research
design requirements and the risk-of-bias rules of the What Works
Clearinghouse (WWC) Procedures and Standards Handbook, Version 5.0, and
reports the highest research rating the data structure is eligible for.
This is a screening aid, not an official WWC review: outcome validity,
confounding factors, researcher control of the independent variable, and
residual treatment effects are not assessed, and visual analysis by
trained reviewers is still required.

## Usage

``` r
wwc_design_check(
  data,
  design = detect_design_type(data),
  time_var = find_time_var(data),
  outcome_var = "outcome",
  intervention_goal = "increase",
  ioa_value = NULL,
  ioa_coverage = NULL,
  ioa_type = c("percentage", "kappa")
)
```

## Arguments

- data:

  Data frame with case, phase, time, and outcome columns.

- design:

  Design type: `"reversal"`, `"changing_criterion"`,
  `"multiple_baseline"`, `"multiple_probe"`, `"ab"`, or `"alternating"`;
  defaults to
  [`detect_design_type()`](https://mshin77.github.io/scdbayes/reference/detect_design_type.md).

- time_var:

  Time column; `NULL` uses row order.

- outcome_var:

  Outcome column.

- intervention_goal:

  `"increase"` or `"decrease"`; orients the baseline-trend and
  reversibility NAP.

- ioa_value:

  Optional overall IOA value (proportion or percentage).

- ioa_coverage:

  Optional proportion of data points with IOA (values above 1 are read
  as percentages); a value outside 0-1 after that scaling is an error.

- ioa_type:

  `"percentage"` (minimum .80) or `"kappa"` (minimum .60).

## Value

A list with `rating` (overall), `reasons`, `experiments` (one row per
experiment or contrast with its rating and reasons), `tiers` (phase
structure per tier), `baseline_trend`, `reversibility`, `ioa` (status
and detail), `design`, and `note`.

## Details

**Common rules.** Every design needs three attempts to demonstrate an
effect at three different points in time. A phase with three or more
data points and no within-phase variability counts toward the Meets
Without Reservations data-point requirements. A design receives the
highest rating any subset of it is eligible for, provided the subset
still holds three demonstrations and compares only adjacent phases;
extra phases or tiers with fewer data points do not lower the rating.

**Reversal/withdrawal (each case is one experiment).** At least two
phases per condition, that is four alternating phases. Without
Reservations: the initial baseline of the case has six or more points,
also when the qualifying window starts at a later phase, and the other
phases five or more; With Reservations: three or more points in two
phases per condition. The check searches every window of four
consecutive alternating baseline/intervention phases. Reasons name the
phases that pass only through zero variability.

**Changing criterion (each case is one experiment).** The reversal rules
apply with every baseline, intervention, or criterion change counted as
a phase change, so three changes are required; maintenance and
generalization phases are not criterion changes, and a series without a
baseline phase does not meet standards. When the data carry a numeric
`criterion`, `crit`, `criterion_level`, or `criterion_value` column,
phases are split on phase label and criterion value together, so each
criterion change opens a phase; a missing criterion value carries the
last one forward within the phase.

**Multiple baseline and multiple probe (each study is one experiment).**
At least six phases, i.e. three tiers with a baseline to intervention
change, and at least three distinct intervention onsets. Without
Reservations: first baseline of six or more points in each tier and
later phases of five or more; With Reservations: three phases per
condition with three or more points. Concurrence: every tier has
baseline data before the first intervention onset, and every tier still
in baseline has data at or after each earlier onset; vertical alignment
is assumed from the shared time axis. Concurrence is judged across all
tiers: a design that fails it is rated Does Not Meet WWC Standards, and
no subset can restore the rating. Multiple probe designs add three
rules: probes in the first three sessions (all three for Without, one
common session for With), probes just before the onset (three
consecutive for Without, at least one within the three prior sessions
for With), and a probe for every untreated tier in the session another
tier starts intervention. With `design` left at its default, a multiple
baseline design with a baseline gap between sessions longer than the
median session spacing of the same tier's intervention phase (baseline
rows absent, as with planned probes) is reviewed as a multiple probe
design. A row with an `NA` outcome is unplanned missing data and stays
under the multiple baseline rules; pass `design` to override either
reading. Concurrence is read strictly: a tier whose baseline data all
end before an earlier tier's onset fails, even when it is measured again
later in intervention.

**Combination designs.** When a reversal or changing-criterion design
has three or more tiers, the first baseline-intervention phases of the
tiers are also screened as a multiple baseline design within each study;
when it rates higher than every tier of that study, a row naming the
multiple baseline reading is added.

A two-phase AB design is screened under the same rules, so AB tiers
sharing one onset fail the distinct-onset requirement.

**Alternating treatments (each case and pair of conditions is one
contrast).** At least five points per condition for Without and four for
With Reservations, and no more than two consecutive points of one
condition within the rapidly alternating phase. A leading baseline of
more than two points is set aside from the alternation, also when the
baseline condition recurs within it. A final run of more than two points
is a separate phase, exempt from the consecutive-point rule and left out
of the point counts, only when it continues the most successful
intervention: a condition with a recognised intervention, maintenance,
or generalization role (see
[`phase_role()`](https://mshin77.github.io/scdbayes/reference/phase_role.md);
single letters C-H also count as interventions, other unrecognised
labels such as `"Ctl"` are never exempt) whose alternating phase mean is
strictly the highest of all conditions, ties excluded and baseline
included (the lead baseline enters with its own mean when it does not
recur in the alternation; lowest when `intervention_goal = "decrease"`);
any other final run stays in the alternation. Trailing maintenance or
generalization phases whose labels never alternate are left out of the
alternating comparison but still count as phases for the IOA rule. When
the baseline condition does not recur in the alternation, the single
lead baseline is compared with each intervention-role condition of the
alternating phase as its own contrast, under the same data-point rule
(five points per condition for Without, four for With); the
consecutive-point rule applies to the alternating condition only.
Unrestricted random assignment of condition order is also exempt but
cannot be detected from the data.

**Risk of bias.** Designs other than alternating treatments that are
eligible for Without Reservations are rated With Reservations at best
when the initial baseline shows a therapeutic trend or reversibility is
incomplete. Baseline trend compares the last three baseline points with
all earlier baseline points (nonoverlap of all pairs, NAP); a baseline
with zero variability and three or more points meets the rule.
Reversibility compares the initial baseline with each return-to-baseline
phase of a reversal or changing-criterion design. NAP of .85 or less
meets each rule; NAP is oriented by `intervention_goal`. For multiple
baseline designs every baseline in the qualifying subset must meet the
trend rule.

**Interassessor agreement (IOA).** At least .80 for percentage agreement
or .60 for Cohen's kappa across phases and cases, collected in each
phase and for at least 20\\ from a numeric column whose name contains
`ioa`, `agreement`, `kappa`, or `reliab` (one value per session with
agreement data, `NA` otherwise; values above 1 are read as percentages),
or from `ioa_value` and `ioa_coverage`. IOA below the minimum fails the
outcome reliability standard and caps the rating at Does Not Meet;
without either source IOA is reported as not assessed. An `ioa_value`
without `ioa_coverage` or an IOA column checks the value only: coverage
is reported as not assessed (status `"partially assessed"`) and does not
cap the rating. The rule that every phase carries IOA data applies only
when the IOA column is the value source, not when `ioa_value` is
supplied. The handbook asks for IOA in each phase without limiting it to
the compared phases, so lead, final, maintenance, and generalization
runs of an alternating design are each a phase for that rule.

Phase labels without a known role (see
[`phase_role()`](https://mshin77.github.io/scdbayes/reference/phase_role.md))
take their roles from first appearance in time, with a warning.

**Study-level rating.** The overall rating is the highest rating among
the experiments.

**Next steps.** Findings that meet standards proceed to effect size
estimation; the WWC computes a design-comparable effect size for
reversal, multiple baseline, and multiple probe designs with three or
more cases, implemented in the scdhlm package. Studies that meet
standards can proceed to that design-comparable BC-SMD, which scdbayes
estimates in a Bayesian model
([`bc_smd_bayes()`](https://mshin77.github.io/scdbayes/reference/bc_smd_bayes.md))
and pools with a Bayesian meta-analysis
([`meta_bayes()`](https://mshin77.github.io/scdbayes/reference/meta_bayes.md));
scdhlm/metafor scripts are optional frequentist comparisons. scdbayes
does not compute the within-case SMD, which is biased under
autocorrelation.

## Examples

``` r
d <- data.frame(case = "P1", time = 1:21,
                phase = rep(c("A", "B", "A", "B"), c(6, 5, 5, 5)),
                outcome = c(3, 4, 3, 4, 3, 4, 8, 9, 8, 9, 8,
                            4, 3, 4, 3, 4, 9, 8, 9, 8, 9))
wwc_design_check(d)$rating
#> [1] "Meets WWC Standards Without Reservations"
```
