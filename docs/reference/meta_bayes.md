# Bayesian random-effects meta-analysis of study effect sizes

Pools study-level effect sizes, such as the \\\delta\_{AB}\\ posteriors
of
[`bc_smd_bayes()`](https://mshin77.github.io/scdbayes/reference/bc_smd_bayes.md),
in the brms model `yi | se(sei) ~ 1 + (1 | study)`, adding
`(1 | study:case)` when several estimates come from one study and
moderators when given.

## Usage

``` r
meta_bayes(
  es_data,
  yi = "yi",
  sei = "sei",
  study = "study",
  case = NULL,
  moderators = NULL,
  priors = NULL,
  metric = c("smd", "lrr"),
  iter = 4000,
  chains = 4,
  mcid = NULL,
  yi_stat = c("median", "mean"),
  control = list(),
  ...
)
```

## Arguments

- es_data:

  Data frame with one row per effect size, or a named list of
  [`bc_smd_bayes()`](https://mshin77.github.io/scdbayes/reference/bc_smd_bayes.md)
  results (names are the studies).

- yi, sei:

  Effect size and standard error columns.

- study:

  Study column.

- case:

  Optional column identifying several effects within a study.

- moderators:

  Optional character vector of moderator columns.

- priors:

  brms priors; `NULL` uses the defaults in Details.

- metric:

  `"smd"` (standardized mean difference, including BC-SMD) or `"lrr"`
  (log response ratio); sets the default prior scale on the mean effect
  and moderator slopes.

- iter, chains:

  Sampler settings.

- mcid:

  Optional minimal clinically important difference on the effect size
  scale; reports \\\Pr(\mu \> \mathrm{MCID})\\.

- yi_stat:

  `"median"` or `"mean"`; the per-study posterior summary used as `yi`
  when `es_data` is a list of
  [`bc_smd_bayes()`](https://mshin77.github.io/scdbayes/reference/bc_smd_bayes.md)
  results.

- control:

  Sampler control list passed to
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html),
  merged over the defaults `adapt_delta = 0.99` and `max_treedepth`.

- ...:

  Passed to
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

## Value

A list with `summary` (data frame with columns `quantity` (`"mu"`,
`"tau"`, `"tau_within"` when `case` is given, `"prediction"`),
`estimate` (posterior median), `sd`, `lower`, `upper`), `study_effects`
(data frame: `study`, `estimate`, `sd`, `lower`, `upper`), `pd`
(probability of direction of \\\mu\\), `prob_mcid` (\\\Pr(\mu \>
\mathrm{MCID})\\, `NA` without `mcid`), `priors` (the priors used),
`convergence` (list: `converged`, `max_rhat`, `min_ess_bulk`,
`min_ess_tail`, `n_divergent`), `diagnostics` (the
[`compute_convergence_diagnostics()`](https://mshin77.github.io/scdbayes/reference/compute_convergence_diagnostics.md)
result), and `model` (the `brmsfit`).

## Details

**Orientation.** Every `yi` must point the same way, with positive
values meaning improvement. Effects on outcomes the intervention aims to
decrease need their sign reversed first
([`bc_smd_bayes()`](https://mshin77.github.io/scdbayes/reference/bc_smd_bayes.md)
does this with `intervention_goal = "decrease"`).

**Inputs.** Each study enters through a normal likelihood with known
standard error, the normal-theory approximation of Bayesian
meta-analysis. `es_data` is a data frame, or a list of
[`bc_smd_bayes()`](https://mshin77.github.io/scdbayes/reference/bc_smd_bayes.md)
results named by study; for a list, `yi` is the posterior median
(`yi_stat = "median"`) or mean (`"mean"`) of the finite \\\delta\_{AB}\\
draws and `sei` their SD. This two-stage approximation has three
consequences. A skewed per-study posterior is summarized by two numbers
only. The per-study summaries already carry the first-stage priors of
[`bc_smd_bayes()`](https://mshin77.github.io/scdbayes/reference/bc_smd_bayes.md),
so the pooled posterior is not the posterior of one joint model. And the
precision weights \\1/(\tau^2 + \mathrm{sei}^2)\\ can correlate with the
estimates, because `sei` grows with \\\|\delta\_{AB}\|\\; such an
association biases average effects and confidence interval coverage in
the meta-analysis of within-case effect sizes. Pooling the full
per-study draws (a mixture approach) is not implemented.

**Priors.** The intercept and moderator slopes get a fixed, weakly
informative `normal(0, s)` on the effect size metric, set before seeing
the data: \\s = 2\\ for standardized mean differences (`metric = "smd"`,
including BC-SMD), which keeps 95\\ mass within \\\pm 4\\ SD units, and
\\s = 1\\ for log response ratios (`metric = "lrr"`), which keeps 95\\
0.14 to 7. Moderator slopes are per unit of the moderator, so center and
scale continuous moderators. Effects on any other scale need explicit
`priors`. Each between-study (and within-study) SD gets a
half-\\t_3\\(0, 0.5) prior; the half-\\t\\ family lets the data dominate
when the likelihood is informative. Supply `priors` to override, and
refit with a wider and a narrower \\s\\ as a prior sensitivity check.

**Convergence.** The fit is checked with
[`compute_convergence_diagnostics()`](https://mshin77.github.io/scdbayes/reference/compute_convergence_diagnostics.md):
R-hat \< 1.01, bulk and tail ESS \> 400, no divergent transitions, and
no transition at max_treedepth. A warning names the failed checks.

**Sampler.** The sampler runs with `adapt_delta = 0.99` and
`max_treedepth` from the `scdbayes.max_treedepth` option (default 12):
with few studies the posterior of \\\tau\\ has a funnel near 0 that
causes divergent transitions at the brms default. Entries of `control`
replace these defaults one at a time, so `list(max_treedepth = 15)`
keeps `adapt_delta = 0.99`.

**Outputs.** The mean effect is `b_Intercept` (the effect at moderator
values of 0). The prediction interval is the posterior predictive
distribution of the true effect in a new study, \\\mu + N(0, \tau^2)\\
per draw. When `case` is given the within-study SD is added, so it is
the prediction for a new case (or measure) in a new study. Study effects
are the shrunken study means \\\mu + u\_{study}\\ (plus moderator
terms), averaged over a study's rows.

## Examples

``` r
if (interactive()) {
  es <- data.frame(study = paste0("S", 1:6), yi = c(0.8, 1.2, 0.4, 1.5, 0.9, 0.6),
                   sei = c(0.3, 0.4, 0.35, 0.5, 0.3, 0.45))
  meta_bayes(es)$summary
}
```
