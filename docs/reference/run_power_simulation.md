# Bayesian power and design analysis by simulation

Simulation-based design analysis for multilevel SCD models. Data are
generated under an assumed treatment effect, the model is refit by
[`update()`](https://rdrr.io/r/stats/update.html) each replication
(reusing one compiled Stan model), and the proportion of fits meeting
the chosen posterior decision rule is the power. The same multilevel
structure covers a single study (cases) and a meta-analysis of
single-case data (cases nested in studies, set `n_studies > 1` with
between-study effect heterogeneity `study_sd`).

## Usage

``` r
run_power_simulation(
  n_sims = 200,
  n_cases = 5,
  n_studies = 1,
  n_phase_a = 5,
  n_phase_b = 5,
  effect_sizes = c(0, 0.5, 1, 1.5),
  autocorr = 0.2,
  icc = 0.2,
  study_sd = 0,
  slope_sd = 0,
  fit_autocor = TRUE,
  iter = 1000,
  chains = 2,
  design = "ab",
  family = "gaussian",
  coefficient = "level",
  decision_rule = "prob_direction",
  reversal_decision = c("all_transitions", "first_transition"),
  intervention_goal = c("increase", "decrease"),
  prob_threshold = 0.975,
  rope = 0.1,
  standardized = TRUE,
  prior = NULL,
  seed = NULL,
  planning_target = c("pooled", "category"),
  studies_per_category = c(4, 4),
  common_effect = 0.5
)
```

## Arguments

- n_sims:

  Replications per effect magnitude. The reported Monte Carlo SE of a
  power estimate is the Agresti-Coull form sqrt(p'(1 - p') / (n_valid +
  4)) with p' = (k + 2) / (n_valid + 4), where k counts the fired
  replications among the `n_valid` converged ones; at power 0.5 that is
  about 0.049 for 100, 0.035 for 200, and 0.025 for 400 valid
  replications.

- n_cases:

  Cases per study; a positive integer or one integer per study.

- n_studies:

  Studies. 1 for a single-study design; `> 1` adds a study-level random
  effect (meta-analysis of single-case data).

- n_phase_a, n_phase_b:

  Time points per phase; a positive integer or one integer per study.

- effect_sizes:

  Treatment-effect magnitudes on the model link scale (log-rate ratio
  for count families). For a Gaussian family with `standardized = TRUE`
  the value is multiplied by the baseline (within-case) SD (a
  Cohen's-d-style planning grid). The element 0 yields the Type I rate.

- autocorr:

  Lag-1 autocorrelation of within-case residuals. Count power requires
  zero; serially dependent count power is unsupported.

- icc:

  Intraclass correlation; sets the between-unit SD via tau = sigma \*
  sqrt(icc / (1 - icc)). With `n_studies > 1`, tau^2 is split evenly
  between the study and case intercepts so the total ICC stays at `icc`.
  Count families have no residual SD, so sigma is fixed at 0.5 on the
  log scale (tau = 0.5 \* sqrt(icc / (1 - icc))); the ICC is then a
  planning value on the latent log-rate scale, not the observed-count
  ICC, and the Poisson or negative binomial noise adds to it. Counts are
  generated from a baseline log-rate of log(50) with no serial
  dependence.

- study_sd:

  Between-study SD of the treatment effect, on the same scale as
  `effect_sizes` (SD units for a standardized Gaussian family, link
  scale for count families); used when `n_studies > 1`. The fitted model
  then carries `(1 + <tested terms> || study)` when `study_sd > 0` and
  `(1 | study)` otherwise.

- slope_sd:

  Between-case SD of the treatment effect, adding case-level
  random-slope heterogeneity to the data-generating process. On the link
  scale for count families; in SD units for a standardized Gaussian
  family. Default 0 (homogeneous effect). With two or more level-2 units
  the fitted model always carries a correlated case random slope on the
  level change, `(1 + phase_num | case)`, as the analysis formula does;
  when `slope_sd > 0` and the tested coefficient is a trend term, an
  uncorrelated random slope on that term is added.

- fit_autocor:

  Logical; add a level-1 AR(1) term over each series,
  `ar(time, gr = case)` or `ar(time, gr = study:case)` with studies, to
  the fitted model so it matches the autocorrelated errors the simulator
  generates. Gaussian family only (default TRUE). Its coefficient gets
  `normal(0, 0.5)`, the default of the app.

- iter, chains:

  MCMC iterations and chains.

- design:

  One of `"ab"`, `"reversal"`, `"multiple_baseline"`. A reversal design
  is simulated as ABAB with a common effect in every B phase (steps of
  +d, -d, +d at the three transitions) and coded with
  [`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md)
  as the analysis codes it: `time_A1` plus a cumulative level and trend
  step per transition (`level_A1B1`, `level_B1A2`, `level_A2B2`, and the
  matching `trend_*`). The fitted model carries all seven terms and,
  from two level-2 units on, the random effects of
  [`default_random_effects()`](https://mshin77.github.io/scdbayes/reference/default_random_effects.md):
  the onset step correlated with the case intercept, the later steps
  uncorrelated.

- family:

  One of `"gaussian"`, `"poisson"`, `"negbinomial"`.

- coefficient:

  Coefficient carrying the effect and being tested: `"level"`
  (b_phase_num), `"trend"` (b_time_center), or `"slope_change"`
  (b_time_since_change). For a reversal design, `"level"` tests the
  level steps `level_A1B1`, `level_B1A2`, `level_A2B2`, `"slope_change"`
  the trend steps `trend_A1B1`, `trend_B1A2`, `trend_A2B2`, and
  `"trend"` the single baseline slope `time_A1`.

- decision_rule:

  One of `"prob_direction"` (probability of direction pd \>=
  prob_threshold), `"prob_meaningful"` (posterior probability of an
  effect at or beyond the ROPE bound in the improvement direction,
  P(effect \>= rope), or P(effect \<= -rope) for
  `intervention_goal = "decrease"`, \>= prob_threshold; one-sided like
  `prob_meaningful_change`), or `"rope"` (posterior probability inside
  the ROPE \<= 1 - prob_threshold). These mirror the `pd`,
  `prob_meaningful_change`, and `rope_prob` quantities reported by the
  analysis decision summaries.

- reversal_decision:

  Reversal designs only. `"all_transitions"` (default) fires only when
  every transition meets the decision rule in its expected direction:
  the A1-B1 and A2-B2 steps in the intervention goal direction and the
  B1-A2 withdrawal step opposite to it, so the effect is demonstrated
  three times. Each transition is then judged one-sided in its expected
  direction (`prob_direction`: P(oriented step \> 0); `prob_meaningful`:
  P(oriented step \>= rope); `rope`: posterior mass inside the ROPE \<=
  1 - prob_threshold with the posterior mean in the expected direction).
  `"first_transition"` tests only the onset step (`level_A1B1` or
  `trend_A1B1`) with the single-coefficient rule.

- intervention_goal:

  `"increase"` (default) or `"decrease"`: the direction of improvement.
  `effect_sizes`, `rope`, and the `prob_meaningful` rule stay in
  magnitudes, positive for improvement; with `"decrease"` the data are
  generated with the negated effect and `prob_meaningful` uses P(effect
  \<= -rope). `prob_direction` on a single coefficient stays two-sided.

- prob_threshold:

  Posterior-probability threshold the chosen rule must meet to flag an
  effect. The default 0.975 corresponds, for the `prob_direction` rule,
  to a 5\\ (pd \>= 0.975 mirrors a two-sided alpha of 0.05); 0.95
  mirrors a 10\\ two-sided rate.

- rope:

  Half-width of the region of practical equivalence, on the same scale
  as `effect_sizes`.

- standardized:

  For a Gaussian family, multiply the magnitude by the baseline
  (within-case) SD (planning in SD units). Ignored for non-Gaussian
  families.

- prior:

  A brms prior. Defaults to the priors the analysis uses:
  [`generate_adaptive_priors()`](https://mshin77.github.io/scdbayes/reference/generate_adaptive_priors.md)
  on the template data (simulated with no effect, so the priors do not
  depend on `effect_sizes`) passed through
  [`assemble_brms_priors()`](https://mshin77.github.io/scdbayes/reference/assemble_brms_priors.md)
  (`normal(0, 2.5 * outcome SD)` on coefficients for Gaussian,
  `normal(0, 2.5)` on the log scale for count families, half-Student-t
  SD priors, `lkj(2)`), plus the `normal(0, 0.5)` AR prior truncated to
  the stationary region \\(-1, 1)\\.

- seed:

  Optional integer. When supplied, sets the R random-number seed for the
  template and reseeds every replication from `seed`, the effect size,
  and the replication index, so each effect-size row is reproducible and
  independent of the rest of `effect_sizes`; the global RNG state
  (`.Random.seed`) in place before the call is restored on exit. `NULL`
  (default) draws from and advances the current RNG state.

- planning_target:

  `"pooled"` tests the overall effect; `"category"` tests the direct
  category B minus A coefficient.

- studies_per_category:

  Two study counts, each at least two, for categories A and B. Category
  planning sets `n_studies` to their sum.

- common_effect:

  Category A effect magnitude in the intervention goal direction.
  Category B adds each `effect_sizes` magnitude. Both use the same scale
  as the effect grid. Category labels are study-level and allocation is
  fixed. Ordinal outcomes and assurance are unsupported.

## Value

A data frame with one row per magnitude: `effect_size`, `true_beta`
(signed effect on the coefficient scale, negative for
`intervention_goal = "decrease"`), `power` (the Type I rate when the
magnitude is 0), `mc_se` (Agresti-Coull Monte Carlo SE of power, which
stays positive when power is 0 or 1), `type_s` (sign-error rate against
the signed true effect, conditional on the posterior-mean sign; `NA`
under a one-sided rule, `prob_meaningful` or a reversal
`all_transitions` decision that tests more than one term
(`coefficient = "level"` or `"slope_change"`), since a fired replication
then has the expected sign by construction; a reversal
`coefficient = "trend"` tests one term and reports `type_s`) and
`type_m` (posterior-mean exaggeration ratio among fired replications;
the posterior mean is shrunk by the prior, so this is not the MLE-based
Type M quantity; for a reversal design both use the onset step),
`power_all` (the firing rate over all `n_sims`, counting non-converged
and failed fits as non-detections: a lower bound on power, and at
magnitude 0 an optimistic (low) Type I rate, since non-convergence
clusters near the null), `mc_se_all` (Agresti-Coull Monte Carlo SE of
`power_all` over `n_sims`), `n_valid`, `n_nonconverged` (replications
that fail the simulation screen: rank-normalized Rhat \>= 1.05, bulk ESS
\< 100, or a non-computable value on a tested coefficient, or any
divergent transition; every fit uses `adapt_delta = 0.99`). The screen
is looser than the Rhat \< 1.01 boundary of the analysis diagnostics
because, at the default 2 chains of 500 post-warmup draws, Rhat crosses
1.01 by noise alone in a share of well-mixed fits; the bulk ESS floor
guards the looser Rhat. Non-converged replications are excluded from
`power` and counted against `power_all`, with a warning naming the
total, since non-convergence is more frequent at small between-case
variance and near null effects, so `power` (and the Type I rate at
magnitude 0) is conditional on convergence; neither column bounds the
Type I rate from above, and a large gap between them flags that
dependence, `n_divergent` (how many of those had a divergent
transition), `n_diagnostic_failed` (replications excluded because
sampler diagnostics are unavailable or invalid), `n_fit_failed`
(replications whose [`update()`](https://rdrr.io/r/stats/update.html)
errored or returned no draws for the tested coefficient), and `n_sims`.
An error is raised when the template model (compiled once and reused by
[`update()`](https://rdrr.io/r/stats/update.html)) fails to fit.

## Examples

``` r
if (interactive()) {
run_power_simulation(n_sims = 50, n_cases = 4,
                     effect_sizes = c(0, 0.5, 1.0),
                     decision_rule = "prob_direction")
}
```
