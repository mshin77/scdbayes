# Assemble brms priors from app-level inputs

Internal helper that builds a named list of brms prior strings based on
the chosen prior type, family, and adaptive prior recommendations.

## Usage

``` r
assemble_brms_priors(
  prior_type,
  prior_effect_mean,
  prior_effect_sd,
  heterogeneous_variance,
  model_family_value,
  adaptive_priors,
  treatment_coef = NULL,
  has_random_slope = FALSE,
  formula_str = NULL
)
```

## Arguments

- prior_type:

  Character: "default", "informative", "meta_analytic", or
  "sensitivity". The informative prior is normal with SD equal to half
  the adaptive weakly-informative SD, centered on `prior_effect_mean`
  when that is a finite number and on 0 otherwise. The sensitivity
  (diffuse) prior doubles the adaptive SD (5 outcome SDs on identity
  links, 5 on log/logit links), so it is never narrower than the
  default, and doubles the session-slope priors of `formula_str` by the
  same factor; comparing the two is the prior sensitivity analysis.
  `"sensitivity"` returns only this diffuse arm: the default arm is the
  `"default"` type, and
  [`run_sensitivity_analysis()`](https://mshin77.github.io/scdbayes/reference/run_sensitivity_analysis.md)
  fits the arms and compares them.

- prior_effect_mean:

  Numeric mean of the elicited treatment prior, used by the
  meta-analytic and informative types.

- prior_effect_sd:

  Numeric SD for the meta-analytic prior; must be positive and finite
  (an error otherwise).

- heterogeneous_variance:

  Logical; if TRUE add separate sigma priors.

- model_family_value:

  Character name of the brms family.

- adaptive_priors:

  List from
  [`generate_adaptive_priors()`](https://mshin77.github.io/scdbayes/reference/generate_adaptive_priors.md).

- treatment_coef:

  Optional name of the treatment coefficient. When supplied with an
  informative or meta-analytic prior type, the elicited treatment prior
  targets only this coefficient (`coef`) and a weakly-informative
  default covers the remaining `b` coefficients. When `NULL` with those
  types, a warning is issued: the elicited mean cannot be applied and
  the treatment SD falls on the whole `b` class with mean 0.

- has_random_slope:

  Logical; used only without `formula_str`: if TRUE an `lkj(2)` prior is
  emitted on the correlation (`cor`) parameter. With `formula_str`, the
  `cor` prior is emitted only when a single-bar group term such as
  `(1 + level_AB | case)` has two or more coefficients; uncorrelated
  `||` terms and one-coefficient terms have no correlation, and brms
  errors on a prior that matches no parameter.

- formula_str:

  Optional model formula string. When supplied, each session-count term
  of the formula (`time_*`, `trend_*`) with a scale in
  `adaptive_priors$slopes` gets its own `normal(0, scale)` prior (see
  [`generate_adaptive_priors()`](https://mshin77.github.io/scdbayes/reference/generate_adaptive_priors.md)).

## Value

Named list of prior specifications with attributes.

## Details

With `heterogeneous_variance = TRUE` the log-scale residual SD intercept
is `normal(log pooled within-phase SD, 1)` and the phase contrasts on it
`normal(0, 1)` (see
[`generate_adaptive_priors()`](https://mshin77.github.io/scdbayes/reference/generate_adaptive_priors.md)).
Shape (negative binomial, Gamma), precision (beta), and
degrees-of-freedom (Student-t) parameters keep the brms defaults.
Default priors have limited influence in larger samples, not with a few
cases; compare a second shape or precision prior in the sensitivity
analysis (see `recommendations$dispersion` of
[`generate_adaptive_priors()`](https://mshin77.github.io/scdbayes/reference/generate_adaptive_priors.md)).

With an elicited (informative or meta-analytic) prior on
`treatment_coef`, the remaining `b` coefficients get a class-level
default prior. When `formula_str` is given and every fixed term already
has its own prior, that class-level prior is dropped, since brms warns
about a prior that matches no parameter.
