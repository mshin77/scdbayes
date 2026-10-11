# Generate adaptive priors based on data characteristics

Generate adaptive priors based on data characteristics

## Usage

``` r
generate_adaptive_priors(
  data,
  structure,
  model_type = "gaussian",
  outcome_var = structure$outcome_var %||% "outcome",
  exposure = NULL
)
```

## Arguments

- data:

  A data frame containing the outcome and grouping variables.

- structure:

  A list describing the hierarchical structure (from
  `detect_hierarchical_structure`).

- model_type:

  Character string specifying the model family. Any brms family name is
  accepted and resolved by its default brms link: identity-link families
  use a response-scale intercept, log-link families (`"poisson"`,
  `"negbinomial"`, `"Gamma"`, `"weibull"`, `"exponential"`, hurdle and
  zero-inflated count families, ...) and `"lognormal"` a log-scale
  intercept, and logit-type links (`"bernoulli"`, `"beta"`,
  `"cumulative"`, ...) a link-scale intercept centered on 0.

- outcome_var:

  Outcome column; defaults to `structure$outcome_var`, then `"outcome"`.

- exposure:

  Optional exposure column of a count model with an
  `offset(log(<exposure>))` term (see
  [`add_exposure_offset()`](https://mshin77.github.io/scdbayes/reference/add_exposure_offset.md));
  the log-link intercept is then centered on the log of total outcome
  over total exposure, the rate per unit of exposure.

## Value

A list of prior specifications including intercept, treatment, sigma
(for identity links), `log_sigma`, `slopes` (named numeric scales for
the session-count columns), random effects (if multilevel), and
recommendations.

## Details

Priors are weakly informative and expressed on the scale of the linear
predictor, with prior width tied to the measurement scale. Prior width
never depends on the number of observations or cases: a small sample is
not information about the effect, and the data, not a narrower prior,
decide how much the posterior moves.

- Identity links: treatment and slope coefficients
  `normal(0, 2.5 * SD)`, intercept `student_t(3, mean, 2.5 * SD)`,
  group-level SDs half-`student_t(3, 0, SD)`, and the residual SD
  half-`student_t(3, 0, SDw)`, where `SD` is the outcome standard
  deviation and `SDw` the pooled within-phase SD (the outcome SD when
  phases are missing). Coefficients and group-level SDs scale to the
  outcome SD, which spans between-case differences and floor-to-ceiling
  effects; the residual SD prior scales to the pooled within-phase SD.
  The half-t form scales the SD priors to the response; brms itself uses
  the scale `max(2.5, mad(y))`, so the two differ when the outcome SD is
  small or the outcome is skewed. The 2.5 multiplier for coefficients
  carries the 2.5-unit logistic regression scale to the response scale,
  a package convention.

- Logit links: treatment coefficients `normal(0, 2.5)`, the weakly
  informative scale for logistic regression coefficients; intercept
  `student_t(3, 0, 2.5)` and group-level SDs half-
  `student_t(3, 0, 2.5)`, as the brms defaults on non-identity links.

- Log links: the same scale 2.5, intercept centered on the log of the
  outcome mean. The 2.5 scale was derived for logistic regression only;
  carrying it to log links is a package convention.

Shape (negative binomial, Gamma), precision (beta), and
degrees-of-freedom (Student-t) parameters keep the brms defaults, for
lack of an established weakly informative prior. For these families
`recommendations$dispersion` suggests adding a second shape or precision
prior to the prior sensitivity analysis.

The outcome column is `outcome_var`, which defaults to
`structure$outcome_var` and then to `"outcome"`; with no such column the
outcome-based scales fall back to 1 and 0.

The 2.5 scale presumes standardized inputs: binary inputs differing by 1
and continuous inputs scaled to SD 0.5. Level steps and condition
dummies are 0/1 inputs and take the scale as is. Session-count terms
(`time_*`, `trend_*`) are not standardized, so `slopes` holds one scale
per such column, `2.5 * SD / (2 * sd(x))` on identity links and
`2.5 / (2 * sd(x))` otherwise: the 2.5 scale for a coefficient on the
input standardized to SD 0.5, expressed per session.
[`assemble_brms_priors()`](https://mshin77.github.io/scdbayes/reference/assemble_brms_priors.md)
applies them to the slope terms of a supplied formula.

`log_sigma` centers the intercept of a log-scale residual SD
(`sigma ~ phase`) on the log of the pooled within-phase SD with scale 1,
tying the prior to the measurement scale; the scale 1 lets the baseline
SD range over a factor of about e either way and is a convention. On
non-identity links the location is 0.

## Examples

``` r
d <- simulate_scd_data(n_cases = 3)
structure <- list(n_levels = 2, levels = list(case = "case"))
generate_adaptive_priors(d, structure)
#> $intercept
#> $intercept$distribution
#> [1] "student_t"
#> 
#> $intercept$df
#> [1] 3
#> 
#> $intercept$location
#> [1] 51.43488
#> 
#> $intercept$scale
#> [1] 28.09376
#> 
#> $intercept$mean
#> [1] 51.43488
#> 
#> $intercept$sd
#> [1] 28.09376
#> 
#> $intercept$description
#> [1] "student_t(3, 51.43, 28.09)"
#> 
#> 
#> $treatment
#> $treatment$distribution
#> [1] "normal"
#> 
#> $treatment$mean
#> [1] 0
#> 
#> $treatment$sd
#> [1] 28.09376
#> 
#> $treatment$description
#> [1] "normal(0, 28.09)"
#> 
#> 
#> $sigma
#> $sigma$distribution
#> [1] "student_t"
#> 
#> $sigma$df
#> [1] 3
#> 
#> $sigma$location
#> [1] 0
#> 
#> $sigma$scale
#> [1] 11.78429
#> 
#> $sigma$mean
#> [1] 0
#> 
#> $sigma$sd
#> [1] 11.78429
#> 
#> $sigma$description
#> [1] "student_t(3, 0.00, 11.78)"
#> 
#> 
#> $log_sigma
#> $log_sigma$location
#> [1] 2.466767
#> 
#> $log_sigma$scale
#> [1] 1
#> 
#> 
#> $slopes
#>       time_center time_since_change 
#>          4.808297          9.765698 
#> 
#> $random_effects
#> $random_effects$sd_group
#> $random_effects$sd_group$distribution
#> [1] "student_t"
#> 
#> $random_effects$sd_group$df
#> [1] 3
#> 
#> $random_effects$sd_group$location
#> [1] 0
#> 
#> $random_effects$sd_group$scale
#> [1] 11.2375
#> 
#> $random_effects$sd_group$mean
#> [1] 0
#> 
#> $random_effects$sd_group$sd
#> [1] 11.2375
#> 
#> $random_effects$sd_group$description
#> [1] "student_t(3, 0.00, 11.24)"
#> 
#> 
#> $random_effects$correlation
#> $random_effects$correlation$distribution
#> [1] "lkj"
#> 
#> $random_effects$correlation$eta
#> [1] 2
#> 
#> $random_effects$correlation$description
#> [1] "lkj(2)"
#> 
#> 
#> 
#> $recommendations
#> $recommendations$sample_size
#> [1] "Small sample: run a prior predictive check and a prior sensitivity analysis before interpreting the posterior."
#> 
#> $recommendations$overall
#> [1] "Prior strength: weakly informative, scaled to the outcome SD or the link scale."
#> 
#> 
```
