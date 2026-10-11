# Sequential Bayesian updating

Refits the model on cumulative case subsets (cases 1-2, then 1-3, ...)
with the same prior each refit, tracking how the treatment-effect
estimate and its interval change as cases accrue. This is a
cumulative-refit variant of sequential Bayesian updating; the posterior
is not literally carried forward as the next prior.

## Usage

``` r
run_sequential_update(
  data,
  case_var = "case",
  formula_str = NULL,
  family = "gaussian",
  iter = 2000,
  chains = 2,
  priors = NULL
)
```

## Arguments

- data:

  Data frame containing SCD data.

- case_var:

  Name of the case variable.

- formula_str:

  Model formula string; defaults to `"outcome ~ phase + (1|case)"`.

- family:

  Model family (default `"gaussian"`).

- iter:

  Number of sampling iterations.

- chains:

  Number of MCMC chains.

- priors:

  A `brmsprior` object applied to every refit. A supplied prior such as
  the analysis model's `model$prior` was scaled on all cases, so early
  refits then draw on outcome scale from cases not yet in the cumulative
  subset. `NULL` (default) fixes every prior class from the first two
  cases only, computed once so every refit shares the same prior: the
  package normal coefficient (`b`) and Student-t `sd` priors, plus the
  brms default Student-t `Intercept` and `sigma` priors evaluated on
  those two cases rather than recomputed on each refit's data; never
  brms flat priors.

## Value

A list with `tracking` (data frame of cumulative estimates) and
`final_model`.

## Examples

``` r
if (interactive()) {
run_sequential_update(simulate_scd_data(n_cases = 5))
}
```
