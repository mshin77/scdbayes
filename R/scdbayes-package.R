# bayesplot and ggh4x are called only from the bundled Shiny app, which
# R CMD check does not scan: their imports keep them declared-and-used.

#' @keywords internal
#' @importFrom bayesplot mcmc_trace
#' @importFrom brms brm fixef ranef as_draws_matrix variables nuts_params prior prior_string threading VarCorr posterior_predict posterior_epred brmsterms loo_compare
#' @importFrom dplyr mutate filter select group_by summarise arrange ungroup across all_of any_of n n_distinct bind_rows left_join pull rename row_number case_when
#' @importFrom ggh4x facet_wrap2
#' @importFrom ggplot2 ggplot
#' @importFrom loo loo
#' @importFrom magrittr %>%
#' @importFrom moments skewness kurtosis
#' @importFrom posterior as_draws_df rhat ess_bulk ess_tail mcse_mean nchains subset_draws summarise_draws quantile2 ess_quantile
#' @importFrom rlang .data sym %||%
#' @importFrom shiny shinyApp
#' @importFrom stats acf as.formula ave coef cor gaussian lm median pnorm qnorm quantile qt pt rnorm sd setNames time update var formula residuals aggregate complete.cases
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom utils capture.output
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
