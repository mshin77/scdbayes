test_that(".response_column finds the outcome column without consulting the model", {
  d <- data.frame(outcome = rnorm(10), case = rep(1:2, 5))
  expect_equal(.response_column(NULL, d), "outcome")
  expect_null(.response_column(NULL, data.frame(case = 1:5)))
})

test_that("a failing diagnostics run records the error and a non-converged flag", {
  conv <- compute_convergence_diagnostics("not a model")
  expect_true(is.character(conv$error) && nzchar(conv$error))
  expect_false(conv$converged$converged)
})

test_that("empty diagnostic vectors give converged NA, not TRUE", {
  empty <- list(basic = list(rhat = numeric(0), ess_bulk = numeric(0),
                             ess_tail = numeric(0), mcse = numeric(0)))
  out <- assess_overall_convergence(empty)
  expect_true(is.na(out$converged))
  expect_equal(out$confidence, 0)
  out <- assess_overall_convergence(c(empty, list(divergence = list(n_divergent = 0,
                                                                     max_treedepth_hit = FALSE))))
  expect_true(is.na(out$converged))
  expect_lt(out$confidence, 1)
  expect_true(is.na(assess_overall_convergence(list())$converged))
  ok <- list(basic = list(rhat = c(a = 1.001, b = NA), ess_bulk = c(900, 1000),
                          ess_tail = c(800, 900)),
             divergence = list(n_divergent = 0, max_treedepth_hit = FALSE))
  expect_true(assess_overall_convergence(ok)$converged)
  expect_equal(assess_overall_convergence(ok)$confidence, 1)
  ok$divergence$n_divergent <- 3
  expect_false(assess_overall_convergence(ok)$converged)
})

test_that("an infinite R-hat fails while all-NA diagnostics are not computable", {
  inf <- list(basic = list(rhat = c(1.00, Inf), ess_bulk = c(900, 1000), ess_tail = c(800, 900)),
              divergence = list(n_divergent = 0, max_treedepth_hit = FALSE))
  expect_false(assess_overall_convergence(inf)$criteria$rhat_ok)
  expect_false(assess_overall_convergence(inf)$converged)
  recs <- expect_silent(generate_diagnostic_recommendations(inf))
  expect_match(recs$rhat, "^WARNING: R-hat >= 1.01")
  expect_match(recs$overall, "convergence warnings")
  na <- list(basic = list(rhat = c(NA_real_, NA_real_), ess_bulk = c(NA_real_, NA_real_),
                          ess_tail = c(NA_real_, NA_real_)),
             divergence = list(n_divergent = 0, max_treedepth_hit = FALSE,
                               energy = list(bfmi = NA_real_)))
  recs <- expect_silent(generate_diagnostic_recommendations(na))
  expect_null(recs$rhat)
  expect_match(recs$overall, "could not be computed")
  ok <- list(basic = list(rhat = c(1.001, 1.002), ess_bulk = c(900, 1000), ess_tail = c(800, 900)),
             divergence = list(n_divergent = 0, max_treedepth_hit = FALSE))
  expect_match(generate_diagnostic_recommendations(ok)$overall, "All diagnostics pass")
})

test_that("recommendations and convergence share boundaries; BFMI and split R-hat are advisory", {
  mk <- function(rhat = 1, ess = 1000, bfmi = 1, split = NA)
    list(basic = list(rhat = rhat, ess_bulk = ess, ess_tail = ess),
         divergence = list(n_divergent = 0, max_treedepth_hit = FALSE, energy = list(bfmi = bfmi)),
         rhat_variants = list(split_rhat = split))
  edge <- mk(rhat = 1.009, ess = 401)
  expect_true(assess_overall_convergence(edge)$converged)
  expect_null(generate_diagnostic_recommendations(edge)$rhat)
  expect_null(generate_diagnostic_recommendations(edge)$ess)
  over <- mk(rhat = 1.01, ess = 400)
  expect_false(assess_overall_convergence(over)$converged)
  expect_match(generate_diagnostic_recommendations(over)$rhat, "^WARNING")
  expect_match(generate_diagnostic_recommendations(over)$ess, "^WARNING")
  low <- mk(bfmi = 0.1, split = 1.06)
  expect_true(assess_overall_convergence(low)$converged)
  expect_false(assess_overall_convergence(low)$advisory$bfmi_ok)
  recs <- generate_diagnostic_recommendations(low)
  expect_match(recs$energy, "^ADVISORY")
  expect_match(recs$split_rhat, "^ADVISORY")
  expect_match(recs$overall, "advisory")
})

test_that("ESS threshold is 100 per chain for bulk and tail", {
  mk <- function(bulk, tail, chains = NULL)
    list(basic = list(rhat = 1, ess_bulk = bulk, ess_tail = tail), n_chains = chains,
         divergence = list(n_divergent = 0, max_treedepth_hit = FALSE))
  expect_true(assess_overall_convergence(mk(201, 201, 2))$converged)
  expect_false(assess_overall_convergence(mk(200, 900, 2))$converged)
  expect_false(assess_overall_convergence(mk(900, 200, 2))$converged)
  expect_false(assess_overall_convergence(mk(500, 500, 6))$converged)
  expect_true(assess_overall_convergence(mk(601, 601, 6))$converged)
  expect_match(generate_diagnostic_recommendations(mk(150, 900, 2))$ess, "ESS <= 200")
  expect_match(generate_diagnostic_recommendations(mk(150, 900))$ess, "ESS <= 400")
  expect_match(generate_diagnostic_recommendations(list(basic = list(rhat = 1.2)))$rhat,
               "R-hat >= 1.01")
})

test_that("PPC p-values count replicates equal to the observed statistic", {
  local_mocked_bindings(ppc_replicates = function(...) matrix(c(1, 2, 3, 2), 4, 4, byrow = TRUE))
  out <- perform_ppc_checks(NULL, data.frame(outcome = c(1, 2, 3, 2)))
  expect_equal(c(out$p_mean, out$p_sd), c(1, 1))
})

test_that("summaries over all-NA diagnostic vectors return NA without warnings", {
  expect_true(is.na(expect_silent(.max_present(c(NA_real_, NA_real_)))))
  expect_true(is.na(expect_silent(.min_present(numeric(0)))))
  expect_equal(.max_present(c(1, Inf, NA)), Inf)
})

skip_if_no_fit <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
}

test_that("diagnostics pipeline populates PPC and a boolean converged flag", {
  skip_if_no_fit()
  old <- options(scdbayes.threads = NULL)
  on.exit(options(old), add = TRUE)
  set.seed(1)
  d <- simulate_scd_data(n_cases = 3)
  d <- process_scd_phases(d, structure = list(), scd_type = "AB")
  m <- build_model(d, "outcome ~ time_A + level_AB + trend_AB + (1|case)",
                   iter = 300, chains = 1, family = "gaussian")
  skip_if(is.null(m), "model did not fit")

  conv <- compute_convergence_diagnostics(m, data = d)
  # diagnose_chains must not abort the pipeline: chains, ppc and converged all set
  expect_false(is.null(conv$chains))
  expect_equal(conv$n_chains, 1)
  expect_false(is.null(conv$ppc))
  expect_true(is.logical(conv$converged$converged))
  expect_true(is.logical(conv$ppc$fit_heuristic))
  # posterior predictive fit is reported separately, never folded into converged
  expect_null(conv$converged$criteria$ppc_ok)
})

test_that("prior_predictive_check returns coverage of the observed range", {
  skip_if_no_fit()
  old <- options(scdbayes.threads = NULL)
  on.exit(options(old), add = TRUE)
  set.seed(2)
  d <- simulate_scd_data(n_cases = 3)
  res <- run_scd_analysis(d, options = list(quick_test = TRUE,
                                            prior_type = "informative"))
  skip_if(is.null(res$model), "model did not fit")

  pp <- prior_predictive_check(res$model)
  skip_if(is.null(pp), "prior-only sampling unavailable")
  expect_true(all(c("predictive_median", "predictive_lower", "predictive_upper",
                    "observed_min", "observed_max", "covers_observed") %in% names(pp)))
  expect_true(pp$predictive_lower <= pp$predictive_upper)
  expect_true(is.logical(pp$covers_observed))
})

test_that("rank R-hat is the max of bulk and rank-normalized folded R-hat", {
  set.seed(1)
  x <- c(rnorm(500), rnorm(500, 0.3), rt(500, 2), rnorm(500, sd = 3))
  d <- posterior::as_draws_df(posterior::as_draws_array(array(x, c(500, 4, 1),
                                                              dimnames = list(NULL, NULL, "a"))))
  v <- compute_rhat_variants(d)
  expect_equal(v$rank_rhat, max(v$bulk_rhat, v$folded_rhat))
  expect_equal(v$rank_rhat, posterior::rhat(posterior::extract_variable_matrix(d, "a")))
  expect_gt(v$folded_rhat, v$bulk_rhat)
})
