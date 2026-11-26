test_that(".build_power_formula always fits the onset slope from two units on", {
  f0 <- .build_power_formula(n_cases = 6, slope_sd = 0, coefficient = "level")
  expect_match(deparse1(f0), "(1 + phase_num | case)", fixed = TRUE)
  f2 <- .build_power_formula(n_cases = 2, slope_sd = 0, coefficient = "level")
  expect_match(deparse1(f2), "(1 + phase_num | case)", fixed = TRUE)
  ft <- .build_power_formula(n_cases = 6, slope_sd = 0.4, coefficient = "trend")
  expect_match(deparse1(ft), "(0 + time_center || case)", fixed = TRUE)
  expect_false(grepl("|", deparse1(.build_power_formula(n_cases = 1)), fixed = TRUE))
  fs <- .build_power_formula(n_cases = 1, n_studies = 3)
  expect_match(deparse1(fs), "(1 + phase_num | case)", fixed = TRUE)
})

test_that("prob_meaningful is one-sided in the improvement direction", {
  rope <- c(-1, 1)
  expect_true(.power_decision(rnorm(2000, 3, 0.5), "prob_meaningful", 0.95, rope))
  expect_false(.power_decision(rnorm(2000, -3, 0.5), "prob_meaningful", 0.95, rope))
  expect_true(.power_decision(rnorm(2000, -3, 0.5), "prob_direction", 0.95, rope))
})

test_that("prob_meaningful follows the intervention goal; pd stays two-sided", {
  rope <- c(-1, 1)
  up <- rnorm(2000, 3, 0.5)
  down <- rnorm(2000, -3, 0.5)
  expect_true(.power_decision(down, "prob_meaningful", 0.95, rope, "decrease"))
  expect_false(.power_decision(up, "prob_meaningful", 0.95, rope, "decrease"))
  expect_true(.power_decision(up, "prob_meaningful", 0.95, rope, "increase"))
  expect_true(.power_decision(up, "prob_direction", 0.95, rope, "decrease"))
  expect_true(.power_decision(down, "prob_direction", 0.95, rope, "increase"))
  expect_true(.power_decision(down, "rope", 0.95, rope, "decrease"))
  expect_false(.power_decision(rnorm(2000, 0, 0.2), "rope", 0.95, rope, "decrease"))
})

test_that("reversal transitions fire only when all three steps replicate the effect", {
  set.seed(4)
  rope <- c(-1, 1)
  step <- function(m) rnorm(2000, m, 0.5)
  signs <- c(1, -1, 1)
  expect_true(.transition_decision(list(step(3), step(-3), step(3)), signs,
                                   "prob_direction", 0.975, rope))
  expect_false(.transition_decision(list(step(3), step(3), step(3)), signs,
                                    "prob_direction", 0.975, rope))
  expect_false(.transition_decision(list(step(3), step(-3), step(0)), signs,
                                    "prob_meaningful", 0.975, rope))
  expect_true(.transition_decision(list(step(-3), step(3), step(-3)), -signs,
                                   "prob_meaningful", 0.975, rope))
  expect_false(.transition_decision(list(step(3), step(3), step(3)), signs, "rope", 0.975, rope))
  expect_equal(.power_terms("reversal", "level")$terms,
               c("level_A1B1", "level_B1A2", "level_A2B2"))
  expect_equal(.power_terms("reversal", "slope_change")$signs, c(1, -1, 1))
  expect_equal(.power_terms("reversal", "level", "first_transition")$terms, "level_A1B1")
  expect_equal(.power_terms("reversal", "trend")$terms, "time_A1")
  expect_equal(.power_terms("ab", "level")$terms, "phase_num")
})

test_that("reversal power data carry the analysis step coding with a common effect", {
  d <- .simulate_power_data("reversal", "gaussian", n_cases = 2, n_studies = 1, n_phase_a = 3,
                            n_phase_b = 2, effect = -1, coefficient = "level", autocorr = 0,
                            icc = 0, study_sd = 0, standardized = FALSE, baseline_sd = 1e-8)
  one <- d[d$case == "S1C1", ]
  expect_true(all(c("time_A1", "level_A1B1", "trend_A1B1", "level_B1A2", "trend_B1A2",
                    "level_A2B2", "trend_A2B2") %in% names(d)))
  expect_equal(one$level_A1B1, rep(c(0, 1), c(3, 7)))
  expect_equal(one$level_B1A2, rep(c(0, 1), c(5, 5)))
  expect_equal(one$level_A2B2, rep(c(0, 1), c(8, 2)))
  expect_equal(one$trend_B1A2, c(rep(0, 5), 0:4))
  expect_equal(one$time_A1, 0:9)
  # steps of -1, +1, -1 for a decrease of magnitude 1
  expect_equal(diff(one$outcome)[c(3, 5, 8)], c(-1, 1, -1), tolerance = 1e-6)
})

test_that("reversal power formula matches the analysis terms and random effects", {
  f <- deparse1(.build_power_formula(n_cases = 4, design = "reversal"))
  expect_match(f, paste("outcome ~ time_A1 + level_A1B1 + trend_A1B1 + level_B1A2 +",
                        "trend_B1A2 + level_A2B2 + trend_A2B2"), fixed = TRUE)
  expect_match(f, "(1 + level_A1B1 | case) + (0 + level_B1A2 + level_A2B2 || case)",
               fixed = TRUE)
  expect_false(grepl("phase_num", f, fixed = TRUE))
  fs <- deparse1(.build_power_formula(n_cases = 3, n_studies = 2, study_sd = 0.3,
                                      slope_sd = 0.3, coefficient = "slope_change",
                                      design = "reversal"))
  expect_match(fs, "(0 + trend_A1B1 + trend_B1A2 + trend_A2B2 || case)", fixed = TRUE)
  expect_match(fs, "(1 + trend_A1B1 + trend_B1A2 + trend_A2B2 || study)", fixed = TRUE)
  expect_false(grepl("|", deparse1(.build_power_formula(n_cases = 1, design = "reversal")),
                     fixed = TRUE))
  d <- data.frame(case = rep(c("C1", "C2"), each = 12), time = rep(1:12, 2),
                  phase = rep(rep(c("A", "B", "A", "B"), each = 3), 2), outcome = rnorm(24))
  expect_equal(default_random_effects(process_scd_phases(d, list(), "reversal"), "reversal"),
               "(1 + level_A1B1 | case) + (0 + level_B1A2 + level_A2B2 || case)")
})

test_that("power priors match the analysis defaults", {
  set.seed(3)
  d <- .simulate_power_data("ab", "gaussian", n_cases = 4, n_studies = 1, n_phase_a = 5,
                            n_phase_b = 5, effect = 0.5, coefficient = "level", autocorr = 0,
                            icc = 0.2, study_sd = 0, standardized = TRUE)
  f <- .build_power_formula(n_cases = 4)
  pr <- .power_priors(d, f, "gaussian", 4, TRUE)
  b <- pr$prior[pr$class == "b" & pr$coef == "" & pr$dpar == ""]
  expect_equal(b, sprintf("normal(0.00, %.2f)", 2.5 * sd(d$outcome)))
  expect_equal(pr$prior[pr$class == "ar"], "normal(0, 0.5)")
  expect_equal(as.numeric(c(pr$lb[pr$class == "ar"], pr$ub[pr$class == "ar"])), c(-1, 1))
  expect_true("lkj(2)" %in% pr$prior[pr$class == "cor"])
})

test_that("reversal power priors validate against the reversal formula", {
  set.seed(5)
  d <- .simulate_power_data("reversal", "gaussian", n_cases = 3, n_studies = 2, n_phase_a = 4,
                            n_phase_b = 4, effect = 0, coefficient = "slope_change",
                            autocorr = 0.2, icc = 0.2, study_sd = 0.2, slope_sd = 0.2,
                            standardized = TRUE)
  f <- .build_power_formula(n_cases = 3, n_studies = 2, study_sd = 0.2, slope_sd = 0.2,
                            coefficient = "slope_change", design = "reversal")
  pr <- .power_priors(d, f, "gaussian", 6, TRUE)
  expect_s3_class(brms::validate_prior(pr, f, data = d, family = gaussian()), "brmsprior")
})

test_that("time_since_change is 0 at the first intervention session", {
  set.seed(8)
  d <- .gen_case("S1", "C1", rep(c("A", "B"), c(3, 4)), rep(c(0, 1), c(3, 4)),
                 "gaussian", 0, 0, "level", 50, 10, 0, standardized = TRUE)
  expect_equal(d$time_since_change, c(0, 0, 0, 0, 1, 2, 3))
  s <- simulate_scd_data(n_cases = 1, n_phase_a = 3, n_phase_b = 4)
  expect_equal(s$time_since_change, c(0, 0, 0, 0, 1, 2, 3))
})

test_that(".simulate_power_data injects between-case slope heterogeneity", {
  set.seed(7)
  case_shift_sd <- function(ss) {
    d <- .simulate_power_data(design = "ab", family = "gaussian", n_cases = 10,
                              n_studies = 1, n_phase_a = 8, n_phase_b = 8,
                              effect = 0.8, coefficient = "level", autocorr = 0,
                              icc = 0.2, study_sd = 0, slope_sd = ss,
                              standardized = TRUE, baseline_sd = 10)
    agg <- aggregate(outcome ~ case + phase, d, mean)
    w <- stats::reshape(agg, idvar = "case", timevar = "phase", direction = "wide")
    stats::sd(w$outcome.B - w$outcome.A)
  }
  expect_gt(case_shift_sd(1.0), case_shift_sd(0))
})

test_that("Agresti-Coull mc_se stays positive at the boundaries", {
  expect_equal(.agresti_coull_se(200, 200), sqrt((202 / 204) * (2 / 204) / 204))
  expect_gt(.agresti_coull_se(0, 200), 0)
  expect_true(is.na(.agresti_coull_se(0, 0)))
})

test_that("quick power uses the two-sample noncentrality on the effective n", {
  q <- quick_power_estimate(n_cases = 4, n_timepoints = 10, effect_size = 0.8,
                            autocorrelation = 0)
  df <- 40 - 2
  ncp <- 0.8 * sqrt(20 * 20 / 40)
  crit <- stats::qt(0.975, df)
  expect_equal(q$power, 1 - stats::pt(crit, df, ncp) + stats::pt(-crit, df, ncp))
  expect_null(q$icc)
})

test_that("meta setting splits the between-unit variance across study and case", {
  set.seed(11)
  sims <- replicate(100, {
    d <- .simulate_power_data("ab", "gaussian", n_cases = 3, n_studies = 4,
                              n_phase_a = 50, n_phase_b = 50, effect = 0,
                              coefficient = "level", autocorr = 0, icc = 0.5,
                              study_sd = 0, standardized = TRUE)
    stats::var(tapply(d$outcome, d$case, mean))
  })
  # E[s^2] = 50 + 1 + 50 * 9 / 11 when tau^2 = 100 splits evenly; ~192 if doubled
  expect_equal(mean(sims), 92, tolerance = 0.1)
})

test_that("run_power_simulation exposes a seed argument", {
  expect_true("seed" %in% names(formals(run_power_simulation)))
})

test_that("quick power carries the cross-phase covariance and floors at alpha", {
  q <- quick_power_estimate(n_cases = 4, n_timepoints = 10, effect_size = 0.5,
                            autocorrelation = 0.5)
  R <- 0.5^abs(outer(1:10, 1:10, "-"))
  cvec <- rep(c(-1 / 5, 1 / 5), each = 5)
  v <- drop(cvec %*% R %*% cvec)
  expect_lt(v, 2 / 5 * (1 + 2 * sum((1 - (1:4) / 5) * 0.5^(1:4))))
  ncp <- 0.5 / sqrt(v / 4)
  df <- 40 / (v / (2 / 5)) - 2
  crit <- stats::qt(0.975, df)
  expect_equal(q$power, 1 - stats::pt(crit, df, ncp) + stats::pt(-crit, df, ncp))
  expect_lt(quick_power_estimate(4, 10, effect_size = 0.001, alpha = 0.01)$power, 0.011)
  expect_equal(quick_power_estimate(4, 10, effect_size = 0, alpha = 0.01)$power, 0.01)
  expect_false("icc" %in% names(formals(quick_power_estimate)))
})

test_that("the seed restorer puts back the global RNG state", {
  set.seed(5)
  restore <- .seed_restorer()
  expected <- stats::runif(1)
  restore()
  set.seed(99)
  stats::runif(3)
  restore()
  expect_equal(stats::runif(1), expected)
})

test_that("simulation convergence screen fails closed", {
  set.seed(12)
  mixed <- posterior::as_draws_df(posterior::draws_array(b_x = array(rnorm(2000), c(1000, 2, 1))))
  stuck <- posterior::as_draws_df(posterior::draws_array(
    b_x = array(c(rnorm(1000), rnorm(1000, 5)), c(1000, 2, 1))))
  expect_true(.sim_converged(mixed, "b_x"))
  expect_false(.sim_converged(stuck, "b_x"))
  expect_false(.sim_converged(mixed, "b_missing"))
})

test_that("simulation screen accepts Rhat between 1.01 and 1.05 but needs bulk ESS", {
  set.seed(21)
  offset <- posterior::draws_array(b_x = array(c(rnorm(1000), rnorm(1000, 0.36)), c(1000, 2, 1)))
  r <- posterior::rhat(posterior::extract_variable_matrix(offset, "b_x"))
  expect_gt(r, 1.01)
  expect_lt(r, 1.05)
  expect_true(.sim_converged(posterior::as_draws_df(offset), "b_x"))
  short <- posterior::as_draws_df(posterior::draws_array(b_x = array(rnorm(60), c(30, 2, 1))))
  expect_lt(posterior::ess_bulk(posterior::extract_variable_matrix(short, "b_x")), 100)
  expect_false(.sim_converged(short, "b_x"))
})

test_that("power_all counts non-converged replications as non-detections", {
  local_mocked_bindings(nuts_params = function(...) data.frame(Value = 0), .package = "brms")
  draw_fit <- function(stuck) {
    x <- if (stuck) c(rnorm(1000, 5), rnorm(1000, 50)) else rnorm(2000, 5)
    posterior::as_draws_df(posterior::draws_array(b_phase_num = array(x, c(1000, 2, 1))))
  }
  calls <- 0L
  local_mocked_bindings(brm = function(...) structure(list(), class = "template_stub"),
                        .package = "brms")
  local_mocked_bindings(update = function(...) { calls <<- calls + 1L; draw_fit(calls > 2) },
                        .package = "stats")
  out <- suppressWarnings(run_power_simulation(n_sims = 4, n_cases = 6, effect_sizes = 0,
                                               fit_autocor = FALSE, seed = 1))
  expect_equal(out$n_valid, 2)
  expect_equal(out$n_nonconverged, 2)
  expect_equal(out$power, 1)
  expect_equal(out$power_all, 0.5)
  expect_equal(out$mc_se_all, .agresti_coull_se(2, 4))
  expect_equal(out$mc_se, .agresti_coull_se(2, 2))
  expect_identical(names(out)[4:6], c("mc_se", "power_all", "mc_se_all"))
  expect_warning(run_power_simulation(n_sims = 4, n_cases = 6, effect_sizes = 0,
                                      fit_autocor = FALSE, seed = 1), "power_all")
})

test_that("count power refuses unsupported serial dependence", {
  expect_error(run_power_simulation(n_sims = 2, n_cases = 6, family = "poisson", autocorr = 0.2), "serial")
})

test_that("replication seeds depend only on seed, effect size and replication", {
  expect_identical(.replication_seed(2026, 0.5, 3), .replication_seed(2026, 0.5, 3))
  seeds <- outer(c(0, 0.5, 1, 1.5), 1:200, Vectorize(function(d, s) .replication_seed(2026, d, s)))
  expect_equal(length(unique(as.vector(seeds))), length(seeds))
  expect_true(all(seeds >= 0))
  draw <- function() {
    set.seed(.replication_seed(2026, 0.5, 7))
    .simulate_power_data("ab", "gaussian", n_cases = 3, n_studies = 1, n_phase_a = 4,
                         n_phase_b = 4, effect = 0.5, coefficient = "level", autocorr = 0.2,
                         icc = 0.2, study_sd = 0, standardized = TRUE)
  }
  first <- draw()
  stats::runif(50)
  expect_identical(draw(), first)
})

test_that("reversal case deviations are drawn independently per transition", {
  set.seed(13)
  d <- .simulate_power_data("reversal", "gaussian", n_cases = 400, n_studies = 1,
                            n_phase_a = 3, n_phase_b = 3, effect = 0, coefficient = "level",
                            autocorr = 0, icc = 0, study_sd = 0, slope_sd = 1,
                            standardized = FALSE, baseline_sd = 1e-8)
  steps <- t(sapply(split(d$outcome, d$case), function(y) diff(y)[c(3, 6, 9)]))
  expect_lt(abs(stats::cor(steps[, 1], steps[, 2])), 0.2)
  expect_lt(abs(stats::cor(steps[, 2], steps[, 3])), 0.2)
  expect_equal(stats::sd(steps[, 2]), 1, tolerance = 0.15)
})

test_that("AB case deviations keep the single onset coding", {
  set.seed(14)
  d <- .gen_case("S1", "C1", rep(c("A", "B"), c(3, 4)), rep(c(0, 1), c(3, 4)),
                 "gaussian", 0, 0, "slope_change", 50, 1e-8, 0, case_slope = 2,
                 standardized = FALSE)
  expect_equal(d$outcome - 50, 2 * d$time_since_change, tolerance = 1e-6)
})

test_that("power template and refits share the analysis adapt_delta", {
  captured <- NULL
  local_mocked_bindings(brm = function(...) { captured <<- list(...); stop("stub") },
                        .package = "brms")
  expect_error(suppressWarnings(run_power_simulation(n_sims = 1, n_cases = 3,
                                                     effect_sizes = 0, seed = 1)),
               "template model failed")
  expect_equal(captured$control$adapt_delta, 0.99)
})

test_that("sequential update uses package default priors, not flat priors", {
  captured <- NULL
  local_mocked_bindings(brm = function(...) { captured <<- list(...); stop("stub") },
                        .package = "brms")
  expect_true("priors" %in% names(formals(run_sequential_update)))
  d <- simulate_scd_data(n_cases = 3)
  expect_error(run_sequential_update(d), "stub")
  b <- captured$prior$prior[captured$prior$class == "b" & captured$prior$coef == ""]
  expect_match(b, "normal(0, ", fixed = TRUE)
  user <- brms::prior_string("normal(0, 1)", class = "b") +
    brms::prior_string("normal(0, 0.5)", class = "ar", lb = -1, ub = 1)
  expect_error(suppressWarnings(run_sequential_update(
    d, formula_str = "outcome ~ phase + (1|case) + ar(time = time, gr = case, p = 1)",
    priors = user)), "stub")
  expect_equal(captured$prior$prior, "normal(0, 1)")
})

test_that("sequential default priors are scaled on the first two cases only", {
  scaled_on <- NULL
  local_mocked_bindings(.default_priors = function(data, ...) { scaled_on <<- data; stop("stub") })
  d <- simulate_scd_data(n_cases = 4)
  expect_error(run_sequential_update(d), "stub")
  expect_setequal(unique(scaled_on$case), unique(d$case)[1:2])
})

test_that("sequential default Intercept and sigma priors are pinned from the first two cases", {
  captured <- NULL
  local_mocked_bindings(brm = function(...) { captured <<- list(...)$prior; stop("stub") },
                        .package = "brms")
  d <- simulate_scd_data(n_cases = 4)
  d$outcome[d$case %in% unique(d$case)[3:4]] <- d$outcome[d$case %in% unique(d$case)[3:4]] + 500
  expect_error(run_sequential_update(d), "stub")
  first_two <- d[d$case %in% unique(d$case)[1:2], ]
  ref <- brms::get_prior(outcome ~ phase + (1 | case), data = first_two)
  pinned <- captured[captured$class %in% c("Intercept", "sigma"), ]
  expect_setequal(pinned$class, c("Intercept", "sigma"))
  expect_equal(pinned$prior[pinned$class == "Intercept"],
               ref$prior[ref$class == "Intercept" & ref$coef == ""])
  expect_equal(pinned$prior[pinned$class == "sigma"], ref$prior[ref$class == "sigma"])
  expect_true(all(pinned$source == "user"))
})
test_that("power rejects invalid planning inputs before sampling", {
  local_mocked_bindings(brm = function(...) stop("sampling reached"), .package = "brms")
  expect_error(run_power_simulation(n_sims = 0), "n_sims")
  expect_error(run_power_simulation(n_cases = 1.5), "n_cases")
  expect_error(run_power_simulation(n_phase_a = NA_real_), "n_phase_a")
  expect_error(run_power_simulation(autocorr = 1), "autocorr")
  expect_error(run_power_simulation(icc = 1), "icc")
  expect_error(run_power_simulation(effect_sizes = numeric()), "effect_sizes")
  expect_error(run_power_simulation(prob_threshold = 1), "prob_threshold")
  expect_error(run_power_simulation(slope_sd = -1), "slope_sd")
  expect_error(run_power_simulation(chains = 1), "chains")
  expect_error(run_power_simulation(seed = NA_real_), "seed")
})

test_that("power rejects absent or invalid divergence diagnostics", {
  local_mocked_bindings(nuts_params = function(...) stop("unavailable"), .package = "brms")
  expect_true(is.na(.power_divergences(NULL)))
  local_mocked_bindings(nuts_params = function(...) data.frame(Value = numeric()), .package = "brms")
  expect_true(is.na(.power_divergences(NULL)))
  local_mocked_bindings(nuts_params = function(...) data.frame(Value = c(0, NA_real_)), .package = "brms")
  expect_true(is.na(.power_divergences(NULL)))
  local_mocked_bindings(nuts_params = function(...) data.frame(Value = c(0, 1, 0)), .package = "brms")
  expect_equal(.power_divergences(NULL), 1)
})

test_that("power excludes replications with unavailable sampler diagnostics", {
  local_mocked_bindings(brm = function(...) structure(list(), class = "template_stub"), .package = "brms")
  draws <- posterior::as_draws_df(posterior::draws_array(
    b_phase_num = array(stats::rnorm(2000, 5), c(1000, 2, 1))))
  local_mocked_bindings(update = function(...) draws, .package = "stats")
  local_mocked_bindings(nuts_params = function(...) stop("unavailable"), .package = "brms")
  out <- suppressWarnings(run_power_simulation(n_sims = 2, n_cases = 6,
    effect_sizes = 0, fit_autocor = FALSE, seed = 17))
  expect_equal(out$n_valid, 0)
  expect_equal(out$n_nonconverged, 2)
  expect_equal(out$n_diagnostic_failed, 2)
  expect_equal(out$n_divergent, 0)
  expect_true(is.na(out$power))
  expect_equal(out$power_all, 0)
})
