test_that(".prior_model_type maps families to the correct link group", {
  expect_equal(.prior_model_type("gaussian"), "gaussian")
  expect_equal(.prior_model_type("negbinomial"), "poisson")
  expect_equal(.prior_model_type("Gamma"), "poisson")
  expect_equal(.prior_model_type("cumulative"), "binomial")
  expect_equal(.prior_model_type("bernoulli"), "binomial")
  expect_equal(.prior_model_type("unknown_family"), "gaussian")
  expect_equal(.prior_model_type("hurdle_poisson"), "poisson")
  expect_equal(.prior_model_type("zero_inflated_negbinomial"), "poisson")
  expect_equal(.prior_model_type("weibull"), "poisson")
  expect_equal(.prior_model_type("exponential"), "poisson")
  expect_equal(.prior_model_type("lognormal"), "poisson")
  expect_equal(.prior_model_type("zero_one_inflated_beta"), "binomial")
  expect_equal(.prior_model_type("student"), "gaussian")
})

test_that("a cor prior is emitted only for a correlated group term", {
  expect_true(.has_correlated_term("y ~ x + (1 + level_AB | case)"))
  expect_true(.has_correlated_term("y ~ x + (1 + level_AB | p | case)"))
  expect_false(.has_correlated_term("y ~ x + (1 | case) + (0 + treatment_B + treatment_C || case)"))
  expect_false(.has_correlated_term("y ~ x + (1 | case) + (0 + level_AB | case)"))
  expect_false(.has_correlated_term("y ~ x"))
  d <- data.frame(outcome = rnorm(12), case = rep(c("a", "b", "c"), 4),
                  treatment_B = rep(0:1, 6), treatment_C = rep(c(0, 0, 1), 4))
  ap <- generate_adaptive_priors(d, list(n_levels = 2))
  f <- "outcome ~ treatment_B + treatment_C + (1 | case) + (0 + treatment_B + treatment_C || case)"
  pl <- assemble_brms_priors("default", NULL, NULL, FALSE, "gaussian", ap,
                             "treatment_B", has_random_slope(f), f)
  expect_null(pl$correlation)
  expect_no_error(brms::validate_prior(priors_to_brms(pl), stats::as.formula(f), data = d))
})

test_that("meta-analytic prior SD must be positive and finite; sensitivity widens slopes", {
  d <- process_scd_phases(simulate_scd_data(n_cases = 3), list(), "AB")
  ap <- generate_adaptive_priors(d, list())
  expect_error(assemble_brms_priors("meta_analytic", 0.5, -1, FALSE, "gaussian", ap), "positive")
  expect_error(assemble_brms_priors("meta_analytic", 0.5, NA, FALSE, "gaussian", ap), "positive")
  f <- "outcome ~ time_A + level_AB + trend_AB"
  sens <- assemble_brms_priors("sensitivity", NULL, NULL, FALSE, "gaussian", ap, "level_AB", FALSE, f)
  expect_equal(sens$slope_time_A$string, sprintf("normal(0, %.4f)", 2 * ap$slopes[["time_A"]]))
})

test_that("non-default priors build a valid brmsprior for every family", {
  set.seed(1)
  d <- simulate_scd_data(n_cases = 3)
  d$outcome <- round(abs(d$outcome) * 3) + 1
  structure <- list(n_levels = 2, levels = list(case = "case"))
  fams <- c("negbinomial", "poisson", "cumulative", "bernoulli", "beta",
            "Gamma", "gaussian")
  for (f in fams) {
    ap <- generate_adaptive_priors(d, structure, model_type = f)
    pl <- suppressWarnings(assemble_brms_priors("informative", NULL, NULL, FALSE, f, ap))
    expect_true(nzchar(pl$intercept$string), info = f)
    expect_s3_class(priors_to_brms(pl), "brmsprior")
  }
})

test_that("intercept prior is built on the family's link scale", {
  set.seed(2)
  d <- simulate_scd_data(n_cases = 3)
  d$outcome <- round(abs(d$outcome) * 3) + 1
  structure <- list(n_levels = 2)
  log_loc <- log(mean(d$outcome))
  ap_pois <- generate_adaptive_priors(d, structure, "negbinomial")
  expect_equal(ap_pois$intercept$mean, log_loc, tolerance = 1e-6)
  ap_logit <- generate_adaptive_priors(d, structure, "cumulative")
  expect_equal(ap_logit$intercept$mean, 0)
})

test_that("shape, phi, and nu keep the brms defaults; Gamma gets no sigma prior", {
  set.seed(3)
  d <- simulate_scd_data(n_cases = 3)
  d$outcome <- round(abs(d$outcome)) + 1
  for (f in c("Gamma", "negbinomial", "beta", "student")) {
    ap <- generate_adaptive_priors(d, list(n_levels = 2), f)
    pl <- suppressWarnings(assemble_brms_priors("informative", NULL, NULL, FALSE, f, ap))
    expect_false(any(c("shape", "phi", "nu") %in% vapply(pl, `[[`, "", "class")), info = f)
  }
  ap <- generate_adaptive_priors(d, list(n_levels = 2), "Gamma")
  expect_null(assemble_brms_priors("default", NULL, NULL, FALSE, "Gamma", ap)[["sigma"]])
  expect_match(ap$recommendations$dispersion, "shape")
  expect_match(generate_adaptive_priors(d, list(), "beta")$recommendations$dispersion, "phi")
  expect_null(generate_adaptive_priors(d, list(), "poisson")$recommendations$dispersion)
})

test_that("adaptive priors read the named outcome column, never a stray 'outcome'", {
  set.seed(6)
  x <- simulate_scd_data(n_cases = 3)
  x$wcpm <- x$outcome * 10
  x$outcome <- NULL
  p <- generate_adaptive_priors(x, list(n_levels = 2), outcome_var = "wcpm")
  expect_equal(p$treatment$sd, 2.5 * sd(x$wcpm))
  p2 <- generate_adaptive_priors(x, list(n_levels = 2, outcome_var = "wcpm"))
  expect_equal(p2$treatment$sd, 2.5 * sd(x$wcpm))
  x$outcome <- 1:nrow(x)
  p3 <- generate_adaptive_priors(x, list(), outcome_var = "wcpm")
  expect_equal(p3$treatment$sd, 2.5 * sd(x$wcpm))
})

test_that("the class-level b prior is dropped when every fixed term has its own prior", {
  d <- process_scd_phases(simulate_scd_data(n_cases = 3), list(), "AB")
  ap <- generate_adaptive_priors(d, list(n_levels = 2), "gaussian")
  full <- "outcome ~ time_A + level_AB + trend_AB + (1 + level_AB | case)"
  pl <- assemble_brms_priors("informative", 1, NULL, FALSE, "gaussian", ap,
                             treatment_coef = "level_AB", has_random_slope = TRUE,
                             formula_str = full)
  expect_null(pl$b_default)
  expect_no_warning(brms::validate_prior(priors_to_brms(pl), stats::as.formula(full), data = d))
  part <- assemble_brms_priors("informative", 1, NULL, FALSE, "gaussian", ap,
                               treatment_coef = "level_AB", formula_str = "outcome ~ level_AB + phase")
  expect_false(is.null(part$b_default))
})

test_that("heterogeneous sigma intercept is centered on the log pooled within-phase SD", {
  d <- simulate_scd_data(n_cases = 3)
  ap <- generate_adaptive_priors(d, list(), "gaussian")
  pl <- assemble_brms_priors("default", NULL, NULL, TRUE, "gaussian", ap)
  expect_equal(pl$sigma_intercept$string,
               sprintf("normal(%.2f, 1.00)", log(.pooled_within_sd(d))))
  expect_equal(pl$sigma_b$string, "normal(0, 1.00)")
  expect_equal(pl$sigma_intercept$dpar, "sigma")
})

test_that("session-count slopes get priors scaled to the input SD", {
  d <- process_scd_phases(simulate_scd_data(n_cases = 3), list(), "AB")
  ap <- generate_adaptive_priors(d, list(), "gaussian")
  expect_equal(ap$slopes[["time_A"]], 2.5 * sd(d$outcome) / (2 * sd(d$time_A)))
  f <- "outcome ~ time_A + level_AB + trend_AB + (1 + level_AB | case)"
  pl <- assemble_brms_priors("default", NULL, NULL, FALSE, "gaussian", ap,
                             has_random_slope = TRUE, formula_str = f)
  expect_equal(pl$slope_time_A$coef, "time_A")
  expect_equal(pl$slope_trend_AB$string, sprintf("normal(0, %.4f)", ap$slopes[["trend_AB"]]))
  expect_null(pl$slope_level_AB)
  expect_null(assemble_brms_priors("default", NULL, NULL, FALSE, "gaussian", ap)$slope_time_A)
  dp <- .default_priors(d, stats::as.formula(f), "gaussian")
  expect_true(all(c("time_A", "trend_AB") %in% dp$coef))
  expect_s3_class(brms::validate_prior(priors_to_brms(pl), stats::as.formula(f), data = d),
                  "brmsprior")
})

test_that("priors_to_brms drops malformed strings and returns NULL when empty", {
  expect_null(priors_to_brms(list(x = list(class = "b", string = character(0)))))
  expect_null(priors_to_brms(list(x = list(class = "b", string = NA_character_))))
})

test_that("default priors scale to the outcome for identity link only", {
  set.seed(4)
  d <- simulate_scd_data(n_cases = 3)
  fo <- stats::as.formula("outcome ~ level_AB + (1|case)")
  sd_y <- sd(d$outcome)
  gauss <- .default_priors(d, fo, "gaussian")
  b_gauss <- gauss$prior[gauss$class == "b"]
  expect_match(b_gauss, sprintf("normal\\(0, %.4f\\)", 2.5 * sd_y))
  pois <- .default_priors(d, fo, "poisson")
  b_pois <- pois$prior[pois$class == "b"]
  expect_match(b_pois, "normal\\(0, 2.5000\\)")
})

test_that("two grouping factors give one sd prior that brms accepts", {
  set.seed(4)
  d <- simulate_scd_data(n_cases = 4)
  d$study <- rep(c("S1", "S2"), length.out = nrow(d))
  ap <- generate_adaptive_priors(d, list(n_levels = 2), "gaussian")
  ap$random_effects <- list(sd_case = list(sd = 1), sd_study = list(sd = 2))
  pl <- assemble_brms_priors("default", NULL, NULL, FALSE, "gaussian", ap)
  expect_equal(sum(vapply(pl, function(p) identical(p$class, "sd"), logical(1))), 1)
  expect_s3_class(
    brms::validate_prior(priors_to_brms(pl), outcome ~ phase + (1 | case) + (1 | study),
                         data = d),
    "brmsprior")
})

test_that("is_prior_string accepts distributions and rejects Stan statements", {
  expect_true(is_prior_string("normal(0, 1)"))
  expect_true(is_prior_string("student_t(3, 0, 2.5)"))
  expect_false(is_prior_string("normal(0, 1"))
  expect_false(is_prior_string("normal(0,1); target += 1e9"))
  expect_false(is_prior_string("normal(0, 1) { }"))
  expect_false(is_prior_string(NA_character_))
  expect_false(is_prior_string(NULL))
})

test_that("is_prior_string enforces a per-class distribution allowlist", {
  expect_true(is_prior_string("normal(-1, 2)", "b"))
  expect_true(is_prior_string("student_t(3, 0, 2.5)", "Intercept"))
  expect_true(is_prior_string("exponential(1)", "sd"))
  expect_true(is_prior_string("gamma(2, 0.1)", "shape"))
  expect_true(is_prior_string("lkj(2)", "cor"))
  expect_true(is_prior_string("uniform(-1, 1)", "ar"))
  expect_false(is_prior_string("lkj(2)", "b"))
  expect_false(is_prior_string("exponential(1)", "b"))
  expect_false(is_prior_string("uniform(-1e9, 1e9)", "b"))
  expect_false(is_prior_string("uniform(-2, 2)", "ar"))
  expect_true(is_prior_string("normal(0, 0.5)", "ar"))
  expect_false(is_prior_string("normal(5, 10)", "ar"))
  expect_false(is_prior_string("normal(-3, 1)", "ma"))
  expect_false(is_prior_string("uniform(-1, 1)"))
  expect_false(is_prior_string("constant(100)"))
  expect_false(is_prior_string("normal(0, 1) * reject(\"x\")"))
  expect_false(is_prior_string("normal(mu, 1)"))
  expect_false(is_prior_string("normal(0, exp(1))"))
  expect_false(is_prior_string("normal(0, 1)", "unknown_class"))
  expect_false(is_prior_string("normal(0, Inf)", "b"))
})

test_that("is_prior_string checks argument counts and positive scales", {
  valid <- c("normal(0, 2.5000)", "student_t(3, 0, 2.5)", "cauchy(0, 1)",
             "skew_normal(0, 1, 2)", "logistic(0, 1)", "double_exponential(0, 1)",
             "exponential(1)", "gamma(2, 0.1)", "inv_gamma(1, 1)", "lkj(2)",
             "lkj_corr_cholesky(1)", "normal(0,1e-3)", " normal( -1 , .5 ) ")
  invalid <- c("normal(0)", "normal(0, 1, 2)", "normal(0, 0)", "normal(0, -1)",
               "student_t(0, 0, 1)", "student_t(3, 0, -1)", "student_t(3, 0)",
               "cauchy(0, 0)", "exponential(0)", "exponential(1, 2)",
               "gamma(0, 1)", "gamma(2, -1)", "inv_gamma(1)", "lkj(0)",
               "lkj(-1)", "skew_normal(0, 0, 1)")
  expect_true(all(vapply(valid, is_prior_string, logical(1))))
  expect_false(any(vapply(invalid, is_prior_string, logical(1))))
  expect_true(is_prior_string("uniform(-0.5, 0.5)", "ar"))
  expect_false(is_prior_string("uniform(0.5, -0.5)", "ar"))
  expect_false(is_prior_string("uniform(0, 0)", "ar"))
})

test_that("is_prior_string rejects hex, integer suffixes, comments, and line breaks", {
  expect_false(is_prior_string("normal(0x0, 1)"))
  expect_false(is_prior_string("normal(0, 1L)"))
  expect_false(is_prior_string("normal(0, 1) # note"))
  expect_false(is_prior_string("normal(0,\n1)"))
  expect_false(is_prior_string("normal(0, 1)\n"))
  expect_false(is_prior_string("normal(0, 1) // x"))
})

test_that(".default_priors adds an sd prior only when the formula has a group term", {
  d <- simulate_scd_data(n_cases = 3)
  grouped <- .default_priors(d, outcome ~ phase_num + (1 | case), "gaussian")
  flat <- .default_priors(d, outcome ~ phase_num, "gaussian")
  expect_true("sd" %in% grouped$class)
  expect_false("sd" %in% flat$class)
  expect_true("b" %in% flat$class)
  d$n <- 10
  trials <- .default_priors(d, outcome | trials(n) ~ phase_num, "binomial")
  expect_false("sd" %in% trials$class)
})

test_that(".default_priors skips class b for an intercept-only formula", {
  d <- simulate_scd_data(n_cases = 3)
  f <- outcome ~ 1 + (1 | case)
  p <- .default_priors(d, f, "gaussian")
  expect_false("b" %in% p$class)
  expect_true("sd" %in% p$class)
  expect_no_error(brms::validate_prior(p, f, data = d))
  ar <- .default_priors(d, outcome ~ 1 + ar(time = time, gr = case, p = 1), "gaussian")
  expect_equal(ar$class, "ar")
})

test_that("informative prior centers the treatment coef on a supplied mean", {
  d <- simulate_scd_data(n_cases = 3)
  ap <- generate_adaptive_priors(d, list(n_levels = 2), "gaussian")
  pl <- assemble_brms_priors("informative", 1.5, NULL, FALSE, "gaussian", ap,
                             treatment_coef = "level_AB")
  expect_equal(attr(pl, "treatment_loc"), 1.5)
  expect_equal(pl$treatment$coef, "level_AB")
  expect_match(pl$treatment$string, "^normal\\(1.50, ")
  pl0 <- assemble_brms_priors("informative", NA, NULL, FALSE, "gaussian", ap,
                              treatment_coef = "level_AB")
  expect_equal(attr(pl0, "treatment_loc"), 0)
})

test_that("elicited prior without a treatment coef warns and drops the mean", {
  d <- simulate_scd_data(n_cases = 3)
  ap <- generate_adaptive_priors(d, list(n_levels = 2), "gaussian")
  expect_warning(
    pl <- assemble_brms_priors("meta_analytic", 0.8, 0.2, FALSE, "gaussian", ap),
    "No treatment coefficient")
  expect_equal(attr(pl, "treatment_loc"), 0)
  expect_null(pl$treatment$coef)
})

test_that("heavy-tail flag uses excess kurtosis", {
  set.seed(5)
  normal_chars <- analyze_data_characteristics(data.frame(outcome = rnorm(500)))
  expect_false(normal_chars$heavy_tailed)
  heavy_chars <- analyze_data_characteristics(data.frame(outcome = rt(500, df = 3)))
  expect_true(heavy_chars$heavy_tailed)
})

test_that("treatment_coef returns the onset column present in the formula", {
  d <- data.frame(level_AB = c(0, 1), trend_AB = c(0, 1), outcome = c(1, 2))
  expect_equal(treatment_coef(d), "level_AB")
  expect_null(treatment_coef(d, "outcome ~ trend_AB"))
})

test_that("logit and log links get normal(0, 2.5) coefficient priors", {
  d <- simulate_scd_data(n_cases = 3)
  d$outcome <- as.integer(d$outcome > stats::median(d$outcome))
  ap <- generate_adaptive_priors(d, list(), "bernoulli")
  expect_equal(ap$treatment$description, "normal(0, 2.50)")
  pl <- assemble_brms_priors("default", NULL, NULL, FALSE, "bernoulli", ap)
  expect_equal(pl$treatment$string, "normal(0.00, 2.50)")
  expect_equal(generate_adaptive_priors(d, list(), "poisson")$treatment$sd, 2.5)
})

test_that("prior width does not shrink with sample size", {
  big <- simulate_scd_data(n_cases = 10, n_phase_a = 30, n_phase_b = 30)
  small <- big[big$case %in% c("C1", "C2"), ]
  small$outcome <- small$outcome * sd(big$outcome) / sd(small$outcome)
  expect_equal(generate_adaptive_priors(small, list(), "gaussian")$treatment$sd,
               generate_adaptive_priors(big, list(), "gaussian")$treatment$sd)
})

test_that("sensitivity prior is wider than the default and lognormal sigma is on the log scale", {
  d <- simulate_scd_data(n_cases = 3)
  ap <- generate_adaptive_priors(d, list(), "gaussian")
  sens <- assemble_brms_priors("sensitivity", NULL, NULL, FALSE, "gaussian", ap)
  expect_equal(attr(sens, "treatment_sd"), 2 * ap$treatment$sd)
  ln <- assemble_brms_priors("default", NULL, NULL, FALSE, "lognormal",
                             generate_adaptive_priors(d, list(), "lognormal"))
  expect_equal(ln$sigma$string, "student_t(3, 0, 2.50)")
})

test_that("pooled within-phase SD excludes the phase shift", {
  d <- data.frame(case = "C1", phase = rep(c("A", "B"), each = 4),
                  outcome = c(1, 2, 1, 2, 101, 102, 101, 102))
  expect_equal(.pooled_within_sd(d), sqrt(2 / 6))
})

test_that("an ar() formula gets a normal(0, 0.5) ar prior bounded to (-1, 1)", {
  d <- process_scd_phases(simulate_scd_data(n_cases = 3), list(), "AB")
  f <- "outcome ~ time_A + level_AB + trend_AB + (1 | case) + ar(time = time, gr = case, p = 1)"
  pr <- .default_priors(d, stats::as.formula(f), "gaussian")
  ar <- pr[pr$class == "ar", ]
  expect_equal(nrow(ar), 1)
  expect_equal(ar$prior, "normal(0, 0.5)")
  expect_equal(c(ar$lb, ar$ub), c("-1", "1"))
  plain <- .default_priors(d, outcome ~ time_A + level_AB + (1 | case), "gaussian")
  expect_false("ar" %in% plain$class)
  expect_equal(sum(.add_ar_prior(pr, f)$class == "ar"), 1)
  expect_equal(.add_ar_prior(NULL, f)$class, "ar")
  skip_on_cran()
  code <- brms::make_stancode(brms::bf(f), data = d, prior = pr)
  expect_match(code, "vector<lower=-1,upper=1>[Kar] ar", fixed = TRUE)
})

test_that("default prior scale is the outcome SD with a fallback of 1", {
  d <- data.frame(case = rep(c("a", "b"), each = 4), role = rep(c(0, 0, 1, 1), 2),
                  outcome = c(1, 2, 11, 12, 1, 2, 11, 12))
  expect_equal(.prior_outcome_sd(d, "outcome"), sd(d$outcome))
  expect_equal(.prior_outcome_sd(data.frame(outcome = c(3, 3)), "outcome"), 1)
})
test_that("adaptive residual priors separate full tiers and repeated phase runs", {
  d <- data.frame(study = rep(c("S1", "S2"), each = 16), case = "P1",
                  time = rep(1:16, 2), phase = rep(rep(c("A", "B", "A", "B"), each = 4), 2),
                  outcome = rep(c(1, 2, 1, 2, 51, 52, 51, 52, 21, 22, 21, 22, 71, 72, 71, 72), 2) +
                    rep(c(0, 100), each = 16))
  priors <- generate_adaptive_priors(d, list(n_levels = 2))
  expect_equal(priors$sigma$scale, sqrt(8 / 24))
  expect_equal(priors$log_sigma$location, log(sqrt(8 / 24)))
  names(d)[names(d) == "phase"] <- "condition"
  expect_equal(generate_adaptive_priors(d, list(n_levels = 2))$sigma$scale, sqrt(8 / 24))
})
