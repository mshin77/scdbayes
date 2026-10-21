test_that("response-scale ratio point estimate is exp(median), consistent with the CI", {
  set.seed(11)
  draws <- stats::rnorm(4000, 0.4, 0.6)
  summ <- .decision_summary(draws, threshold = NULL, rope = NULL,
                            intervention_goal = "increase", ci_level = 0.95)
  # point is exp(posterior median), which lands inside the exp(quantile) CI and
  # is the median of the ratio; the old geometric mean exp(mean(log)) is not
  expect_equal(summ$median_native, stats::median(draws))
  expect_false(isTRUE(all.equal(summ$median_native, summ$mean_native)))
  expect_gte(exp(summ$median_native), exp(summ$lower_native))
  expect_lte(exp(summ$median_native), exp(summ$upper_native))
})

test_that("decrease goal does not change the native ratio inputs", {
  set.seed(12)
  draws <- stats::rnorm(2000, 0.3, 0.5)
  inc <- .decision_summary(draws, NULL, NULL, "increase", 0.95)
  dec <- .decision_summary(draws, NULL, NULL, "decrease", 0.95)
  expect_equal(inc$median_native, dec$median_native)
  expect_equal(inc$lower_native, dec$lower_native)
})

test_that(".anova_icc corrects the upward bias of the naive mean-variance ratio", {
  set.seed(21)
  grp <- rep(1:8, each = 10)
  y <- rep(stats::rnorm(8, 0, 2), each = 10) + stats::rnorm(80, 0, 3)
  gm <- tapply(y, grp, mean)
  naive <- var(gm) / (var(gm) + mean(tapply(y, grp, var)))
  anova_icc <- .anova_icc(y, grp)
  expect_lt(anova_icc, naive)
  expect_gte(anova_icc, 0)
  expect_lte(anova_icc, 1)
})

test_that("variance summaries use Student variance and stationary AR variance", {
  draws <- data.frame(sigma = rep(2, 20), nu = rep(4, 20), sd_case__Intercept = rep(1, 20))
  local_mocked_bindings(as_draws_df = function(...) draws, .package = "brms")
  model <- structure(list(family = list(family = "student", link = "identity")), class = "brmsfit")
  expect_equal(bootstrap_variance_components(model)$summary["mean", "sigma2"], 8)
  draws[["ar[1]"]] <- rep(0.5, 20)
  model$formula <- brms::bf(outcome ~ 1 + ar(time = time, gr = case, p = 1))
  out <- bootstrap_variance_components(model)
  expect_equal(out$summary["mean", "sigma2"], 8 / 0.75)
  expect_equal(out$summary["mean", "iccs.case"], 1 / (1 + 8 / 0.75))
  draws$nu <- rep(2, 20)
  expect_message(out <- bootstrap_variance_components(model), "undefined")
  expect_true(all(is.na(out$summary[, "iccs.case"])))
})

test_that(".anova_icc phase adjustment removes the intervention-shift distortion", {
  set.seed(22)
  grp <- rep(1:8, each = 10)
  y <- rep(stats::rnorm(8, 0, 2), each = 10) + stats::rnorm(80, 0, 3)
  phase <- rep(rep(c("A", "B"), each = 5), 8)
  y_shift <- y + ifelse(phase == "B", 6, 0)
  unadj <- .anova_icc(y_shift, grp)
  adj <- .anova_icc(y_shift, grp, phase = phase)
  # the A->B shift distorts the unadjusted estimate; adjustment recovers the
  # clustering close to the no-shift value
  expect_true(is.finite(adj))
  expect_lt(abs(adj - .anova_icc(y, grp)), abs(unadj - .anova_icc(y, grp)))
})

test_that("build_default_random_effects nests case within study (study:case)", {
  df <- data.frame(study = rep(c("S1", "S2"), each = 40),
                   case = rep(c("A", "B"), each = 20, times = 2),
                   phase = rep(c("A", "B"), 40),
                   level_AB = rep(c(0, 1), 40))
  out <- build_default_random_effects(df)
  expect_match(out, "study:case", fixed = TRUE)
  expect_false(grepl("\\+ \\(1 \\+ level_AB \\| case\\)$", out))
})

test_that("withdrawal steps take the opposite reflection of onset steps", {
  expect_true(.is_withdrawal_term("b_level_B1A2"))
  expect_true(.is_withdrawal_term("trend_B2A3"))
  expect_false(.is_withdrawal_term("level_A1B1"))
  expect_false(.is_withdrawal_term("level_AB"))
  expect_true(.is_withdrawal_term("level_ret1"))
  expect_true(.is_withdrawal_term("b_trend_ret2"))
  expect_false(.is_withdrawal_term("level_crit2"))
  expect_equal(.term_goal("b_level_ret1", "increase"), "decrease")
  expect_equal(.term_goal("level_A1B1", "decrease"), "decrease")
  expect_equal(.term_goal("level_B1A2", "decrease"), "increase")
  expect_equal(.term_goal("level_B1A2", "increase"), "decrease")
  draws <- rnorm(400, 2, 0.1)
  onset <- .decision_summary(draws, NULL, NULL, .term_goal("level_A1B1", "decrease"), 0.95)
  withdrawal <- .decision_summary(draws, NULL, NULL, .term_goal("level_B1A2", "decrease"), 0.95)
  expect_lt(onset$mean, 0)
  expect_gt(withdrawal$mean, 0)
})

test_that("ratio labels follow the family", {
  fake <- function(family, link) list(family = list(family = family, link = link))
  expect_equal(.response_scale_ratio(fake("poisson", "log")), "irr")
  expect_equal(.response_scale_ratio(fake("gamma", "log")), "mean_ratio")
  expect_equal(.response_scale_ratio(fake("gaussian", "log")), "mean_ratio")
  expect_null(.response_scale_ratio(fake("custom", "log")))
  expect_equal(.response_scale_ratio(fake("lognormal", "identity")), "median_ratio")
  expect_equal(.response_scale_ratio(fake("bernoulli", "logit")), "or")
  expect_null(.response_scale_ratio(fake("gaussian", "identity")))
})

test_that("attached ratios are labeled as conditional (typical-case) ratios", {
  fake <- function(family, link) list(family = list(family = family, link = link))
  rows <- data.frame(term = "level_AB", mean = 0.4, median_native = 0.4,
                     lower_native = 0.1, upper_native = 0.7, mean_native = 0.4)
  irr <- .attach_response_ratio(rows, fake("poisson", "log"))
  expect_equal(irr$ratio_type, "conditional")
  expect_equal(irr$irr, exp(0.4))
  expect_false(any(c("median_native", "lower_native") %in% names(irr)))
  ident <- .attach_response_ratio(rows, fake("gaussian", "identity"))
  expect_false("ratio_type" %in% names(ident))
})

test_that("withdrawal steps are recognized from phase roles, not letters", {
  expect_true(.is_withdrawal_term("level_Intervention1Baseline2"))
  expect_true(.is_withdrawal_term("b_trend_Treatment1Withdrawal1"))
  expect_false(.is_withdrawal_term("level_Baseline1Intervention1"))
  expect_false(.is_withdrawal_term("level_Withdrawal1Reinstatement1"))
  expect_equal(.term_goal("level_Intervention1Baseline2", "increase"), "decrease")
})

test_that("Tx and Wait labels carry intervention and baseline roles", {
  tx <- c("time_Tx1", "level_Tx1Wait1", "level_Wait1Tx2", "level_Tx2Wait2")
  expect_equal(vapply(tx[-1], .is_withdrawal_term, logical(1), nm = tx, USE.NAMES = FALSE),
               c(TRUE, FALSE, TRUE))
  expect_equal(.term_goal("level_Wait1Tx2", "increase", tx), "increase")
  expect_false(.is_withdrawal_term("level_Wait1Tx2"))
})

test_that("unknown labels take their roles from the tier sequence", {
  nc <- c("time_NoChoice1", "level_NoChoice1Choice1", "level_Choice1NoChoice2",
          "level_NoChoice2Choice2")
  expect_true(.is_withdrawal_term("b_level_Choice1NoChoice2", paste0("b_", nc)))
  expect_equal(.term_goal("level_NoChoice2Choice2", "increase", nc), "increase")
})

test_that("threshold and ROPE apply to level terms only", {
  set.seed(5)
  draws <- data.frame(b_Intercept = rnorm(400), b_time_A = rnorm(400, 0.1, 0.05),
                      b_level_AB = rnorm(400, 2, 0.3), b_trend_AB = rnorm(400, 0.5, 0.1))
  out <- summarize_fixed_effect_decisions(draws, threshold = 1, rope = c(-0.5, 0.5))
  slope <- out$term %in% c("time_A", "trend_AB")
  expect_true(all(is.na(out$prob_meaningful_change[slope])))
  expect_true(all(is.na(out$rope_prob[slope])))
  expect_equal(out$prob_meaningful_change[out$term == "level_AB"],
               mean(draws$b_level_AB >= 1))
})

test_that("default_rope width follows the link scale", {
  d <- data.frame(case = "C1", phase = rep(c("A", "B"), each = 4),
                  outcome = c(1, 2, 1, 2, 101, 102, 101, 102))
  expect_equal(default_rope(d, "gaussian"), c(-0.1, 0.1) * sd(d$outcome))
  expect_equal(default_rope(d, "student"), c(-0.1, 0.1) * sd(d$outcome))
  expect_equal(default_rope(d, "negbinomial"), c(-1, 1) * log(1.1))
  expect_equal(default_rope(d, "poisson"), c(-1, 1) * log(1.1))
  expect_equal(default_rope(d, "bernoulli"), c(-0.1, 0.1) * pi / sqrt(3))
  expect_equal(default_rope(d, "binomial")[2], 0.1813799, tolerance = 1e-6)
})

skip_if_no_fit <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
}

test_that("posterior variance components count the residual once and orient consistency", {
  skip_if_no_fit()
  old <- options(scdbayes.threads = NULL)
  on.exit(options(old), add = TRUE)
  set.seed(9)
  d <- process_scd_phases(simulate_scd_data(n_cases = 4, effect_size = -1.5), list(), "AB")
  m <- suppressWarnings(build_model(d, "outcome ~ time_A + level_AB + (1 + level_AB | case)",
                                    iter = 400, chains = 1, family = "gaussian"))
  skip_if(is.null(m), "model did not fit")
  vc <- bootstrap_variance_components(m, n_boot = 100)
  expect_equal(colnames(vc$summary), c("sigma2", "case", "iccs.case"))
  expect_equal(rownames(vc$summary), c("mean", "sd", "ci_lower.2.5%", "ci_upper.97.5%"))
  expect_equal(vc$n_successful, 100)
  dr <- as.data.frame(brms::as_draws_df(m))
  idx <- unique(round(seq(1, nrow(dr), length.out = 100)))
  icc <- dr$sd_case__Intercept[idx]^2 / (dr$sd_case__Intercept[idx]^2 + dr$sigma[idx]^2)
  expect_equal(vc$summary["mean", "iccs.case"], mean(icc))
  cc_dec <- summarize_cross_case_consistency(m, intervention_goal = "decrease")
  cc_inc <- summarize_cross_case_consistency(m)
  row <- cc_dec$term == "level_AB"
  expect_equal(cc_dec$prob_consistent[row], cc_dec$prob_all_negative[row])
  expect_equal(cc_inc$prob_consistent[row], cc_inc$prob_all_positive[row])
})

fake_re_model <- function() {
  set.seed(9)
  d <- data.frame(case = rep(c("P1", "P2"), each = 4), behavior = rep(c("b1", "b2"), 4))
  draws <- function(levels, terms) {
    array(stats::rnorm(5 * length(levels) * length(terms)), c(5, length(levels), length(terms)),
          dimnames = list(NULL, levels, terms))
  }
  re <- list(case = draws(c("P1", "P2"), "Intercept"),
             `case:behavior` = draws(c("P1_b1", "P1_b2", "P2_b1", "P2_b2"),
                                     c("Intercept", "level_AB")))
  fixed <- matrix(stats::rnorm(10), 5, 2, dimnames = list(NULL, c("Intercept", "level_AB")))
  list(model = structure(list(data = d, family = brms::brmsfamily("gaussian")), class = "brmsfit"),
       re = re, fixed = fixed)
}

test_that("per-case effects use the grouping that carries the treatment slope", {
  f <- fake_re_model()
  local_mocked_bindings(
    ranef = function(object, summary = TRUE, ...) f$re,
    fixef = function(object, summary = TRUE, ...) {
      if (summary) matrix(0, 2, 1, dimnames = list(colnames(f$fixed), "Estimate")) else f$fixed
    },
    .package = "brms")
  re <- .case_random_effects(f$model, "case")
  expect_equal(re$group, "case:behavior")
  out <- summarize_per_case_effects(f$model)
  expect_setequal(unique(out$case), c("P1_b1", "P1_b2", "P2_b1", "P2_b2"))
  # the baseline level (Intercept) is not a treatment effect and gets no decision row
  expect_setequal(unique(out$term), "level_AB")
  row <- out$case == "P2_b1" & out$term == "level_AB"
  expect_equal(out$mean[row], mean(f$fixed[, "level_AB"] + f$re$`case:behavior`[, "P2_b1", "level_AB"]))
  # nested intercepts still add up through every level
  j <- match("P2_b1", dimnames(re$draws)[[2]])
  manual <- f$fixed[, "Intercept"] + f$re$`case:behavior`[, "P2_b1", "Intercept"] +
    f$re$case[, "P2", "Intercept"]
  expect_equal(.unit_draws(f$fixed, re, j, "Intercept"), manual)
  cc <- summarize_cross_case_consistency(f$model)
  expect_true("level_AB" %in% cc$term)
  expect_false("Intercept" %in% cc$term)
})

test_that("inverse links orient fixed and unit decisions in the response direction", {
  f <- fake_re_model()
  f$model$family <- brms::brmsfamily("Gamma", link = "inverse")
  f$fixed[, "level_AB"] <- -0.5
  f$re$`case:behavior`[, , "level_AB"] <- 0
  local_mocked_bindings(
    as_draws_df = function(x, ...) data.frame(b_level_AB = rep(-0.5, 5)),
    ranef = function(object, summary = TRUE, ...) f$re,
    fixef = function(object, summary = TRUE, ...) {
      if (summary) matrix(0, 2, 1, dimnames = list(colnames(f$fixed), "Estimate")) else f$fixed
    }, .package = "brms")
  fixed <- summarize_fixed_effect_decisions(f$model, threshold = 0.25)
  expect_equal(fixed$mean, 0.5)
  expect_equal(fixed$prob_meaningful_change, 1)
  expect_equal(summarize_per_case_effects(f$model, threshold = 0.25)$mean, rep(0.5, 4))
  expect_equal(summarize_cross_case_consistency(f$model)$prob_consistent, 1)
  adverse <- summarize_fixed_effect_decisions(f$model, threshold = 0.25,
                                              intervention_goal = "decrease")
  expect_equal(adverse$pd, 1)
  expect_equal(adverse$prob_meaningful_change, 0)
})

test_that("reference sessions and phase contrasts retain complete tier identity", {
  d <- data.frame(study = rep(c("S1", "S2"), each = 4), case = "P1",
                  time = rep(1:4, 2), phase = rep(c("A", "A", "B", "B"), 2),
                  outcome = c(1, 2, 3, 4, 10, 20, 40, 60))
  expect_equal(.select_reference_rows(d[d$phase == "B", ], "last", "case", "time")$outcome,
               c(4, 60))
  local_mocked_bindings(posterior_epred = function(object, newdata, ...) {
    matrix(rep(newdata$outcome, each = 5), nrow = 5)
  }, .package = "brms")
  model <- list(data = d, family = brms::brmsfamily("gaussian"))
  out <- compute_phase_contrast(model)
  expect_equal(out$population_contrast$mean, 21)
  expect_equal(out$per_case_contrast$mean, c(2, 40))
  expect_equal(out$per_case_contrast$study, c("S1", "S2"))
})

test_that("default ROPE follows the actual link and rejects unsupported scales", {
  d <- data.frame(outcome = 1:8)
  expect_equal(default_rope(d, brms::brmsfamily("gaussian", link = "log")),
               c(-1, 1) * log(1.1))
  expect_error(default_rope(d, brms::brmsfamily("Gamma", link = "inverse")), "explicit")
})
test_that("variance summaries distinguish latent and joint AR residuals", {
  draws <- data.frame(sigma = rep(2, 20), sderr = rep(3, 20),
                      sd_case__Intercept = rep(1, 20), check.names = FALSE)
  draws[["ar[1]"]] <- rep(0.5, 20)
  local_mocked_bindings(as_draws_df = function(...) draws, .package = "brms")
  model <- structure(list(family = brms::skew_normal(),
    formula = brms::bf(outcome ~ 1 + ar(time = time, gr = case, p = 1, cov = TRUE))), class = "brmsfit")
  out <- bootstrap_variance_components(model)
  expect_equal(out$summary["mean", "sigma2"], 4 + 9 / 0.75)
  expect_equal(out$summary["mean", "iccs.case"], 1 / (1 + 4 + 9 / 0.75))
  model$formula <- brms::bf(outcome ~ 1 + ar(time = time, gr = case, p = 1, cov = FALSE))
  expect_message(out <- bootstrap_variance_components(model), "undefined")
  expect_true(all(is.na(out$summary[, "sigma2"])))
  model$formula <- brms::bf(outcome ~ 1 + ar(time = time, gr = case, p = 1, cov = TRUE))
  draws$sderr <- NULL
  expect_message(out <- bootstrap_variance_components(model), "undefined")
  expect_true(all(is.na(out$summary[, "sigma2"])))
  model$formula <- brms::bf(outcome ~ 1 + ar(time = time, gr = case, p = 2))
  expect_message(out <- bootstrap_variance_components(model), "undefined")
  expect_true(all(is.na(out$summary[, "iccs.case"])))
})

test_that("variance summaries reject dispersion parameters outside response SD families", {
  draws <- data.frame(sigma = rep(2, 20), sd_case__Intercept = rep(1, 20))
  local_mocked_bindings(as_draws_df = function(...) draws, .package = "brms")
  model <- structure(list(family = brms::lognormal()), class = "brmsfit")
  expect_message(out <- bootstrap_variance_components(model), "undefined")
  expect_true(all(is.na(out$summary[, "sigma2"])))
})
test_that("variance summaries require a positive integer draw count", {
  draws <- data.frame(sigma = rep(2, 20), sd_case__Intercept = rep(1, 20))
  local_mocked_bindings(as_draws_df = function(...) draws, .package = "brms")
  model <- structure(list(family = stats::gaussian()), class = "brmsfit")
  expect_error(bootstrap_variance_components(model, n_boot = 0), "n_boot")
  expect_error(bootstrap_variance_components(model, n_boot = 1.5), "n_boot")
  expect_error(bootstrap_variance_components(model, n_boot = Inf), "n_boot")
})

test_that("variance summaries reject invalid response SD draws", {
  draws <- data.frame(sigma = rep(Inf, 20), sd_case__Intercept = rep(1, 20))
  local_mocked_bindings(as_draws_df = function(...) draws, .package = "brms")
  model <- structure(list(family = stats::gaussian()), class = "brmsfit")
  expect_message(out <- bootstrap_variance_components(model), "undefined")
  expect_true(all(is.na(out$summary[, "sigma2"])))
})
