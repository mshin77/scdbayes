make_mbd_study <- function(seed = 1) {
  set.seed(seed)
  onsets <- c(5, 7, 9, 11)
  d <- do.call(rbind, lapply(seq_along(onsets), function(i) {
    data.frame(case = paste0("c", i), time = 1:16,
               phase = ifelse(1:16 >= onsets[i], "B", "A"))
  }))
  d$outcome <- 10 + 5 * (d$phase == "B") + rep(stats::rnorm(4), each = 16) +
    stats::rnorm(nrow(d))
  d
}

test_that("delta is p'beta over sqrt(r'theta) from known parameter values", {
  draws <- data.frame(b_level_AB = c(2, 3), b_trend_AB = c(0.5, 0.25),
                      sd_case__Intercept = c(1, 2), sd_case__time_A = c(0.2, 0.1),
                      cor_case__Intercept__time_A = c(0.5, -0.5), sigma = c(1, 1.5),
                      `ar[1]` = c(0.6, 0), check.names = FALSE)
  got <- .bc_smd_delta(draws, "level_AB", "trend_AB", trend_lag = 4, time_lag = 10)
  num <- c(2 + 0.5 * 4, 3 + 0.25 * 4)
  between <- c(1 + 2 * 0.5 * 1 * 0.2 * 10 + 0.04 * 100, 4 - 2 * 0.5 * 2 * 0.1 * 10 + 0.01 * 100)
  within <- c(1 / (1 - 0.36), 1.5^2)
  expect_equal(got, num / sqrt(between + within))
  mb1 <- .bc_smd_delta(draws[c("b_level_AB", "sd_case__Intercept", "sigma")], "level_AB")
  expect_equal(mb1, c(2, 3) / sqrt(c(1, 4) + c(1, 2.25)))
})

test_that("study coding and formulas follow the design and trend options", {
  cd <- .bc_smd_code(make_mbd_study(), "multiple_baseline", "outcome", "case", "time")
  expect_equal(cd$time_A, cd$time - 1)
  expect_equal(cd$level_AB, as.integer(cd$time >= rep(c(5, 7, 9, 11), each = 16)))
  expect_equal(cd$trend_AB[cd$case == "c1" & cd$time == 8], 3)
  times <- .marginal_times(cd, "time", "case", ab_rule = "shortest_treatment")
  expect_equal(c(times$time_A, times$time_B), c(4, 10))
  scd <- .marginal_times(cd, "time", "case")
  expect_equal(c(scd$time_A, scd$time_B), c(4, 16))
  expect_equal(.marginal_times(cd, "time", "case", duration = 3)$time_B, 7)
  expect_equal(.bc_smd_formula("multiple_baseline", "none", FALSE), "outcome ~ level_AB + (1 | case)")
  expect_equal(.bc_smd_formula("multiple_baseline", "random", TRUE),
               paste("outcome ~ level_AB + time_A + trend_AB + (1 + time_A | case) +",
                     "ar(time = time, gr = case, p = 1)"))
  expect_equal(.bc_smd_formula("reversal", "fixed", FALSE),
               "outcome ~ treatment + time_A + (1 | case)")
  expect_equal(.bc_smd_formula("multiple_baseline", "none", FALSE, random_effect = TRUE),
               "outcome ~ level_AB + (1 + level_AB | case)")
  expect_true(formals(bc_smd_bayes)$random_effect)
  expect_equal(.bc_smd_formula("multiple_baseline", "none", FALSE, outer = TRUE),
               "outcome ~ level_AB + (1 | outer) + (1 | case)")
  abab <- data.frame(case = rep(c("c1", "c2", "c3"), each = 8), time = rep(1:8, 3),
                     phase = rep(rep(c("A", "B", "A", "B"), each = 2), 3), outcome = 1)
  rc <- .bc_smd_code(abab, "reversal", "outcome", "case", "time")
  expect_equal(rc$treatment[1:8], c(0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(unique(rc$first_trt_end), 4)
  first_ab <- rc[rc$time <= rc$first_trt_end, ]
  expect_equal(unlist(.marginal_times(first_ab, "time", "case")[c("time_A", "time_B")]),
               c(time_A = 2, time_B = 4))
})

test_that("time_A counts from the earliest session across series", {
  d <- make_mbd_study()
  d <- d[!(d$case == "c3" & d$time < 3), ]
  cd <- .bc_smd_code(d, "multiple_baseline", "outcome", "case", "time")
  expect_equal(cd$time_A, cd$time - 1)
  expect_equal(min(cd$time_A[cd$case == "c3"]), 2)
  expect_warning(es <- .es_from_fits(list(S1 = list(draws = c(1, 2)), S2 = list(draws = 1)), "median"),
                 "S2")
  expect_true(is.na(es$sei[2]))
})

test_that("series of one participant share a participant intercept and count once", {
  set.seed(2)
  d <- do.call(rbind, lapply(1:3, function(s) data.frame(
    case = "P1", behavior = paste0("beh", s), session = 1:20,
    phase = ifelse(1:20 > 4 + 3 * s, "B", "A"), outcome = rnorm(20))))
  cd <- .bc_smd_code(d, "multiple_baseline", "outcome", "case", "session")
  expect_equal(length(unique(cd$case)), 3)
  expect_equal(length(unique(cd$participant)), 1)
  expect_true("outer" %in% names(cd))
  expect_error(bc_smd_bayes(d, design = "multiple_baseline"), "have 1")
  draws <- data.frame(b_level_AB = 2, sd_outer__Intercept = 1, sd_case__Intercept = 1, sigma = 1)
  expect_equal(.bc_smd_delta(draws, "level_AB"), 2 / sqrt(3))
})

test_that("non-finite AR draws are dropped, counted, and the AR prior is bounded", {
  dr <- data.frame(b_level_AB = 1, sd_case__Intercept = 1, sigma = 1)[rep(1, 3), ]
  dr[["ar[1]"]] <- c(0.5, 1, 1.02)
  x <- .bc_smd_delta(dr, "level_AB")
  expect_equal(sum(!is.finite(x)), 2)
  expect_false(is.na(.summarize_draws(x[is.finite(x)])$estimate))
  cd <- .bc_smd_code(make_mbd_study(), "multiple_baseline", "outcome", "case", "time")
  pr <- .bc_smd_priors(cd, .bc_smd_formula("multiple_baseline", "none", TRUE), TRUE)
  ar <- pr[pr$class == "ar", ]
  expect_equal(c(ar$prior, ar$lb, ar$ub), c("uniform(-1, 1)", "-1", "1"))
  expect_false("ar" %in% .bc_smd_priors(cd, "outcome ~ level_AB + (1 | case)", FALSE)$class)
})

test_that("bc_smd_bayes rejects non-Gaussian families, other designs, and fewer than 3 cases", {
  d <- make_mbd_study()
  expect_error(bc_smd_bayes(d, design = "multiple_baseline", family = "poisson"), "gaussian")
  expect_error(bc_smd_bayes(d, design = "alternating"), "not supported")
  expect_error(bc_smd_bayes(d[d$case %in% c("c1", "c2"), ], design = "multiple_baseline"),
               "at least 3 cases")
})

test_that("meta_bayes builds the se() formula and default priors", {
  expect_equal(.meta_formula("yi", "sei", "study", NULL, NULL), "yi | se(sei) ~ 1 + (1 | study)")
  expect_equal(.meta_formula("g", "se", "study", "case", "age"),
               "g | se(se) ~ 1 + age + (1 | study) + (1 | study:case)")
  pr <- .meta_priors(FALSE)
  expect_equal(pr$prior[pr$class == "Intercept"], "normal(0, 2.00)")
  expect_equal(pr$prior[pr$class == "sd"], "student_t(3, 0, 0.5)")
  mods <- .meta_priors(TRUE, "smd")
  expect_equal(mods$prior[mods$class == "b"], "normal(0, 2.00)")
  lrr <- .meta_priors(TRUE, "lrr")
  expect_equal(lrr$prior[lrr$class %in% c("Intercept", "b")], rep("normal(0, 1.00)", 2))
  expect_error(.meta_prior_scale("raw"))
  expect_error(meta_bayes(data.frame(yi = 1, sei = 1)), "study")
  fits <- list(S1 = list(draws = c(1, 2, 3, Inf)), S2 = list(draws = c(2, 4, 9)))
  es <- .es_from_fits(fits, "median")
  expect_equal(es$yi, c(2, 4))
  expect_equal(es$sei, c(1, sd(c(2, 4, 9))))
  expect_equal(.es_from_fits(fits, "mean")$yi, c(2, 5))
  expect_error(.es_from_fits(unname(fits), "median"), "named")
})

test_that("meta_bayes merges control over the adapt_delta and max_treedepth defaults", {
  es <- data.frame(study = paste0("S", 1:3), yi = c(0.5, 1, 0.8), sei = c(0.3, 0.4, 0.35))
  seen <- NULL
  local_mocked_bindings(brm = function(..., control) {
    seen <<- control
    stop("mocked brm")
  }, .package = "brms")
  withr::local_options(scdbayes.max_treedepth = 13L)
  expect_error(meta_bayes(es), "mocked brm")
  expect_equal(seen, list(adapt_delta = 0.99, max_treedepth = 13L))
  expect_error(meta_bayes(es, control = list(max_treedepth = 15)), "mocked brm")
  expect_equal(seen, list(adapt_delta = 0.99, max_treedepth = 15))
  expect_error(meta_bayes(es, control = list(adapt_delta = 0.9)), "mocked brm")
  expect_equal(seen, list(adapt_delta = 0.9, max_treedepth = 13L))
})

test_that("bc_smd_bayes and meta_bayes return the documented structures", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  old <- options(scdbayes.threads = NULL)
  on.exit(options(old), add = TRUE)
  res <- suppressWarnings(bc_smd_bayes(make_mbd_study(), design = "multiple_baseline",
                                       ar = FALSE, iter = 600, chains = 1))
  expect_named(res, c("summary", "draws", "n_nonfinite", "time_A", "time_B", "n_cases",
                      "n_series", "design", "model"))
  expect_named(res$summary, c("estimate", "sd", "lower", "upper"))
  expect_equal(res$n_cases, 4)
  expect_gt(res$summary$estimate, 0)
  dec <- suppressWarnings(bc_smd_bayes(make_mbd_study(), design = "multiple_baseline",
                                       ar = FALSE, iter = 600, chains = 1,
                                       intervention_goal = "decrease"))
  expect_lt(dec$summary$estimate, 0)
  es <- data.frame(study = paste0("S", 1:5), yi = c(0.8, 1.2, 0.4, 1.5, 0.9),
                   sei = c(0.3, 0.4, 0.35, 0.5, 0.3))
  meta <- suppressWarnings(meta_bayes(es, iter = 1000, chains = 1, mcid = 0.5))
  expect_named(meta, c("summary", "study_effects", "pd", "prob_mcid", "priors",
                     "convergence", "diagnostics", "model"))
  expect_equal(meta$summary$quantity, c("mu", "tau", "prediction"))
  expect_equal(nrow(meta$study_effects), 5)
  expect_true(meta$prob_mcid >= 0 && meta$prob_mcid <= 1)
})

test_that("meta convergence summary warns when a check fails", {
  ok <- list(basic = list(rhat = c(1.001, 1.004), ess_bulk = c(900, 1200), ess_tail = c(700, 800)),
             divergence = list(n_divergent = 0), converged = list(converged = TRUE))
  conv <- expect_silent(.meta_convergence(ok))
  expect_equal(conv[c("max_rhat", "min_ess_bulk", "min_ess_tail", "n_divergent")],
               list(max_rhat = 1.004, min_ess_bulk = 900, min_ess_tail = 700, n_divergent = 0))
  bad <- modifyList(ok, list(divergence = list(n_divergent = 12), converged = list(converged = FALSE)))
  expect_warning(conv <- .meta_convergence(bad), "12 divergent transitions")
  expect_false(conv$converged)
  deep <- modifyList(bad, list(divergence = list(n_divergent = 0, max_treedepth_hit = TRUE)))
  expect_warning(.meta_convergence(deep), "transitions at max_treedepth")
  expect_warning(.meta_convergence(list()), "could not run")
})
