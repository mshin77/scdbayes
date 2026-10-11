make_ppc_data <- function() {
  data.frame(
    case    = rep(c("c1", "c2"), each = 10),
    phase   = rep(rep(c("A", "B"), each = 5), 2),
    time    = rep(1:10, 2),
    outcome = c(1, 2, 3, 2, 1,  6, 7, 8, 7, 6,
                0, 1, 0, 1, 0,  5, 5, 6, 5, 6),
    stringsAsFactors = FALSE
  )
}

test_that(".ppc_meta assigns the first phase per case to the baseline condition", {
  d <- make_ppc_data()
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  expect_equal(meta$case_levels, c("c1", "c2"))
  expect_equal(unique(meta$condition[d$phase == "A"]), "baseline")
  expect_equal(unique(meta$condition[d$phase == "B"]), "intervention")
})

test_that(".ppc_meta keeps label roles for ABAB and treatment-first BAB", {
  abab <- data.frame(case = "c1", time = 1:8, outcome = 1:8,
                     phase = rep(c("A1", "B1", "A2", "B2"), each = 2))
  meta <- .ppc_meta(abab, "outcome", "case", "phase", "time")
  expect_equal(meta$condition, rep(c("baseline", "intervention"), each = 2, times = 2))
  bab <- data.frame(case = "c1", time = 1:6, outcome = 1:6,
                    phase = rep(c("B", "A", "B"), each = 2))
  meta <- .ppc_meta(bab, "outcome", "case", "phase", "time")
  expect_equal(meta$condition, rep(c("intervention", "baseline", "intervention"), each = 2))
})

test_that(".ppc_frame computes level, absence and initial performance per case and pooled", {
  d <- make_ppc_data()
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  fr <- .ppc_frame(meta$outcome, meta,
                   statistics = c("initial", "level", "absence"),
                   goal = "increase")

  pick <- function(stat, case, phase = NA) {
    row <- fr$statistic == stat & fr$case == case &
      (is.na(phase) & is.na(fr$phase) | fr$phase == phase)
    fr$value[which(row)[1]]
  }
  expect_equal(pick("level", "c1", "A1"), 1.8)
  expect_equal(pick("level", "c2", "B1"), 5.4)
  expect_equal(pick("level", "pooled", "A1"), 1.1)
  expect_equal(pick("absence", "c2", "A1"), 0.6)
  expect_equal(pick("absence", "c1", "A1"), 0)
  expect_equal(pick("initial", "c1"), 1)
  expect_equal(pick("initial", "pooled"), 0.5)
})

test_that("features are computed per phase occurrence, not pooled by role", {
  d <- data.frame(case = "c1", time = 1:12, outcome = c(1, 1, 1, 9, 9, 9, 3, 3, 3, 5, 5, 5),
                  phase = rep(c("A", "B", "A", "B"), each = 3))
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  expect_equal(meta$phase, rep(c("A1", "B1", "A2", "B2"), each = 3))
  fr <- .ppc_frame(meta$outcome, meta, c("level", "variability"), "increase")
  lv <- fr[fr$statistic == "level" & fr$case == "c1", ]
  expect_equal(lv$value[match(c("A1", "B1", "A2", "B2"), lv$phase)], c(1, 9, 3, 5))
  expect_equal(fr$value[fr$statistic == "variability" & fr$case == "c1"], rep(0, 4))
  expect_length(meta$pairs, 3)
  expect_equal(meta$withdrawal, c(FALSE, TRUE, FALSE))
})

test_that("pooled level and absence weight tiers equally by default", {
  d <- data.frame(case = rep(c("c1", "c2"), c(8, 6)), time = c(1:8, 1:6),
                  phase = c(rep("A", 6), "B", "B", rep("A", 2), rep("B", 4)),
                  outcome = c(0, 0, 0, 0, 0, 0, 5, 5, 4, 4, 6, 6, 6, 6))
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  pooled <- function(fr, stat) fr$value[fr$statistic == stat & fr$case == "pooled" &
                                          fr$phase == "A1"]
  by_case <- .ppc_frame(meta$outcome, meta, c("level", "absence"), "increase")
  all_obs <- .ppc_frame(meta$outcome, meta, c("level", "absence"), "increase", pool = "all_obs")
  expect_equal(pooled(by_case, "level"), mean(c(0, 4)))
  expect_equal(pooled(by_case, "absence"), mean(c(1, 0)))
  expect_equal(pooled(all_obs, "level"), 8 / 8)
  expect_equal(pooled(all_obs, "absence"), 6 / 8)
})

test_that("tiers split repeated case IDs across studies", {
  d <- rbind(transform(make_ppc_data(), study = "S1"), transform(make_ppc_data(), study = "S2"))
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  expect_equal(meta$case_levels, c("S1.c1", "S1.c2", "S2.c1", "S2.c2"))
})

test_that("immediacy is reported per time point from the baseline line", {
  d <- data.frame(case = "c1", time = 1:12,
                  outcome = c(1, 2, 3, 4, 5, 6, 20, 21, 25, 22, 30, 40),
                  phase = rep(c("A", "B"), each = 6))
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  fr <- .ppc_frame(meta$outcome, meta, "immediacy", "increase")
  own <- fr[fr$case == "c1", ]
  expect_equal(own$statistic, paste0("immediacy_t", 1:5))
  expect_equal(own$value, c(20, 21, 25, 22, 30) - (7:11))
  expect_equal(fr$value[fr$case == "pooled"], own$value)
  down <- .ppc_frame(meta$outcome, meta, "immediacy", "decrease")
  expect_equal(down$value, -fr$value)
})

test_that("Tau compares each adjacent baseline-intervention pair", {
  expect_equal(.ppc_tau(c(1, 2), c(3, 4)), 1)
  expect_equal(.ppc_tau(c(1, 3), c(2, 3)), 0.25)
  d <- data.frame(case = "c1", time = 1:12, outcome = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6),
                  phase = rep(c("A", "B", "A", "B"), each = 3))
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  fr <- .ppc_frame(meta$outcome, meta, "nonoverlap", "increase")
  expect_equal(fr$phase[fr$case == "c1"], c("B1", "A2", "B2"))
  expect_equal(fr$value[fr$case == "c1"], c(1, 1, 1))
})

test_that("ABAB withdrawal Tau flips direction and immediacy skips withdrawals", {
  d <- data.frame(case = "c1", time = 1:16,
                  outcome = c(1, 2, 1, 2, 8, 9, 8, 9, 3, 2, 3, 2, 9, 8, 9, 7),
                  phase = rep(c("A", "B", "A", "B"), each = 4))
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  up <- .ppc_frame(meta$outcome, meta, c("nonoverlap", "immediacy"), "increase")
  tau <- up[up$statistic == "nonoverlap" & up$case == "c1", ]
  expect_equal(tau$phase, c("B1", "A2", "B2"))
  expect_equal(tau$value, c(1, 1, 1))
  expect_equal(.ppc_tau(c(8, 9, 8, 9), c(3, 2, 3, 2)), -1)
  imm <- up[grepl("^immediacy", up$statistic) & up$case == "c1", ]
  expect_equal(unique(imm$phase), c("B1", "B2"))
  down <- .ppc_frame(meta$outcome, meta, "nonoverlap", "decrease")
  expect_equal(down$value[down$case == "c1"], c(-1, -1, -1))
})

skip_if_no_fit <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
}

test_that("posterior_predictive_features returns aligned p-values and flags", {
  skip_if_no_fit()
  old <- options(scdbayes.threads = NULL)
  on.exit(options(old), add = TRUE)
  set.seed(1)
  d <- simulate_scd_data(n_cases = 3)
  d <- process_scd_phases(d, structure = list(), scd_type = "AB")
  m <- build_model(d, "outcome ~ time_A + level_AB + trend_AB + (1|case)",
                   iter = 300, chains = 1, family = "gaussian")
  skip_if(is.null(m), "model did not fit")

  res <- posterior_predictive_features(m, data = d, n_draws = 100)
  expect_true(all(c("statistic", "case", "phase", "observed", "p_value", "flag")
                  %in% names(res$summary)))
  ok <- is.finite(res$summary$p_value)
  expect_true(all(res$summary$p_value[ok] >= 0 & res$summary$p_value[ok] <= 1))
  expect_true(is.logical(res$summary$flag))
  expect_true(nrow(res$draws) > 0)

  res_new <- posterior_predictive_features(m, data = d, n_draws = 50, mode = "new")
  expect_true(nrow(res_new$summary) == nrow(res$summary))
})

test_that("ppc_overlay_draws returns observed and replicate series", {
  skip_if_no_fit()
  old <- options(scdbayes.threads = NULL)
  on.exit(options(old), add = TRUE)
  set.seed(2)
  d <- simulate_scd_data(n_cases = 3)
  d <- process_scd_phases(d, structure = list(), scd_type = "AB")
  m <- build_model(d, "outcome ~ time_A + level_AB + trend_AB + (1|case)",
                   iter = 300, chains = 1, family = "gaussian")
  skip_if(is.null(m), "model did not fit")

  ov <- ppc_overlay_draws(m, data = d, n_lines = 10)
  expect_true(all(c("case", "time", "condition", "outcome") %in% names(ov$observed)))
  expect_true("draw" %in% names(ov$replicates))
  expect_equal(length(unique(ov$replicates$draw)), 10)
})

test_that("missing outcomes keep replicates aligned and redraw observation-level effects", {
  skip_if_no_fit()
  old <- options(scdbayes.threads = NULL)
  on.exit(options(old), add = TRUE)
  set.seed(3)
  d <- process_scd_phases(simulate_scd_data(n_cases = 3), list(), "AB")
  d$obs <- factor(seq_len(nrow(d)))
  d$outcome <- stats::rpois(nrow(d), 5)
  d$outcome[c(4, 17)] <- NA
  m <- suppressWarnings(build_model(d, "outcome ~ level_AB + (1 | case) + (1 | obs)",
                                    iter = 300, chains = 1, family = "poisson"))
  skip_if(is.null(m), "model did not fit")
  expect_equal(nrow(m$data), nrow(d) - 2)
  yrep <- .ppc_yrep(m, d, 20, "same", observed = d$outcome)
  expect_equal(ncol(yrep), nrow(d))
  expect_true(all(is.na(yrep[, c(4, 17)])))
  expect_true(all(is.finite(yrep[, -c(4, 17)])))
  res <- posterior_predictive_features(m, data = d, n_draws = 20)
  expect_true(all(c("immediacy_t1", "level") %in% res$summary$statistic))
})

test_that(".ppc_p_value is the share of replicates at or above the observed value", {
  expect_equal(.ppc_p_value(c(1, 2, 3, 4), 2.5), 0.5)
  expect_equal(.ppc_p_value(c(0, 0, 0, 0), 0), 1)
  expect_equal(.ppc_p_value(c(1, 2, 2, 3), 2), 0.75)
  expect_equal(.ppc_p_value(c(3, 4), 5), 0)
})

test_that("gls autocorrelation matches a direct nlme fit of Eq. A19", {
  skip_if_not_installed("nlme")
  set.seed(4)
  d <- data.frame(case = rep(c("c1", "c2", "c3"), each = 20), time = rep(1:20, 3))
  d$phase <- ifelse(d$time > rep(c(6, 9, 12), each = 20), "B", "A")
  d$outcome <- 2 + 0.1 * d$time + 3 * (d$phase == "B") +
    as.numeric(unlist(lapply(1:3, function(i) stats::arima.sim(list(ar = 0.4), 20))))
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  fr <- .ppc_frame(meta$outcome, meta, "autocorrelation", "increase", ar_method = "gls")
  d$H <- as.integer(d$phase == "B")
  ref <- nlme::gls(outcome ~ case + case:time + case:H + case:time:H, data = d,
                   correlation = nlme::corAR1(form = ~ time | case), method = "REML")
  expect_equal(fr$value, as.numeric(stats::coef(ref$modelStruct$corStruct,
                                                  unconstrained = FALSE)), tolerance = 1e-4)
  moment <- .ppc_frame(meta$outcome, meta, "autocorrelation", "increase")
  expect_false(isTRUE(all.equal(fr$value, moment$value)))
})

test_that("initial performance skips a missing first baseline value", {
  d <- make_ppc_data()
  d$outcome[1] <- NA
  meta <- .ppc_meta(d, "outcome", "case", "phase", "time")
  fr <- .ppc_frame(meta$outcome, meta, "initial", "increase")
  expect_equal(fr$value[fr$statistic == "initial" & fr$case == "c1"], 2)
})

test_that("autocorrelation ignores rows with a missing phase", {
  d <- make_ppc_data()
  full <- .ppc_meta(d, "outcome", "case", "phase", "time")
  d_na <- d
  d_na$phase[3] <- NA
  meta <- .ppc_meta(d_na, "outcome", "case", "phase", "time")
  dropped <- .ppc_meta(d[-3, ], "outcome", "case", "phase", "time")
  value <- .ppc_ar1_level1(meta$outcome, meta$time, meta$case, meta$group_idx)
  expect_true(is.finite(value))
  expect_equal(value, .ppc_ar1_level1(dropped$outcome, dropped$time, dropped$case,
                                      dropped$group_idx))
  expect_false(isTRUE(all.equal(value, .ppc_ar1_level1(full$outcome, full$time, full$case,
                                                       full$group_idx))))
})

fake_fit <- function(family, formula, data) {
  structure(list(family = brms::brmsfamily(family), data = data,
                 formula = brms::bf(stats::as.formula(formula)),
                 version = list(brms = utils::packageVersion("brms"))), class = "brmsfit")
}

test_that("latent ar() models are detected and never return empty silently", {
  d <- make_ppc_data()
  pois <- fake_fit("poisson", "outcome ~ phase + ar(time = time, gr = case)", d)
  gaus <- fake_fit("gaussian", "outcome ~ phase + ar(time = time, gr = case)", d)
  expect_true(.latent_ar(pois))
  expect_false(.latent_ar(gaus))
  expect_error(.ppc_yrep(pois, d, 5, "new", observed = d$outcome), "latent ar")
  local_mocked_bindings(posterior_predict = function(object, ...) {
    matrix(1, 5, nrow(object$data) - 2)
  }, .package = "brms")
  expect_error(.ppc_yrep(pois, d, 5, "same", observed = d$outcome), "does not match")
  d$outcome[c(3, 15)] <- NA
  yrep <- .ppc_yrep(pois, d, 5, "same", observed = d$outcome)
  expect_equal(dim(yrep), c(5, nrow(d)))
  expect_true(all(is.na(yrep[, c(3, 15)])))
  res <- posterior_predictive_features(pois, data = d, n_draws = 5)
  expect_gt(nrow(res$summary), 0)
  expect_match(res$note, "latent residuals")
})

test_that("absence is computed for every family and unflagged when replicates tie", {
  d <- make_ppc_data()
  d$outcome <- d$outcome + 1
  local_mocked_bindings(posterior_predict = function(object, newdata, ndraws, ...) {
    matrix(stats::rpois(ndraws * nrow(newdata), 3) + 1, ndraws, nrow(newdata))
  }, .package = "brms")
  p <- posterior_predictive_features(fake_fit("poisson", "outcome ~ phase", d), d, n_draws = 20)
  ab <- p$summary[p$summary$statistic == "absence", ]
  expect_true(all(ab$p_value == 1))
  expect_false(any(ab$flag))
  local_mocked_bindings(posterior_predict = function(object, newdata, ndraws, ...) {
    matrix(stats::rnorm(ndraws * nrow(newdata), -3, 1), ndraws, nrow(newdata))
  }, .package = "brms")
  g <- posterior_predictive_features(fake_fit("gaussian", "outcome ~ phase", d), d, n_draws = 20)
  ab <- g$summary[g$summary$statistic == "absence", ]
  expect_gt(nrow(ab), 0)
  expect_true(all(ab$p_value == 1))
  expect_true(all(ab$flag))
})

test_that("p_tie is reported and the flag uses the mid-p value", {
  d <- make_ppc_data()
  reps_with <- function(n_same, shift) function(object, newdata, ndraws, ...) {
    y <- newdata$outcome
    rbind(matrix(rep(y, n_same), n_same, length(y), byrow = TRUE),
          matrix(y + shift, ndraws - n_same, length(y), byrow = TRUE))
  }
  fit <- fake_fit("gaussian", "outcome ~ time + phase", d)
  local_mocked_bindings(posterior_predict = reps_with(40, 0.1), .package = "brms")
  ties <- posterior_predictive_features(fit, d, statistics = "level", n_draws = 100)$summary
  expect_true("p_tie" %in% names(ties))
  expect_equal(ties$p_value, rep(1, nrow(ties)))
  expect_equal(ties$p_tie, rep(0.4, nrow(ties)))
  expect_false(any(ties$flag))
  local_mocked_bindings(posterior_predict = reps_with(0, 10), .package = "brms")
  above <- posterior_predictive_features(fit, d, statistics = "level", n_draws = 20)$summary
  expect_equal(above$p_tie, rep(0, nrow(above)))
  expect_true(all(above$flag))
})

test_that("the pooled initial row without a time term averages tier means on both sides", {
  d <- make_ppc_data()
  local_mocked_bindings(posterior_predict = function(object, newdata, ndraws, ...) {
    matrix(newdata$outcome, ndraws, nrow(newdata), byrow = TRUE)
  }, .package = "brms")
  res <- posterior_predictive_features(fake_fit("gaussian", "outcome ~ phase", d), d,
                                       statistics = "initial", n_draws = 10)
  pooled <- res$summary[res$summary$case == "pooled", ]
  expect_equal(pooled$observed, 1.1)
  expect_equal(unique(res$draws$value[res$draws$case == "pooled"]), 1.1)
  expect_equal(pooled$p_tie, 1)
  expect_false(pooled$flag)
})

test_that("a time variable in a random-effect or smooth term counts as a trend", {
  d <- make_ppc_data()
  expect_true(.has_time_trend(fake_fit("gaussian", "outcome ~ phase + (1 + time | case)", d)))
  expect_true(.has_time_trend(fake_fit("gaussian", "outcome ~ phase + s(time)", d)))
  expect_false(.has_time_trend(fake_fit("gaussian", "outcome ~ phase + (1 | case)", d)))
  expect_false(.has_time_trend(fake_fit("gaussian",
                                        "outcome ~ phase + ar(time = time, gr = case, p = 1)", d)))
})

test_that("a derived time column counts as a trend", {
  d <- make_ppc_data()
  d$session_c <- d$time - 1
  expect_true(.has_time_trend(fake_fit("gaussian", "outcome ~ phase + session_c", d)))
  expect_true(.has_time_trend(fake_fit("gaussian", "outcome ~ phase + wk_time", d), "time"))
  expect_false(.has_time_trend(fake_fit("gaussian", "outcome ~ phase + age", d), "time"))
  d$treatment <- d$t <- d$time
  expect_false(.has_time_trend(fake_fit("gaussian", "outcome ~ treatment", d), "t"))
  expect_true(.has_time_trend(fake_fit("gaussian", "outcome ~ treatment + t", d), "t"))
  d$wk.t <- d$tx <- d$time
  expect_true(.has_time_trend(fake_fit("gaussian", "outcome ~ treatment + wk.t", d), "wk.t"))
  expect_false(.has_time_trend(fake_fit("gaussian", "outcome ~ treatment + tx", d), "t."))
})

test_that("the pooled initial row is dropped when it repeats the pooled A1 level row", {
  d <- make_ppc_data()
  local_mocked_bindings(posterior_predict = function(object, newdata, ndraws, ...) {
    matrix(newdata$outcome, ndraws, nrow(newdata), byrow = TRUE)
  }, .package = "brms")
  fit <- fake_fit("gaussian", "outcome ~ phase", d)
  both <- posterior_predictive_features(fit, d, statistics = c("initial", "level"), n_draws = 10)$summary
  expect_false(any(both$statistic == "initial" & both$case == "pooled"))
  expect_equal(both$observed[both$statistic == "level" & both$case == "pooled" & both$phase == "A1"], 1.1)
  all_obs <- posterior_predictive_features(fit, d, statistics = c("initial", "level"), n_draws = 10,
                                           pool = "all_obs")$summary
  expect_true(any(all_obs$statistic == "initial" & all_obs$case == "pooled"))
})

test_that("initial performance without a time term compares every initial baseline point", {
  d <- make_ppc_data()
  local_mocked_bindings(posterior_predict = function(object, newdata, ndraws, ...) {
    matrix(2, ndraws, nrow(newdata))
  }, .package = "brms")
  flat <- posterior_predictive_features(fake_fit("gaussian", "outcome ~ phase", d), d,
                                        statistics = "initial", n_draws = 10)$summary
  c1 <- flat[flat$case == "c1", ]
  expect_equal(c1$observed, 1.8)
  expect_equal(c1$p_value, 0.8)
  expect_equal(c1$p_tie, 0.4)
  expect_equal(flat$observed[flat$case == "c2"], 0.4)
  expect_equal(flat$observed[flat$case == "pooled"], 1.1)
  trend <- posterior_predictive_features(fake_fit("gaussian", "outcome ~ time + phase", d), d,
                                         statistics = "initial", n_draws = 10)$summary
  expect_equal(trend$observed[trend$case == "c1"], 1)
  expect_equal(trend$p_value[trend$case == "c1"], 1)
  expect_equal(trend$p_tie[trend$case == "c1"], 0)
})

test_that("requested statistics that cannot be computed are reported in the note", {
  d <- make_ppc_data()
  d$phase <- "A"
  local_mocked_bindings(posterior_predict = function(object, newdata, ndraws, ...) {
    matrix(stats::rnorm(ndraws * nrow(newdata), 3), ndraws, nrow(newdata))
  }, .package = "brms")
  r <- posterior_predictive_features(fake_fit("gaussian", "outcome ~ 1", d), d,
                                     statistics = c("level", "immediacy", "nonoverlap", "bogus"),
                                     n_draws = 10)
  expect_true("level" %in% r$summary$statistic)
  expect_match(r$note, "Not computed: bogus, immediacy, nonoverlap")
  e <- posterior_predictive_features(fake_fit("gaussian", "outcome ~ 1", d), d[c("time", "outcome")],
                                     statistics = "level", n_draws = 10)
  expect_match(e$note, "Not computed: level")
})

test_that("a non-brms model stops with a clear error", {
  expect_error(posterior_predictive_features(lm(outcome ~ phase, make_ppc_data())), "must be a brmsfit")
})
