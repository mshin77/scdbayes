make_mbd <- function() {
  d <- data.frame(case = rep(c("c1", "c2", "c3"), each = 12), time = rep(1:12, 3),
                  phase = c(rep(c("A", "B"), c(4, 8)), rep(c("A", "B"), c(6, 6)),
                            rep(c("A", "B"), c(8, 4))),
                  outcome = rep(c(2, 3), 18))
  process_scd_phases(d, structure = list(), scd_type = "multiple_baseline")
}

test_that("default A and B follow the scdhlm or shortest-treatment rule", {
  times <- .marginal_times(make_mbd(), "time", "case", ab_rule = "shortest_treatment")
  expect_equal(times$time_A, 4)
  expect_equal(times$time_B, 8)
  expect_equal(times$onset, 5)
  expect_equal(times$start, 1)
  expect_equal(.marginal_times(make_mbd(), "time", "case")$time_B, 12)
  expect_equal(.marginal_times(make_mbd(), "time", "case", duration = 2)$time_B, 6)
  fixed <- .marginal_times(make_mbd(), "time", "case", time_A = 5, time_B = 11)
  expect_equal(c(fixed$time_A, fixed$time_B, fixed$onset), c(5, 11, 6))
  expect_error(.marginal_times(make_mbd(), "time", "case", time_A = 6, time_B = 6), "later")
})

test_that("counterfactual rows borrow observed covariates and phase labels per arm", {
  set.seed(1)
  d <- make_mbd()
  d$age <- rep(c(7, 9, 11), each = 12)
  times <- .marginal_times(d, "time", "case")
  rows <- .counterfactual_rows(d, "case", n_new = 300, times = times, time_var = "time")
  expect_true(all(rows$age[1:300] == rows$age[301:600]))
  expect_equal(sort(unique(rows$age)), c(7, 9, 11))
  expect_equal(unique(as.character(rows$phase[rows$.arm == 0])), "A")
  expect_equal(unique(as.character(rows$phase[rows$.arm == 1])), "B")
})

test_that("counterfactual rows code treated and untreated arms at session B", {
  d <- make_mbd()
  d$obs <- seq_len(nrow(d))
  times <- .marginal_times(d, "time", "case", ab_rule = "shortest_treatment")
  rows <- .counterfactual_rows(d, c("case", "obs"), n_new = 3, times = times, time_var = "time")
  expect_equal(nrow(rows), 6)
  expect_equal(rows$.arm, c(0, 0, 0, 1, 1, 1))
  expect_equal(rows$level_AB, rows$.arm)
  expect_equal(rows$trend_AB, rows$.arm * (8 - 5))
  expect_equal(unique(rows$time_A), 7)
  expect_equal(unique(rows$time), 8)
  expect_equal(rows$case[1:3], rows$case[4:6])
  expect_equal(rows$obs, rows$case)
  expect_false(any(rows$case %in% d$case))
  d$time_B <- d$time - 5
  rows <- .counterfactual_rows(d, "case", n_new = 3, times = times, time_var = "time")
  expect_equal(rows$time_B, rep(8 - 5, 6))
})

test_that("tiers without treated sessions do not set A or B", {
  d <- make_mbd()
  extra <- d[d$case == "c1", ]
  extra$case <- "c4"
  extra$level_AB <- 0
  times <- .marginal_times(rbind(d, extra), "time", "case", ab_rule = "shortest_treatment")
  expect_equal(c(times$time_A, times$time_B), c(4, 8))
})

test_that("latent AR factor uses the stationary residual variance sderr^2 / (1 - phi^2)", {
  fake <- structure(list(formula = brms::bf(outcome ~ ar(time = time, gr = case, cov = TRUE))),
                    class = "brmsfit")
  local_mocked_bindings(as_draws_df = function(x, ...) {
    data.frame(sderr = c(0.5, 1), `ar[1]` = c(0.5, -0.9), check.names = FALSE)
  }, .package = "brms")
  expect_equal(.latent_ar_factor(fake, 1:2), exp(c(0.25 / 0.75, 1 / 0.19) / 2))
})

test_that("marginal_contrast rejects designs other than AB and multiple baseline", {
  abab <- data.frame(case = "c1", time = 1:8, outcome = 1:8,
                     phase = rep(c("A", "B", "A", "B"), each = 2))
  abab <- process_scd_phases(abab, list(), "reversal")
  fake <- structure(list(data = abab, family = list(family = "poisson", link = "log")),
                    class = "brmsfit")
  expect_error(marginal_contrast(fake, abab), "AB and multiple-baseline")
})

test_that("marginal_contrast returns the documented structure on a Poisson fit", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  old <- options(scdbayes.threads = NULL)
  on.exit(options(old), add = TRUE)
  set.seed(5)
  d <- make_mbd()
  d$outcome <- stats::rpois(nrow(d), exp(1 + 0.5 * d$level_AB))
  m <- suppressWarnings(build_model(d, "outcome ~ time_A + level_AB + trend_AB + (1 | case)",
                                    iter = 400, chains = 1, family = "poisson"))
  skip_if(is.null(m), "model did not fit")
  res <- marginal_contrast(m, d, n_new = 50, ndraws = 100)
  expect_named(res, c("summary", "draws", "n_draws", "time_A", "time_B"))
  expect_equal(res$n_draws, 100)
  expect_named(res$summary, c("quantity", "median", "lower", "upper"))
  expect_equal(res$summary$quantity, c("mean_untreated", "mean_treated", "difference",
                                       "ratio", "prob_improvement"))
  expect_equal(nrow(res$draws), 100)
  expect_equal(res$draws$ratio, res$draws$mean_treated / res$draws$mean_untreated)
  expect_equal(c(res$time_A, res$time_B), c(4, 12))
  y2 <- d
  y2$outcome[1] <- y2$outcome[1] + 100
  m2 <- m
  m2$data$outcome[1] <- m2$data$outcome[1] + 100
  set.seed(9)
  a <- marginal_contrast(m, d, n_new = 50, ndraws = 100)$draws
  set.seed(9)
  b <- marginal_contrast(m2, y2, n_new = 50, ndraws = 100)$draws
  expect_equal(a, b)
})


test_that("marginal tiers retain identities containing interaction separators", {
  d <- data.frame(study = c("b.c", "c"), case = c("a", "a.b"), time = 1)
  expect_equal(length(unique(.marginal_tiers(d, "time", "case"))), 2L)
  missing <- rbind(d, data.frame(study = NA_character_, case = "a", time = 1))
  expect_true(is.na(.marginal_tiers(missing, "time", "case")[3]))
})

test_that("latent AR factors distinguish conditional and covariance modes", {
  local_mocked_bindings(as_draws_df = function(x, ...) {
    data.frame(sderr = c(0.5, 1), `ar[1]` = c(0.5, -0.9), check.names = FALSE)
  }, .package = "brms")
  conditional <- structure(list(formula = brms::bf(outcome ~ ar(time = time, gr = case))),
                           class = "brmsfit")
  covariance <- structure(list(formula = brms::bf(outcome ~ 1,
    autocor = ~ ar(time = time, gr = case, cov = TRUE))), class = "brmsfit")
  expect_equal(.latent_ar_factor(conditional, 1:2), exp(c(0.25 * 1.25, 1 * 1.81) / 2))
  expect_equal(.latent_ar_factor(covariance, 1:2), exp(c(0.25 / 0.75, 1 / 0.19) / 2))
  missing <- structure(list(), class = "brmsfit")
  expect_error(.latent_ar_factor(missing, 1:2), "AR.*mode")
  second_order <- structure(list(formula = brms::bf(outcome ~ ar(time = time, p = 2))),
                            class = "brmsfit")
  expect_error(.latent_ar_factor(second_order, 1:2), "AR\\(1\\)")
})


test_that("AR specifications reject unverified modes and moving averages", {
  no_serial <- structure(list(formula = brms::bf(outcome ~ level_AB)), class = "brmsfit")
  expect_null(.model_ar_spec(no_serial))
  qualified <- structure(list(formula = brms::bf(outcome ~ brms::ar(time = time, cov = TRUE))),
                         class = "brmsfit")
  expect_identical(.model_ar_spec(qualified)$cov, TRUE)
  moving_average <- structure(list(formula = brms::bf(outcome ~ arma(time = time, p = 1, q = 1))),
                              class = "brmsfit")
  expect_error(.model_ar_spec(moving_average), "AR\\(1\\)")
  default_arma <- structure(list(formula = brms::bf(outcome ~ arma(time = time))), class = "brmsfit")
  expect_error(.model_ar_spec(default_arma), "AR\\(1\\)")
  ambiguous <- structure(list(formula = brms::bf(outcome ~ ar(time = time, cov = flag))),
                         class = "brmsfit")
  expect_error(.model_ar_spec(ambiguous), "logical covariance flag")
})
