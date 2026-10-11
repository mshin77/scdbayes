ab_series <- function(case, n_a, n_b, y) {
  data.frame(case = case, time = seq_len(n_a + n_b),
             phase = rep(c("A", "B"), c(n_a, n_b)), outcome = y)
}

test_that("standardize_by_case recovers the residual SD of each tier", {
  withr::local_seed(2026)
  sigmas <- c(P1 = 1, P2 = 3, P3 = 8)
  d <- do.call(rbind, lapply(names(sigmas), function(cs) {
    t <- 1:120
    b <- as.integer(t > 60)
    y <- 20 + 0.05 * t + 4 * b + 0.1 * (t - 61) * b + stats::rnorm(120, 0, sigmas[[cs]])
    ab_series(cs, 60, 60, y)
  }))
  out <- standardize_by_case(d)
  tab <- attr(out, "case_sigma")
  expect_equal(tab$sigma, unname(sigmas), tolerance = 0.15)
  expect_equal(out$outcome_std, d$outcome / tab$sigma[match(d$case, tab$tier)])
  expect_true(all(tab$status == "ok"))
})

test_that("a no-effect series keeps a zero effect after standardization", {
  flat <- c(1, 3, 2, 2, 3, 1)
  d <- rbind(ab_series("P1", 6, 6, c(flat, flat)),
             ab_series("P2", 6, 6, 10 * c(flat, flat)))
  out <- standardize_by_case(d)
  coded <- process_scd_phases(out, list(), "multiple_baseline")
  fit <- stats::lm(outcome_std ~ time_A + level_AB + trend_AB, data = coded[coded$case == "P2", ])
  expect_equal(unname(stats::coef(fit)[c("level_AB", "trend_AB")]), c(0, 0), tolerance = 1e-10)
  expect_equal(attr(out, "case_sigma")$sigma[2], 10 * attr(out, "case_sigma")$sigma[1])
})

test_that("standardization rescales coefficients without changing their t-values", {
  withr::local_seed(11)
  d <- ab_series("P1", 10, 10, 30 + stats::rnorm(20, 0, 4))
  out <- standardize_by_case(d, formula_fixed = "~ time_A + level_AB + trend_AB")
  coded <- process_scd_phases(out, list(), "ab")
  raw <- summary(stats::lm(outcome ~ time_A + level_AB + trend_AB, data = coded))$coefficients
  std <- summary(stats::lm(outcome_std ~ time_A + level_AB + trend_AB, data = coded))$coefficients
  expect_equal(std[, "Estimate"], raw[, "Estimate"] / attr(out, "case_sigma")$sigma)
  expect_equal(std[, "t value"], raw[, "t value"])
})

test_that("tiers too short for the fixed terms are NA with a warning", {
  withr::local_seed(3)
  d <- rbind(ab_series("P1", 8, 8, stats::rnorm(16, 10, 2)),
             ab_series("P2", 2, 2, c(4, 5, 7, 8)))
  expect_warning(out <- standardize_by_case(d), "P2")
  tab <- attr(out, "case_sigma")
  expect_true(is.na(tab$sigma[tab$tier == "P2"]))
  expect_match(tab$status[tab$tier == "P2"], "too few")
  expect_true(all(is.na(out$outcome_std[out$case == "P2"])))
  expect_false(anyNA(out$outcome_std[out$case == "P1"]))
})

test_that("a tier with an exact piecewise fit has zero residual SD and is NA", {
  d <- rbind(ab_series("P1", 6, 6, c(1:6, 10:15)),
             ab_series("P2", 6, 6, c(2, 4, 3, 5, 4, 6, 9, 8, 10, 9, 11, 10)))
  expect_warning(out <- standardize_by_case(d), "P1")
  expect_equal(attr(out, "case_sigma")$status[1], "zero residual SD")
})

test_that("random-effect and ar terms in formula_fixed are ignored", {
  withr::local_seed(5)
  d <- ab_series("P1", 8, 8, stats::rnorm(16, 10, 2))
  a <- standardize_by_case(d, formula_fixed = "outcome ~ time_A + level_AB + (1 | case)")
  b <- standardize_by_case(d, formula_fixed = ~ time_A + level_AB)
  expect_equal(a$outcome_std, b$outcome_std)
  expect_error(standardize_by_case(d, formula_fixed = "~ missing_col"), "missing_col")
})

test_that("standardize_by_case matches tiers to rows after phase coding drops a row", {
  set.seed(1)
  d <- do.call(rbind, lapply(1:3, function(k) data.frame(
    case = paste0("C", k), time = 1:12, phase = rep(c("A", "B"), each = 6),
    outcome = c(stats::rnorm(6, 10, k), stats::rnorm(6, 15, k)))))
  d$phase[2] <- NA
  tab <- attr(suppressWarnings(standardize_by_case(d)), "case_sigma")
  expect_equal(tab$n, c(11, 12, 12))
  d1 <- subset(d, case == "C1" & !is.na(phase))
  b <- d1$phase == "B"
  ref <- summary(stats::lm(outcome ~ I(time - 1) + b + I((time - 7) * b), d1))$sigma
  expect_equal(tab$sigma[1], ref)
})
