test_that("missing phase indicators retain an observed active phase", {
  d <- data.frame(A1 = c(NA, 1, 0), B1 = c(1, NA, NA), outcome = 1:3)
  expect_equal(consolidate_phase_indicators(d)$phase, c("B1", "A1", NA_character_))
})

test_that("residual standardization preserves intercept removal", {
  d <- data.frame(case = "P1", x = 1:8, outcome = c(5, 7, 6, 9, 8, 11, 10, 13))
  out <- standardize_by_case(d, "~ 0 + x")
  sigma <- summary(stats::lm(outcome ~ 0 + x, data = d))$sigma
  expect_equal(attr(out, "case_sigma")$sigma, sigma)
  expect_equal(out$outcome_std, d$outcome / sigma)
  expect_equal(attr(out, "case_sigma")$p, 1)
})

test_that("explicit intercept-only standardization fits the supplied model", {
  d <- data.frame(case = "P1", outcome = c(1, 3, 2, 5, 4, 8))
  out <- standardize_by_case(d, "~ 1")
  expect_equal(attr(out, "case_sigma")$sigma, stats::sd(d$outcome))
  expect_equal(attr(out, "case_sigma")$p, 1)
  zero <- standardize_by_case(d, "~ 0")
  expect_equal(attr(zero, "case_sigma")$sigma, sqrt(mean(d$outcome^2)))
  expect_equal(attr(zero, "case_sigma")$p, 0)
})

test_that("invalid outcomes return validation issues without diagnostic errors", {
  d <- data.frame(case = "P1", phase = rep(c("A", "B"), each = 4),
                  time = 1:8, outcome = letters[1:8])
  expect_silent(out <- validate_scd_data(d))
  expect_false(out$valid)
  expect_true("Outcome variable must be numeric" %in% out$issues)
  d$outcome <- c(Inf, 2:8)
  expect_silent(out <- validate_scd_data(d))
  expect_false(out$valid)
})

test_that("incomplete baseline times and constant times skip trend diagnostics", {
  d <- data.frame(case = "P1", phase = rep(c("A", "B"), each = 4),
                  time = c(rep(NA_real_, 4), 5:8), outcome = 1:8)
  expect_silent(out <- validate_scd_data(d))
  expect_length(out$trend_cases, 0)
  d$time <- 1
  expect_silent(out <- validate_scd_data(d))
  expect_length(out$trend_cases, 0)
})

test_that("phase removal leaves an invalid empty dataset without warnings", {
  d <- data.frame(case = "P1", phase = NA_character_, time = 1:8, outcome = 1:8)
  expect_silent(out <- validate_scd_data(d))
  expect_false(out$valid)
  expect_equal(out$n_obs, 0)
})
