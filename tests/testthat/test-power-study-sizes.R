test_that("power generation accepts study-specific cases and phase lengths", {
  set.seed(12)
  d <- scdbayes:::.simulate_power_data(design = "ab", family = "gaussian",
    n_cases = c(2, 4), n_studies = 2, n_phase_a = c(3, 5), n_phase_b = c(4, 6),
    effect = 0.5, coefficient = "level", autocorr = 0, icc = 0.2,
    study_sd = 0.1, slope_sd = 0.1, standardized = TRUE)
  expect_equal(length(unique(d$case[d$study == "S1"])), 2)
  expect_equal(length(unique(d$case[d$study == "S2"])), 4)
  expect_equal(sum(d$study == "S1"), 14)
  expect_equal(sum(d$study == "S2"), 44)
})

test_that("study size vectors reject incomplete or invalid values", {
  expect_error(scdbayes:::.power_study_sizes(c(2, 3), 3), "one value or")
  expect_error(scdbayes:::.power_study_sizes(c(2, NA), 2), "positive integers")
  expect_equal(scdbayes:::.power_study_sizes(3, 2), c(3, 3))
})
