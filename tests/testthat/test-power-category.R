test_that("category allocation stays constant within studies", {
  set.seed(51)
  data <- .simulate_power_data("ab", "gaussian", 2, 5, 4, 4,
    0.3, "level", 0, 0, 0, standardized = TRUE,
    studies_per_category = c(2, 3), common_effect = 0.5)
  expect_equal(unique(data[c("study", "category")])$category, c(0, 0, 1, 1, 1))
  expect_equal(length(unique(data$case)), 10)
})

test_that("category models test direct interactions", {
  f <- .build_power_formula(2, 5, FALSE, planning_target = "category")
  expect_match(deparse1(f), "phase_num:category", fixed = TRUE)
  data <- .simulate_power_data("ab", "gaussian", 2, 5, 4, 4,
    0.3, "level", 0, 0, 0, standardized = TRUE,
    studies_per_category = c(2, 3), common_effect = 0.5)
  code <- brms::make_standata(f, data, family = gaussian())
  expect_true("phase_num:category" %in% colnames(code$X))
  reversed <- .build_power_formula(2, 5, FALSE, design = "reversal",
    planning_target = "category")
  expect_match(deparse1(reversed), "level_B1A2:category", fixed = TRUE)
})

test_that("category planning rejects unidentifiable allocations before fitting", {
  expect_error(run_power_simulation(planning_target = "category",
    studies_per_category = c(1, 4)), "at least 2")
  expect_error(run_power_simulation(planning_target = "category",
    common_effect = -1), "nonnegative")
})

test_that("category decisions target benefit differences and withdrawal signs", {
  expect_true(.power_decision(rep(-2, 100), "prob_meaningful", 0.95,
    c(-1, 1), "decrease"))
  expect_false(.power_decision(rep(2, 100), "prob_meaningful", 0.95,
    c(-1, 1), "decrease"))
  expect_true(.transition_decision(list(rep(-2, 100), rep(2, 100), rep(-2, 100)),
    c(-1, 1, -1), "prob_meaningful", 0.95, c(-1, 1)))
})
