test_that("validate_scd_data flags missing columns", {
  result <- validate_scd_data(data.frame(x = 1:5))
  expect_false(result$valid)
  expect_true(any(grepl("Missing required", result$issues)))
})

test_that("validate_scd_data accepts valid data", {
  d <- data.frame(
    case = rep(c("A", "B", "C"), each = 10),
    phase = rep(c("A", "B"), 15),
    outcome = rnorm(30),
    time = rep(1:10, 3)
  )
  result <- validate_scd_data(d)
  expect_true(result$valid)
})

test_that("validate_scd_data warns on few cases", {
  d <- data.frame(case = rep("A", 10), phase = rep(c("A", "B"), 5),
                  outcome = rnorm(10), time = 1:10)
  result <- validate_scd_data(d)
  expect_true(any(grepl("Only 1 case", result$warnings)))
})

test_that("check_autocorrelation returns NULL warning when columns missing", {
  result <- scdbayes:::check_autocorrelation(data.frame(x = 1))
  expect_null(result$warning)
})

test_that("check_baseline_trend returns NULL warning when columns missing", {
  result <- scdbayes:::check_baseline_trend(data.frame(x = 1))
  expect_null(result$warning)
})

test_that("check_data_size passes small data and gates large data", {
  small <- data.frame(case = rep(c("C1", "C2"), each = 10), outcome = 1:20)
  expect_null(check_data_size(small, "fit"))
  warn <- check_data_size(data.frame(case = 1, outcome = seq_len(2000)), "fit")
  expect_equal(warn$level, "warn")
  block <- check_data_size(data.frame(case = 1, outcome = seq_len(6000)), "fit")
  expect_equal(block$level, "block")
  many <- data.frame(case = seq_len(30), outcome = 1)
  expect_equal(check_data_size(many, "sequential")$level, "block")
  expect_null(check_data_size(many, "upload"))
})

test_that("validate_scd_data reports rows with a missing phase label by tier", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 10), time = rep(1:10, 2),
                  phase = rep(rep(c("A", "B"), each = 5), 2), outcome = rnorm(20))
  d$phase[c(3, 13)] <- NA
  v <- validate_scd_data(d)
  expect_true(any(grepl("2 row(s) have a missing phase label (tier: C1, C2)",
                        unlist(v$warnings), fixed = TRUE)))
  expect_equal(v$n_phases, 2)
})

test_that("per-case counts exclude rows dropped for a missing phase", {
  d <- data.frame(case = rep(c("C1", "C2", "C3"), each = 6), time = rep(1:6, 3),
                  phase = rep(rep(c("A", "B"), each = 3), 3), outcome = rnorm(18))
  d$phase[d$case == "C1"][1:2] <- NA
  v <- validate_scd_data(d)
  expect_true(any(grepl("min: 4 )", unlist(v$warnings), fixed = TRUE)))
})

test_that("an outcome with one observed value is an issue, not an error", {
  v <- validate_scd_data(data.frame(case = "a", phase = c("A", "B", "B"), outcome = c(1, NA, NA)))
  expect_false(v$valid)
  expect_true("Outcome has zero variance" %in% unlist(v$issues))
})

test_that("autocorrelation is read from within-phase residuals over complete pairs", {
  set.seed(11)
  d <- data.frame(case = "P1", time = 1:20, phase = rep(c("A", "B"), each = 10),
                  outcome = rep(c(1, 10), each = 10) + rnorm(20, 0, 0.1))
  expect_gt(stats::acf(d$outcome, lag.max = 1, plot = FALSE)$acf[2], 0.5)
  expect_length(scdbayes:::check_autocorrelation(d)$cases, 0)
  smooth <- transform(d, outcome = outcome + c(1:10, 1:10) / 2)
  smooth$outcome[4] <- NA
  expect_equal(scdbayes:::check_autocorrelation(smooth)$cases, "P1")
})

test_that("validation checks phase runs and tiers rather than labels and case IDs", {
  bl <- data.frame(case = rep(c("P1", "P2", "P3"), each = 16), time = rep(1:16, 3),
                   phase = rep(rep(c("Baseline", "Treatment"), c(8, 8)), 3))
  bl$outcome <- 2 * bl$time + rnorm(48, 0, 0.2)
  expect_length(validate_scd_data(bl)$trend_cases, 3)
  rev <- data.frame(case = "P1", time = 1:16, phase = rep(c("A", "B", "A", "B"), c(2, 5, 4, 5)),
                    outcome = rnorm(16))
  expect_true(any(grepl("fewer than 3", unlist(validate_scd_data(rev)$warnings))))
  multi <- rbind(transform(bl, study = "S1"), transform(bl, study = "S2"))
  expect_equal(validate_scd_data(multi)$n_cases, 6)
})

test_that("check_data_size counts cases from a non-default case column", {
  d <- data.frame(participant = rep(paste0("P", 1:30), each = 2), outcome = 1)
  expect_equal(find_case_var(d), "participant")
  expect_match(check_data_size(d, "sequential")$message, "30 cases")
})
