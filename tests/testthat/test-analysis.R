test_that("detect_design_type returns ab for simple two-phase data", {
  d <- data.frame(
    case = rep(c("C1", "C2"), each = 10),
    phase = rep(rep(c("A", "B"), each = 5), 2),
    outcome = rnorm(20),
    time = rep(1:10, 2)
  )
  result <- detect_design_type(d)
  expect_equal(result, "ab")
})

test_that("detect_design_type returns reversal for ABAB data", {
  d <- data.frame(
    case = rep(c("C1", "C2"), each = 12),
    phase = rep(rep(c("A", "B", "A", "B"), each = 3), 2),
    outcome = rnorm(24),
    time = rep(1:12, 2)
  )
  expect_equal(detect_design_type(d), "reversal")
})

test_that("detect_design_type reads ABA withdrawal as reversal", {
  d <- data.frame(
    case = "C1",
    phase = rep(c("A", "B", "A"), each = 4),
    outcome = rnorm(12),
    time = 1:12
  )
  expect_equal(detect_design_type(d), "reversal")
})

test_that("detect_design_type reads A1B1A2B2 labels as reversal, not CCD", {
  d <- data.frame(
    case = "C1",
    phase = rep(c("A1", "B1", "A2", "B2"), each = 3),
    outcome = rnorm(12),
    time = 1:12
  )
  expect_equal(detect_design_type(d), "reversal")
})

test_that("detect_design_type keeps digit-coded criteria as changing_criterion", {
  d <- data.frame(
    case = "C1",
    phase = c(rep("A", 4), rep("C1", 4), rep("C2", 4), rep("C3", 4)),
    outcome = rnorm(16),
    time = 1:16
  )
  expect_equal(detect_design_type(d), "changing_criterion")
})

test_that("detect_design_type returns unknown when columns missing", {
  result <- detect_design_type(data.frame(x = 1))
  expect_equal(result, "unknown")
})

test_that("detect_hierarchy_levels finds two levels by default", {
  d <- data.frame(case = rep(c("A", "B"), each = 5), time = rep(1:5, 2),
                  outcome = rnorm(10))
  result <- detect_hierarchy_levels(d)
  expect_equal(result$n_levels, 2)
  expect_equal(result$level2_var, "case")
})

test_that("recommend_distribution_family handles binary data", {
  result <- recommend_distribution_family(c(0, 1, 0, 1, 1, 0))
  expect_true(grepl("Bernoulli", result$recommended))
})

test_that("recommend_distribution_family handles empty data", {
  result <- recommend_distribution_family(c())
  expect_true(grepl("Gaussian", result$recommended))
})

test_that("detect_design_type reads staggered A-B-M data as multiple baseline", {
  lens <- list(c(4, 6, 3), c(6, 4, 3), c(8, 2, 3))
  d <- do.call(rbind, lapply(seq_along(lens), function(i) {
    data.frame(case = paste0("C", i), time = 1:13,
               phase = rep(c("A", "B", "M"), lens[[i]]), outcome = rnorm(13))
  }))
  expect_equal(detect_design_type(d), "multiple_baseline")
})

test_that("detect_design_type needs a strict majority of alternating cases", {
  atd <- data.frame(case = "C1", time = 1:8, phase = rep(c("A", "B"), 4), outcome = rnorm(8))
  ab <- data.frame(case = "C2", time = 1:8, phase = rep(c("A", "B"), each = 4), outcome = rnorm(8))
  expect_equal(detect_design_type(rbind(atd, ab)), "reversal")
  atd2 <- transform(atd, case = "C3")
  expect_equal(detect_design_type(rbind(atd, atd2, ab)), "alternating")
})

test_that("detect_design_type splits repeated case IDs by study", {
  d <- do.call(rbind, lapply(1:2, function(s) do.call(rbind, lapply(1:3, function(i) {
    data.frame(study = paste0("S", s), case = paste0("C", i), time = 1:12,
               phase = rep(c("A", "B"), c(2 + i + 2 * s, 10 - i - 2 * s)), outcome = rnorm(12))
  }))))
  expect_equal(detect_design_type(d), "multiple_baseline")
})

test_that("detect_design_type reads Withdrawal-labeled ABAB as reversal", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 12), time = rep(1:12, 2), outcome = rnorm(24),
                  phase = rep(rep(c("Baseline", "Intervention", "Withdrawal", "Reinstatement"),
                                  each = 3), 2))
  expect_equal(detect_design_type(d), "reversal")
  d$phase <- rep(rep(c("Baseline1", "Intervention1", "Baseline2", "Intervention2"), each = 3), 2)
  expect_equal(detect_design_type(d), "reversal")
})

test_that("detect_design_type prefers staggered A-B-M-G over changing criterion", {
  lens <- list(c(3, 4, 3, 2), c(5, 3, 2, 2), c(7, 2, 2, 1))
  d <- do.call(rbind, lapply(seq_along(lens), function(i) {
    data.frame(case = paste0("C", i), time = 1:12, outcome = rnorm(12),
               phase = rep(c("A", "B", "M", "G"), lens[[i]]))
  }))
  expect_equal(detect_design_type(d), "multiple_baseline")
})

test_that("detect_design_type accepts a CCD returning to an earlier criterion", {
  d <- data.frame(case = "C1", time = 1:18, outcome = rnorm(18),
                  phase = rep(c("A", "C1", "C2", "C3", "C2", "C4"), each = 3))
  expect_equal(detect_design_type(d), "changing_criterion")
})

test_that("detect_design_type reads baseline plus three alternating conditions as ATD", {
  d <- data.frame(case = "C1", time = 1:17, outcome = rnorm(17),
                  phase = c(rep("A", 5), rep(c("X", "Y", "Z"), 4)))
  expect_equal(detect_design_type(d), "alternating")
})

test_that("sensitivity arms refit a fitted model with update() and keep its submodels", {
  d <- process_scd_phases(simulate_scd_data(n_cases = 3), list(), "AB")
  form <- brms::bf(outcome ~ time_A + level_AB + trend_AB + (1 | case), sigma ~ phase)
  base_prior <- c(brms::prior_string("normal(0, 25)", class = "b"),
                  brms::prior_string("student_t(3, 0, 10)", class = "sd"))
  fit <- structure(list(data = d, formula = form, family = brms::brmsfamily("gaussian"),
                        prior = base_prior), class = "brmsfit")
  seen <- list()
  local_mocked_bindings(update = function(object, ...) {
    seen[[length(seen) + 1]] <<- list(...)
    object
  }, .package = "stats")
  local_mocked_bindings(fixef = function(object, ...) matrix(1, 1, 1), .package = "brms")
  res <- run_sensitivity_analysis(model = fit, iter = 10, chains = 1)
  expect_setequal(names(res), c("prior_default", "prior_narrow", "prior_diffuse",
                                "prior_sd_small", "student_t"))
  expect_length(seen, 5)
  narrow <- seen[[2]]$prior
  expect_true(any(narrow$coef == "level_AB" & narrow$class == "b"))
  sd_small <- seen[[4]]$prior
  expect_equal(sum(sd_small$class == "sd"), 1)
  expect_equal(seen[[5]]$family$family, "student")
  expect_null(seen[[1]]$family)
  expect_error(run_sensitivity_analysis(), "needs")
  expect_equal(attr(res, "treatment_coef"), "level_AB")
})

test_that("sensitivity arms scale to the formula response and skip the t arm for a t fit", {
  d <- process_scd_phases(simulate_scd_data(n_cases = 3), list(), "AB")
  names(d)[names(d) == "outcome"] <- "wcpm"
  fit <- structure(list(data = d, formula = brms::bf(wcpm ~ level_AB + (1 | case)),
                        family = brms::brmsfamily("student"),
                        prior = brms::prior_string("normal(0, 25)", class = "b")), class = "brmsfit")
  local_mocked_bindings(update = function(object, ...) object, .package = "stats")
  local_mocked_bindings(fixef = function(object, ...) matrix(1, 1, 1), .package = "brms")
  res <- run_sensitivity_analysis(model = fit, iter = 10, chains = 1)
  expect_equal(attr(res, "treatment_sd"), 2.5 * sd(d$wcpm))
  expect_false("student_t" %in% names(res))
})

test_that("detect_design_type reads A-B-M-G as AB, not changing criterion", {
  d <- data.frame(case = "C1", time = 1:22, outcome = rnorm(22),
                  phase = rep(c("A", "B", "M", "G"), c(6, 6, 5, 5)))
  expect_equal(detect_design_type(d), "ab")
  b <- d
  b$phase <- rep(c("A", "B1", "B2", "B3"), c(6, 6, 5, 5))
  expect_equal(detect_design_type(b), "changing_criterion")
})

test_that("a criterion that steps within intervention runs is changing criterion, not reversal", {
  ph <- rep(c("A", "B", "B", "B", "A", "B"), each = 4)
  d <- data.frame(case = "P1", time = seq_along(ph), phase = ph, outcome = rnorm(24),
                  criterion = rep(c(NA, 10, 15, 10, NA, 20), each = 4))
  expect_equal(detect_design_type(d), "changing_criterion")
  d$criterion <- NULL
  expect_equal(detect_design_type(d), "reversal")
  abab <- data.frame(case = "P1", time = 1:16, outcome = rnorm(16),
                     phase = rep(c("A", "B", "A", "B"), each = 4),
                     criterion = rep(c(NA, 10, NA, 20), each = 4))
  expect_equal(detect_design_type(abab), "reversal")
})

test_that("a criterion constant within each tier of a multiple baseline does not escalate", {
  d <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(case = paste0("C", i), time = 1:12, phase = rep(c("A", "B"), c(3 + i, 9 - i)),
               outcome = rnorm(12), criterion = c(80, 90, 100)[i])
  }))
  expect_equal(detect_design_type(d), "multiple_baseline")
})

test_that("labeled criterion subphases with a return to baseline are a changing criterion", {
  d <- data.frame(case = "C1", time = 1:24, outcome = rnorm(24),
                  phase = rep(c("A", "B1", "B2", "B3", "A", "B4"), each = 4))
  expect_equal(detect_design_type(d), "changing_criterion")
  pd <- process_scd_phases(d, list(), "changing_criterion")
  expect_equal(grep("^level_", names(pd), value = TRUE),
               c(paste0("level_crit", 1:4), "level_ret1"))
  abab <- data.frame(case = "C1", time = 1:16, outcome = rnorm(16),
                     phase = rep(c("A1", "B1", "A2", "B2"), each = 4))
  expect_equal(detect_design_type(abab), "reversal")
})

test_that("an A-B1-B2 staggered design is a multiple baseline", {
  d <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(case = paste0("C", i), time = 1:15, outcome = rnorm(15),
               phase = rep(c("A", "B1", "B2"), c(3 + i, 6, 6 - i)))
  }))
  expect_equal(detect_design_type(d), "multiple_baseline")
})

test_that("a criterion returning to an earlier level is a changing criterion", {
  d <- data.frame(case = "P1", time = 1:21, phase = rep(c("A", "B"), c(6, 15)), outcome = rnorm(21),
                  criterion = c(rep(NA, 6), rep(c(2, 4, 2), each = 5)))
  expect_equal(detect_design_type(d), "changing_criterion")
})

test_that("a staggered A-B1-B2-M design is a multiple baseline", {
  d <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(case = paste0("C", i), time = 1:(19 + 2 * i), outcome = rnorm(19 + 2 * i),
               phase = rep(c("A", "B1", "B2", "M"), c(4 + 2 * i, 5, 5, 5)))
  }))
  expect_equal(detect_design_type(d), "multiple_baseline")
  d$phase[d$phase == "M"] <- "B3"
  expect_equal(detect_design_type(d), "changing_criterion")
})

test_that("bundled sample data keep their detected designs", {
  dir <- system.file("scdbayes.app", "data", package = "scdbayes")
  skip_if(!nzchar(dir), "sample data not installed")
  got <- vapply(c("reversal_design", "changing_criterion_design", "ab_design",
                  "multiple_baseline_design", "alternating_treatment_design"),
                function(f) detect_design_type(utils::read.csv(file.path(dir, paste0(f, ".csv")))),
                character(1))
  expect_equal(unname(got), c("reversal", "changing_criterion", "ab", "multiple_baseline",
                              "alternating"))
})
