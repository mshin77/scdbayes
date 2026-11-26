# Tests for grouping-variable detection and random-effects formula building.

test_that("find_case_var matches standard SCED patterns", {
  expect_equal(find_case_var(data.frame(case = 1:3, x = 1:3)), "case")
  expect_equal(find_case_var(data.frame(participant = 1:3, x = 1:3)), "participant")
  expect_equal(find_case_var(data.frame(subject = 1:3, x = 1:3)), "subject")
  expect_equal(find_case_var(data.frame(id = 1:3, x = 1:3)), "id")
  expect_null(find_case_var(data.frame(client_id = 1:3, x = 1:3)))
})

test_that("find_*_var helpers are case-insensitive and preserve original casing", {
  expect_equal(find_case_var(data.frame(Case = 1:3, x = 1:3)), "Case")
  expect_equal(find_case_var(data.frame(PARTICIPANT = 1:3, x = 1:3)), "PARTICIPANT")
  expect_equal(find_phase_var(data.frame(Phase = c("A","B"), x = 1:2)), "Phase")
  expect_equal(find_time_var(data.frame(Session = 1:3, x = 1:3)), "Session")
  expect_equal(find_study_var(data.frame(Study_ID = 1:3, x = 1:3)), "Study_ID")
})

test_that("find_study_var includes site, school, district", {
  expect_equal(find_study_var(data.frame(site = c("A","B"), x = 1:2)), "site")
  expect_equal(find_study_var(data.frame(school = c("X","Y"), x = 1:2)), "school")
  expect_equal(find_study_var(data.frame(district = c("N","S"), x = 1:2)), "district")
})

test_that("find_cluster_var matches mid-level grouping patterns", {
  expect_equal(find_cluster_var(data.frame(teacher = c("T1","T2"), x = 1:2)), "teacher")
  expect_equal(find_cluster_var(data.frame(classroom = c("C1","C2"), x = 1:2)), "classroom")
  expect_equal(find_cluster_var(data.frame(group = c("G1","G2"), x = 1:2)), "group")
  expect_null(find_cluster_var(data.frame(x = 1:3)))
})

test_that("find_time_var picks 'timepoint' and 'observation'", {
  expect_equal(find_time_var(data.frame(timepoint = 1:3, x = 1:3)), "timepoint")
  expect_equal(find_time_var(data.frame(observation = 1:3, x = 1:3)), "observation")
})

test_that("find_nest_var: name patterns including extended list", {
  patterns <- c("behavior", "setting", "target", "goal", "tier",
                "stimulus", "context", "skill", "task", "lesson",
                "topic", "domain")
  lapply(patterns, function(p) {
    df <- data.frame(case = rep(c("a", "b"), each = 4),
                     v = rep(c("x", "y", "z", "w"), 2))
    names(df)[2] <- p
    expect_equal(find_nest_var(df, exclude = "case"), p)
  })
})

test_that("find_nest_var: structure-based detection needs concurrent series", {
  df <- data.frame(case = rep(c("a", "b"), each = 4),
                   client_label = rep(c("X1", "X2"), 4),
                   time = rep(rep(1:2, each = 2), 2))
  expect_equal(find_nest_var(df, exclude = "case", case_var = "case"), "client_label")
  expect_null(find_nest_var(df[c("case", "client_label")], exclude = "case"))
})

test_that("a character date column never becomes a nest", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 6), time = rep(1:6, 2),
                  date = as.character(as.Date("2024-01-01") + 0:11),
                  phase = rep(rep(c("A", "B"), each = 3), 2), outcome = rnorm(12))
  expect_null(find_nest_var(d, exclude = "case", case_var = "case"))
  expect_equal(tier_vars(d), "case")
})

test_that("tier_vars keeps a behavior nest when onsets differ without repeated times", {
  beh <- data.frame(case = "C1", behavior = rep(c("b1", "b2", "b3"), each = 8),
                    time = 1:24, outcome = rnorm(24),
                    phase = c(rep(c("A", "B"), c(3, 5)), rep(c("A", "B"), c(5, 3)),
                              rep(c("A", "B"), c(6, 2))))
  expect_equal(tier_vars(beh), c("case", "behavior"))
  setting <- data.frame(case = "C1", time = 1:12, outcome = rnorm(12),
                        phase = rep(c("A", "B"), each = 6),
                        setting = rep(c("home", "school"), each = 6))
  expect_equal(tier_vars(setting), "case")
})

test_that("find_nest_var: ignores numeric, phase/time/outcome, piecewise, treatment", {
  df <- data.frame(
    case = rep(c("a", "b"), 6),
    phase = rep(c("A", "B"), 6),
    time = 1:12,
    outcome = rnorm(12),
    treatment = rep(c("ctrl", "exp"), 6),
    condition = rep(c("ctrl", "exp"), 6),
    intervention = rep(c("ctrl", "exp"), 6),
    level_AB = rep(c(0, 1), 6),
    trend_AB = 1:12,
    numeric_factor = rep(1:3, 4)
  )
  expect_null(find_nest_var(df, exclude = "case"))
})

test_that("find_nest_var returns NULL when nothing qualifies", {
  expect_null(find_nest_var(data.frame(x = 1:3, y = rnorm(3))))
})

test_that("build_default_random_effects: single case, no nesting -> NULL", {
  df <- data.frame(case = rep("S1", 20),
                   time = 1:20,
                   phase = rep(c("A", "B"), each = 10),
                   level_AB = rep(c(0, 1), each = 10))
  expect_null(build_default_random_effects(df))
})

test_that("build_default_random_effects: n=1 case with tier nesting", {
  df <- data.frame(case = rep("S1", 40),
                   time = 1:40,
                   skill = rep(c("read", "math", "write", "sci"), 10),
                   phase = rep(c("A", "B"), each = 20),
                   level_AB = rep(c(0, 1), each = 20))
  expect_equal(build_default_random_effects(df),
               "(1 + level_AB | case:skill)")
})

test_that("build_default_random_effects: n>=2 with tier nesting -> 3-level", {
  df <- data.frame(case = rep(c("A", "B", "C"), each = 40),
                   task = rep(c("t1", "t2", "t3", "t4"), 30),
                   phase = rep(c("A", "B"), 60),
                   level_AB = rep(c(0, 1), 60))
  expect_equal(build_default_random_effects(df),
               "(1 | case) + (1 + level_AB | case:task)")
})

test_that("build_default_random_effects: n>=4 with no tier", {
  df <- data.frame(case = rep(c("A", "B", "C", "D"), each = 20),
                   phase = rep(c("A", "B"), 40),
                   level_AB = rep(c(0, 1), 40))
  expect_equal(build_default_random_effects(df),
               "(1 + level_AB | case)")
})

test_that("build_default_random_effects: explicit nest_var override", {
  df <- data.frame(case = rep(c("A", "B"), each = 20),
                   weird = rep(c("x", "y"), 20),
                   phase = rep(c("A", "B"), 20),
                   level_AB = rep(c(0, 1), 20))
  expect_equal(
    build_default_random_effects(df, nest_var = "weird"),
    "(1 | case) + (1 + level_AB | case:weird)"
  )
})

test_that("build_default_random_effects: meta structure with study + tier", {
  df <- data.frame(study = rep(c("S1", "S2"), each = 40),
                   case = rep(c("A", "B"), each = 20, times = 2),
                   behavior = rep(c("b1", "b2"), 40),
                   phase = rep(c("A", "B"), 40),
                   level_AB = rep(c(0, 1), 40))
  out <- build_default_random_effects(df)
  expect_equal(out, paste("(1 | study)", "(1 | study:case)",
                          "(1 + level_AB | study:case:behavior)", sep = " + "))
})

test_that("build_default_random_effects counts repeated case IDs per study", {
  df <- data.frame(study = rep(c("S1", "S2"), each = 20), case = "C1",
                   phase = rep(c("A", "B"), 20), level_AB = rep(c(0, 1), 20))
  expect_equal(build_default_random_effects(df), "(1 + level_AB | study:case)")
})

test_that("default random effects add cluster and study intercepts from tier_vars", {
  set.seed(2)
  one <- function(study, teacher, case, onset) {
    data.frame(study = study, teacher = teacher, case = case, time = 1:16,
               phase = rep(c("A", "B"), c(onset - 1, 17 - onset)), outcome = rnorm(16))
  }
  d <- rbind(one("S1", "T1", "P1", 6), one("S1", "T1", "P2", 8), one("S1", "T2", "P1", 7),
             one("S1", "T2", "P2", 9), one("S2", "T3", "P1", 6), one("S2", "T3", "P2", 10))
  p <- process_scd_phases(d, list(), "multiple_baseline")
  expected <- paste("(1 | study)", "(1 | study:teacher)",
                    "(1 + level_AB | study:teacher:case)", sep = " + ")
  expect_equal(default_random_effects(d), expected)
  expect_equal(default_random_effects(p), expected)
  expect_equal(.scd_random_effects(p, .scd_fixed_terms(names(p))), expected)
  expect_equal(build_default_random_effects(p), expected)
})

test_that("alternating-treatment dummies all enter as uncorrelated slopes", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 12), time = rep(1:12, 2), outcome = rnorm(24),
                  phase = rep(rep(c("X", "Y", "Z"), 4), 2))
  expect_equal(default_random_effects(d, "alternating"),
               "(1 | case) + (0 + atd_Y + atd_Z || case)")
})

test_that("build_default_random_effects lets every reversal step vary", {
  d <- data.frame(case = rep(c("C1", "C2", "C3"), each = 12), time = rep(1:12, 3),
                  phase = rep(rep(c("A", "B", "A", "B"), each = 3), 3), outcome = rnorm(36))
  pd <- process_scd_phases(d, list(), "reversal")
  expect_equal(build_default_random_effects(pd),
               "(1 + level_A1B1 | case) + (0 + level_B1A2 + level_A2B2 || case)")
})

test_that("has_random_slope detects variables on the bar LHS only", {
  expect_false(has_random_slope("y ~ x + (1 | case)"))
  expect_false(has_random_slope("y ~ x + (1 || case)"))
  expect_false(has_random_slope("y ~ x + (0 + 1 | case)"))
  expect_false(has_random_slope("y ~ x"))
  expect_false(has_random_slope("y ~ x + (1 | study) + (1 | study:case)"))
  expect_false(has_random_slope("y ~ x + (1 | ID | case)"))
  expect_true(has_random_slope("y ~ x + (level_AB | case)"))
  expect_true(has_random_slope("y ~ x + (0 + x | g)"))
  expect_true(has_random_slope("y ~ x + (1 + x || case)"))
  expect_true(has_random_slope("y ~ x + (1 | study) + (1 + x | study:case)"))
  expect_false(has_random_slope("y ~ x + ar(time = time, gr = case)"))
  expect_false(has_random_slope("not a formula ~~ ("))
})

test_that("build_default_random_effects: no case column -> NULL", {
  df <- data.frame(x = 1:20, phase = rep(c("A", "B"), 10),
                   level_AB = rep(c(0, 1), 10))
  expect_null(build_default_random_effects(df))
})

test_that("tier_vars orders study, cluster, case and skips IOA columns", {
  d <- data.frame(study = "S1", classroom = "K1", case = rep(c("C1", "C2"), each = 4),
                  observer = rep(c("O1", "O2"), 4), time = rep(1:4, 2), outcome = rnorm(8))
  expect_equal(tier_vars(d), c("study", "classroom", "case"))
  expect_null(find_cluster_var(data.frame(observer = 1:2, rater = 1:2)))
  beh <- data.frame(case = "C1", behavior = rep(c("b1", "b2"), each = 4),
                    time = rep(1:4, 2), outcome = rnorm(8))
  expect_equal(tier_vars(beh), c("case", "behavior"))
})

test_that("an integer-coded nest within a case splits concurrent series", {
  mb <- data.frame(case = "P1", outcome_id = rep(1:3, each = 12), time = rep(1:12, 3),
                   phase = c(rep(c("A", "B"), c(4, 8)), rep(c("A", "B"), c(6, 6)), rep(c("A", "B"), c(8, 4))),
                   outcome = rnorm(36))
  expect_equal(tier_vars(mb), c("case", "outcome_id"))
})
