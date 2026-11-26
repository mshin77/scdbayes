test_that("consolidate_phase_indicators handles no indicator cols", {
  d <- data.frame(case = "A", outcome = 1)
  result <- consolidate_phase_indicators(d)
  expect_false("phase" %in% names(result))
})

test_that("consolidate_phase_indicators converts binary columns", {
  d <- data.frame(A1 = c(1, 1, 0, 0), B1 = c(0, 0, 1, 1), outcome = rnorm(4))
  result <- consolidate_phase_indicators(d)
  expect_true("phase" %in% names(result))
  expect_equal(result$phase, c("A1", "A1", "B1", "B1"))
})

test_that("process_scd_phases adds piecewise columns for AB design", {
  d <- data.frame(
    case = rep("C1", 10),
    phase = rep(c("A", "B"), each = 5),
    outcome = rnorm(10),
    time = 1:10
  )
  structure <- list(levels = list(), time_vars = "time")
  result <- process_scd_phases(d, structure, scd_type = "AB")
  expect_true("level_AB" %in% names(result))
  expect_true("trend_AB" %in% names(result))
})

test_that("reversal coding yields formula terms for BAB and ABABAB", {
  bab <- data.frame(case = rep(c("C1", "C2"), each = 9), time = rep(1:9, 2),
                    phase = rep(rep(c("B", "A", "B"), each = 3), 2), outcome = rnorm(18))
  pd <- process_scd_phases(bab, list(levels = list()), scd_type = "reversal")
  terms <- reversal_terms(names(pd))
  expect_equal(terms, c("time_B1", "level_B1A1", "trend_B1A1", "level_A1B2", "trend_A1B2"))
  expect_true(all(terms %in% names(pd)))
  f <- .build_scd_formula(pd)
  expect_true(all(terms %in% all.vars(stats::as.formula(f))))
  expect_equal(.reversal_onset(names(pd)), "level_A1B2")

  ab3 <- data.frame(case = "C1", time = 1:18, outcome = rnorm(18),
                    phase = rep(c("A", "B", "A", "B", "A", "B"), each = 3))
  pd3 <- process_scd_phases(ab3, list(levels = list()), scd_type = "reversal")
  expect_true(all(c("level_B2A3", "level_A3B3") %in% reversal_terms(names(pd3))))
})

test_that("unrecognized phase labels are ordered by first appearance", {
  expect_equal(.phase_numeric(c("Pre", "Pre", "Post"), 1:3), c(0, 0, 1))
  expect_equal(.phase_numeric(c("Tx", "Tx", "Wait"), c(3, 4, 1)), c(1, 1, 0))
  expect_equal(.phase_numeric(c("Baseline", "Intervention"), 1:2), c(0, 1))
})

test_that("a covariate containing 'crit' does not force changing-criterion", {
  d <- data.frame(case = rep(c("C1", "C2", "C3"), each = 10), time = rep(1:10, 3),
                  phase = rep(rep(c("A", "B"), each = 5), 3), outcome = rnorm(30),
                  critical_incidents = rep(1:10, 3))
  expect_equal(detect_design_type(d), "ab")
  names(d)[names(d) == "critical_incidents"] <- "criterion"
  expect_equal(detect_design_type(d), "changing_criterion")
})

test_that("reversal time-in-phase follows the time values across gaps", {
  d <- data.frame(case = "C1", time = c(1, 2, 5, 6, 8, 12, 13, 20),
                  phase = rep(c("A", "B"), each = 4), outcome = rnorm(8))
  pd <- process_scd_phases(d, list(levels = list()), scd_type = "reversal")
  pd <- pd[order(pd$time), ]
  expect_equal(pd$time_A1, c(0, 1, 4, 5, 7, 11, 12, 19))
  expect_equal(pd$trend_A1B1, c(0, 0, 0, 0, 0, 4, 5, 12))
})

test_that("AB coder adds maintenance terms only when a maintenance phase exists", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 6), time = rep(1:6, 2),
                  phase = rep(c("A", "A", "B", "B", "B", "M"), 2), outcome = rnorm(12))
  pd <- process_scd_phases(d, list(levels = list()), scd_type = "AB")
  expect_equal(pd$time_M, rep(-5:0, 2))
  expect_equal(pd$level_BM, rep(c(0, 0, 0, 0, 0, 1), 2))
  expect_equal(pd$trend_AB, rep(c(0, 0, 0, 1, 2, 3), 2))
  ab <- process_scd_phases(d[d$phase != "M", ], list(levels = list()), scd_type = "AB")
  expect_false(any(c("time_M", "level_BM", "trend_BM") %in% names(ab)))
})

test_that("alternating-treatment blocks restart within each case", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 4), time = rep(1:4, 2),
                  phase = rep(c("a", "B", "a", "B"), 2), outcome = rnorm(8))
  pd <- process_scd_phases(d, list(levels = list()), scd_type = "alternating")
  expect_equal(pd$alternation_block[pd$case == "C2"], c(1, 2, 3, 4))
  expect_true("atd_B" %in% names(pd))
  expect_false("atd_a" %in% names(pd))
})

test_that("staggered multiple baseline with baseline trend and no effect yields null steps", {
  set.seed(11)
  onsets <- c(6, 9, 12)
  d <- do.call(rbind, lapply(seq_along(onsets), function(i) {
    t <- 1:20
    data.frame(case = paste0("C", i), time = t,
               phase = ifelse(t < onsets[i], "A", "B"),
               outcome = 10 + 3 * i + t + rnorm(20, 0, 0.05))
  }))
  pd <- process_scd_phases(d, list(), "multiple_baseline")
  expect_equal(pd$time_A, pd$time - 1)
  b <- stats::coef(stats::lm(outcome ~ case + time_A + level_AB + trend_AB, data = pd))
  expect_lt(abs(b[["level_AB"]]), 0.2)
  expect_lt(abs(b[["trend_AB"]]), 0.05)
  expect_equal(b[["time_A"]], 1, tolerance = 0.05)
})

test_that("ABAB straight line with no effect gives null reversal steps", {
  d <- data.frame(case = "C1", time = 1:20,
                  phase = rep(c("A", "B", "A", "B"), each = 5))
  d$outcome <- 2 + 0.5 * d$time
  pd <- process_scd_phases(d, list(), "reversal")
  terms <- reversal_terms(names(pd))
  expect_equal(terms, c("time_A1", "level_A1B1", "trend_A1B1", "level_B1A2",
                        "trend_B1A2", "level_A2B2", "trend_A2B2"))
  b <- stats::coef(stats::lm(stats::reformulate(terms, "outcome"), data = pd))
  expect_equal(unname(b[grep("^(level|trend)_", names(b))]), rep(0, 6), tolerance = 1e-8)
  expect_equal(pd$trend_A1B1, pmax(0, d$time - 6))
  expect_equal(pd$level_B1A2, as.integer(d$time >= 11))
})

test_that("repeated case IDs across studies are coded per study tier", {
  d <- rbind(
    data.frame(study = "S1", case = "C1", time = 1:10, phase = rep(c("A", "B"), c(3, 7))),
    data.frame(study = "S2", case = "C1", time = 1:10, phase = rep(c("A", "B"), c(6, 4))))
  d$outcome <- rnorm(20)
  pd <- process_scd_phases(d, list(), "multiple_baseline")
  expect_equal(pd$time_B[pd$study == "S1"], (1:10) - 4)
  expect_equal(pd$time_B[pd$study == "S2"], (1:10) - 7)
  expect_equal(pd$level_AB[pd$study == "S2"], as.integer(1:10 >= 7))
  expect_equal(sort(unique(pd$stagger_group)), 1:2)
})

test_that("changing criterion steps are cumulative with continuing trends", {
  d <- data.frame(case = "C1", time = 1:12, outcome = rnorm(12),
                  phase = rep(c("A", "C1", "C2", "C1"), each = 3))
  pd <- process_scd_phases(d, list(), "changing_criterion")
  expect_equal(pd$time_A, 0:11)
  expect_equal(pd$level_crit1, as.integer(d$time >= 4))
  expect_equal(pd$level_crit3, as.integer(d$time >= 10))
  expect_equal(pd$trend_crit2, pmax(0, d$time - 7))
  expect_equal(pd$time_treatment, pmax(0, d$time - 4))
  crit <- transform(d, phase = rep(c("A", "B"), c(3, 9)),
                    criterion = rep(c(NA, 2, 4, 6), each = 3))
  pc <- process_scd_phases(crit, list(), "changing_criterion")
  expect_equal(as.integer(pc$level_crit3), as.integer(d$time >= 10))
})

test_that("withdrawal labels code the reversal steps by role", {
  d <- data.frame(case = "C1", time = 1:12, outcome = rnorm(12),
                  phase = rep(c("Baseline", "Intervention", "Withdrawal", "Reinstatement"), each = 3))
  pd <- process_scd_phases(d, list(), "reversal")
  steps <- grep("^level_", reversal_terms(names(pd)), value = TRUE)
  expect_length(steps, 3)
  expect_equal(.reversal_onset(names(pd)), steps[1])
  expect_true(.is_withdrawal_term(steps[2]))
  expect_false(.is_withdrawal_term(steps[3]))
})

test_that("phase_role strips occurrence numbers with separators", {
  expect_equal(phase_role(c("Baseline 1", "Treatment 1", "A_1", "B-2", "A.1", "a1", "M",
                            "Generalization 2", 0, 1, 2, 3, "Probe")),
               c(0, 1, 0, 1, 0, 0, 2, 3, 0, 1, 2, 3, NA))
})

test_that("numbered word labels code as a reversal with role-letter steps", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 12), time = rep(1:12, 2), outcome = rnorm(24),
                  phase = rep(rep(c("Baseline 1", "Treatment 1", "Baseline 2", "Treatment 2"),
                                  each = 3), 2))
  expect_equal(detect_design_type(d), "reversal")
  pd <- process_scd_phases(d, list(), "reversal")
  expect_equal(grep("^level_", reversal_terms(names(pd)), value = TRUE),
               c("level_A1B1", "level_B1A2", "level_A2B2"))
})

test_that("numeric phase codes map to role letters", {
  d <- data.frame(case = "C1", time = 1:12, outcome = rnorm(12),
                  phase = rep(c(0, 1, 0, 1), each = 3))
  expect_equal(detect_design_type(d), "reversal")
  pd <- process_scd_phases(d, list(), "reversal")
  expect_equal(reversal_terms(names(pd)),
               c("time_A1", "level_A1B1", "trend_A1B1", "level_B1A2", "trend_B1A2",
                 "level_A2B2", "trend_A2B2"))
})

test_that("A1/B1/A2/B2 plus maintenance gives distinct steps", {
  d <- data.frame(case = "C1", time = 1:15, outcome = rnorm(15),
                  phase = rep(c("A1", "B1", "A2", "B2", "M"), each = 3))
  pd <- process_scd_phases(d, list(), "reversal")
  steps <- grep("^level_", reversal_terms(names(pd)), value = TRUE)
  expect_equal(steps, c("level_A1B1", "level_B1A2", "level_A2B2", "level_B2M1"))
  expect_equal(pd$level_B2M1, as.integer(d$time >= 13))
})

test_that("BAB treatment coefficient is the A to B onset", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 9), time = rep(1:9, 2),
                  phase = rep(rep(c("B", "A", "B"), each = 3), 2), outcome = rnorm(18))
  pd <- process_scd_phases(d, list(), "reversal")
  expect_equal(treatment_coef(pd), "level_A1B2")
  expect_true(.is_withdrawal_term("level_B1A1", names(pd)))
})

test_that("tiers starting in different phases keep every first-run time and warn", {
  d <- rbind(data.frame(case = "C1", time = 1:12, phase = rep(c("A", "B", "A", "B"), each = 3)),
             data.frame(case = "C2", time = 1:9, phase = rep(c("B", "A", "B"), each = 3)))
  d$outcome <- rnorm(nrow(d))
  expect_warning(pd <- process_scd_phases(d, list(), "reversal"), "start in different phases")
  expect_true(all(c("time_A1", "time_B1") %in% reversal_terms(names(pd))))
  expect_equal(pd$time_B1[pd$case == "C1"], rep(0, 12))
  expect_equal(pd$time_B1[pd$case == "C2"], 0:8)
  f <- .build_scd_formula(pd)
  expect_true(all(c("time_A1", "time_B1", "level_A1B1", "level_A1B2") %in%
                  all.vars(stats::as.formula(f))))
})

test_that("process_scd_phases finds a capitalized time column", {
  d <- data.frame(case = "C1", score = 6:1, Session = c(3, 1, 2, 6, 5, 4),
                  phase = c("A", "A", "A", "B", "B", "B"), outcome = rnorm(6))
  pd <- process_scd_phases(d, list(), "AB")
  expect_equal(pd$time_A, d$Session - 1)
  expect_false("time" %in% names(pd))
})

test_that("compute_case_features drops missing outcomes with their times", {
  d <- data.frame(case = rep(c("P1", "P2"), each = 10), time = rep(1:10, 2),
                  phase = rep(rep(c("A", "B"), each = 5), 2),
                  outcome = c(1, NA, 3, 4, 5, 9, 10, NA, 12, 13, 2, 3, 4, 5, 6, 8, 9, 10, 11, NA))
  f <- compute_case_features(d)
  expect_equal(nrow(f), 2)
  expect_equal(f$slope_A[f$case == "P1"], 1)
  expect_equal(f$mean_B[f$case == "P2"], mean(c(8, 9, 10, 11)))
})

test_that("A-B-G tiers get a generalization step without a maintenance step", {
  mk <- function(case, lens) data.frame(case = case, time = seq_len(sum(lens)),
                                        phase = rep(c("A", "B", "G"), lens))
  d <- rbind(mk("P1", c(5, 6, 4)), mk("P2", c(7, 5, 3)), mk("P3", c(9, 4, 3)))
  d$outcome <- 3 + 0.5 * d$time
  pd <- process_scd_phases(d, list(), "multiple_baseline")
  expect_false(any(c("time_M", "level_BM", "trend_BM") %in% names(pd)))
  expect_equal(pd$level_BG, as.integer(pd$phase == "G"))
  fixed <- .scd_fixed_terms(names(pd))
  X <- stats::model.matrix(stats::reformulate(fixed), pd)
  expect_equal(qr(X)$rank, ncol(X))
  b <- qr.coef(qr(X), pd$outcome)
  expect_equal(unname(b[c("level_AB", "trend_AB", "level_BG", "trend_BG")]), rep(0, 4),
               tolerance = 1e-8)
})

test_that("A-B-M-G keeps distinct maintenance and generalization onsets", {
  d <- data.frame(case = "P1", time = 1:16, phase = rep(c("A", "B", "M", "G"), each = 4),
                  outcome = 1:16)
  pd <- process_scd_phases(d, list(), "AB")
  expect_equal(pd$level_BM, rep(c(0, 0, 1, 1), each = 4))
  expect_equal(pd$level_BG, rep(c(0, 0, 0, 1), each = 4))
})

test_that("phase_role reads common abbreviations and leaves probe unassigned", {
  expect_equal(phase_role(c("Tx", "trt", "Treat 2", "BSL", "Wait", "waitlist", "WL", "Probe")),
               c(1, 1, 1, 0, 0, 0, 0, NA))
})

test_that("roles inferred from first appearance raise a warning", {
  d <- data.frame(case = "P1", time = 1:8, phase = rep(c("NoChoice", "Choice"), each = 4),
                  outcome = 1:8)
  expect_warning(pd <- process_scd_phases(d, list(), "AB"), "NoChoice")
  expect_equal(pd$level_AB, rep(0:1, each = 4))
  expect_no_warning(process_scd_phases(transform(d, phase = rep(c("A", "B"), each = 4)),
                                       list(), "AB"))
})

test_that("compute_case_features splits repeated case IDs and behaviors into tiers", {
  one <- function(study, behavior, shift) {
    data.frame(study = study, case = "P1", behavior = behavior, time = 1:10,
               phase = rep(c("A", "B"), c(4 + shift, 6 - shift)),
               outcome = c(rep(1, 4 + shift), rep(5, 6 - shift)) + shift + seq(0, 0.9, 0.1))
  }
  d <- rbind(one("S1", "read", 0), one("S1", "math", 1), one("S2", "read", 2))
  f <- compute_case_features(d)
  expect_equal(nrow(f), 3)
  expect_setequal(f$tier, c("S1 / P1 / read", "S1 / P1 / math", "S2 / P1 / read"))
  expect_true(all(f$case == "P1"))
  expect_equal(f$mean_A[f$tier == "S2 / P1 / read"], mean(3 + seq(0, 0.5, 0.1)))
})

test_that("ATD dummies carry the atd_ prefix and ignore treatment_ data columns", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 18), time = rep(1:18, 2), outcome = rnorm(36),
                  phase = rep(rep(c("Baseline", "Control", "B"), 6), 2))
  pd <- process_scd_phases(d, list(), "alternating")
  expect_setequal(grep("^atd_", names(pd), value = TRUE), c("atd_Control", "atd_B"))
  expect_equal(.scd_fixed_terms(names(pd)), c("time_session", "atd_Control", "atd_B"))
  m <- data.frame(case = rep(c("P1", "P2", "P3"), each = 14), time = rep(1:14, 3),
                  phase = c(rep(c("A", "B"), c(6, 8)), rep(c("A", "B"), c(8, 6)), rep(c("A", "B"), c(10, 4))),
                  outcome = rnorm(42), treatment_fidelity = runif(42))
  pm <- process_scd_phases(m, list(), "multiple_baseline")
  expect_equal(.scd_fixed_terms(names(pm)), c("time_A", "level_AB", "trend_AB"))
  expect_equal(default_random_effects(m), "(1 + level_AB | case)")
})

test_that("a changing-criterion return to baseline adds its own step", {
  d <- data.frame(case = "C1", time = 1:21, outcome = rnorm(21),
                  phase = rep(c("A", "B1", "B2", "A", "B3"), c(5, 4, 4, 4, 4)))
  pd <- process_scd_phases(d, list(), "changing_criterion")
  expect_equal(pd$level_ret1, as.integer(d$time >= 14))
  expect_equal(.scd_fixed_terms(names(pd)), c("time_A", paste0("level_crit", 1:3), "level_ret1"))
})

test_that("a changing-criterion design without baseline drops level_crit1", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 16), time = rep(1:16, 2),
                  phase = rep(rep(c("C1", "C2", "C3", "C4"), each = 4), 2))
  d$outcome <- 3 + 0.5 * d$time
  expect_warning(pd <- process_scd_phases(d, list(), "changing_criterion"), "level_crit1")
  fixed <- .scd_fixed_terms(names(pd))
  expect_false("level_crit1" %in% fixed)
  X <- stats::model.matrix(stats::reformulate(fixed), pd)
  expect_equal(qr(X)$rank, ncol(X))
})

test_that("without a time column sessions are numbered within each tier", {
  d <- data.frame(case = rep(c("P1", "P2"), c(10, 12)), outcome = rnorm(22),
                  phase = c(rep(c("A", "B"), c(4, 6)), rep(c("A", "B"), c(6, 6))))
  expect_warning(pd <- process_scd_phases(d, list(), "multiple_baseline"), "concurrence")
  expect_equal(pd$time, c(1:10, 1:12))
})

test_that("compute_case_features compares the first baseline run with the next intervention run", {
  y <- c(1:6, 21:25, 1:5, 21:25)
  d <- data.frame(case = "P1", time = 1:21, outcome = y, phase = rep(c("A", "B", "A", "B"), c(6, 5, 5, 5)))
  f <- compute_case_features(d)
  expect_equal(f$mean_A, mean(1:6))
  expect_equal(f$mean_B, mean(21:25))
  d$phase <- rep(c("Baseline 1", "Treatment 1", "Baseline 2", "Treatment 2"), c(6, 5, 5, 5))
  expect_equal(compute_case_features(d)[c("mean_A", "mean_B")], f[c("mean_A", "mean_B")])
  bab <- data.frame(case = "P2", time = 1:12, outcome = c(9, 9, 9, 9, 1, 2, 3, 4, 8, 8, 8, 8),
                    phase = rep(c("B", "A", "B"), each = 4))
  expect_equal(compute_case_features(bab)$mean_A, 2.5)
})

mbd_na_phase <- function() {
  d <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(case = paste0("P", i), time = 1:20, outcome = seq_len(20) + i,
               phase = ifelse(1:20 < c(6, 10, 14)[i], "A", "B"), stringsAsFactors = FALSE)
  }))
  d$phase[d$case == "P2" & d$time == 3] <- NA
  d
}

test_that("a missing phase label gets no role and its rows are dropped before coding", {
  d <- mbd_na_phase()
  expect_true(is.na(.phase_numeric(c("A", NA, "B"), 1:3)[2]))
  expect_equal(detect_design_type(d), "multiple_baseline")
  expect_warning(pd <- process_scd_phases(d, list(), "multiple_baseline"),
                 "Dropped 1 row(s) with a missing phase label (tier: P2)", fixed = TRUE)
  expect_equal(nrow(pd), 59)
  p2 <- pd[pd$case == "P2", ]
  expect_equal(unique(p2$intervention_start), 10)
  expect_equal(p2$level_AB, as.integer(p2$time >= 10))
  expect_warning(rv <- process_scd_phases(d, list(), "reversal"), "missing phase label")
  expect_equal(nrow(rv), 59)
})

test_that("an unknown third phase label is named as maintenance in the warning", {
  d <- data.frame(case = "P1", time = 1:15, outcome = 1:15,
                  phase = rep(c("A", "B", "Followup"), each = 5))
  expect_warning(pd <- process_scd_phases(d, list(), "AB"),
                 "'Followup' is treated as maintenance (M)", fixed = TRUE)
  expect_true("level_BM" %in% names(pd))
})

test_that("a missing criterion inside an intervention phase carries the last one forward", {
  d <- data.frame(case = "C1", time = 1:12, phase = rep(c("A", "B"), c(3, 9)), outcome = rnorm(12),
                  criterion = c(NA, NA, NA, 5, NA, 5, 10, NA, 10, 15, 15, NA))
  pd <- process_scd_phases(d, list(), "changing_criterion")
  expect_equal(grep("^level_crit", names(pd), value = TRUE), paste0("level_crit", 1:3))
  expect_equal(pd$criterion_phase, rep(1:4, each = 3))
})

test_that("a changing-criterion tier without a baseline is named in a warning", {
  d <- rbind(data.frame(case = "C1", time = 1:12, phase = rep(c("A", "B"), c(3, 9)),
                        criterion = rep(c(NA, 5, 10, 15), each = 3), outcome = rnorm(12)),
             data.frame(case = "C2", time = 1:9, phase = "B",
                        criterion = rep(c(5, 10, 15), each = 3), outcome = rnorm(9)))
  expect_warning(pd <- process_scd_phases(d, list(), "changing_criterion"),
                 "Tier(s) C2 have no baseline phase", fixed = TRUE)
  expect_true("level_crit1" %in% names(pd))
})

test_that("criterion steps follow local target direction under either goal", {
  d <- data.frame(case = "C1", time = 1:15, phase = rep(c("A", "B"), c(3, 12)), outcome = rnorm(15),
                  criterion = rep(c(NA, 5, 10, 5, 15), each = 3))
  pd <- process_scd_phases(d, list(), "changing_criterion")
  steps <- paste0("level_crit", 1:4)
  expect_equal(vapply(steps, function(v) isTRUE(attr(pd[[v]], "decreasing")), logical(1)),
               stats::setNames(c(FALSE, FALSE, TRUE, FALSE), steps))
  expect_false(any(grepl("^down_", names(pd))))
  expect_equal(.term_goal("b_level_crit3", "increase", data = pd), "decrease")
  expect_equal(.term_goal("b_level_crit3", "decrease", data = pd), "decrease")
  expect_equal(.term_goal("b_level_crit4", "increase", data = pd), "increase")
  expect_equal(.term_goal("trend_crit3", "decrease", data = pd), "decrease")
  expect_equal(.term_goal("trend_crit4", "decrease", data = pd), "increase")
  goal <- .term_goal("level_crit3", "decrease", data = pd)
  summ <- .decision_summary(rep(-2, 10), 1, NULL, goal, 0.95)
  expect_equal(summ$mean, 2)
  expect_equal(summ$prob_meaningful_change, 1)
})

test_that("a probe session during baseline does not start the intervention", {
  ph <- c("Baseline", "Baseline", "Probe", "Baseline", "Baseline",
          "Intervention", "Intervention", "Probe", "Intervention")
  d <- data.frame(case = "c1", time = seq_along(ph), phase = ph, outcome = seq_along(ph))
  out <- suppressWarnings(process_scd_phases(d, list(levels = list(case = "case")), "multiple_probe"))
  expect_equal(out$level_AB, c(0, 0, 0, 0, 0, 1, 1, 1, 1))
  expect_equal(out$trend_AB, c(0, 0, 0, 0, 0, 0, 1, 2, 3))
})

test_that("generalization sessions between intervention sessions are coded as session indicators", {
  ph <- c("A", "A", "A", "B", "B", "G", "B", "B", "G", "B")
  d <- data.frame(case = "c1", time = seq_along(ph), phase = ph, outcome = seq_along(ph))
  out <- suppressWarnings(process_scd_phases(d, list(levels = list(case = "case")), "ab"))
  expect_equal(out$level_AB, c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1))
  expect_equal(out$level_BG, as.integer(ph == "G"))
})

test_that("a final A-B-M-G sequence keeps cumulative maintenance and generalization steps", {
  ph <- c("A", "A", "A", "B", "B", "B", "M", "M", "G", "G")
  d <- data.frame(case = "c1", time = seq_along(ph), phase = ph, outcome = seq_along(ph))
  out <- suppressWarnings(process_scd_phases(d, list(levels = list(case = "case")), "ab"))
  expect_equal(out$level_BM, c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1))
  expect_equal(out$level_BG, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1))
})
