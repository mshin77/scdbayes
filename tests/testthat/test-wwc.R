wwc_sample <- function(file) {
  utils::read.csv(system.file("scdbayes.app", "data", file, package = "scdbayes"))
}

level_pattern <- function(n, base) rep(base + c(0, 1), length.out = n)

reversal_case <- function(lens, case = "P1", levels = c(2, 8, 2, 8)) {
  y <- unlist(Map(level_pattern, lens, levels))
  data.frame(case = case, time = seq_along(y),
             phase = rep(c("A", "B", "A", "B"), lens), outcome = y)
}

mbd_data <- function(base_lens, tx_len = 5, base_fun = function(n) level_pattern(n, 2)) {
  total <- max(base_lens) + tx_len
  do.call(rbind, lapply(seq_along(base_lens), function(k) {
    b <- base_lens[k]
    data.frame(case = paste0("P", k), time = seq_len(b + tx_len),
               phase = rep(c("A", "B"), c(b, tx_len)),
               outcome = c(base_fun(b), level_pattern(tx_len, 8)))
  }))
}

test_that("sample datasets receive the expected screening ratings", {
  expect_equal(wwc_design_check(wwc_sample("reversal_design.csv"))$rating,
               "Meets WWC Standards Without Reservations")
  expect_equal(wwc_design_check(wwc_sample("multiple_baseline_design.csv"))$rating,
               "Meets WWC Standards Without Reservations")
  expect_equal(wwc_design_check(wwc_sample("changing_criterion_design.csv"))$rating,
               "Meets WWC Standards With Reservations")
  expect_equal(wwc_design_check(wwc_sample("alternating_treatment_design.csv"))$rating,
               "Meets WWC Standards Without Reservations")
  ab <- wwc_design_check(wwc_sample("ab_design.csv"))
  expect_equal(ab$rating, "Does Not Meet WWC Standards")
  expect_match(ab$experiments$reasons, "1 distinct time")
  meta <- wwc_design_check(wwc_sample("meta_analysis.csv"))
  expect_equal(nrow(meta$experiments), 4)
  expect_true(all(meta$experiments$rating == "Meets WWC Standards Without Reservations"))
})

test_that("multiple probe sample is screened under the probe rules", {
  mp <- suppressWarnings(wwc_design_check(wwc_sample("multiple_probe_design.csv")))
  expect_equal(mp$design, "multiple_probe")
  expect_equal(mp$rating, "Meets WWC Standards Without Reservations")
  expect_equal(mp$tiers$first_baseline_n, c(6, 6, 7))
})

test_that("reversal baselines of six points pass Without and of three points pass With", {
  without <- wwc_design_check(reversal_case(c(6, 5, 5, 5)))
  expect_equal(without$rating, "Meets WWC Standards Without Reservations")
  expect_equal(wwc_design_check(reversal_case(c(3, 4, 3, 4)))$rating,
               "Meets WWC Standards With Reservations")
  expect_equal(wwc_design_check(reversal_case(c(6, 2, 2, 5)))$rating,
               "Does Not Meet WWC Standards")
  aba <- reversal_case(c(6, 5, 5, 5))[1:16, ]
  expect_equal(wwc_design_check(aba, design = "reversal")$rating, "Does Not Meet WWC Standards")
})

test_that("incomplete reversibility caps a reversal design at With Reservations", {
  d <- reversal_case(c(6, 5, 5, 5), levels = c(2, 8, 7, 8))
  res <- wwc_design_check(d)
  expect_equal(res$rating, "Meets WWC Standards With Reservations")
  expect_match(res$reversibility$status, "not met")
  expect_match(res$experiments$reasons, "reversibility")
})

test_that("a therapeutic baseline trend caps the rating", {
  d <- reversal_case(c(6, 5, 5, 5))
  d$outcome[1:6] <- c(1, 2, 3, 4, 5, 6)
  res <- wwc_design_check(d)
  expect_equal(res$rating, "Meets WWC Standards With Reservations")
  expect_equal(res$baseline_trend$nap, 1)
  expect_equal(wwc_design_check(d, intervention_goal = "decrease")$baseline_trend$nap, 0)
})

test_that("multiple baseline tiers follow the six-phase and data-point rules", {
  expect_equal(wwc_design_check(mbd_data(c(6, 8, 10)))$rating,
               "Meets WWC Standards Without Reservations")
  expect_equal(wwc_design_check(mbd_data(c(4, 6, 8)))$rating,
               "Meets WWC Standards With Reservations")
  two <- wwc_design_check(mbd_data(c(6, 9)))
  expect_equal(two$rating, "Does Not Meet WWC Standards")
  expect_match(two$experiments$reasons, "2 tier")
})

test_that("a three-point zero-variability baseline counts toward Without", {
  d <- mbd_data(c(3, 6, 9), base_fun = function(n) if (n == 3) rep(0, 3) else level_pattern(n, 2))
  res <- wwc_design_check(d)
  expect_equal(res$rating, "Meets WWC Standards Without Reservations")
  expect_equal(res$tiers$zero_variability_phases, c(1, 0, 0))
  expect_match(res$baseline_trend$status[1], "zero variability")
  d$outcome[1:3] <- c(0, 1, 0)
  expect_equal(wwc_design_check(d)$rating, "Meets WWC Standards With Reservations")
})

test_that("a tier without baseline data before the first onset fails concurrence", {
  d <- mbd_data(c(6, 8, 10))
  d <- d[!(d$case == "P3" & d$time < 7), ]
  res <- wwc_design_check(d, design = "multiple_baseline")
  expect_equal(res$rating, "Does Not Meet WWC Standards")
  expect_match(res$experiments$reasons, "before the first intervention onset")
})

test_that("alternating treatments use data-point and contiguity rules", {
  atd <- function(seq) data.frame(case = "P1", time = seq_along(seq), phase = seq,
                                  outcome = ifelse(seq == "A", 2, 8))
  expect_equal(wwc_design_check(atd(rep(c("A", "B"), 5)), design = "alternating")$rating,
               "Meets WWC Standards Without Reservations")
  expect_equal(wwc_design_check(atd(rep(c("A", "B"), 4)), design = "alternating")$rating,
               "Meets WWC Standards With Reservations")
  runs <- c("A", "B", "A", "A", "A", "B", "A", "B", "B", "A", "B", "A", "B")
  expect_equal(wwc_design_check(atd(runs), design = "alternating")$rating,
               "Does Not Meet WWC Standards")
})

test_that("interassessor agreement is checked when supplied", {
  d <- reversal_case(c(6, 5, 5, 5))
  expect_equal(wwc_design_check(d)$ioa$status, "not assessed")
  low <- wwc_design_check(d, ioa_value = 0.70, ioa_coverage = 0.30)
  expect_equal(low$ioa$status, "not met")
  expect_equal(low$rating, "Does Not Meet WWC Standards")
  expect_equal(wwc_design_check(d, ioa_value = 0.65, ioa_coverage = 0.30, ioa_type = "kappa")$ioa$status,
               "met")
  d$ioa <- NA_real_
  d$ioa[c(1, 5, 8, 12, 16, 18)] <- 92
  col <- wwc_design_check(d)
  expect_equal(col$ioa$status, "met")
  expect_equal(col$rating, "Meets WWC Standards Without Reservations")
  d$ioa[c(12, 16)] <- NA
  expect_match(wwc_design_check(d)$ioa$detail, "phase")
})

test_that("IOA coverage given as a percentage is rescaled", {
  d <- reversal_case(c(6, 5, 5, 5))
  res <- wwc_design_check(d, ioa_value = 0.9, ioa_coverage = 15)
  expect_equal(res$ioa$coverage, 0.15)
  expect_equal(res$ioa$status, "not met")
  expect_equal(wwc_design_check(d, ioa_value = 0.9, ioa_coverage = 30)$ioa$status, "met")
})

test_that("the IOA every-phase rule treats an alternating phase as one phase", {
  path <- system.file("scdbayes.app", "data", "alternating_treatment_design.csv",
                      package = "scdbayes")
  skip_if(!nzchar(path), "sample data not installed")
  d <- utils::read.csv(path)
  d$ioa <- ifelse(d$time %in% c(1:3, 10:12), 0.95, NA)
  res <- wwc_design_check(d)
  expect_equal(res$design, "alternating")
  expect_equal(res$ioa$status, "met")
  expect_equal(res$ioa$coverage, 1 / 3)
})

test_that("rows with a missing phase are dropped before the design check", {
  d <- reversal_case(c(6, 5, 5, 5))
  na <- d
  na$phase[9] <- NA
  expect_warning(res <- wwc_design_check(na), "missing phase label")
  expect_equal(res$rating, wwc_design_check(d[-9, ])$rating)
  expect_false(any(grepl("NA", res$tiers$phases)))
})

test_that("a numeric criterion column splits changing-criterion phases", {
  d <- data.frame(case = "P1", time = 1:21, phase = rep(c("A", "B"), c(6, 15)),
                  outcome = c(rep(1, 6), rep(c(3, 5, 7), each = 5)) + rep(c(0, 0.5), length.out = 21))
  d$criterion <- c(rep(NA, 6), rep(c(3, 5, 7), each = 5))
  res <- wwc_design_check(d, design = "changing_criterion")
  expect_equal(res$rating, "Meets WWC Standards Without Reservations")
  expect_equal(nrow(res$tiers), 1)
  expect_equal(res$tiers$n_phases, 4)
})

test_that("an IOA value without coverage leaves coverage unassessed and does not cap", {
  d <- reversal_case(c(6, 5, 5, 5))
  res <- wwc_design_check(d, ioa_value = 0.9)
  expect_equal(res$ioa$status, "partially assessed")
  expect_match(res$ioa$detail, "coverage not assessed")
  expect_equal(res$rating, "Meets WWC Standards Without Reservations")
  expect_equal(wwc_design_check(d, ioa_value = 0.7)$rating, "Does Not Meet WWC Standards")
})

test_that("a recurring baseline label still opens an ATD with a lead baseline", {
  ph <- c(rep("Baseline", 5), rep(c("Baseline", "Treatment"), 6))
  d <- data.frame(case = "P1", time = seq_along(ph), phase = ph,
                  outcome = ifelse(ph == "Baseline", 2, 8))
  res <- wwc_design_check(d, design = "alternating")
  expect_match(res$experiments$reasons, "Treatment(6) Baseline(5)", fixed = TRUE)
  expect_equal(res$rating, "Meets WWC Standards Without Reservations")
})

test_that("baseline sessions skipped on the session grid mark a multiple probe design", {
  d <- mbd_data(c(6, 9, 12))
  d <- d[!(d$case == "P2" & d$time %in% 4:5) & !(d$case == "P3" & d$time %in% c(4, 5, 8)), ]
  expect_equal(wwc_design_check(d)$design, "multiple_probe")
  uneven <- do.call(rbind, lapply(1:3, function(k) {
    t <- seq(1, by = k + 1, length.out = 6 + 2 * k + 5)
    data.frame(case = paste0("P", k), time = t, phase = rep(c("A", "B"), c(6 + 2 * k, 5)),
               outcome = 1)
  }))
  expect_equal(wwc_design_check(uneven)$design, "multiple_baseline")
})

test_that("reasons name phases that pass through zero variability", {
  d <- data.frame(case = "P1", time = 1:12, phase = rep(c("A", "B", "A", "B"), each = 3),
                  outcome = rep(c(0, 9, 0, 9), each = 3))
  res <- wwc_design_check(d)
  expect_equal(res$rating, "Meets WWC Standards Without Reservations")
  expect_match(res$experiments$reasons, "zero variability")
  expect_match(res$experiments$reasons, "A(3)", fixed = TRUE)
})

test_that("one nonconcurrent tier of four makes the design Does Not Meet", {
  d <- mbd_data(c(6, 8, 10, 12))
  d <- d[!(d$case == "P4" & d$time < 7), ]
  res <- wwc_design_check(d, design = "multiple_baseline")
  expect_equal(res$rating, "Does Not Meet WWC Standards")
  expect_match(res$experiments$reasons, "P4 has no baseline data before the first intervention onset")
})

test_that("a lead baseline that does not recur is compared with each alternating condition", {
  ph <- c(rep("A", 5), rep(c("B", "C"), 5))
  d <- data.frame(case = "P1", time = seq_along(ph), phase = ph,
                  outcome = c(rep(2, 5), rep(c(8, 5), 5)))
  res <- wwc_design_check(d, design = "alternating")
  lead <- res$experiments[grepl("initial baseline", res$experiments$experiment), ]
  expect_equal(nrow(lead), 2)
  expect_true(all(lead$rating == "Meets WWC Standards Without Reservations"))
  expect_true(any(res$experiments$experiment == "P1: B vs C"))
  short <- d[-1, ]
  short_lead <- wwc_design_check(short, design = "alternating")$experiments
  expect_true(all(short_lead$rating[grepl("initial baseline", short_lead$experiment)] ==
                    "Meets WWC Standards With Reservations"))
})

test_that("the reversal six-point minimum belongs to the initial baseline", {
  lens <- c(4, 5, 6, 5, 6, 5)
  y <- unlist(Map(level_pattern, lens, c(2, 8, 2, 8, 2, 8)))
  d <- data.frame(case = "P1", time = seq_along(y), outcome = y,
                  phase = rep(c("A", "B", "A", "B", "A", "B"), lens))
  res <- wwc_design_check(d, design = "reversal")
  expect_equal(res$rating, "Meets WWC Standards With Reservations")
})

test_that("a single A-B-M-G case holds one demonstration and does not meet standards", {
  d <- data.frame(case = "P1", time = 1:22, phase = rep(c("A", "B", "M", "G"), c(6, 6, 5, 5)),
                  outcome = c(level_pattern(6, 2), level_pattern(16, 8)))
  res <- wwc_design_check(d)
  expect_equal(res$design, "ab")
  expect_equal(res$rating, "Does Not Meet WWC Standards")
})

test_that("NA baseline outcomes stay under the multiple baseline rules; absent rows read as probes", {
  d <- mbd_data(c(6, 8, 10))
  d$outcome[d$case == "P3" & d$time == 9] <- NA
  expect_equal(wwc_design_check(d)$design, "multiple_baseline")
  absent <- d[!(d$case == "P3" & d$time %in% 4:7), ]
  absent$outcome[is.na(absent$outcome)] <- 2
  expect_equal(wwc_design_check(absent)$design, "multiple_probe")
})

test_that("a reversal design with three tiers takes the higher multiple baseline rating", {
  d <- rbind(reversal_case(c(6, 5, 3, 3)),
             transform(mbd_data(c(8, 10), tx_len = 7)[, c("case", "time", "phase", "outcome")],
                       case = ifelse(case == "P1", "P2", "P3")))
  res <- wwc_design_check(d, design = "reversal")
  expect_equal(res$rating, "Meets WWC Standards Without Reservations")
  expect_match(res$reasons, "multiple baseline across the first", all = FALSE)
})

test_that("missing time values do not break the series order", {
  d <- reversal_case(c(6, 5, 5, 5))
  d$time[3] <- NA
  expect_no_error(wwc_design_check(d))
})

test_that("ioa_value replaces the every-phase IOA column rule", {
  d <- mbd_data(c(6, 8, 10))
  d$ioa <- NA
  d$ioa[d$case != "P2"] <- 0.9
  expect_equal(wwc_design_check(d)$ioa$status, "not met")
  expect_false(identical(wwc_design_check(d, ioa_value = 0.9)$ioa$status, "not met"))
})

test_that("a missing criterion value inside a phase does not split the phase", {
  d <- data.frame(case = "P1", time = 1:21, phase = rep(c("A", "B"), c(6, 15)), outcome = level_pattern(21, 2),
                  criterion = c(rep(NA, 6), rep(c(2, 4, 6), each = 5)))
  d$criterion[8] <- NA
  res <- wwc_design_check(d, design = "changing_criterion")
  expect_equal(res$rating, "Meets WWC Standards Without Reservations")
  expect_equal(res$tiers$n_phases, 4)
  d$ioa <- ifelse(d$time %in% c(1, 7, 12, 17), 0.9, NA)
  expect_false(grepl("without IOA", wwc_design_check(d, design = "changing_criterion")$ioa$detail))
})

test_that("a final run after the alternation does not count toward the conditions", {
  s <- c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "B", "B")
  d <- data.frame(case = "P1", time = seq_along(s), phase = s, outcome = ifelse(s == "A", 2, 8))
  res <- wwc_design_check(d, design = "alternating")
  expect_equal(res$rating, "Meets WWC Standards With Reservations")
  expect_match(res$experiments$reasons, "A(5) B(4)", fixed = TRUE)
})

test_that("a changing criterion design without a baseline does not meet", {
  d <- data.frame(case = "P1", time = 1:20, phase = rep(paste0("B", 1:4), each = 5),
                  outcome = rnorm(20))
  res <- wwc_design_check(d, design = "changing_criterion")
  expect_equal(res$rating, "Does Not Meet WWC Standards")
  expect_match(res$experiments$reasons, "no baseline phase")
})

test_that("IOA phases of an alternating design keep the lead and final runs apart", {
  s <- c(rep("A", 5), rep(c("B", "C"), 5), rep("B", 4))
  d <- data.frame(case = "P1", time = seq_along(s), phase = s,
                  outcome = c(A = 2, B = 8, C = 5)[s])
  d$ioa <- ifelse(seq_len(nrow(d)) %in% c(1, 2, 6:10), 0.9, NA)
  res <- wwc_design_check(d, design = "alternating")
  expect_equal(res$ioa$status, "not met")
  expect_match(res$ioa$detail, "1 phase(s) without IOA data", fixed = TRUE)
})

test_that("ioa_coverage outside 0-1 after scaling is an error", {
  d <- mbd_data(c(6, 8, 10))
  expect_error(wwc_design_check(d, ioa_value = 90, ioa_coverage = 150), "ioa_coverage")
  expect_equal(wwc_design_check(d, ioa_value = 90, ioa_coverage = 25)$ioa$coverage, 0.25)
})

test_that("a maintenance phase is no criterion change in a changing criterion design", {
  d <- data.frame(case = "P1", time = 1:21, phase = rep(c("A", "B1", "B2", "M"), c(6, 5, 5, 5)),
                  outcome = rnorm(21))
  res <- wwc_design_check(d, design = "changing_criterion")
  expect_equal(res$rating, "Does Not Meet WWC Standards")
  expect_match(res$experiments$reasons, "2 phase/criterion change")
})

test_that("a changing criterion return to baseline is checked for reversibility", {
  ccd <- function(return_level) {
    lens <- c(6, 5, 5, 5, 5)
    y <- unlist(Map(level_pattern, lens, c(2, 5, return_level, 7, 9)))
    data.frame(case = "P1", time = seq_along(y), outcome = y,
               phase = rep(c("A", "B1", "A", "B2", "B3"), lens))
  }
  flat <- wwc_design_check(ccd(5), design = "changing_criterion")
  expect_equal(flat$rating, "Meets WWC Standards With Reservations")
  expect_match(flat$reversibility$status, "not met")
  expect_match(flat$experiments$reasons, "reversibility")
  back <- wwc_design_check(ccd(2), design = "changing_criterion")
  expect_equal(back$rating, "Meets WWC Standards Without Reservations")
  expect_equal(back$reversibility$status, "met (NAP <= .85)")
})

test_that("the combination multiple baseline row is compared within its own study", {
  tiers <- function(study, lens_fun) do.call(rbind, lapply(1:3, function(k) {
    lens <- lens_fun(k)
    y <- unlist(Map(level_pattern, lens, c(2, 8, 2, 8)))
    data.frame(study = study, case = paste0(study, "P", k), time = seq_along(y), outcome = y,
               phase = rep(c("A", "B", "A", "B"), lens))
  }))
  d <- rbind(tiers("S1", function(k) c(6, 5, 5, 5)),
             tiers("S2", function(k) c(4 + 2 * k, 5, 2, 2)))
  res <- wwc_design_check(d, design = "reversal")
  row <- res$experiments[grepl("multiple baseline across tiers", res$experiments$experiment), ]
  expect_equal(nrow(row), 1)
  expect_match(row$experiment, "S2")
  expect_equal(row$rating, "Meets WWC Standards Without Reservations")
})

test_that("only a final run of the most successful intervention is exempt", {
  atd <- function(tail_cond) {
    s <- c(rep(c("A", "B"), 5), "A", rep(tail_cond, 3))
    data.frame(case = "P1", time = seq_along(s), phase = s, outcome = ifelse(s == "A", 2, 8))
  }
  expect_equal(wwc_design_check(atd("B"), design = "alternating")$rating,
               "Meets WWC Standards Without Reservations")
  expect_equal(wwc_design_check(atd("A"), design = "alternating")$rating,
               "Does Not Meet WWC Standards")
  expect_equal(wwc_design_check(atd("B"), design = "alternating", intervention_goal = "decrease")$rating,
               "Does Not Meet WWC Standards")
  expect_equal(wwc_design_check(atd("A"), design = "alternating", intervention_goal = "decrease")$rating,
               "Does Not Meet WWC Standards")
  two <- c(rep(c("B", "Tx"), 5), "B", rep("Tx", 3))
  d <- data.frame(case = "P1", time = seq_along(two), phase = two, outcome = ifelse(two == "B", 8, 5))
  expect_equal(wwc_design_check(d, design = "alternating")$rating, "Does Not Meet WWC Standards")
  expect_equal(wwc_design_check(d, design = "alternating", intervention_goal = "decrease")$rating,
               "Meets WWC Standards Without Reservations")
  d$phase[d$phase == "Tx"] <- "C"
  expect_equal(suppressWarnings(wwc_design_check(d, design = "alternating",
                                                 intervention_goal = "decrease"))$rating,
               "Meets WWC Standards Without Reservations")
})

test_that("a lead baseline absent from the alternation enters the final-run ranking", {
  atd <- function(lead_level) {
    s <- c(rep("A", 5), rep(c("B", "Tx"), 5), rep("B", 3))
    data.frame(case = "P1", time = seq_along(s), phase = s,
               outcome = c(A = lead_level, B = 3, Tx = 2)[s])
  }
  pair <- function(res) res$experiments$rating[res$experiments$experiment == "P1: B vs Tx"]
  expect_equal(pair(wwc_design_check(atd(9), design = "alternating")), "Does Not Meet WWC Standards")
  expect_equal(pair(wwc_design_check(atd(1), design = "alternating")),
               "Meets WWC Standards Without Reservations")
})

test_that("a final run that only ties another condition is not exempt", {
  s <- c(rep(c("A", "B"), 5), "A", rep("B", 3))
  d <- data.frame(case = "P1", time = seq_along(s), phase = s, outcome = 5)
  expect_equal(wwc_design_check(d, design = "alternating")$rating, "Does Not Meet WWC Standards")
  d$outcome <- ifelse(s == "A", 0.3, 0.1 + 0.2)
  expect_equal(wwc_design_check(d, design = "alternating")$rating, "Does Not Meet WWC Standards")
})

test_that("a final run of an unrecognised label is not exempt", {
  s <- c(rep(c("B", "Ctl"), 5), "B", rep("Ctl", 3))
  d <- data.frame(case = "P1", time = seq_along(s), phase = s, outcome = ifelse(s == "Ctl", 8, 2))
  res <- suppressWarnings(wwc_design_check(d, design = "alternating"))
  expect_equal(res$rating, "Does Not Meet WWC Standards")
  expect_match(res$experiments$reasons, "more than two consecutive points")
})

test_that("a trailing maintenance note survives the fewer-than-two-conditions return", {
  s <- c(rep("A", 5), rep("B", 3), rep("M", 3))
  d <- data.frame(case = "P1", time = seq_along(s), phase = s, outcome = c(A = 2, B = 8, M = 8)[s])
  res <- wwc_design_check(d, design = "alternating")
  expect_equal(res$rating, "Does Not Meet WWC Standards")
  expect_match(res$experiments$reasons,
               "fewer than two alternating conditions; final M phase outside the alternating comparison",
               fixed = TRUE)
})

test_that("a final intervention run worse than baseline is not exempt", {
  s <- c(rep(c("A", "B"), 5), "A", rep("B", 3))
  d <- data.frame(case = "P1", time = seq_along(s), phase = s, outcome = ifelse(s == "A", 8, 2))
  res <- wwc_design_check(d, design = "alternating")
  expect_equal(res$rating, "Does Not Meet WWC Standards")
  expect_match(res$experiments$reasons, "more than two consecutive points")
})

test_that("a trailing maintenance phase stays outside the alternating comparison", {
  s <- c(rep(c("A", "B"), 5), rep("Maintenance", 4))
  d <- data.frame(case = "P1", time = seq_along(s), phase = s,
                  outcome = c(A = 2, B = 8, Maintenance = 8)[s])
  res <- wwc_design_check(d, design = "alternating")
  expect_equal(nrow(res$experiments), 1)
  expect_equal(res$rating, "Meets WWC Standards Without Reservations")
  expect_match(res$experiments$reasons, "final Maintenance phase outside the alternating comparison")
  d$ioa <- ifelse(d$time %in% c(1, 2), 0.9, NA)
  expect_match(wwc_design_check(d, design = "alternating")$ioa$detail,
               "1 phase(s) without IOA data", fixed = TRUE)
})

test_that("the baseline-to-intervention change counts toward the three criterion changes", {
  d <- data.frame(case = "P1", time = 1:21, phase = rep(c("A", "B1", "B2", "B3"), c(6, 5, 5, 5)),
                  outcome = c(level_pattern(6, 2), level_pattern(5, 4), level_pattern(5, 6),
                              level_pattern(5, 8)))
  expect_equal(wwc_design_check(d, design = "changing_criterion")$rating,
               "Meets WWC Standards Without Reservations")
})
