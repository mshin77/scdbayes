make_design <- function(scd_type, n_cases = 2, phases) {
  per <- length(phases) * 5
  d <- data.frame(
    case = rep(paste0("C", seq_len(n_cases)), each = per),
    phase = rep(rep(phases, each = 5), n_cases),
    outcome = stats::rnorm(per * n_cases),
    time = rep(seq_len(per), n_cases)
  )
  process_scd_phases(d, structure = list(), scd_type = scd_type)
}

test_that(".build_scd_formula references only columns the design produced", {
  cases <- list(
    list(t = "AB", ph = c("A", "B"), n = 4),
    list(t = "reversal", ph = c("A", "B", "A", "B"), n = 2),
    list(t = "changing_criterion", ph = c("A", "B", "C"), n = 2),
    list(t = "multiple_baseline", ph = c("A", "B"), n = 4)
  )
  for (cs in cases) {
    d <- make_design(cs$t, cs$n, cs$ph)
    f <- .build_scd_formula(d)
    rhs <- setdiff(all.vars(stats::as.formula(f)), "outcome")
    expect_true(all(rhs %in% names(d)),
                info = sprintf("%s: missing %s", cs$t,
                               paste(setdiff(rhs, names(d)), collapse = ",")))
  }
})

test_that(".build_scd_formula fits a random treatment slope from two level-2 units on", {
  d <- make_design("AB", 8, c("A", "B"))
  expect_match(.build_scd_formula(d), "(1 + level_AB | case)", fixed = TRUE)
  one <- make_design("AB", 1, c("A", "B"))
  expect_equal(.build_scd_formula(one), "outcome ~ time_A + level_AB + trend_AB")
  expect_true(all(c("time_A", "level_AB", "trend_AB") %in%
                  all.vars(stats::as.formula(.build_scd_formula(d)))))
})

test_that(".build_scd_formula picks design-specific changepoint terms", {
  rev <- make_design("reversal", 2, c("A", "B", "A", "B"))
  expect_match(.build_scd_formula(rev), "level_A1B1")
  expect_match(.build_scd_formula(rev), "(1 + level_A1B1 | case)", fixed = TRUE)
  cc <- make_design("changing_criterion", 2, c("A", "B", "C"))
  expect_match(.build_scd_formula(cc), "level_crit")
  expect_false(grepl("trend_crit", .build_scd_formula(cc)))
})

test_that("every level step varies across cases; later steps are uncorrelated", {
  rev <- make_design("reversal", 3, c("A", "B", "A", "B"))
  f <- .build_scd_formula(rev)
  expect_match(f, "(1 + level_A1B1 | case) + (0 + level_B1A2 + level_A2B2 || case)", fixed = TRUE)
  m <- make_design("multiple_baseline", 3, c("A", "B", "M"))
  expect_match(.build_scd_formula(m), "(0 + level_BM || case)", fixed = TRUE)
  expect_true(has_random_slope(f))
})

test_that("alternating-treatment formula has dummies and a centered session time", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 12), time = rep(1:12, 2), outcome = rnorm(24),
                  phase = rep(c(rep("A", 4), rep(c("X", "Y"), 4)), 2))
  pd <- process_scd_phases(d, list(), "alternating")
  expect_equal(pd$time_session, rep(0:11, 2))
  f <- .build_scd_formula(pd)
  expect_match(f, "^outcome ~ time_session \\+ atd_X \\+ atd_Y \\+")
  expect_match(f, "(1 | case) + (0 + atd_X + atd_Y || case)", fixed = TRUE)
})

test_that("a pure session trend in an ATD loads on time_session, not the dummies", {
  d <- data.frame(case = rep(c("C1", "C2", "C3"), each = 18), time = rep(1:18, 3),
                  phase = c(rep("A", 6), rep(c("X", "Y"), 6), rep("A", 6), rep(c("Y", "X"), 6),
                            rep("A", 4), rep(c("X", "X", "Y", "Y"), 3), "X", "Y"))
  d$outcome <- 2 + 0.5 * d$time
  pd <- process_scd_phases(d, list(), "alternating")
  fixed <- .scd_fixed_terms(names(pd))
  X <- stats::model.matrix(stats::reformulate(fixed), pd)
  expect_equal(qr(X)$rank, ncol(X))
  b <- qr.coef(qr(X), pd$outcome)
  expect_equal(unname(b[c("atd_X", "atd_Y")]), c(0, 0), tolerance = 1e-8)
  expect_equal(unname(b["time_session"]), 0.5, tolerance = 1e-8)
  no_time <- qr.coef(qr(X[, colnames(X) != "time_session"]), pd$outcome)
  expect_gt(max(abs(no_time[c("atd_X", "atd_Y")])), 1)
})

test_that("repeated case IDs across studies are separate level-2 units", {
  d <- do.call(rbind, lapply(c("S1", "S2"), function(s) {
    data.frame(study = s, case = "C1", time = 1:10,
               phase = rep(c("A", "B"), c(if (s == "S1") 4 else 6, if (s == "S1") 6 else 4)),
               outcome = rnorm(10))
  }))
  pd <- process_scd_phases(d, list(), "multiple_baseline")
  expect_equal(.n_units(pd), 2L)
  f <- .build_scd_formula(pd)
  expect_match(f, "(1 + level_AB | study:case)", fixed = TRUE)
})

test_that("binomial family uses a trials column and errors without one", {
  d <- make_design("AB", 3, c("A", "B"))
  d$outcome <- rbinom(nrow(d), 10, 0.4)
  d$trials <- 10
  expect_match(.build_scd_formula(d, "binomial"), "^outcome \\| trials\\(trials\\) ~ time_A")
  d$trials <- NULL
  expect_error(.build_scd_formula(d, "binomial"), "trials column")
})

test_that(".build_scd_formula adds maintenance and generalization terms when present", {
  d <- make_design("multiple_baseline", 4, c("A", "B", "M"))
  expect_true(all(c("level_BM", "trend_BM") %in% names(d)))
  full <- all.vars(stats::as.formula(.build_scd_formula(d)))
  expect_true(all(c("level_BM", "trend_BM") %in% full))
  ab <- make_design("AB", 4, c("A", "B"))
  expect_false(grepl("_BM", .build_scd_formula(ab)))
})

test_that("model cache key changes with sampler settings", {
  skip_if_not_installed("digest")
  d <- simulate_scd_data(n_cases = 2)
  key <- function(iter) .model_cache_file("outcome ~ phase_num", NULL, d, "gaussian",
                                          sampler = list(iter = iter, chains = 2))
  expect_false(identical(key(1000), key(2000)))
  expect_identical(key(1000), key(1000))
})

test_that("build_model keys the cache on seed, init, save_pars, sample_prior, and backend", {
  skip_if_not_installed("digest")
  d <- simulate_scd_data(n_cases = 2)
  files <- character(0)
  local_mocked_bindings(brm = function(...) {
    files <<- c(files, list(...)$file)
    stop("stop after capture")
  })
  fit <- function(...) suppressWarnings(suppressMessages(
    tryCatch(build_model(d, "outcome ~ phase_num", iter = 10, chains = 1, ...),
             error = function(e) NULL)))
  withr::with_options(list(scdbayes.backend = "rstan"), {
    fit(seed = 1)
    fit(seed = 2)
    fit(seed = 1, init = 0)
    fit(seed = 1, save_pars = brms::save_pars(all = TRUE))
    fit(seed = 1, sample_prior = "yes")
  })
  withr::with_options(list(scdbayes.backend = "cmdstanr"), fit(seed = 1))
  expect_length(files, 6)
  expect_length(unique(files), 6)
})

test_that("model cache keeps only the newest files", {
  dir <- file.path(tempdir(), "scdbayes-prune-test")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  files <- file.path(dir, sprintf("scdbayes-model-%02d.rds", 1:5))
  invisible(lapply(files, function(f) saveRDS(1, f)))
  Sys.setFileTime(files, Sys.time() - (5:1) * 60)
  removed <- .prune_model_cache(dir, keep = 2)
  expect_setequal(basename(removed), basename(files[1:3]))
  expect_setequal(list.files(dir), basename(files[4:5]))
})

test_that(".stan_model_args sets stanc options for cmdstanr only", {
  expect_equal(.stan_model_args("rstan"), list())
  expect_true("stanc_options" %in% names(.stan_model_args("cmdstanr")))
})

test_that("ar() warns on unequal spacing and missing outcomes, not on regular series", {
  f <- "outcome ~ level_AB + ar(time = time, gr = case, p = 1)"
  d <- data.frame(case = rep(c("C1", "C2"), each = 6), time = rep(1:6, 2), outcome = rnorm(12))
  expect_silent(.ar_spacing_warning(d, f))
  gaps <- transform(d, time = rep(c(1, 2, 3, 5, 6, 7), 2))
  expect_warning(.ar_spacing_warning(gaps, f), "not constant")
  miss <- d
  miss$outcome[3] <- NA
  expect_warning(.ar_spacing_warning(miss, f), "missing outcomes")
  expect_silent(.ar_spacing_warning(gaps, "outcome ~ level_AB"))
})

test_that("run_scd_analysis detects the design and warns about omitted slopes at small N", {
  captured <- NULL
  local_mocked_bindings(build_model = function(data, formula_str, ...) {
    captured <<- formula_str
    NULL
  })
  d <- data.frame(case = rep(c("C1", "C2", "C3"), each = 12), time = rep(1:12, 3),
                  phase = rep(rep(c("A", "B", "A", "B"), each = 3), 3), outcome = rnorm(36))
  expect_warning(run_scd_analysis(d), "random treatment slope")
  expect_match(captured, "level_A1B1")
  expect_match(captured, "level_B1A2")
  d$trials <- 10
  d$outcome <- rbinom(nrow(d), 10, 0.5)
  suppressWarnings(run_scd_analysis(d, list(family = "binomial")))
  expect_match(captured, "^outcome \\| trials\\(trials\\) ~")
  d$outcome <- d$outcome / 10
  d$outcome[1] <- 0
  expect_error(suppressWarnings(run_scd_analysis(d, list(family = "beta"))),
               "Squeeze them with", fixed = TRUE)
})

test_that("prior types share every prior except the treatment prior", {
  captured <- list()
  local_mocked_bindings(build_model = function(data, formula_str, priors = NULL, ...) {
    captured[[length(captured) + 1]] <<- priors
    NULL
  })
  set.seed(2)
  d <- simulate_scd_data(n_cases = 4)
  types <- c("default", "informative", "sensitivity")
  invisible(lapply(types, function(t) suppressWarnings(run_scd_analysis(d, list(prior_type = t)))))
  rows <- lapply(captured, function(p) {
    p <- as.data.frame(p)
    out <- p[p$class != "b", c("prior", "class", "coef")]
    rownames(out) <- NULL
    out
  })
  expect_length(captured, 3)
  expect_true(all(vapply(captured, function(p) "sd" %in% p$class, logical(1))))
  expect_equal(rows[[2]], rows[[1]])
  expect_equal(rows[[3]], rows[[1]])
})

test_that("ar spacing check keys series by the full gr interaction", {
  d <- expand.grid(time = 1:10, case = c("c1", "c2"), study = c("s1", "s2"))
  d$outcome <- stats::rnorm(nrow(d))
  expect_no_warning(.ar_spacing_warning(d, "outcome ~ 1 + ar(time = time, gr = study:case)"))
  expect_warning(.ar_spacing_warning(d, "outcome ~ 1 + ar(time = time, gr = case)"),
                 "repeat within an ar")
  expect_no_warning(.ar_spacing_warning(d, "outcome ~ 1 + ar(time, study:case, p = 1)"))
  gap <- d[!(d$study == "s1" & d$case == "c1" & d$time == 5), ]
  expect_warning(.ar_spacing_warning(gap, "outcome ~ 1 + ar(time = time, gr = study:case)"),
                 "not constant")
})

test_that("power-simulation ar() term groups by the study:case tier with studies", {
  f <- paste(deparse(.build_power_formula(n_cases = 3, n_studies = 2)), collapse = " ")
  expect_match(f, "gr = study:case", fixed = TRUE)
  f1 <- paste(deparse(.build_power_formula(n_cases = 3, n_studies = 1)), collapse = " ")
  expect_match(f1, "gr = case", fixed = TRUE)
})

exposure_data <- function() {
  d <- process_scd_phases(simulate_scd_data(n_cases = 3), list(), "AB")
  d$outcome <- stats::rpois(nrow(d), 4)
  d$minutes <- rep(c(10, 20), length.out = nrow(d))
  d
}

test_that("add_exposure_offset appends a log offset for count families", {
  d <- exposure_data()
  f <- "outcome ~ time_A + level_AB + trend_AB + (1 | case)"
  out <- add_exposure_offset(f, d, "minutes", "poisson")
  expect_equal(out, paste(f, "+ offset(log(minutes))"))
  expect_equal(add_exposure_offset(out, d, "minutes", "negbinomial"), out)
  expect_equal(add_exposure_offset(f, d, NULL, "poisson"), f)
  expect_true(is_safe_formula(out, names(d)))
  expect_error(add_exposure_offset(f, d, "minutes", "gaussian"), "families only")
  expect_error(add_exposure_offset(f, d, "nope", "poisson"), "one column")
  expect_error(add_exposure_offset(f, d, c("minutes", "time"), "poisson"), "single column name")
  expect_error(add_exposure_offset(f, d, "minutes", NULL), "families only")
  d$sessions <- 2
  expect_error(add_exposure_offset(out, d, "sessions", "poisson"), "one exposure offset only")
  expect_equal(add_exposure_offset(f, d, "minutes", "zero_inflated_poisson"), out)
  expect_equal(add_exposure_offset(f, d, "minutes", "zero_inflated_negbinomial"), out)
  d$minutes[2] <- 0
  expect_error(add_exposure_offset(f, d, "minutes", "poisson"), "> 0")
  d$minutes[2] <- NA
  expect_error(add_exposure_offset(f, d, "minutes", "poisson"), "every row")
})

test_that("is_safe_formula accepts only offset(log(<column>))", {
  vars <- c("outcome", "level_AB", "minutes")
  expect_true(is_safe_formula("outcome ~ level_AB + offset(log(minutes))", vars))
  expect_false(is_safe_formula("outcome ~ level_AB + offset(minutes)", vars))
  expect_false(is_safe_formula("outcome ~ level_AB + offset(exp(minutes))", vars))
  expect_false(is_safe_formula("outcome ~ level_AB + offset(log(other))", vars))
  expect_false(is_safe_formula("outcome ~ level_AB + log(minutes)", vars))
})

test_that("an exposure offset centers the count intercept on the rate per exposure unit", {
  d <- exposure_data()
  plain <- generate_adaptive_priors(d, list(), model_type = "poisson")
  rate <- generate_adaptive_priors(d, list(), model_type = "poisson", exposure = "minutes")
  expect_equal(plain$intercept$location, log(mean(d$outcome)))
  expect_equal(rate$intercept$location, log(sum(d$outcome) / sum(d$minutes)))
})

test_that("a poisson formula with an exposure offset passes brms prior validation", {
  d <- exposure_data()
  f <- add_exposure_offset("outcome ~ time_A + level_AB + trend_AB + (1 | case)", d,
                           "minutes", "poisson")
  vp <- brms::validate_prior(brms::prior_string("normal(0, 2.5)", class = "b"),
                             stats::as.formula(f), data = d, family = stats::poisson())
  expect_true("b" %in% vp$class)
  expect_false(any(grepl("minutes", vp$coef)))
})
