params <- c("b_Intercept", "b_level_AB", "b_trend_AB", "b_level_AB:mod", "sd_case__Intercept")
cols <- c("y", "level_AB", "trend_AB", "case", "study", "time", "n", "mod")

test_that("is_safe_hypothesis accepts parameter arithmetic", {
  expect_true(is_safe_hypothesis("level_AB > 0", params))
  expect_true(is_safe_hypothesis("b_level_AB + b_trend_AB = 0", params))
  expect_true(is_safe_hypothesis("level_AB:mod = 0", params))
  expect_true(is_safe_hypothesis("-2 * (level_AB + trend_AB) < 1.5", params))
  expect_true(is_safe_hypothesis("level_AB^2 == 0", params))
})

test_that("is_safe_hypothesis rejects function calls and code", {
  bad <- c("b_level_AB*0 + q() = 0",
           "b_level_AB*0 + nchar(Sys.getenv()) = 0",
           "b_level_AB*0 + Sys.getenv('GEMINI_API_KEY') = 0",
           "level_AB + system('id') = 0",
           "`q()` = 0", "level_AB = 0; q()", "level_AB <= 0", "level_AB",
           "base::q() = 0", "x$y = 0", "unknown_par = 0", "'a' = 0",
           "eval(parse(text = 'q()')) = 0", "get('q')() = 0")
  expect_false(any(vapply(bad, is_safe_hypothesis, logical(1), params = params)))
  expect_false(is_safe_hypothesis(NA_character_, params))
  expect_false(is_safe_hypothesis("level_AB > 0", character(0)))
})

test_that("a rejected hypothesis never reaches brms::hypothesis()", {
  hit <- new.env()
  hit$n <- 0
  withr::local_options(list(scdbayes_test_env = hit))
  h <- "b_level_AB*0 + (function() { assign('n', 1, envir = getOption('scdbayes_test_env')); 0 })() = 0"
  run_payload <- function(x) eval(str2lang(sub("=[^=]*$", "", x)), list(b_level_AB = 1))
  run_payload(h)
  expect_equal(hit$n, 1)
  hit$n <- 0
  calls <- character(0)
  local_mocked_bindings(hypothesis = function(x, hypothesis, ...) {
    calls <<- c(calls, hypothesis)
    run_payload(hypothesis)
  }, variables = function(x, ...) params, .package = "brms")
  # the app's own guard, taken from server.R and run against the package check
  src <- readLines(system.file("scdbayes.app", "server.R", package = "scdbayes"))
  start <- grep("^\\s*\\.hypothesis_ok <- function", src)[1]
  end <- Find(function(k) !inherits(try(parse(text = src[start:k]), silent = TRUE), "try-error"),
              seq(start, start + 50))
  env <- new.env()
  env$showNotification <- function(...) invisible(NULL)
  eval(parse(text = src[start:end]), envir = env)
  guarded <- function(x) if (env$.hypothesis_ok(NULL, x)) brms::hypothesis(NULL, x)
  guarded(h)
  expect_length(calls, 0)
  expect_equal(hit$n, 0)
  guarded("b_level_AB > 0")
  expect_equal(calls, "b_level_AB > 0")
})

test_that("is_safe_formula accepts app formula shapes", {
  expect_true(is_safe_formula(paste("y ~ level_AB + trend_AB + mod + level_AB:mod +",
    "(1 + level_AB | case) + ar(time = time, gr = study:case, cov = TRUE)"), cols))
  expect_true(is_safe_formula("y | trials(n) ~ 0 + level_AB + (1 | study/case)", cols))
  expect_true(is_safe_formula("y ~ 1 + (1 + level_AB || case)", cols))
  expect_true(is_safe_formula("y ~ level_AB * (mod) - 1", cols))
  expect_true(is_safe_formula("level_AB + (1 | gr(case, by = study))", cols))
})

test_that("is_safe_formula rejects calls, strings, and accessors", {
  bad <- c("y ~ level_AB + I(q())", "y ~ Sys.getenv()", "y ~ x$y", "y ~ x@y",
           "y ~ stats::lag(level_AB)", "y ~ log(level_AB)", "y ~ level_AB + 'a'",
           "y ~ ar(time = q())", "y ~ ar(time, case)", "y ~ level_AB + 2",
           "y ~ unknown_col", "y ~ level_AB; q()", "y ~ `I(q())`")
  expect_false(any(vapply(bad, is_safe_formula, logical(1), allowed_vars = cols)))
})

crafted_data <- function(marker) {
  set.seed(1)
  mk <- function(id) data.frame(case = id, time = 1:20, phase = rep(c("A", "B"), each = 10),
                                outcome = rnorm(20) + rep(c(0, 2), each = 10))
  d <- rbind(mk("C1"), mk("C2"), mk("C3"))
  d$atd_x <- rep(c(0, 1), each = 10)
  d[[sprintf("atd_x+I(file.create('%s'))", marker)]] <- 0
  d
}

test_that("safe_column_names renames crafted names to syntactic ones", {
  marker <- withr::local_tempfile()
  d <- safe_column_names(crafted_data(marker))
  expect_true(all(grepl("^[A-Za-z.][A-Za-z0-9._]*$", names(d))))
  expect_true(all(make.names(names(d)) == names(d)))
  expect_false(anyDuplicated(names(d)) > 0)
  expect_length(attr(d, "renamed"), 1)
  expect_match(names(attr(d, "renamed")), "file.create", fixed = TRUE)
  odd <- safe_column_names(stats::setNames(data.frame(1, 2, 3, 4, 5),
                                           c(" a b ", "1x", "été", "if", "a b")))
  expect_true(all(grepl("^[A-Za-z.][A-Za-z0-9._]*$", names(odd))))
  expect_true(all(make.names(names(odd)) == names(odd)))
})

test_that("the reserved names ... and ..1 are renamed and rejected", {
  d <- safe_column_names(stats::setNames(data.frame(1, 2, 3), c("...", "..1", "..x")))
  expect_equal(names(d), c("X...", "X..1", "..x"))
  expect_equal(.syntactic(c("...", "..2", "..x", "a")), c("..x", "a"))
  expect_error(.check_syntactic("..1"), "syntactic")
})

test_that("a crafted column name never runs code through standardize_by_case", {
  marker <- withr::local_tempfile()
  d <- crafted_data(marker)
  evil <- grep("file.create", names(d), fixed = TRUE, value = TRUE)
  expect_false(evil %in% .scd_fixed_terms(names(d)))
  expect_false(evil %in% .step_columns(names(d)))
  out <- suppressWarnings(standardize_by_case(d, outcome_var = "outcome"))
  expect_true("outcome_std" %in% names(out))
  expect_error(standardize_by_case(d, formula_fixed = paste("~", evil), outcome_var = "outcome"),
               "syntactic")
  expect_error(standardize_by_case(d, formula_fixed = "~ level_AB + I(file.create('x'))"),
               "syntactic")
  expect_false(file.exists(marker))
})

test_that("formula builders refuse non-syntactic column names", {
  marker <- withr::local_tempfile()
  evil <- sprintf("I(file.create('%s'))", marker)
  expect_error(build_comparison_models(evil, "phase", "case", "time"), "syntactic")
  d <- crafted_data(marker)
  expect_error(run_model_comparison(d, list(m = paste(evil, "~ phase + (1 | case)"))),
               "Comparison formulas")
  expect_false(file.exists(marker))
})

test_that("standardize_by_case reports a failed regression in the status", {
  d <- data.frame(case = rep(c("C1", "C2"), each = 6), y = rnorm(12),
                  x = c(rep(NA_real_, 6), rnorm(6)))
  tab <- attr(suppressWarnings(standardize_by_case(d, "~ x", outcome_var = "y", tiers = "case")),
              "case_sigma")
  expect_match(tab$status[1], "^regression failed")
  expect_equal(tab$status[2], "ok")
})
