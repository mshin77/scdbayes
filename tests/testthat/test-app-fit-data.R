app_fit_helpers <- new.env(parent = globalenv())
helper_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "R", "fit-data.R")
if (!file.exists(helper_path)) helper_path <- system.file("scdbayes.app/R/fit-data.R", package = "scdbayes")
if (file.exists(helper_path)) sys.source(helper_path, app_fit_helpers)

test_that("selected observations define moderator constants and Beta squeeze", {
  d <- data.frame(outcome = c(0, 1, 0.5, 0.7), x = c(2, 4, 100, 200),
                  measure = c("target", "target", "other", "other"))
  prep <- app_fit_helpers$prepare_fit_data(d, "outcome", "beta", "x", TRUE, TRUE,
                                          subset_var = "measure", subset_values = "target")
  expect_equal(prep$data$x, c(-1, 1) / sqrt(2))
  expect_equal(prep$data$outcome, c(0.25, 0.75))
  expect_equal(prep$moderators$x$center, 3)
  expect_equal(prep$beta_n, 2)
  replay <- new.env(parent = baseenv())
  replay$data <- d[d$measure == "target", ]
  eval(parse(text = app_fit_helpers$fit_transform_code(prep)), replay)
  expect_equal(replay$data, prep$data)
})

test_that("count conversion and categorical moderators share preparation", {
  d <- data.frame(outcome = c(0.25, 0.75), exposure = c(20, 20),
                  x = c(4, 4), group = c("a", "b"))
  prep <- app_fit_helpers$prepare_fit_data(d, "outcome", "poisson", c("x", "group"),
                                          TRUE, TRUE, "exposure", outcome_units = "rate")
  expect_equal(prep$data$outcome, c(5, 15))
  expect_equal(prep$data$x, c(0, 0))
  expect_identical(prep$data$group, d$group)
  replay <- new.env(parent = baseenv())
  replay$data <- d
  eval(parse(text = app_fit_helpers$fit_transform_code(prep)), replay)
  expect_equal(replay$data, prep$data)
})

test_that("explicit count units distinguish integer rates from counts", {
  d <- data.frame(outcome = c(2, 3), exposure = c(10, 20))
  counts <- app_fit_helpers$prepare_fit_data(d, "outcome", "poisson", exposure = "exposure", outcome_units = "count")
  rates <- app_fit_helpers$prepare_fit_data(d, "outcome", "poisson", exposure = "exposure", outcome_units = "rate")
  expect_equal(counts$data$outcome, c(2, 3))
  expect_equal(rates$data$outcome, c(20, 60))
  expect_error(app_fit_helpers$prepare_fit_data(d, "outcome", "poisson", outcome_units = "rate"), "exposure")
  d$outcome <- c(0.5, 1.5)
  expect_error(app_fit_helpers$prepare_fit_data(d, "outcome", "poisson", outcome_units = "count"), "integer")
})

test_that("forest scale states reflected native ratios and response contrasts", {
  expect_match(app_fit_helpers$forest_scale_text("link", "log", "decrease"), "exp\\(-estimate\\)")
  expect_match(app_fit_helpers$forest_scale_text("link", "logit", "increase"), "exp\\(estimate\\)")
  expect_identical(app_fit_helpers$forest_scale_text("response", "log", "decrease"),
                   "Response-scale benefit difference; positive = benefit")
})

test_that("meta previews retain elicited means and the fitted sigma formula", {
  skip_if_not_installed("brms")
  server_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "server.R")
  if (!file.exists(server_path)) server_path <- system.file("scdbayes.app/server.R", package = "scdbayes")
  expressions <- parse(server_path)
  server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("shinyServer")), expressions)[[1]][[2]]
  assignments <- as.list(server[[3]])[-1]
  env <- new.env(parent = asNamespace("scdbayes"))
  ids <- c(".meta_family", ".input_value", ".fit_in", ".scd_fit_priors")
  selected <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
                       is.symbol(x[[2]]) && as.character(x[[2]]) %in% ids, assignments)
  invisible(lapply(selected, eval, envir = env))
  env$input <- list(meta_outcome_type = "continuous", meta_prior_type = "informative",
                    meta_prior_effect_mean = -2, meta_heterogeneous_variance = TRUE)
  env$values <- list(meta_fit_pending = TRUE, model_structure = list(n_levels = 2))
  env$.exposure_in <- function(meta) NULL
  env$.adaptive_priors <- function(data, structure, model_type, outcome_var, exposure = NULL)
    generate_adaptive_priors(data, structure, model_type, outcome_var = outcome_var)
  d <- data.frame(outcome = rep(1:8, 3), case = rep(c("a", "b", "c"), each = 8),
                  time = rep(1:8, 3), phase = rep(rep(c("A", "B"), each = 4), 3))
  d <- process_scd_phases(d, list(), "AB")
  f <- "outcome ~ time_A + level_AB + trend_AB + (1 + level_AB | case)"
  fitted <- env$.scd_fit_priors(d, f, "gaussian", "outcome")
  env$values$meta_fit_pending <- FALSE
  preview <- env$.scd_fit_priors(d, f, "gaussian", "outcome", TRUE)
  expect_equal(preview$brms_priors, fitted$brms_priors)
  expect_equal(preview$formula, fitted$formula)
  expect_match(preview$prior_list$treatment$string, "normal(-2", fixed = TRUE)
  code <- brms::make_stancode(preview$formula, data = d, family = gaussian(), prior = preview$brms_priors)
  expect_match(code, "sigma")
  d$outcome <- rep(c(0, .2, .3, .4, .6, .7, .8, 1), 3)
  prepared <- app_fit_helpers$prepare_fit_data(d, "outcome", "beta")
  stan_data <- brms::make_standata(as.formula(f), data = prepared$data, family = brms::Beta())
  expect_true(all(stan_data$Y > 0 & stan_data$Y < 1))
})

test_that("study summaries orient inverse links without indexing role vectors", {
  server_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "server.R")
  if (!file.exists(server_path)) server_path <- system.file("scdbayes.app/server.R", package = "scdbayes")
  expressions <- parse(server_path)
  server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("shinyServer")), expressions)[[1]][[2]]
  assignments <- as.list(server[[3]])[-1]
  selected <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name(".per_study_effects")), assignments)
  env <- new.env(parent = asNamespace("scdbayes"))
  invisible(lapply(selected, eval, envir = env))
  env$.pick_treatment_term <- function(terms) "level_AB"
  env$.meta_goal <- function() "increase"
  env$input <- list(meta_ci_level = 95)
  re <- array(0, c(5, 2, 1), dimnames = list(NULL, c("S1", "S2"), "level_AB"))
  local_mocked_bindings(ranef = function(...) list(study = re),
      fixef = function(...) matrix(-0.5, 5, 1, dimnames = list(NULL, "level_AB")), .package = "brms")
  model <- list(data = data.frame(study = c("S1", "S2")),
                family = brms::brmsfamily("Gamma", link = "inverse"))
  expect_equal(env$.per_study_effects(model)$df$est, c(0.5, 0.5))
})
