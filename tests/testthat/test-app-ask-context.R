test_that("Ask AI context describes the selected completed fit", {
  server_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "server.R")
  if (!file.exists(server_path)) server_path <- system.file("scdbayes.app/server.R", package = "scdbayes")
  expressions <- parse(server_path)
  server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("shinyServer")), expressions)[[1]][[2]]
  assignments <- as.list(server[[3]])[-1]
  select <- function(name) Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name(name)), assignments)[[1]]
  env <- new.env(parent = asNamespace("scdbayes"))
  env$dense_rank <- dplyr::dense_rank
  env$input <- list(ask_share_context = TRUE, main_nav = "meta", meta_nav = "meta_fit", outcome_var = "pending")
  env$.scope_inputs <- function(...) list(design = "alternating", goal = "increase", family = "poisson", outcome_var = "pending")
  env$wwc_checks <- list(meta = function() NULL, scd = function() NULL)
  d <- data.frame(study = "s", case = "c", phase = rep(c("A", "B"), each = 3), time = 1:6, score = 11:16)
  diag <- list(basic = list(rhat = 1.002, ess_bulk = 800), divergence = list(n_divergent = 2))
  env$values <- list(data = transform(d, outcome = 100), meta_model_fitted = TRUE, meta_model_stale = TRUE,
    meta_model = structure(list(family = stats::gaussian(), formula = list(formula = score ~ time + level_AB), data = d), class = "glm"),
    meta_fit_scope = list(design = "ab", goal = "decrease", outcome_var = "score"), meta_diagnostics = diag,
    diagnostics = list(basic = list(rhat = 1.2, ess_bulk = 5), divergence = list(n_divergent = 50)),
    formula_display = "pending ~ treatment")
  eval(select(".redact_formula"), env)
  eval(select(".ask_context"), env)
  ctx <- env$.ask_context()
  expect_match(ctx, "Design: ab", fixed = TRUE)
  expect_match(ctx, "Intervention goal: decrease", fixed = TRUE)
  expect_match(ctx, "mean=12.00", fixed = TRUE)
  expect_match(ctx, "outcome ~ time + level_AB", fixed = TRUE)
  expect_match(ctx, "max R-hat 1.0020, min bulk-ESS 800, divergences 2", fixed = TRUE)
  expect_match(ctx, "inputs changed since the fit", fixed = TRUE)
  expect_false(grepl("pending|100.00|1.2000", ctx))
  env$input$main_nav <- "scd"
  env$values$model_fitted <- TRUE
  env$values$model <- env$values$meta_model
  env$values$fit_scope <- env$values$meta_fit_scope
  env$values$diagnostics <- list(basic = list(rhat = numeric(), ess_bulk = numeric()))
  expect_match(env$.ask_context(), "max R-hat NA, min bulk-ESS NA, divergences NA", fixed = TRUE)
  env$values$model_fitted <- FALSE
  expect_match(env$.ask_context(), "Design: alternating", fixed = TRUE)
  expect_match(env$.ask_context(), "Model: not yet fitted.", fixed = TRUE)
  env$input$main_nav <- "power"
  env$input$power_target <- "pooled"
  env$input$power_design <- "multiple_baseline"
  env$input$power_n_studies <- 6
  env$input$power_n_cases <- 5
  expect_match(env$.ask_context(), "Studies: 6; cases per study: 5", fixed = TRUE)
  expect_match(env$.ask_context(), "Design: multiple_baseline", fixed = TRUE)
  expect_false(grepl("mean=|pending|Model: FITTED", env$.ask_context()))
  env$input$ask_share_context <- FALSE
  expect_match(env$.ask_context(), "Context sharing is off", fixed = TRUE)
})


