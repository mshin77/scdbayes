test_that("analysis export preserves phase references before measure selection", {
  skip_if_not_installed("brms")
  server_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "server.R")
  if (!file.exists(server_path)) server_path <- system.file("scdbayes.app/server.R", package = "scdbayes")
  expressions <- parse(server_path)
  server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("shinyServer")), expressions)[[1]][[2]]
  assignments <- as.list(server[[3]])[-1]
  handler <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], quote(output$downloadCode)), assignments)[[1]]
  env <- new.env(parent = asNamespace("scdbayes"))
  env$output <- list()
  env$input <- list(intervention_goal = "increase")
  env$req <- shiny::req
  env$downloadHandler <- function(filename, content) list(filename = filename, content = content)
  env$.mcmc_settings <- function(...) list(chains = 2, iter = 1000, warmup = 500,
                                          adapt_delta = 0.95, max_treedepth = 10)
  env$.prior_code <- function(...) NULL
  env$.default_rope <- function(...) NULL
  env$fit_transform_code <- function(...) ""
  env$values <- list(model = list(), formula_display = "outcome ~ time_A + level_AB + trend_AB",
                     fit_design = "AB", meta_fit_prep = list(subset = "measure target",
                       subset_var = "measure", subset_values = "target"))
  eval(handler, env)
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  env$output$downloadCode$content(script)
  emitted <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name("data")) &&
    !identical(x[[3]][[1]], as.name("read.csv")), parse(script))
  d <- data.frame(case = "p", time = 1:8, phase = rep(c("A", "B"), each = 4),
                   measure = c("other", "target", "target", "target", "other", "target", "target", "target"),
                   outcome = 1:8)
  fitted <- process_scd_phases(d, list(), "AB")
  fitted <- fitted[fitted$measure == "target", , drop = FALSE]
  replay <- new.env(parent = asNamespace("scdbayes"))
  replay$data <- d
  invisible(lapply(emitted, eval, envir = replay))
  expect_equal(replay$data$time_A, fitted$time_A)
  expect_equal(replay$data$trend_AB, fitted$trend_AB)
})

test_that("pending refits preserve metadata attached to the last completed fit", {
  server_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "server.R")
  if (!file.exists(server_path)) server_path <- system.file("scdbayes.app/server.R", package = "scdbayes")
  expressions <- parse(server_path)
  server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("shinyServer")), expressions)[[1]][[2]]
  assignments <- as.list(server[[3]])[-1]
  run <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name("runModelLogic")), assignments)[[1]][[3]][[3]]
  fields <- c("meta_fit_prep", "fit_design", "fit_prior_list", "fit_prior_sources",
              "fit_prior_type", "fit_brms_priors", "fit_hetero_sigma")
  find_assignments <- function(x) {
    if (!is.call(x) || identical(x[[1]], as.name("function"))) return(list())
    if (identical(x[[1]], as.name("<-")) && is.call(x[[2]]) &&
        identical(x[[2]][[1]], as.name("$")) && identical(x[[2]][[2]], as.name("values")) &&
        as.character(x[[2]][[3]]) %in% fields) return(list(x))
    unlist(lapply(as.list(x)[-1], find_assignments), recursive = FALSE)
  }
  pending <- find_assignments(run)
  env <- new.env(parent = asNamespace("scdbayes"))
  previous <- list(meta_fit_prep = list(subset_values = "previous"), fit_design = "AB",
                   fit_prior_list = list(treatment = "previous"), fit_prior_sources = "previous",
                   fit_prior_type = "default", fit_brms_priors = "previous", fit_hetero_sigma = FALSE)
  env$values <- c(previous, list(prior_overrides_active = TRUE, prior_sources = "pending"))
  env$input <- list()
  env$meta_prep <- list(subset_values = "pending")
  env$prior_list <- list(treatment = "pending")
  env$brms_priors <- "pending"
  env$local_formula <- structure(list(), class = "brmsformula")
  env$.fit_in <- function(...) "pending"
  env$.design_choice <- function(...) "reversal"
  invisible(lapply(pending, eval, envir = env))
  expect_identical(env$values[fields], previous)
  find_callbacks <- function(x) {
    if (!is.call(x)) return(list())
    if (identical(x[[1]], as.name("function")) &&
        identical(names(x[[2]]), "fitted_model")) return(list(x[[3]]))
    unlist(lapply(as.list(x)[-1], find_callbacks), recursive = FALSE)
  }
  completed <- find_assignments(find_callbacks(run)[[1]])
  env$local_fit_design <- "reversal"
  env$local_prior_sources <- "pending"
  env$local_prior_type <- "informative"
  env$local_priors <- "pending"
  invisible(lapply(completed, eval, envir = env))
  expect_identical(env$values[fields], list(meta_fit_prep = list(subset_values = "pending"),
    fit_design = "reversal", fit_prior_list = list(treatment = "pending"),
    fit_prior_sources = "pending", fit_prior_type = "informative",
    fit_brms_priors = "pending", fit_hetero_sigma = TRUE))
})
