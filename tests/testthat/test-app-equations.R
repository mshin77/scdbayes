equation_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "R", "model-equations.R")
if (!file.exists(equation_path)) equation_path <- system.file("scdbayes.app/R/model-equations.R", package = "scdbayes")
if (file.exists(equation_path)) source(equation_path, local = TRUE)

test_that("equation previews reject components without a representation", {
  expect_error(app_model_equations("score ~ time", brms::lognormal()), "Unsupported family")
  expect_error(app_model_equations(brms::bf(score ~ time, zi ~ time),
                                  "zero_inflated_poisson"), "distributional")
  expect_error(app_model_equations(brms::bf(score ~ time, sigma = 2)), "fixed distributional")
  expect_error(app_model_equations("score | cens(status) ~ time"), "response")
  expect_error(app_model_equations(brms::bf(score ~ time, autocor = ~ ma(time = time, gr = case))), "autocorrelation")
  expect_error(app_model_equations(brms::bf(score ~ time, nl = TRUE)), "nonlinear")
  expect_error(app_model_equations("score ~ mo(grade)"), "special term")
  expect_error(app_model_equations("score ~ time + (1 | gr(case, by = phase))"), "grouping")
  expect_error(app_model_equations("score ~ time + (1 | mm(case1, case2))"), "special term")
  expect_error(app_model_equations("score ~ time + (1 | id | case) + (time | id | case)"), "covariance ID")
  expect_error(app_model_equations("score ~ time + ar(time = time, p = 2)"), "AR order")
  expect_error(app_model_equations("score ~ time", sigma_formula = ~ phase + (1 | case)), "sigma")
  expect_error(app_model_equations("score ~ time", brms::brmsfamily("gaussian", link_sigma = "identity"),
                                  sigma_formula = ~ phase), "sigma link")
})

test_that("embedded brms families retain the fitted likelihood", {
  eq <- app_model_equations(brms::bf(score ~ time, family = poisson()))
  expect_match(eq$likelihood, "Poisson", fixed = TRUE)
})

test_that("brms autocorrelation attributes retain serial dependence", {
  eq <- app_model_equations(brms::bf(score ~ time, autocor = ~ ar(time = time, gr = case, p = 1)))
  expect_match(eq$serial, "rho", fixed = TRUE)
})

test_that("latent serial equations match generated Stan components", {
  conditional <- app_model_equations("score ~ time", "poisson", ar = TRUE)
  expect_match(conditional$serial, "e_t\\sim N(0,s_e^2)", fixed = TRUE)
  expect_match(conditional$tex, "+e_t+\\rho e_{t-1}", fixed = TRUE, all = FALSE)
  expect_false(grepl("epsilon", conditional$serial, fixed = TRUE))
  skew <- app_model_equations("score ~ time", "skew_normal", ar = TRUE, ar_covariance = TRUE)
  expect_match(skew$serial, "s_e", fixed = TRUE)
  expect_match(skew$serial, "latent", fixed = TRUE)
  expect_match(skew$likelihood, "SkewNormal", fixed = TRUE)
  skew <- app_model_equations("score ~ time", "skew_normal", ar = TRUE)
  expect_match(skew$serial, "e_t\\sim N(0,s_e^2)", fixed = TRUE)
  expect_match(skew$likelihood, "SkewNormal}_{meanSD}(\\mu_t,\\sigma_t", fixed = TRUE)
})

test_that("namespace-qualified design transforms retain matrix columns", {
  d <- data.frame(score = 1:6, time = 1:6)
  eq <- app_model_equations(score ~ stats::poly(time, 2), data = d)
  expect_equal(eq$fixed_columns, colnames(model.matrix(~ stats::poly(time, 2), d))[-1])
})

test_that("constant trials and scale offsets retain their values", {
  eq <- app_model_equations("score | trials(2) ~ time", "binomial")
  expect_match(eq$likelihood, "Binomial}(2,p_t)", fixed = TRUE)
  eq <- app_model_equations("score ~ time", sigma_formula = ~ phase + offset(log(duration)))
  expect_match(eq$sigma, "\\log(\\mathrm{duration}_t)", fixed = TRUE)
})

test_that("autocorrelation remains separate from fixed terms", {
  eq <- app_model_equations("score ~ time + ar(time = time, gr = case, p = 1, cov = TRUE)", "Gamma")
  expect_equal(eq$fixed_terms, "time")
  expect_match(eq$serial, "s_e", fixed = TRUE)
  expect_false(grepl("sigma", eq$serial, fixed = TRUE))
})

test_that("explicit grouping controls and logarithmic offsets retain meaning", {
  eq <- app_model_equations("score ~ time + offset(log(duration)) + (1 + time | gr(case, cor = FALSE))", "poisson")
  expect_equal(eq$groups[[1]]$name, "case")
  expect_false(eq$groups[[1]]$correlated)
  expect_match(eq$predictor, "\\log(\\mathrm{duration}_t)", fixed = TRUE)
})

test_that("equations retain actual fixed and slope-only random terms", {
  expect_true(exists("app_model_equations", mode = "function"))
  skip_if_not(exists("app_model_equations", mode = "function"))
  eq <- app_model_equations("score ~ 0 + moderator + time:phase + (0 + moderator | case)")
  expect_equal(eq$fixed_terms, c("moderator", "time:phase"))
  expect_false(grepl("beta_0", eq$predictor, fixed = TRUE))
  expect_equal(eq$groups[[1]]$terms, "moderator")
  expect_false(eq$groups[[1]]$intercept)
})

test_that("intercept-only and pure random formulas add no slope terms", {
  skip_if_not(exists("app_model_equations", mode = "function"))
  eq <- app_model_equations("score ~ 1")
  expect_identical(eq$predictor, "\\beta_0")
  random <- app_model_equations("score ~ (1 | study) + (1 | case)")
  expect_length(random$groups, 2)
  expect_false(grepl("b_{1,1", random$predictor, fixed = TRUE))
})

test_that("equations expand every nested grouping level", {
  skip_if_not(exists("app_model_equations", mode = "function"))
  eq <- app_model_equations("score ~ time * phase + (1 + time | study/cluster/case/behavior)")
  expect_equal(vapply(eq$groups, `[[`, "", "name"), c("study", "study:cluster", "study:cluster:case", "study:cluster:case:behavior"))
  expect_true(all(vapply(eq$groups, `[[`, FALSE, "intercept")))
  expect_true(all(vapply(eq$groups, function(x) identical(x$terms, "time"), FALSE)))
  expect_equal(eq$fixed_terms, c("time", "phase", "time:phase"))
})

test_that("ordinal thresholds replace the fixed intercept", {
  skip_if_not(exists("app_model_equations", mode = "function"))
  eq <- app_model_equations("score ~ time + (1 | case)", "cumulative")
  expect_false(grepl("beta_0", eq$predictor, fixed = TRUE))
  expect_match(eq$likelihood, "tau_c", fixed = TRUE)
  expect_true(eq$groups[[1]]$intercept)
})

test_that("family likelihoods specify mean and scale parameterizations", {
  skip_if_not(exists("app_model_equations", mode = "function"))
  families <- c("gaussian", "student", "skew_normal", "Gamma", "poisson", "negbinomial", "zero_inflated_poisson", "zero_inflated_negbinomial", "beta", "binomial", "bernoulli", "cumulative")
  eq <- lapply(families, function(f) app_model_equations("score ~ time", f))
  expect_true(all(vapply(eq, function(x) nzchar(x$likelihood), FALSE)))
  expect_match(eq[[3]]$likelihood, "meanSD", fixed = TRUE)
  expect_match(eq[[4]]$likelihood, "rate", fixed = TRUE)
  expect_match(eq[[6]]$likelihood, "mu_t^2/a", fixed = TRUE)
  expect_match(eq[[9]]$likelihood, "(1-", fixed = TRUE)
})

test_that("distributional formulas and serial models remain explicit", {
  skip_if_not(exists("app_model_equations", mode = "function"))
  eq <- app_model_equations("score | trials(total) ~ time + offset(log(duration))", "binomial", ar = TRUE)
  expect_match(eq$likelihood, "total", fixed = TRUE)
  expect_match(eq$predictor, "duration", fixed = TRUE)
  expect_match(eq$serial, "latent", fixed = TRUE)
  cov <- app_model_equations("score ~ time", ar = TRUE, ar_covariance = TRUE, sigma_formula = ~ 0 + phase)
  expect_match(cov$serial, "|t-s|", fixed = TRUE)
  expect_match(cov$sigma, "phase", fixed = TRUE)
  expect_false(grepl("gamma_0", cov$sigma, fixed = TRUE))
  innovation <- app_model_equations("score ~ time", ar = TRUE)
  expect_match(innovation$serial, "epsilon", fixed = TRUE)
  expect_match(innovation$serial, "e_0=0", fixed = TRUE)
})

test_that("brms formulas and special names retain formula structure", {
  skip_if_not(exists("app_model_equations", mode = "function"))
  skip_if_not_installed("brms")
  eq <- app_model_equations(brms::bf(score ~ `time_index` + (1 | case), sigma ~ phase))
  expect_equal(eq$fixed_terms, "time_index")
  expect_match(eq$predictor, "time\\_index", fixed = TRUE)
  expect_match(eq$sigma, "phase", fixed = TRUE)
})

test_that("equation panels retain display math delimiters", {
  server_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "server.R")
  if (!file.exists(server_path)) server_path <- system.file("scdbayes.app/server.R", package = "scdbayes")
  expressions <- parse(server_path)
  server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("shinyServer")), expressions)[[1]][[2]]
  assignments <- as.list(server[[3]])[-1]
  selected <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name(".render_equations")), assignments)
  env <- new.env(parent = asNamespace("shiny"))
  invisible(lapply(selected, eval, envir = env))
  rendered <- as.character(env$.render_equations(list(tex = "a=b")))
  expect_true(any(grepl("$$a=b$$", rendered, fixed = TRUE)))
})

test_that('equation columns match categorical model matrices', {
  d <- data.frame(phase = factor(rep(c('A', 'B', 'C'), 2)), time = 1:6,
                  case = rep(c('C1', 'C2'), each = 3))
  eq <- app_model_equations('score ~ phase * time + (1 + phase | case)', data = d,
                            sigma_formula = ~ phase)
  columns <- colnames(model.matrix(~ phase * time, d))[-1]
  expect_identical(eq$fixed_columns, columns)
  expect_identical(eq$groups[[1]]$columns, c('phaseB', 'phaseC'))
  expect_true(all(vapply(columns, function(x) grepl(.equation_name(x), eq$predictor, fixed = TRUE), FALSE)))
  expect_match(eq$sigma, 'phaseB', fixed = TRUE)
  expect_match(eq$sigma, 'phaseC', fixed = TRUE)
  expect_identical(eq$fixed_terms, c('phase', 'time', 'phase:time'))
  slope <- app_model_equations('score ~ 0 + phase + (0 + phase | case)', data = d)
  expect_identical(slope$fixed_columns, colnames(model.matrix(~ 0 + phase, d)))
  expect_identical(slope$groups[[1]]$columns, c('phaseA', 'phaseB', 'phaseC'))
})
