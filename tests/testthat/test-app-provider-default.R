test_that("hosted provider defaults to Gemini", {
  path <- system.file("scdbayes.app", "global.R", package = "scdbayes")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "global.R")
  expressions <- parse(path)
  assignment <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name(".llm_default")), expressions)[[1]]
  env <- new.env()
  env$llm_default_provider <- function(...) "openai"
  env$.scdbayes_hosted <- TRUE
  eval(assignment, env)
  expect_identical(env$.llm_default, "gemini")
  env$.scdbayes_hosted <- FALSE
  eval(assignment, env)
  expect_identical(env$.llm_default, "openai")
})
