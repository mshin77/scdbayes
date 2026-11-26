test_that("optional numeric fields render valid blank HTML", {
  app <- system.file("scdbayes.app", package = "scdbayes")
  if (!nzchar(app)) app <- testthat::test_path("..", "..", "inst", "scdbayes.app")
  ids <- c("bcsmd_duration", "bcsmd_mcid", "marginal_duration", "rep_estimate", "rep_se")
  collect <- function(node) {
    if (!is.call(node) && !is.expression(node) && !is.pairlist(node)) return(list())
    if (is.call(node) && identical(node[[1]], as.name("numericInput")) &&
        is.character(node[[2]]) && node[[2]] %in% ids) return(list(node))
    unlist(lapply(as.list(node), collect), recursive = FALSE)
  }
  controls <- collect(parse(file.path(app, "server.R")))
  expect_length(controls, length(ids))
  html <- vapply(controls, function(control)
    as.character(eval(control, envir = asNamespace("shiny"))), character(1))
  expect_false(any(grepl('value="NA"', html, fixed = TRUE)))
  expect_false(any(grepl('value="NaN"', html, fixed = TRUE)))
})
