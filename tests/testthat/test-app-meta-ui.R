test_that("lazy meta model controls contain data choices and valid selections", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("xml2")
  ui_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "ui_meta.R")
  if (!file.exists(ui_path)) ui_path <- system.file("scdbayes.app/ui_meta.R", package = "scdbayes")
  env <- new.env(parent = asNamespace("shiny"))
  env$empty_state <- function(...) NULL
  env$info_link <- function(...) NULL
  env$info_popup_link <- function(...) NULL
  env$unifiedRandomEffectsUI <- function(...) NULL
  sys.source(ui_path, env)
  d <- data.frame(outcome = 1:4, alternate = 5:8, time = 1:4,
                   case = c("a", "a", "b", "b"), phase = c("A", "B", "A", "B"))
  html <- xml2::read_html(as.character(env$meta_model_main(d)))
  options <- xml2::xml_find_all(html, "//select[@id='meta_outcome_var']/option")
  expect_setequal(xml2::xml_attr(options, "value"), c("outcome", "alternate", "time"))
  selected <- xml2::xml_find_all(html, "//select[@id='meta_outcome_var']/option[@selected]")
  expect_identical(xml2::xml_attr(selected, "value"), "outcome")
  html <- xml2::read_html(as.character(env$meta_model_main(d, "alternate", c("case", "missing"))))
  selected <- xml2::xml_find_all(html, "//select[@id='meta_outcome_var']/option[@selected]")
  expect_identical(xml2::xml_attr(selected, "value"), "alternate")
  moderators <- xml2::xml_find_all(html, "//select[@id='meta_moderator_vars']/option")
  expect_setequal(xml2::xml_attr(moderators, "value"), names(d))
  selected <- xml2::xml_find_all(html, "//select[@id='meta_moderator_vars']/option[@selected]")
  expect_identical(xml2::xml_attr(selected, "value"), "case")
  html <- xml2::read_html(as.character(env$meta_model_main(d, "missing")))
  selected <- xml2::xml_find_all(html, "//select[@id='meta_outcome_var']/option[@selected]")
  expect_identical(xml2::xml_attr(selected, "value"), "outcome")
})
