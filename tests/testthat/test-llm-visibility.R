test_that("App helpers remain internal and available to the app", {
  helpers <- c("llm_ask", "llm_base_url", "llm_chat", "llm_default_instructions",
               "llm_default_model", "llm_default_provider", "llm_dialog_turn",
               "llm_is_local_host", "llm_model_label", "llm_privacy_note",
               "llm_test_connection", "render_llm_markdown", "sanitize_llm_text")
  expect_length(intersect(helpers, getNamespaceExports("scdbayes")), 0)

  expressions <- parse(test_path("..", "..", "inst", "scdbayes.app", "global.R"))
  imports <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("list2env")),
                    expressions)
  expect_length(imports, 1)
  app <- new.env(parent = baseenv())
  invisible(lapply(imports, eval, envir = app))
  expect_true(all(vapply(helpers, exists, logical(1), envir = app, inherits = FALSE)))
  expect_identical(app$llm_base_url, getFromNamespace("llm_base_url", "scdbayes"))
})
