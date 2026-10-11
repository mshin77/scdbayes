test_that("App activity logging is absent from the package and Shiny app", {
  symbols <- ls(asNamespace("scdbayes"), all.names = TRUE)
  expect_false(any(grepl("^(\\.?plog_|session_pseudonym$)", symbols)))

  files <- file.path(test_path("..", "..", "inst", "scdbayes.app"),
                     c("global.R", "server.R", "ui.R"))
  expressions <- lapply(files, parse)
  calls <- unique(unlist(lapply(expressions, all.names)))
  expect_length(intersect(c(".track", "track_off", "plog_click", "plog_opt_out",
                            "plog_enabled", "session_pseudonym"), calls), 0)
  text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
  expect_false(grepl("plog_click|plog_opt_out|SCDBAYES_LOG_|scdbayes_opt_out", text))
})
