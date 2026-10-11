test_that("app code reaches package internals only through scdbayes:::", {
  app <- system.file("scdbayes.app", package = "scdbayes", mustWork = TRUE)
  files <- list.files(app, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  ns <- asNamespace("scdbayes")
  internal <- setdiff(ls(ns, all.names = TRUE), getNamespaceExports("scdbayes"))
  internal <- internal[vapply(internal, function(n) is.function(get(n, ns)), logical(1))]

  heads <- function(e) {
    if (!is.call(e)) return(character(0))
    fn <- e[[1]]
    own <- if (is.symbol(fn)) as.character(fn) else character(0)
    qualified <- is.call(fn) && as.character(fn[[1]]) %in% c("::", ":::")
    c(own, if (!qualified && is.call(fn)) heads(fn), unlist(lapply(as.list(e)[-1], heads)))
  }
  assigned <- function(e) {
    if (!is.call(e)) return(character(0))
    lhs <- if (as.character(e[[1]])[1] %in% c("<-", "=", "<<-") && is.symbol(e[[2]])) as.character(e[[2]])
    c(lhs, unlist(lapply(as.list(e), assigned)))
  }
  exprs <- unlist(lapply(files, function(f) as.list(parse(f))), recursive = FALSE)
  imported <- grep("^llm_|^render_llm_markdown$|^sanitize_llm_text$", internal, value = TRUE)
  called <- unique(unlist(lapply(exprs, heads)))
  defined <- unique(unlist(lapply(exprs, assigned)))
  expect_identical(sort(setdiff(intersect(called, internal), c(defined, imported))), character(0))
})
