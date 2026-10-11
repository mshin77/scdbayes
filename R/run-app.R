#' Launch the scdbayes Shiny application
#'
#' @param ... Arguments passed to [shiny::runApp()].
#' @return Called for its side effect (launches the app).
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {
  app_dir <- system.file("scdbayes.app", package = "scdbayes")
  if (app_dir == "") {
    stop("Could not find the scdbayes app directory. Try re-installing `scdbayes`.",
         call. = FALSE)
  }

  app_pkgs <- c("curl", "DT", "future", "later", "plotly", "promises", "readxl",
                "rmarkdown", "shinybusy", "shinyjs", "shinyWidgets", "SingleCaseES")
  missing <- app_pkgs[!vapply(app_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(sprintf(
      "The scdbayes Shiny app requires these packages: %s\nInstall them with:\n  install.packages(c(%s))",
      paste(missing, collapse = ", "),
      paste(sprintf('"%s"', missing), collapse = ", ")
    ), call. = FALSE)
  }

  old_opts <- options(
    mc.cores = parallel::detectCores(),
    scdbayes.backend = .default_backend(),
    scdbayes.threads = brms::threading(2),
    scdbayes.stanc_options = list("O1"),
    scdbayes.cache_dir = file.path(tempdir(), "scdbayes_models"),
    shiny.maxRequestSize = 10 * 1024^2
  )
  on.exit(options(old_opts), add = TRUE)
  # options that the app's global.R may set
  global_opts <- "scdbayes.gemini_model"
  old_global <- sapply(global_opts, getOption, simplify = FALSE)
  on.exit(options(old_global), add = TRUE)

  if (requireNamespace("future", quietly = TRUE)) {
    old_plan <- future::plan(future::multisession)
    on.exit(future::plan(old_plan), add = TRUE)
  }

  cache_dir <- getOption("scdbayes.cache_dir")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  shiny::runApp(app_dir, ...)
}
