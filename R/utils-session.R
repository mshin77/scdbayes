# Session cleanup helpers for background fits

#' Run a background fit inside the session folder, then delete sampler temp files
#'
#' Points the package model cache at the session folder (removed when the
#' session ends) while `value` is evaluated in a background worker, then
#' removes the Stan CSV and data JSON files cmdstanr leaves in that worker's
#' private `tempdir()`. brms has read the draws into memory by then, so no
#' session data outlives the session.
#'
#' @param cache_dir Session folder for cached fits, or `NULL` to keep the
#'   current cache folder.
#' @param value An expression, evaluated lazily after the cache folder is set.
#' @return The value of `value`.
#' @noRd
.drop_sampler_files <- function(cache_dir, value) {
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    old <- options(scdbayes.cache_dir = cache_dir)
    on.exit(options(old), add = TRUE)
  }
  # cmdstanr draw CSVs (model-<timestamp>-<chain>-<id>.csv), data JSON (standata-*), and tempfile() data/inits
  on.exit(unlink(list.files(tempdir(), full.names = TRUE, pattern =
    "(^standata-.*\\.json|^file[0-9a-f]+\\.(json|csv)|-[0-9]{12}-[0-9]+-[0-9a-f]+\\.csv)$")), add = TRUE)
  value
}
