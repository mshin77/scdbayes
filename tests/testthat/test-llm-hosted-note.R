test_that("hosted demo note points to the GitHub install only while fitting is off", {
  keep <- Sys.getenv(c("SCDBAYES_HOSTED", "SCDBAYES_HOSTED_FITS"), unset = NA)
  on.exit(for (v in names(keep)) if (is.na(keep[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(keep[v])))

  Sys.unsetenv(c("SCDBAYES_HOSTED", "SCDBAYES_HOSTED_FITS"))
  expect_identical(.llm_hosted_note(), "")

  Sys.setenv(SCDBAYES_HOSTED = "true")
  note <- .llm_hosted_note()
  expect_match(note, "https://github.com/mshin77/scdbayes", fixed = TRUE)
  expect_match(note, "install_github", fixed = TRUE)

  Sys.setenv(SCDBAYES_HOSTED_FITS = "true")
  expect_identical(.llm_hosted_note(), "")
})
