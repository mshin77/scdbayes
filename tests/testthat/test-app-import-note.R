test_that("the data sidebar shows the import self-check note on both tabs", {
  skip_if_not_installed("shiny")
  ui_path <- testthat::test_path("..", "..", "inst", "scdbayes.app", "R", "ui-data.R")
  if (!file.exists(ui_path)) ui_path <- system.file("scdbayes.app/R/ui-data.R", package = "scdbayes")
  # shiny exports only, as the app sees them
  shiny_exports <- getNamespaceExports("shiny")
  env <- new.env(parent = list2env(mget(shiny_exports, envir = asNamespace("shiny"), inherits = TRUE),
                                   parent = baseenv()))
  sys.source(ui_path, env)
  for (prefix in c("", "meta_")) {
    html <- as.character(env$data_sidebar(prefix))
    expect_match(html, "Check that files have no student names or other private information.",
                 fixed = TRUE)
    expect_match(html, sprintf('id="%sfile1_privacy"', prefix), fixed = TRUE)
    expect_match(html, sprintf('aria-describedby="%sfile1_privacy"', prefix), fixed = TRUE)
    expect_false(grepl("margin-top: -8px", html, fixed = TRUE))
  }
})

test_that("private-information columns are flagged by name and by email-like values", {
  d <- data.frame(
    case = rep(c("S1", "S2", "S3"), each = 6), time = rep(1:6, 3),
    phase = rep(rep(c("A", "B"), each = 3), 3), outcome = c(1:9, 2:10),
    student_name = "Ana Diaz", contact = "ana@school.org", `Date of Birth` = "2015-01-02",
    session = 1, check.names = FALSE, stringsAsFactors = FALSE)
  expect_setequal(detect_private_columns(d), c("student_name", "contact", "Date of Birth"))

  v <- validate_scd_data(d)
  hit <- grep("may hold private information", unlist(v$warnings), value = TRUE)
  expect_length(hit, 1)
  expect_match(hit, "student_name, contact, Date of Birth", fixed = TRUE)
  expect_true(v$valid)
})

test_that("ordinary SCD columns and the bundled datasets raise no privacy warning", {
  expect_identical(detect_private_columns(data.frame(
    id = 1, case = "S1", time = 1, phase = "A", outcome = 1, session = 1, study = "St1",
    intervention_type = "x", age_group = "y", criterion = 2, phase_name = "A",
    condition_name = "x", stringsAsFactors = FALSE)), character())
  expect_identical(detect_private_columns(data.frame(Name = "x", Last.Name = "y", case_name = "z",
                                                     emailed = 1, phone_number = 2)),
                   c("Name", "Last.Name", "case_name", "phone_number"))
  data_dir <- testthat::test_path("..", "..", "inst", "scdbayes.app", "data")
  if (!dir.exists(data_dir)) data_dir <- system.file("scdbayes.app/data", package = "scdbayes")
  files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  skip_if(length(files) == 0, "bundled datasets not found")
  for (f in files) {
    expect_identical(detect_private_columns(utils::read.csv(f, stringsAsFactors = FALSE)),
                     character(), info = basename(f))
  }
  expect_identical(detect_private_columns(NULL), character())
})
