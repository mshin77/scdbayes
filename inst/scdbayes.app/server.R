# server.R - Bayesian SCD Meta-Analysis: Reactive Logic
# Utility functions are in R/ (auto-sourced by Shiny)
# Libraries and options are in global.R

# a backtick ends a markdown code span or fence, so data-derived text loses it before entering a report
.md_code <- function(x) gsub("`", "'", gsub("[\r\n]", " ", as.character(x)))

# first line of an error, without file paths, for messages shown in the browser
.short_error <- function(e) {
  msg <- c(strsplit(if (inherits(e, "condition")) conditionMessage(e) else as.character(e), "\n", fixed = TRUE)[[1]], "")[1]
  msg <- gsub("([A-Za-z]:)?([\\\\/][^\\\\/ '\"]+){2,}[\\\\/]?", "<path>", msg)
  substr(msg, 1, 300)
}

# data-derived names inside MathJax HTML: no TeX control characters, then HTML-escaped
.tex_name <- function(x) as.character(htmltools::htmlEscape(gsub("[{}\\\\$^%#&~]", "", as.character(x))))

# table cells appended to a knitted Rmd: no inline code, math, pipes, or raw HTML
.md_cell <- function(x) gsub("[`$|\\\\<>\r\n]", " ", as.character(x))

.write_fallback_report <- function(file, model, results,
                                   sections = c("methods", "results", "tables", "diagnostics"),
                                   format = "md", formula_str = NULL, priors = NULL) {
  lines <- c("# scdbayes Analysis Report", paste("Date:", Sys.Date()), "")

  if ("methods" %in% sections) {
    formula_str <- formula_str %||% paste(deparse(stats::formula(model)$formula), collapse = " ")
    lines <- c(lines,
      "## Methods",
      sprintf("- Family: `%s`", .md_code(stats::family(model)$family)),
      sprintf("- Formula: `%s`", .md_code(paste(formula_str, collapse = " "))),
      sprintf("- Chains: %s, Iterations: %s, Warmup: %s",
              model$fit@sim$chains, model$fit@sim$iter, model$fit@sim$warmup),
      "")
    if (is.data.frame(priors) && nrow(priors))
      lines <- c(lines, "### Priors", "",
                 "| Class | Coef | Group | Prior | Source |", "|---|---|---|---|---|",
                 sprintf("| %s | %s | %s | %s | %s |", .md_cell(priors$class), .md_cell(priors$coef),
                         .md_cell(priors$group), .md_cell(priors$prior), .md_cell(priors$source)), "")
  }

  if ("results" %in% sections) {
    lines <- c(lines, "## Model Summary", "```", .md_code(capture.output(summary(model))), "```", "")
    if (!is.null(results$effects)) {
      ef <- results$effects
      lines <- c(lines, "## Phase descriptives")
      if (!is.na(ef$raw_difference)) lines <- c(lines, sprintf("- Raw mean difference (B - A): %.3f", ef$raw_difference))
      if (!is.null(ef$phase_a_mean) && !is.na(ef$phase_a_mean)) lines <- c(lines, sprintf("- Phase A mean: %.3f (n = %d)", ef$phase_a_mean, ef$phase_a_n))
      if (!is.null(ef$phase_b_mean) && !is.na(ef$phase_b_mean)) lines <- c(lines, sprintf("- Phase B mean: %.3f (n = %d)", ef$phase_b_mean, ef$phase_b_n))
      lines <- c(lines, "")
    }
  }

  if ("diagnostics" %in% sections) {
    lines <- c(lines, "## Diagnostics", "```",
      capture.output({
        cat(sprintf("Max R-hat: %.4f\n", max(brms::rhat(model), na.rm = TRUE)))
        cat(sprintf("Min ESS ratio: %.3f\n", min(brms::neff_ratio(model), na.rm = TRUE)))
      }),
      "```", "")
  }

  md_text <- paste(lines, collapse = "\n")

  if (format == "html") {
    html_body <- if (requireNamespace("commonmark", quietly = TRUE)) {
      commonmark::markdown_html(md_text)
    } else {
      paste0("<pre>", htmltools::htmlEscape(md_text), "</pre>")
    }
    writeLines(c(
      "<!DOCTYPE html><html><head><meta charset='utf-8'>",
      "<title>scdbayes Report</title>",
      "<style>body{font-family:system-ui,sans-serif;max-width:800px;margin:2em auto;padding:0 1em;line-height:1.6}",
      "pre{background:#f5f5f5;padding:1em;overflow-x:auto;border-radius:4px}",
      "code{background:#f5f5f5;padding:2px 4px;border-radius:3px}</style>",
      "</head><body>", html_body, "</body></html>"
    ), file)
  } else {
    writeLines(md_text, file)
  }
}

# brms ar(gr =) accepts an interaction of grouping columns, so each tier gets its own series
.ar_group <- function(data, fallback = "case") {
  tiers <- tryCatch(.tier_cols(data), error = function(e) character(0))
  if (length(tiers)) paste(tiers, collapse = ":") else fallback
}

# default time B: shortest baseline plus the treatment length of that case, or the shortest treatment
.ab_rule_input <- function(id) {
  selectInput(id, "Default B rule", choices = c("scdhlm (default_times)" = "scdhlm",
                                               "Shortest treatment duration" = "shortest_treatment"),
              selected = "scdhlm")
}

# the selected outcome sets the adaptive prior scales; a name outside the data falls back to "outcome"
.adaptive_priors <- function(data, structure, model_type, outcome_var, exposure = NULL) {
  outcome_var <- if (isTRUE(outcome_var %in% names(data))) outcome_var else "outcome"
  exposure <- if (isTRUE(exposure %in% names(data))) exposure
  generate_adaptive_priors(data, structure %||% list(), model_type = model_type, outcome_var = outcome_var,
                           exposure = exposure)
}

# one series per full tier key (study, cluster, case, nest); repeated case ids across studies stay apart
.with_tier <- function(d) {
  tiers <- intersect(tryCatch(.tier_cols(d), error = function(e) character(0)), names(d))
  if (!length(tiers)) tiers <- intersect("case", names(d))
  d$.tier <- if (length(tiers)) do.call(paste, c(lapply(d[tiers], as.character), sep = " / ")) else "all"
  d
}

.design_choice <- function(selected, data) {
  if (!is.null(selected) && nzchar(selected)) {
    return(switch(selected, multiple_probe = "multiple_baseline", selected))
  }
  tryCatch(detect_design_type(data), error = function(e) "unknown")
}

.env_flag <- function(option, env) {
  v <- getOption(option, Sys.getenv(env, ""))
  isTRUE(v) || tolower(as.character(v %||% "")[1]) %in% c("1", "true", "yes")
}

# brms leaves Post.Prob and Evid.Ratio NA for point hypotheses without sample_prior = "yes"
.drop_na_postprob <- function(df) df[!(names(df) %in% c("Post.Prob", "Post Prob", "Evid.Ratio", "Evid Ratio") & colSums(!is.na(df)) == 0)]

# faceted plot height in px from its panel rows; the 2400 px ceiling keeps many-case plots renderable
.facet_px <- function(n_panels, per_row = 220, min_px = 360, max_px = 2400, ncol = NULL, extra = 90) {
  n <- max(1, n_panels, na.rm = TRUE)
  ncol <- ncol %||% ceiling(sqrt(n))
  min(max_px, max(min_px, extra + per_row * ceiling(n / ncol)))
}
.facet_capped <- function(n_panels, ...) .facet_px(n_panels, ..., max_px = Inf) > 2400

# browser values are untrusted: bound and round sampler settings; SCDBAYES_MAX_ITER caps hosted runs
.mcmc_num <- function(x, default) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1 || !is.finite(v)) default else v
}
.mcmc_clamp <- function(x, lo, hi, default) round(min(max(.mcmc_num(x, default), lo), hi))
.mcmc_settings <- function(chains, iter, warmup, adapt_delta, max_treedepth) {
  cap <- .llm_limit("SCDBAYES_MAX_ITER", 20000)
  iter <- .mcmc_clamp(iter, 200, cap, 2000)
  list(chains = as.integer(.mcmc_clamp(chains, 1, 8, 4)),
       iter = as.integer(iter),
       warmup = as.integer(.mcmc_clamp(warmup, 100, iter - 1, min(1000, iter %/% 2))),
       adapt_delta = min(max(.mcmc_num(adapt_delta, 0.95), 0.8), 0.999),
       max_treedepth = as.integer(.mcmc_clamp(max_treedepth, 8, 20, 12)))
}

# same default fixed part as the package formula builder, used until the term checkboxes render
.default_fixed_terms <- function(data) {
  nm <- names(data)
  nm <- nm[make.names(nm) == nm]
  crit <- c(.sort_index(grep("^level_crit[0-9]+$", nm, value = TRUE)),
            .sort_index(grep("^level_ret[0-9]+$", nm, value = TRUE)))
  atd <- grep("^atd_", nm, value = TRUE)
  rev_terms <- reversal_terms(nm)
  terms <- if (length(rev_terms)) rev_terms
    else if (length(crit)) c("time_A", crit)
    else if (all(c("treatment", "time_treatment") %in% nm)) c("time_A", "treatment", "time_treatment")
    else if (length(atd)) c("time_session", atd)
    else c("time_A", "level_AB", "trend_AB", "level_BM", "trend_BM", "level_BG", "trend_BG")
  intersect(terms, nm)
}

.sort_index <- function(x) x[order(as.integer(sub("\\D+", "", x)))]

# outermost balanced parentheses, so gr(case, by = phase) stays inside its bar term
.paren_groups <- function(f) regmatches(f, gregexpr("\\((?:[^()]++|(?R))*\\)", f, perl = TRUE))[[1]]

# a piecewise random slope is kept only when its fixed effect is in the model
.prune_slopes <- function(re_term, fixed_terms) {
  parts <- regmatches(re_term, regexec("^\\((.*?)(\\|\\|?)(.*)\\)$", trimws(re_term)))[[1]]
  if (length(parts) < 4) return(re_term)
  lhs <- trimws(strsplit(parts[2], "+", fixed = TRUE)[[1]])
  keep <- lhs[!grepl("^(time_|level_|trend_|treatment$|atd_)", lhs) | lhs %in% fixed_terms]
  if (length(keep) == 0) keep <- "1"
  paste0("(", paste(keep, collapse = " + "), " ", parts[3], " ", trimws(parts[4]), ")")
}

# piecewise term checkboxes by design: label -> column, shared by the SCD and meta model tabs
.piecewise_choices <- function(md, design) {
  nm <- names(md)
  step_label <- function(term) gsub("([A-Za-z]+[0-9]+)([A-Za-z]+[0-9]+)", "\\1-\\2",
                                    sub("^(level|trend)_", "", term))
  crit_k <- function(term) sub("^(level|trend)_crit", "", term)
  if (design == "reversal") {
    rev_cols <- reversal_terms(nm)
    firsts <- grep("^time_[A-Za-z]+1$", rev_cols, value = TRUE)
    steps <- rev_cols[grepl("^(level|trend)_", rev_cols)]
    first_run <- sub("^time_", "", firsts)
    c(stats::setNames(firsts, ifelse(firsts == "time_A1", "Baseline trend",
                                     sprintf("Trend in first phase (%s)", first_run))),
      stats::setNames(steps, sprintf("%s (%s)", ifelse(startsWith(steps, "level_"), "Level change", "Trend change"),
                                     step_label(steps))))
  } else if (design == "alternating") {
    cond_vars <- grep("^atd_", nm, value = TRUE)
    c(if ("time_session" %in% nm) c(`Session trend (centered)` = "time_session"),
      stats::setNames(cond_vars, paste0("Condition (", sub("^atd_", "", cond_vars), ")")))
  } else if (design == "changing_criterion") {
    crit <- grep("^(level|trend)_crit[0-9]+$", nm, value = TRUE)
    crit <- crit[order(as.integer(crit_k(crit)), !startsWith(crit, "level_"))]
    ret <- .sort_index(grep("^level_ret[0-9]+$", nm, value = TRUE))
    c(if ("time_A" %in% nm) c(`Baseline trend` = "time_A"),
      if ("treatment" %in% nm) c(`Level change (baseline-criterion, collapsed)` = "treatment"),
      if ("time_treatment" %in% nm) c(`Trend within criterion (collapsed)` = "time_treatment"),
      stats::setNames(crit, paste0(ifelse(startsWith(crit, "level_"), "Level change (criterion ",
                                          "Trend within criterion "), crit_k(crit),
                                   ifelse(startsWith(crit, "level_"), ")", ""))),
      stats::setNames(ret, sprintf("Level change (return to baseline %s)", sub("^level_ret", "", ret))))
  } else {
    gen_from <- if ("level_BM" %in% nm) "M" else "B"
    labels <- c(time_A = "Baseline trend", level_AB = "Level change (A-B)",
                trend_AB = "Trend change (A-B)", level_BM = "Level change (B-M)",
                trend_BM = "Trend change (B-M)",
                level_BG = sprintf("Level change (%s-G)", gen_from),
                trend_BG = sprintf("Trend change (%s-G)", gen_from))
    present <- intersect(names(labels), nm)
    stats::setNames(present, labels[present])
  }
}

# Shiny server function

shinyServer(function(input, output, session) {

    session$onSessionEnded(function() {
        isolate({
            values$model <- NULL
            values$meta_model <- NULL
            values$prior_predictive_model <- NULL
            values$results <- NULL
            values$meta_results <- NULL
            values$power_results <- NULL
            values$model_loo <- NULL
        })
        try(unlink(c(.session_model_file(), .uploads$paths)), silent = TRUE)
        try(unlink(.session_dir, recursive = TRUE), silent = TRUE)
    })

    .session_model_file <- function() {
        file.path(brms_cache_dir, sprintf("main-model-%s.rds", session$token))
    }
    .session_dir <- file.path(brms_cache_dir, paste0("session-", session$token))

    # brms fits ar() in its fast regression form only for families with natural
    # residuals; other families need cov = TRUE (latent residuals), see ?brms::ar
    .ar_cov_flag <- function(family, user_flag) {
        natural <- family %in% c("gaussian", "student", "skew_normal")
        if (isTRUE(user_flag) || !natural) ", cov = TRUE" else ""
    }

    # Center/scale numeric moderators on the data that is fitted, so a level or
    # slope term is the effect at the moderator mean when centering is chosen
    .center_moderators <- function(d, meta = isTRUE(values$meta_fit_pending)) {
        prepare_fit_data(d, .input_value("outcome_var", meta), "gaussian",
                         .input_value("moderator_vars", meta),
                         isTRUE(.input_value("center_moderators", meta)),
                         isTRUE(.input_value("scale_moderators", meta)))$data
    }
    # every upload this session, not only the latest, is removed at session end
    .uploads <- new.env(parent = emptyenv())
    .uploads$paths <- character(0)
    observe(.uploads$paths <- unique(c(.uploads$paths, input$file1$datapath, input$meta_file1$datapath)))

    .cached_loo <- function() {
        m <- values$model
        if (is.null(m)) return(NULL)
        hit <- isolate(values$model_loo)
        if (!is.null(hit)) return(hit)
        res <- tryCatch(suppressWarnings(brms::loo(m)), error = function(e) NULL)
        values$model_loo <- res
        res
    }

    .cached_meta_loo <- function() {
        m <- values$meta_model
        if (is.null(m)) return(.cached_loo())
        hit <- isolate(values$meta_model_loo)
        if (!is.null(hit)) return(hit)
        res <- tryCatch(suppressWarnings(brms::loo(m)), error = function(e) NULL)
        values$meta_model_loo <- res
        res
    }

    # a failing post-load step warns and returns NULL instead of ending the session
    # package warnings such as rows dropped for a missing phase label reach the user; the id avoids repeats
    .process_phases <- function(data, structure, design) {
        withCallingHandlers(process_scd_phases(data, structure, design), warning = function(w) {
            showNotification(conditionMessage(w), id = "phase_coding_warning", type = "warning", duration = 10)
            invokeRestart("muffleWarning")
        })
    }

    .try_step <- function(label, expr) {
        tryCatch(expr, error = function(e) {
            showNotification(sprintf("%s failed: %s", label, conditionMessage(e)),
                             type = "warning", duration = 8)
            NULL
        })
    }

    # the ROPE noise scale reads the processed data (phase and tier columns), not the model frame
    .default_rope <- function(model) {
        response <- all.vars(stats::formula(model)$formula)[1]
        tryCatch(default_rope(model$data, stats::family(model), response = response),
                 error = function(e) NULL)
    }

    # brms labels nested study:case levels "<study>_<case>"; a case id that already carries the study prefix is shown once
    .case_display <- function(labels, fit_data) {
        study_var <- find_study_var(fit_data)
        case_var <- find_case_var(fit_data)
        labels <- as.character(labels)
        if (is.null(study_var) || is.null(case_var)) return(labels)
        uniq <- unique(fit_data[c(study_var, case_var)])
        study_chr <- as.character(uniq[[study_var]])
        case_chr <- as.character(uniq[[case_var]])
        shown <- ifelse(startsWith(case_chr, paste0(study_chr, "_")), case_chr,
                        paste(study_chr, case_chr, sep = "_"))
        bare <- !case_chr %in% case_chr[duplicated(case_chr)]
        lut <- stats::setNames(c(shown, shown[bare]), c(paste(study_chr, case_chr, sep = "_"), case_chr[bare]))
        out <- unname(lut[labels])
        ifelse(is.na(out), labels, out)
    }

    .step_roles <- function(term) {
        parts <- regmatches(term, regexec("^(level|trend)_([A-Za-z]+)([0-9]+)([A-Za-z]+)([0-9]+)$", term))[[1]]
        if (length(parts) < 6) return(NULL)
        list(from = paste0(parts[3], parts[4]), to = paste0(parts[5], parts[6]),
             from_role = phase_role(parts[3]), to_role = phase_role(parts[5]))
    }

    .announce <- function(msg) {
        shinyjs::html("status_region", htmltools::htmlEscape(msg))
    }

    # lazy-load heavy tab UIs on first visit (main_nav, scd_nav, meta_nav)
    model_tab_loaded         <- reactiveVal(FALSE)
    fit_tab_loaded           <- reactiveVal(FALSE)
    effects_tab_loaded       <- reactiveVal(FALSE)
    percase_tab_loaded       <- reactiveVal(FALSE)
    meta_data_tab_loaded     <- reactiveVal(FALSE)
    meta_model_tab_loaded    <- reactiveVal(FALSE)
    meta_fit_tab_loaded      <- reactiveVal(FALSE)
    meta_effects_tab_loaded  <- reactiveVal(FALSE)
    meta_perstudy_tab_loaded <- reactiveVal(FALSE)
    meta_export_tab_loaded   <- reactiveVal(FALSE)

    observeEvent(input$scd_nav, {
      if (identical(input$scd_nav, "setup"))   model_tab_loaded(TRUE)
      if (identical(input$scd_nav, "fit"))     fit_tab_loaded(TRUE)
      if (identical(input$scd_nav, "effects")) effects_tab_loaded(TRUE)
      if (identical(input$scd_nav, "percase")) percase_tab_loaded(TRUE)
    }, ignoreInit = FALSE)

    observeEvent(input$meta_nav, {
      if (identical(input$meta_nav, "meta_data"))    meta_data_tab_loaded(TRUE)
      if (identical(input$meta_nav, "meta_model"))   meta_model_tab_loaded(TRUE)
      if (identical(input$meta_nav, "meta_fit"))     meta_fit_tab_loaded(TRUE)
      if (identical(input$meta_nav, "meta_effects")) meta_effects_tab_loaded(TRUE)
      if (identical(input$meta_nav, "meta_perstudy")) meta_perstudy_tab_loaded(TRUE)
      if (identical(input$meta_nav, "meta_export"))  meta_export_tab_loaded(TRUE)
    }, ignoreInit = FALSE)

    observeEvent(input$main_nav, {
      if (identical(input$main_nav, "meta"))  meta_data_tab_loaded(TRUE)   # default-load first meta tab
    }, ignoreInit = FALSE)

    output$model_tab_ui <- renderUI({ model_main() })
    outputOptions(output, "model_tab_ui", suspendWhenHidden = FALSE)

    output$fit_tab_ui <- renderUI({
      req(fit_tab_loaded())
      fit_main()
    })

    output$effects_tab_ui <- renderUI({
      req(effects_tab_loaded())
      effects_main()
    })

    output$percase_tab_ui <- renderUI({
      req(percase_tab_loaded())
      percase_main()
    })

    # Meta-Analysis peer tabs (6, mirror Single-Case Design)
    output$meta_data_tab_ui <- renderUI({
      req(meta_data_tab_loaded())
      tagList(
        conditionalPanel(condition = "!output.hasMultipleStudies",
          empty_state("studies",
            "Load a multi-study dataset to begin. Required columns: study, case, time, phase, outcome.")
        ),
        conditionalPanel(condition = "output.hasMultipleStudies",
          tags$p(style = "font-size: 15px; color: #374151; margin: 12px 0;",
            "Per-study summary."),
          DT::dataTableOutput("metaStudyTable"),
          uiOutput("metaComparabilityWarning"),
          hr(),
          verbatimTextOutput("metaStructureSummary"),
          hr(),
          data_panels("meta_", include_between_phase = FALSE)
        )
      )
    })

    output$meta_model_tab_ui <- renderUI({
      req(meta_model_tab_loaded())
      meta_model_main(values$data, isolate(input$meta_outcome_var), isolate(input$meta_moderator_vars))
    })
    output$meta_fit_tab_ui     <- renderUI({ req(meta_fit_tab_loaded());     meta_fit_main() })
    output$meta_perstudy_tab_ui <- renderUI({ req(meta_perstudy_tab_loaded()); meta_perstudy_main() })
    output$meta_effects_tab_ui  <- renderUI({ req(meta_effects_tab_loaded());  meta_effects_main() })

    output$meta_export_tab_ui <- renderUI({
      req(meta_export_tab_loaded())
      conditional_no_data <- conditionalPanel(
        condition = "!output.hasMultipleStudies",
        empty_state("studies",
          "Load a multi-study dataset in Meta-Analysis > Data first.")
      )
      conditional_ready <- conditionalPanel(
        condition = "output.hasMultipleStudies",
        div(style = "max-width: 960px; margin: 0 auto; padding: 24px;",
          tags$h2("Two-stage Bayesian BC-SMD ", info_link("bc_smd"),
                  style = "color: #0c1f4a; font-weight: 600; margin-bottom: 8px;"),
          tags$p(style = "color: #475569; margin-bottom: 10px;",
            "BC-SMD per study, pooled with a Bayesian meta-analysis; positive values mean improvement. ",
            "Studies with under 3 cases, ATD, or CCD are skipped."),
          uiOutput("bcsmdOutcomeNote"),
          fluidRow(
            column(3, .ab_rule_input("bcsmd_ab_rule")),
            column(3, numericInput("bcsmd_duration", "Benchmark duration B - A (blank = rule)",
                                   value = NULL, min = 1, step = 1)),
            column(3, selectInput("bcsmd_trend", "Time trend",
                                  choices = c("None (level change)" = "none", "Fixed trend" = "fixed",
                                              "Random trend" = "random"), selected = "none")),
            column(3, numericInput("bcsmd_mcid", "MCID (BC-SMD units, optional)", value = NULL, min = 0, step = 0.05))
          ),
          fluidRow(
            column(4, actionButton("runBCSMD", "Estimate BC-SMD", class = "btn-primary w-100 mb-2")),
            column(4, actionButton("runBCSMDPool", "Pool (Bayesian)", class = "btn-primary w-100 mb-2")),
            column(4, checkboxInput("bcsmd_common_effect", "Common treatment effect (sensitivity)", value = FALSE))
          ),
          DT::dataTableOutput("bcsmdTable"),
          tags$h3("Pooled posterior", style = "color: #0c1f4a; font-weight: 600; margin-top: 16px;"),
          DT::dataTableOutput("bcsmdPoolTable"),
          verbatimTextOutput("bcsmdPoolPriors"),
          plotOutput("bcsmdForest", height = "auto"),
          fluidRow(
            column(4, downloadButton("downloadBCSMD", "Per-study BC-SMD (yi, sei)", class = "btn-default w-100 mb-2")),
            column(4, downloadButton("downloadBCSMDCode", "brms code (both stages)", class = "btn-default w-100 mb-2"))
          ),
          hr(),
          tags$h2("Bayesian template for first-stage effect sizes ", info_link("first_stage_es"),
                  style = "color: #0c1f4a; font-weight: 600; margin-bottom: 8px;"),
          tags$p(style = "color: #64748B; margin-bottom: 12px;",
            "brms template for the effect-size file downloaded from the sidebar."),
          fluidRow(
            column(4, downloadButton("downloadMetaBrmsScript", "brms template", class = "btn-primary w-100 mb-2"))
          ),
          hr(),
          tags$h2("Optional frequentist comparison (export only)",
                  style = "color: #0c1f4a; font-weight: 600; margin-bottom: 8px;"),
          tags$p(style = "color: #64748B; margin-bottom: 12px;",
            "Scripts for running metafor and scdhlm outside the app. ",
            "The app itself computes nothing frequentist."),
          fluidRow(
            column(4, downloadButton("downloadMetaScript",        "Metafor",    class = "btn-default w-100 mb-2")),
            column(4, downloadButton("downloadPubBiasScript",     "Bias",       class = "btn-default w-100 mb-2")),
            column(4, downloadButton("downloadNetworkMetaScript", "Network",    class = "btn-default w-100 mb-2"))
          ),
          fluidRow(
            column(4, downloadButton("downloadMLMetaScript",      "Moderators", class = "btn-default w-100 mb-2")),
            column(4, downloadButton("downloadBCSMDFreqScript",   "scdhlm BC-SMD script", class = "btn-default w-100 mb-2")),
            column(4, downloadButton("downloadScdhlmData",        "scdhlm-ready data", class = "btn-default w-100 mb-2"))
          )
        )
      )
      tagList(conditional_no_data, conditional_ready)
    })

    # Accessibility

    # Plot theme
    accessible_plot_theme <- function() {
        theme_minimal(base_size = 14) +
            theme(
                plot.title = element_text(size = 15, face = "bold"),
                axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 8)),
                axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
                axis.text = element_text(size = 13),
                legend.position = "bottom",
                legend.title = element_text(size = 13),
                legend.text = element_text(size = 13),
                strip.text = element_text(size = 13, face = "bold"),
                panel.grid.minor = element_blank()
            )
    }

    # empty plotly canvas styled like empty_state()
    empty_plot <- function(msg = "") {
        annotations <- list(
            list(text = "○", x = 0.5, y = 0.62, xref = "paper", yref = "paper",
                 showarrow = FALSE,
                 font = list(size = 48, color = "#CBD5E1"))
        )
        if (nzchar(msg)) {
            annotations[[length(annotations) + 1]] <- list(
                text = msg, x = 0.5, y = 0.40, xref = "paper", yref = "paper",
                showarrow = FALSE,
                font = list(size = 19, color = "#64748B"))
        }
        plotly::plot_ly(type = "scatter", mode = "markers",
                        x = 0, y = 0, marker = list(opacity = 0),
                        hoverinfo = "none", showlegend = FALSE) %>%
            plotly::layout(
                annotations = annotations,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                margin = list(t = 40, b = 40, l = 0, r = 0)
            ) %>%
            plotly::config(displayModeBar = FALSE)
    }

    # screen-reader name for a table; DT renders it as a <caption>
    .dt_caption <- function(text) if (!is.null(text)) tags$caption(class = "sr-only", text)

    # DT datatable helper
    dt_table <- function(df, pageLength = 10, digits = NULL, caption = NULL) {
        out <- DT::datatable(df,
            extensions = "Buttons",
            options = list(dom = "Bfrtip", buttons = c("copy", "excel", "pdf"),
                           pageLength = pageLength, scrollX = TRUE),
            class = "cell-border stripe", rownames = FALSE,
            caption = .dt_caption(caption))
        if (!is.null(digits)) {
            num_cols <- names(df)[sapply(df, is.numeric)]
            if (length(num_cols) > 0) out <- DT::formatRound(out, columns = num_cols, digits = digits)
        }
        out
    }

    # ggplotly wrapper with consistent tooltip font size
    styled_ggplotly <- function(...) {
        suppressWarnings(plotly::ggplotly(...)) %>%
            plotly::layout(hoverlabel = list(font = list(size = 16)))
    }

    # at most 2 facet columns on phones; NULL keeps the default layout
    .phone_ncol <- function() if (isTRUE(.mcmc_num(input$viewport_width, Inf) < 768)) 2

    # Plotly config
    plotly_config <- function() {
        list(
            displayModeBar = TRUE,
            modeBarButtonsToRemove = c("lasso2d", "select2d"),
            displaylogo = FALSE,
            toImageButtonOptions = list(
                format = "png",
                filename = "scdbayes_plot",
                width = 1200,
                height = 800
            )
        )
    }

    # Reactive values
    values <- reactiveValues(
        data = NULL,
        processed_data = NULL,
        model_structure = NULL,
        current_model = NULL,
        models = list(),
        diagnostics = list(),
        effect_sizes = NULL,
        icc_results = NULL,
        variance_components = NULL,
        meta_summary = NULL,
        covariate_summary = NULL,
        validation_passed = FALSE,
        model_fitted = FALSE,  # Track if initial model has been fitted
        model_refitting = FALSE,  # Track if model is currently refitting
        formula_display = NULL,
        capture_file = NULL,
        capture_last_line = 0,
        results = NULL,
        model = NULL,
        model_stale = FALSE,

        # Meta-Analysis independent state, driven by input$meta_* widgets.
        meta_model = NULL,
        meta_results = NULL,
        meta_model_fitted = FALSE,
        meta_model_refitting = FALSE,
        meta_model_stale = FALSE,
        meta_fit_pending = FALSE,
        meta_fit_pending_start = FALSE,
        prior_sources = character(0)
    )

    # Random effects module
    random_effects_config <- unifiedRandomEffectsServer(
        "randomEffects",
        data_reactive = reactive(values$processed_data %||% values$data),
        formula_reactive = reactive(NULL)
    )

    # Meta-Analysis random effects module (separate instance)
    meta_random_effects_config <- unifiedRandomEffectsServer(
        "metaRandomEffects",
        data_reactive = reactive(values$processed_data %||% values$data),
        formula_reactive = reactive(NULL)
    )


    # Polling timer
    poll_timer <- reactiveTimer(500, session)

    # File upload
    observeEvent(input$file1, {
        req(input$file1)
        ext <- tolower(tools::file_ext(input$file1$name %||% input$file1$datapath))
        if (!ext %in% c("csv", "xlsx", "xls")) {
            showNotification(sprintf("Unsupported file type '.%s'. Upload a .csv, .xlsx, or .xls file.", ext),
                             type = "error", duration = 8)
            return()
        }
        df <- tryCatch(
            if (ext == "csv") read.csv(input$file1$datapath, stringsAsFactors = FALSE, check.names = FALSE)
            else readxl::read_excel(input$file1$datapath),
            error = function(e) {
                showNotification(paste("Error loading file:", conditionMessage(e)), type = "error")
                NULL
            })
        if (is.null(df)) return()
        values$upload_names <- names(df)
        updateSelectizeInput(session, "sample_dataset", selected = "")
        updateSelectizeInput(session, "meta_sample_dataset", selected = "")
        unlink(.session_model_file())
        tryCatch(.ingest_uploaded(df), error = function(e) {
            showNotification(paste("Error loading file:", conditionMessage(e)), type = "error")
            values$data <- NULL
        })
    })

    # size gates apply only on the demo server
    .size_gate <- function(data, task) {
        if (!.scdbayes_hosted) return(TRUE)
        case_col <- find_case_var(data)
        # a case is a full tier key, so case labels repeated across studies count separately
        n_cases <- if (is.null(case_col)) 0L else dplyr::n_distinct(.with_tier(data)$.tier)
        # uploads only warn; every analysis task is blocked above the demo limits
        if (nrow(data) > .demo_max_rows || n_cases > .demo_max_cases) {
            msg <- sprintf("Large for the demo (%d rows, %d cases). The full R package is released after validation.", nrow(data), n_cases)
            showNotification(msg, type = if (task == "upload") "warning" else "error", duration = 12)
            .announce(msg)
            if (task != "upload") return(FALSE)
        }
        gate <- check_data_size(data, task)
        if (is.null(gate)) return(TRUE)
        showNotification(gate$message,
            type = if (gate$level == "block") "error" else "warning",
            duration = 12)
        .announce(gate$message)
        gate$level != "block"
    }

    # new data invalidates any earlier fit, so results, downloads and report cannot describe other data
    fit_ids <- new.env(parent = emptyenv())
    fit_ids$run <- 0
    .clear_fits <- function() {
        fit_ids$run <- fit_ids$run + 1
        values$model <- NULL
        values$model_loo <- NULL
        values$results <- NULL
        values$model_fitted <- FALSE
        values$meta_model <- NULL
        values$meta_model_loo <- NULL
        values$meta_results <- NULL
        values$meta_model_fitted <- FALSE
        values$model_stale <- FALSE
        values$meta_model_stale <- FALSE
    }

    .ingest_uploaded <- function(df) {
        .clear_fits()
        if (!.size_gate(df, "upload")) {
            values$data <- NULL
            values$processed_data <- NULL
            values$case_features <- NULL
            return(invisible(FALSE))
        }
        df <- safe_column_names(as.data.frame(df))
        renamed <- attr(df, "renamed")
        attr(df, "renamed") <- NULL
        if (length(renamed)) {
            pairs <- sprintf("'%s' -> '%s'", substr(names(renamed), 1, 40), substr(renamed, 1, 40))
            showNotification(sprintf("Renamed %d column(s) to syntactic names: %s%s",
                                     length(renamed), paste(utils::head(pairs, 5), collapse = "; "),
                                     if (length(pairs) > 5) "; ..." else "."),
                             type = "warning", duration = 12)
        }
        values$data <- df

        # numeric coercion for character/factor columns holding only numbers
        for (col in names(values$data)) {
            if (is.character(values$data[[col]]) || is.factor(values$data[[col]])) {
                test_vals <- trimws(as.character(values$data[[col]]))
                test_vals <- test_vals[!is.na(test_vals) & test_vals != ""]
                if (length(test_vals) > 0) {
                    suppressWarnings({ test_numeric <- as.numeric(test_vals) })
                    if (all(!is.na(test_numeric))) {
                        values$data[[col]] <- as.numeric(as.character(values$data[[col]]))
                    }
                }
            }
        }

        # flexible column detection
        col_names <- names(values$data)
        case_col <- find_case_var(values$data)
        if (is.null(case_col)) {
            case_col <- col_names[grepl("case|participant|subject|^id$|_id$", col_names, ignore.case = TRUE) &
                                  !grepl("study|site|school|cluster|class|group", col_names, ignore.case = TRUE)]
        }

        time_col <- find_time_var(values$data)
        if (is.null(time_col)) {
            time_col <- col_names[grepl("time|session|wave|point|observation", col_names, ignore.case = TRUE)]
            if (length(time_col) == 0) {
                role_like <- grepl("outcome|score|value|response|dv|phase|condition|treatment",
                                   col_names, ignore.case = TRUE)
                numeric_cols <- setdiff(col_names[sapply(values$data, is.numeric) & !role_like], case_col)
                time_col <- utils::head(Filter(function(col) all(diff(sort(unique(values$data[[col]]))) %in% c(0, 1)),
                                               numeric_cols), 1)
            }
        }

        phase_col <- find_phase_var(values$data)
        if (is.null(phase_col)) {
            phase_col <- col_names[grepl("phase|condition|treatment", col_names, ignore.case = TRUE)]
        }

        # binary phase indicators (A1, B1, A2, B2, M) when no phase column found
        if (length(phase_col) == 0) {
            values$data <- consolidate_phase_indicators(values$data)
            col_names <- names(values$data)
            if ("phase" %in% col_names) {
                phase_col <- "phase"
                showNotification("Reconstructed phase column from binary indicators", type = "message", duration = 5)
            }
        }

        outcome_col <- intersect(c("outcome", "score", "value", "dv", "response", "Outcome", "Score", "Value", "Response"), col_names)
        if (length(outcome_col) == 0) {
            outcome_col <- col_names[grepl("outcome|score|value|response|dv", col_names, ignore.case = TRUE)]
            if (length(outcome_col) == 0) {
                numeric_cols <- col_names[sapply(values$data, is.numeric)]
                outcome_col <- setdiff(numeric_cols, c(case_col, time_col, phase_col))
            }
        }

        # standardize detected column names
        if (length(case_col) > 0 && case_col[1] != "case") {
            names(values$data)[names(values$data) == case_col[1]] <- "case"
        }
        if (length(time_col) > 0 && time_col[1] != "time") {
            names(values$data)[names(values$data) == time_col[1]] <- "time"
        }
        if (length(phase_col) > 0 && phase_col[1] != "phase") {
            names(values$data)[names(values$data) == phase_col[1]] <- "phase"
        }
        if (length(outcome_col) > 0 && outcome_col[1] != "outcome") {
            names(values$data)[names(values$data) == outcome_col[1]] <- "outcome"
        }

        if (length(case_col) == 0 || length(phase_col) == 0 || length(outcome_col) == 0) {
            showNotification(
                paste("Could not identify required columns.",
                      "Need: case/participant, phase/condition, and outcome/score.",
                      "Found columns:", paste(col_names, collapse = ", ")),
                type = "error",
                duration = 10
            )
            values$data <- NULL
            return(invisible(FALSE))
        }

        if (length(time_col) == 0) {
            tiers <- intersect(c(tryCatch(tier_vars(values$data, NULL), error = function(e) NULL), "case"),
                               names(values$data))
            tier_key <- interaction(values$data[tiers], drop = TRUE)
            values$data$time <- stats::ave(seq_len(nrow(values$data)), tier_key, FUN = seq_along)
            showNotification(paste("No time column: sessions are numbered in row order within each case.",
                                   "Concurrence across tiers cannot be assessed."),
                             type = "warning", duration = 8)
        }

        # hierarchical structure detection
        if (exists("detect_hierarchical_structure")) {
            if (!is.null(input$outcome_var) && input$outcome_var != "" &&
                input$outcome_var %in% names(values$data)) {
                temp_data <- values$data
                if (input$outcome_var != "outcome") {
                    temp_data$outcome <- temp_data[[input$outcome_var]]
                }
                values$detected_hierarchy <- .try_step("Hierarchy detection",
                    detect_hierarchical_structure(temp_data, max_levels = 5, method = "auto"))
            } else {
                values$detected_hierarchy <- .try_step("Hierarchy detection",
                    detect_hierarchical_structure(values$data, max_levels = 5, method = "auto"))
            }
            values$model_structure <- .try_step("Level detection", detect_hierarchy_levels(values$data)) %||% list()
            if (!is.null(values$detected_hierarchy)) {
                if (length(values$detected_hierarchy$level_variables) > 0) {
                    valid_vars <- Filter(function(v) v %in% names(values$data) && length(unique(values$data[[v]])) >= 2,
                                         values$detected_hierarchy$level_variables)
                    for (i in seq_along(valid_vars)) {
                        values$model_structure[[paste0("level", i + 1, "_var")]] <- valid_vars[i]
                    }
                    values$model_structure$n_levels <- length(valid_vars) + 1
                }
                values$icc_values <- values$detected_hierarchy$icc_values
                values$hierarchy_recommendations <- values$detected_hierarchy$recommendations
                if (!is.null(values$hierarchy_recommendations$message)) {
                    showNotification(values$hierarchy_recommendations$message, type = "message", duration = 5)
                }
                if (!is.null(values$hierarchy_recommendations$estimation_message)) {
                    showNotification(values$hierarchy_recommendations$estimation_message, type = "message", duration = 5)
                }
            }
        } else {
            values$model_structure <- .try_step("Level detection", detect_hierarchy_levels(values$data)) %||% list()
        }

        design_type <- .try_step("Design detection", detect_design_type(values$data)) %||% "ab"
        .derive_features(design_type)
        invisible(TRUE)
    }

    .derive_features <- function(design_type) {
        values$processed_data <- .try_step("Phase coding",
            .process_phases(values$data, values$model_structure, design_type)) %||% values$data
        values$processed_data <- .try_step("Autocorrelation features",
            compute_autocorrelation_features(values$processed_data)) %||% values$processed_data
        values$case_features <- .try_step("Case features", compute_case_features(values$processed_data))
        values$meta_summary <- .try_step("Study summary",
            calculate_meta_summary(values$processed_data, values$model_structure))
        values$covariate_summary <- .try_step("Covariate summary",
            analyze_covariates(values$processed_data, values$model_structure))
        values$data_loaded_trigger <- runif(1)
    }

    # Sample data loader
        .load_sample <- function(csv_file, design_type, msg, preprocess = NULL) {
            unlink(.session_model_file())
            values$upload_names <- NULL
            sample_data <- .try_step("Reading the example dataset",
                read.csv(file.path("data", csv_file), stringsAsFactors = FALSE))
            if (is.null(sample_data)) return(invisible(FALSE))
            .clear_fits()
            if (!is.null(preprocess)) sample_data <- preprocess(sample_data)
            sample_data <- safe_column_names(sample_data)
            attr(sample_data, "renamed") <- NULL
            values$data <- sample_data

            values$model_structure <- .try_step("Level detection", detect_hierarchy_levels(values$data)) %||% list()
            if (exists("detect_hierarchical_structure") && "study" %in% names(sample_data) &&
                length(unique(sample_data$study)) >= 2) {
                values$detected_hierarchy <- tryCatch(
                    detect_hierarchical_structure(sample_data, max_levels = 5, method = "auto"),
                    error = function(e) NULL
                )
                if (!is.null(values$detected_hierarchy)) {
                    # only override with variables that have 2+ levels
                    valid_vars <- Filter(function(v) v %in% names(sample_data) && length(unique(sample_data[[v]])) >= 2,
                                         values$detected_hierarchy$level_variables)
                    if (length(valid_vars) > 0) {
                        for (i in seq_along(valid_vars)) {
                            values$model_structure[[paste0("level", i + 1, "_var")]] <- valid_vars[i]
                        }
                        values$model_structure$n_levels <- length(valid_vars) + 1
                    }
                    values$icc_values <- values$detected_hierarchy$icc_values
                    values$hierarchy_recommendations <- values$detected_hierarchy$recommendations
                }
            }

            .derive_features(design_type)
            showNotification(msg, type = "message", duration = 3)
            invisible(TRUE)
        }

        sample_datasets <- list(
            ab       = list("ab_design.csv", "ab", "AB design loaded (3 cases)"),
            reversal = list("reversal_design.csv", "reversal", "Reversal design loaded (3 cases)"),
            mbd      = list("multiple_baseline_design.csv", "multiple_baseline", "Multiple baseline loaded (4 cases)"),
            mp       = list("multiple_probe_design.csv", "multiple_baseline", "Multiple probe loaded (3 cases)"),
            atd      = list("alternating_treatment_design.csv", "alternating_treatment", "Alternating treatment loaded (3 cases)"),
            ccd      = list("changing_criterion_design.csv", "changing_criterion", "Changing criterion loaded (3 cases)"),
            meta     = list("meta_analysis.csv", "multiple_baseline", "Meta-analysis loaded (4 studies, 12 cases)")
        )

        observeEvent(input$sample_dataset, {
            req(input$sample_dataset != "")
            updateSelectizeInput(session, "meta_sample_dataset", selected = "")
            info <- sample_datasets[[input$sample_dataset]]
            .load_sample(info[[1]], info[[2]], info[[3]])
        })

        # Meta-Analysis > Data: multi-study example selector over the shared values$data
        observeEvent(input$meta_sample_dataset, {
            req(input$meta_sample_dataset != "")
            updateSelectizeInput(session, "sample_dataset", selected = "")
            info <- sample_datasets[[input$meta_sample_dataset]]
            .load_sample(info[[1]], info[[2]], info[[3]])
        })

        observeEvent(input$meta_file1, {
            req(input$meta_file1)
            ext <- tolower(tools::file_ext(input$meta_file1$name))
            if (!ext %in% c("csv", "xlsx", "xls")) {
                showNotification(sprintf("Unsupported file type '.%s'. Upload a .csv, .xlsx, or .xls file.", ext),
                                 type = "error", duration = 8)
                return()
            }
            df <- tryCatch({
                if (ext %in% c("xlsx", "xls")) readxl::read_excel(input$meta_file1$datapath)
                else utils::read.csv(input$meta_file1$datapath, stringsAsFactors = FALSE, check.names = FALSE)
            }, error = function(e) NULL)
            if (is.null(df)) {
                showNotification("Could not read the uploaded file.", type = "error", duration = 8)
                return()
            }
            values$upload_names <- names(df)
            updateSelectizeInput(session, "sample_dataset", selected = "")
            updateSelectizeInput(session, "meta_sample_dataset", selected = "")
            unlink(.session_model_file())
            if (isTRUE(.try_step("Loading the file", .ingest_uploaded(df)))) {
                showNotification(sprintf("Loaded %d rows from %s.", nrow(df), input$meta_file1$name),
                                 type = "message", duration = 5)
            }
        })

    observe({
        req(values$data)
        values$validation_result <- .try_step("Data validation", validate_scd_data(values$data))
    })

    # Data sub-tab outputs per nav section (prefix "" or "meta_"); meta omits Between-Phase
    register_data_outputs <- function(prefix = "", include_between_phase = TRUE) {
        id  <- function(s) paste0(prefix, s)
        inp <- function(s) input[[paste0(prefix, s)]]
        # NULL until the user picks a design, so the Data tab follows detection
        sel_design <- function() {
            sid <- if (nzchar(prefix)) "meta_research_design" else "research_design"
            if (isTRUE(values$design_override[sid])) input[[sid]]
        }

        output[[id("validationIssues")]] <- renderUI({
            req(values$validation_result)
            issues <- values$validation_result$issues
            if (length(issues) == 0) return(NULL)
            div(style = "margin-top: 8px;",
                lapply(issues, function(issue) {
                    div(style = "color: #dc3545; font-size: 15px; padding: 4px 0;",
                        tags$i(class = "fa fa-times-circle", style = "margin-right: 6px;"),
                        issue)
                }))
        })

        output[[id("validationWarnings")]] <- renderUI({
            req(values$validation_result)
            warnings <- values$validation_result$warnings
            if (length(warnings) == 0) return(NULL)
            div(style = "margin-top: 8px;",
                lapply(warnings, function(w) {
                    div(style = "color: #b45309; font-size: 15px; padding: 4px 0;",
                        tags$i(class = "fa fa-exclamation-triangle", style = "margin-right: 6px;"),
                        w)
                }))
        })

        output[[id("validationRecommendationsGeneral")]] <- renderUI({
            req(values$validation_result)
            recommendations <- values$validation_result$recommendations
            if (length(recommendations) == 0) return(NULL)
            div(style = "margin-top: 8px;",
                lapply(recommendations, function(rec) {
                    div(style = "color: #1d4ed8; font-size: 15px; padding: 4px 0;",
                        tags$i(class = "fa fa-info-circle", style = "margin-right: 6px;"),
                        rec)
                }))
        })

        output[[id("dataTable")]] <- renderDT({
            if (is.null(values$data)) {
                empty_df <- data.frame(Message = "No data loaded yet. Please upload a file.")
                return(DT::datatable(empty_df, options = list(dom = 't'),
                                     caption = .dt_caption("Uploaded data (none yet)")))
            }
            internal_cols <- c("phase_numeric", "phase_plot", "time_val",
                               "min_time_A", "min_time_B", "min_time_M", "min_time_G",
                               "time_A", "time_B", "time_M", "time_G",
                               "level_AB", "trend_AB", "level_BM", "trend_BM", "level_BG", "trend_BG",
                               "intervention_start", "stagger_group", "in_treatment",
                               "time_since_intervention",
                               "alternation_block", "criterion_phase", "is_baseline",
                               "time_in_criterion",
                               "treatment", "time_treatment",
                               "A1", "B1", "A2", "B2", "A3", "B3", "M",
                               "generalization", "mixed_problem",
                               ".obs", ".phase_change", ".vline_x",
                               "ar1", "ar2", "ar3", "acf_lag1", "acf_lag2", "acf_lag3")
            internal_cols <- c(internal_cols,
                grep("^(time|level|trend)_[A-Za-z]+[0-9]+$", names(values$data), value = TRUE))
            display_data <- values$data[, !names(values$data) %in% internal_cols, drop = FALSE]
            names(display_data) <- gsub("_", " ", names(display_data))
            names(display_data) <- gsub("\\b(\\w)", "\\U\\1", names(display_data), perl = TRUE)
            digits <- inp("display_digits") %||% 2
            display_data[sapply(display_data, is.numeric)] <- lapply(
                display_data[sapply(display_data, is.numeric)],
                function(x) round(x, digits)
            )
            dt_table(display_data, pageLength = 10, caption = "Uploaded data")
        })

        output[[id("columnRoleSelectors")]] <- renderUI({
            req(values$data)
            cols <- names(values$data)
            none_option <- c("(none)" = "")
            study_sel <- if ("study" %in% cols) "study" else ""
            cluster_sel <- if ("cluster" %in% cols) "cluster" else
                           if ("group" %in% cols) "group" else ""
            case_sel <- if ("case" %in% cols) "case" else ""
            phase_sel <- if ("phase" %in% cols) "phase" else ""
            time_sel <- if ("time" %in% cols) "time" else ""
            outcome_sel <- if ("outcome" %in% cols) "outcome" else ""
            fluidRow(
                column(6,
                    selectInput(id("role_study"), "Study:", choices = c(none_option, cols), selected = study_sel, width = "100%"),
                    selectInput(id("role_cluster"), "Cluster / Group:", choices = c(none_option, cols), selected = cluster_sel, width = "100%"),
                    selectInput(id("role_case"), "Case / Participant:", choices = c(none_option, cols), selected = case_sel, width = "100%")
                ),
                column(6,
                    selectInput(id("role_phase"), "Phase / Condition:", choices = c(none_option, cols), selected = phase_sel, width = "100%"),
                    selectInput(id("role_time"), "Time / Session:", choices = c(none_option, cols), selected = time_sel, width = "100%"),
                    selectInput(id("role_outcome"), "Outcome:", choices = c(none_option, cols), selected = outcome_sel, width = "100%")
                )
            )
        })

        .apply_roles <- function() {
            req(values$data)
            d <- values$data
            role_map <- list(
                study = inp("role_study"),
                cluster = inp("role_cluster"),
                case = inp("role_case"),
                phase = inp("role_phase"),
                time = inp("role_time"),
                outcome = inp("role_outcome")
            )
            for (role in names(role_map)) {
                src <- role_map[[role]]
                if (!is.null(src) && src != "" && src %in% names(d) && src != role) {
                    if (role %in% names(d) && role != src) names(d)[names(d) == role] <- paste0("_orig_", role)
                    names(d)[names(d) == src] <- role
                }
            }
            values$data <- d
        }
        observeEvent(input[[id("role_study")]],   .apply_roles(), ignoreInit = TRUE)
        observeEvent(input[[id("role_cluster")]], .apply_roles(), ignoreInit = TRUE)
        observeEvent(input[[id("role_case")]],    .apply_roles(), ignoreInit = TRUE)
        observeEvent(input[[id("role_phase")]],   .apply_roles(), ignoreInit = TRUE)
        observeEvent(input[[id("role_time")]],    .apply_roles(), ignoreInit = TRUE)
        observeEvent(input[[id("role_outcome")]], .apply_roles(), ignoreInit = TRUE)

        output[[id("dataUploaded")]] <- reactive({ !is.null(values$data) })
        outputOptions(output, id("dataUploaded"), suspendWhenHidden = FALSE)

        output[[id("isCCD")]] <- reactive({
            req(values$data)
            identical(.design_choice(sel_design(), values$data), "changing_criterion")
        })
        outputOptions(output, id("isCCD"), suspendWhenHidden = FALSE)

        output[[id("continuousVarsTable")]] <- renderDT({
            req(values$data)
            num_vars <- names(values$data)[sapply(values$data, is.numeric)]
            shiny::validate(need(length(num_vars) > 0, "No numeric variables"))
            summary_df <- do.call(rbind, lapply(num_vars, function(v) {
                vals <- values$data[[v]][!is.na(values$data[[v]])]
                data.frame(
                    Variable = v,
                    N = length(vals),
                    Mean = round(mean(vals), 2),
                    SD = round(sd(vals), 2),
                    Median = round(median(vals), 2),
                    Min = round(min(vals), 2),
                    Max = round(max(vals), 2),
                    stringsAsFactors = FALSE
                )
            }))
            dt_table(summary_df, digits = 2, caption = "Continuous variable summaries")
        })

        output[[id("categoricalVarsTable")]] <- renderDT({
            req(values$data)
            cat_vars <- names(values$data)[!sapply(values$data, is.numeric)]
            shiny::validate(need(length(cat_vars) > 0, "No categorical variables"))
            summary_df <- do.call(rbind, lapply(cat_vars, function(v) {
                lvls <- unique(values$data[[v]])
                data.frame(
                    Variable = v,
                    Levels = length(lvls),
                    Values = paste(head(lvls, 8), collapse = ", "),
                    Mode = names(sort(table(values$data[[v]]), decreasing = TRUE))[1],
                    stringsAsFactors = FALSE
                )
            }))
            dt_table(summary_df, caption = "Categorical variable summaries")
        })

        output[[id("processedDataTable")]] <- renderDT({
            req(values$processed_data)
            pd <- values$processed_data
            piecewise_cols <- c("case", "time", "phase", "outcome",
                "phase_numeric", "time_A", "time_B", "time_M", "time_G",
                "level_AB", "trend_AB", "level_BM", "trend_BM", "level_BG", "trend_BG",
                "A1", "B1", "A2", "B2",
                grep("^(time|level|trend)_[A-Za-z]+[0-9]+$", names(pd), value = TRUE),
                "intervention_start", "stagger_group",
                "time", "alternation_block",
                "criterion_phase", "is_baseline", "time_in_criterion",
                "treatment", "time_treatment")
            show_cols <- intersect(piecewise_cols, names(pd))
            display <- pd[, show_cols, drop = FALSE]
            numeric_cols <- names(display)[sapply(display, is.numeric)]
            display[numeric_cols] <- lapply(display[numeric_cols], function(x) round(x, 3))
            dt_table(display, pageLength = 20, digits = 3, caption = "Processed data with piecewise terms")
        })

        output[[id("caseFeaturesTable")]] <- renderDT({
            req(values$case_features)
            df <- values$case_features
            drop_cols <- intersect(c("NAP", "PND"), names(df))
            if (length(drop_cols) > 0) df <- df[, !names(df) %in% drop_cols, drop = FALSE]
            dt_table(df, digits = 3, caption = "Case-level features")
        })

        output[[id("featureDistPlot")]] <- plotly::renderPlotly({
            req(values$case_features)
            df <- values$case_features
            key_cols <- intersect(c("mean_diff", "pooled_SD", "autocor_A", "autocor_B"), names(df))
            if (length(key_cols) == 0) {
                return(empty_plot("No features available"))
            }
            id_col <- if ("tier" %in% names(df)) "tier" else "case"
            long <- tidyr::pivot_longer(df[, c(id_col, key_cols)], cols = dplyr::all_of(key_cols),
                                        names_to = "feature", values_to = "value")
            p <- ggplot(long, aes(x = feature, y = value, fill = feature)) +
                geom_boxplot(alpha = 0.7) +
                geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
                theme_minimal(base_size = 13) +
                theme(legend.position = "none",
                      axis.text.x = element_text(size = 13, angle = 30, hjust = 1),
                      axis.text.y = element_text(size = 13),
                      axis.title.y = element_text(size = 14, face = "bold")) +
                labs(x = NULL, y = "Value")
            styled_ggplotly(p)
        })

        output[[id("facet_var_selector")]] <- renderUI({
            req(values$data)
            grouping_candidates <- intersect(
                c("study", "cluster", "stagger_group", "intervention_type"),
                names(values$data))
            if (length(grouping_candidates) > 0) {
                default_sel <- if ("study" %in% grouping_candidates &&
                                   length(unique(values$data$study)) >= 2) "study" else ""
                selectInput(id("facet_group_var"), "Group by",
                            choices = c("(none)" = "", grouping_candidates),
                            selected = default_sel)
            }
        })

        # start at the viewport width on a narrow screen rather than a flat 900px
        observeEvent(input$viewport_width, {
            vw <- suppressWarnings(as.numeric(input$viewport_width))
            if (isTRUE(vw > 0 && vw < 900))
                updateSliderInput(session, id("plot_width"), value = max(280, floor(vw) - 40))
        }, once = TRUE, ignoreNULL = TRUE)

        # one stacked panel per tier, so the default height follows the tier count
        observeEvent(values$data, {
            d <- values$data
            n <- if ("case" %in% names(d)) length(unique(.with_tier(d)$.tier)) else 1
            updateSliderInput(session, id("plot_height"),
                              value = .facet_px(n, per_row = 170, min_px = 400, ncol = 1, extra = 120))
        }, ignoreNULL = TRUE)

        output[[id("rawDataPlotUI")]] <- renderUI({
            h <- inp("plot_height") %||% 600
            w <- inp("plot_width") %||% 900
            plotOutput(id("rawDataPlot"), height = paste0(h, "px"), width = paste0(w, "px"))
        })

        output[[id("rawDataPlot")]] <- renderPlot({
            req(values$data)
            data <- values$data
            req("outcome" %in% names(data), "case" %in% names(data))
            phase_var <- find_phase_var(data)
            time_var <- find_time_var(data)
            if (is.null(time_var)) data$.obs <- seq_len(nrow(data))
            x_col <- time_var %||% ".obs"
            data <- .with_tier(data)
            if (!is.null(phase_var)) {
                # numeric 0/1 codes become discrete labels for the qualitative palette
                ph <- as.character(data[[phase_var]])
                data$.phase_lab <- factor(ph, levels = unique(ph[order(data[[x_col]])]))
                phase_var <- ".phase_lab"
                data <- data %>%
                    group_by(.tier) %>%
                    arrange(.data[[x_col]]) %>%
                    mutate(.phase_piece = cumsum(c(1, diff(as.numeric(factor(.data[[phase_var]]))) != 0))) %>%
                    ungroup()
            }
            p <- ggplot(data, aes(x = .data[[x_col]], y = .data[["outcome"]]))
            if (!is.null(phase_var)) {
                p <- p +
                    geom_line(aes(color = .data[[phase_var]],
                                  group = interaction(.tier, .phase_piece)),
                              linewidth = 0.5) +
                    geom_point(aes(color = .data[[phase_var]]), size = 2)
            } else {
                p <- p + geom_point(size = 2) + geom_line(aes(group = .tier), linewidth = 0.5)
            }
            if (isTRUE(inp("show_phase_lines")) && !is.null(phase_var)) {
                transitions <- data %>%
                    group_by(.tier) %>%
                    arrange(.data[[x_col]]) %>%
                    filter(.phase_piece != lag(.phase_piece, default = .phase_piece[1])) %>%
                    mutate(.vline_x = .data[[x_col]] - 0.5) %>%
                    ungroup()
                if (nrow(transitions) > 0) {
                    p <- p + geom_vline(data = transitions,
                        aes(xintercept = .vline_x),
                        linetype = "longdash", linewidth = 0.5, color = "#4c4c4c")
                }
            }
            if (isTRUE(inp("show_phase_means")) && !is.null(phase_var)) {
                phase_means <- data %>%
                    group_by(.tier, .phase_piece, .data[[phase_var]]) %>%
                    summarise(
                        y_mean = mean(.data[["outcome"]], na.rm = TRUE),
                        x_min = min(.data[[x_col]], na.rm = TRUE),
                        x_max = max(.data[[x_col]], na.rm = TRUE),
                        .groups = "drop"
                    )
                p <- p + geom_segment(
                    data = phase_means,
                    aes(x = x_min, xend = x_max, y = y_mean, yend = y_mean,
                        color = .data[[phase_var]]),
                    linetype = "solid", linewidth = 0.4
                )
            }
            if (isTRUE(inp("show_trend_lines")) && !is.null(phase_var)) {
                p <- p + geom_smooth(
                    aes(color = .data[[phase_var]],
                        group = interaction(.tier, .phase_piece)),
                    method = "lm", formula = y ~ x, se = FALSE,
                    linetype = "dashed", linewidth = 0.5)
            }
            facet_var <- inp("facet_group_var")
            if (!is.null(facet_var) && facet_var != "" && facet_var %in% names(data)) {
                p <- p + ggh4x::facet_wrap2(ggplot2::vars(.data[[facet_var]], .data[[".tier"]]),
                                            ncol = 1, axes = "all", remove_labels = "all")
            } else {
                p <- p + ggh4x::facet_wrap2(~.tier, ncol = 1, axes = "all", remove_labels = "all")
            }
            p <- p +
                scale_y_continuous(breaks = function(lim) {
                    brks <- pretty(lim)
                    brks[brks == floor(brks)]
                }) +
                scale_color_brewer(type = "qual", palette = 2) +
                theme_minimal(base_size = 16) +
                theme(
                    legend.position = "top",
                    legend.text = element_text(size = 16),
                    legend.title = element_text(size = 16),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.line = element_line(),
                    axis.line.x = element_line(color = "#3B3B3B"),
                    axis.text.x = element_text(size = 16, color = "#3B3B3B"),
                    axis.ticks = element_line(color = "#3B3B3B"),
                    axis.ticks.length = unit(0.15, "cm"),
                    axis.text.y = element_text(size = 16, color = "#3B3B3B"),
                    axis.title.x = element_text(face = "bold", size = 18, color = "#3B3B3B",
                                                margin = margin(t = 10)),
                    axis.title.y = element_text(face = "bold", size = 18, color = "#3B3B3B",
                                                margin = margin(r = 10)),
                    strip.text = element_text(size = 16, face = "bold", color = "#3B3B3B")
                ) +
                labs(x = if (!is.null(time_var)) "Sessions" else "Observation",
                     y = "Outcome", color = "Phase")
            p
        }, alt = "Line graph of outcome by session for each case, colored by phase")

        output[[id("phaseDescriptivesTable")]] <- renderDT({
            req(values$data)
            data <- values$data
            req("outcome" %in% names(data), "phase" %in% names(data), "case" %in% names(data))
            x_col <- if ("time" %in% names(data)) "time" else NULL
            data <- .with_tier(data)
            data$case <- data$.tier
            if (!is.null(x_col)) data <- data[order(data$case, data$time), ]
            atd <- identical(.design_choice(sel_design(), values$data), "alternating")
            # one row per run within each tier: a repeated label becomes A1, A2, ...; ATD pools each condition
            data <- data %>%
                group_by(case) %>%
                mutate(run = if (atd) 1L else cumsum(dplyr::coalesce(c(TRUE, as.character(phase)[-1] != as.character(phase)[-n()]), TRUE))) %>%
                group_by(case, phase) %>%
                mutate(label = if (n_distinct(run) > 1)
                                   paste0(phase, ifelse(nchar(as.character(phase)) > 1, " ", ""), dense_rank(run))
                               else as.character(phase)) %>%
                ungroup() %>%
                mutate(phase = label)
            phase_desc <- data %>%
                group_by(case, run, phase) %>%
                summarise(
                    N = sum(!is.na(outcome)),
                    Level = mean(outcome, na.rm = TRUE),
                    Median = median(outcome, na.rm = TRUE),
                    SD = sd(outcome, na.rm = TRUE),
                    Range = paste(min(outcome, na.rm = TRUE), "-", max(outcome, na.rm = TRUE)),
                    .groups = "drop"
                )
            # one fit per run; a singular fit (constant session value) has no slope row
            phase_trend <- function(y, x) {
                cf <- tryCatch(suppressWarnings(summary(stats::lm(y ~ x))$coefficients),
                               error = function(e) NULL)
                if (is.null(cf) || nrow(cf) < 2) c(NA_real_, NA_real_) else unname(cf[2, 1:2])
            }
            trend_data <- if (is.null(x_col)) NULL else data %>%
                group_by(case, run, phase) %>%
                filter(sum(!is.na(outcome)) >= 3)
            if (!is.null(trend_data) && nrow(trend_data) > 0) {
                trends <- trend_data %>%
                    summarise(fit = list(phase_trend(outcome, .data[[x_col]])), .groups = "drop") %>%
                    mutate(Trend = vapply(fit, `[`, numeric(1), 1L),
                           SE = vapply(fit, `[`, numeric(1), 2L),
                           fit = NULL)
                phase_desc <- left_join(phase_desc, trends, by = c("case", "run", "phase"))
            }
            phase_desc$run <- NULL
            names(phase_desc)[names(phase_desc) == "case"] <- "Case"
            names(phase_desc)[names(phase_desc) == "phase"] <- "Phase"
            dt_table(phase_desc, digits = 2, caption = "Descriptive statistics by case and phase")
        })

        output[[id("baselineTrendTable")]] <- renderDT({
            req(values$data)
            data <- values$data
            req("outcome" %in% names(data), "phase" %in% names(data), "case" %in% names(data))
            data <- .with_tier(data)
            data$case <- data$.tier
            if ("time" %in% names(data)) data <- data[order(data$case, data$time), ]
            improvement <- inp("va_es_improvement") %||%
                (if (nzchar(prefix)) input$meta_intervention_goal else input$intervention_goal) %||% "increase"
            atd <- identical(.design_choice(sel_design(), values$data), "alternating")
            # WWC 5.0 risk-of-bias criterion: last three baseline points against all earlier ones
            baseline_nap <- function(y) {
                if (length(y) >= 3 && stats::sd(y) == 0) return(0.5)
                if (length(y) < 4) return(NA_real_)
                pairs <- outer(tail(y, 3), head(y, -3), function(l, e) (l > e) + 0.5 * (l == e))
                nap <- mean(pairs)
                if (improvement == "decrease") 1 - nap else nap
            }
            rows <- lapply(unique(data$case), function(cs) {
                cd <- data[data$case == cs, ]
                first_run <- rle(as.character(cd$phase))$lengths[1]
                baseline <- cd[seq_len(first_run), ]
                y <- baseline$outcome[!is.na(baseline$outcome)]
                slope <- if (length(y) >= 3 && "time" %in% names(baseline)) {
                    unname(coef(lm(outcome ~ time, data = baseline))[2])
                } else NA_real_
                nap <- if (atd) NA_real_ else baseline_nap(y)
                risk <- if (atd) "exempt (alternating treatments)"
                        else if (is.na(nap)) "n/a (< 4 points)"
                        else if (stats::sd(y) == 0) "limited (constant baseline)"
                        else if (nap > 0.85) "elevated" else "limited"
                data.frame(
                    Case = cs,
                    N = length(y),
                    Slope = slope,
                    `Trend NAP` = nap,
                    `Risk of Bias` = risk,
                    check.names = FALSE,
                    stringsAsFactors = FALSE
                )
            })
            dt_table(do.call(rbind, rows), digits = 3, caption = "Baseline trend and risk of bias by case")
        })

        wwc_check <- reactive({
            req(values$data)
            d <- values$data
            req(all(c("outcome", "phase") %in% names(d)), !is.null(find_case_var(d)))
            goal <- inp("va_es_improvement") %||%
                (if (nzchar(prefix)) input$meta_intervention_goal else input$intervention_goal) %||% "increase"
            ioa_val <- inp("wwc_ioa_value")
            ioa_cov <- inp("wwc_ioa_coverage")
            args <- list(data = d, outcome_var = "outcome", intervention_goal = goal,
                         ioa_value = if (isTRUE(is.finite(ioa_val))) ioa_val,
                         ioa_coverage = if (isTRUE(is.finite(ioa_cov))) ioa_cov,
                         ioa_type = inp("wwc_ioa_type") %||% "percentage")
            # a design picked by the user is passed as is; detection may read planned gaps as multiple probe
            args$design <- sel_design()
            tryCatch(do.call(wwc_design_check, args), error = function(e) e)
        })

        output[[id("wwcSummary")]] <- renderUI({
            res <- wwc_check()
            if (inherits(res, "error")) {
                return(div(class = "status-main-warning", "WWC check unavailable: ", conditionMessage(res)))
            }
            color <- if (grepl("Without", res$rating)) "#166534"
                     else if (grepl("With Reservations", res$rating)) "#92400e" else "#991b1b"
            div(style = sprintf("border-left: 4px solid %s; padding: 8px 12px; margin: 8px 0; background: #f9fafb;", color),
                tags$p(style = "font-size: 17px; margin: 0 0 4px;",
                       tags$strong("Eligible rating: "), res$rating,
                       sprintf(" (screened as %s; highest rating across experiments)", gsub("_", " ", res$design))),
                tags$p(style = "font-size: 13px; color: #374151; margin: 0;",
                       sprintf("IOA %s: %s.", res$ioa$status, res$ioa$detail)))
        })

        output[[id("wwcExperimentTable")]] <- renderDT({
            res <- wwc_check()
            shiny::validate(need(!inherits(res, "error"), "WWC check unavailable for these data."))
            dt_table(res$experiments, caption = "Experiments detected in the design")
        })

        output[[id("wwcTierTable")]] <- renderDT({
            res <- wwc_check()
            shiny::validate(need(!inherits(res, "error"), "WWC check unavailable for these data."))
            dt_table(res$tiers, caption = "Phases per tier")
        })

        output[[id("wwcTrendTable")]] <- renderDT({
            res <- wwc_check()
            shiny::validate(need(!inherits(res, "error"), "WWC check unavailable for these data."))
            dt_table(res$baseline_trend, digits = 3, caption = "Baseline trend by tier")
        })

        output[[id("wwcReversibilityTable")]] <- renderDT({
            res <- wwc_check()
            shiny::validate(need(!inherits(res, "error") && nrow(res$reversibility) > 0,
                                 "Reversibility applies to reversal designs."))
            dt_table(res$reversibility, digits = 3, caption = "Reversibility by tier")
        })

        output[[id("immediacyTable")]] <- renderDT({
            req(values$data)
            d <- values$data
            req("outcome" %in% names(d), "phase" %in% names(d), "case" %in% names(d))
            d <- .with_tier(d)
            d$case <- d$.tier
            if ("time" %in% names(d)) d <- d[order(d$case, d$time), ]
            shiny::validate(need(!identical(.design_choice(sel_design(), d), "alternating"),
                                 "Immediacy applies to phase changes; alternating treatments have no phase transitions."))
            orient <- if (identical(inp("va_es_improvement") %||% "increase", "decrease")) -1 else 1
            rows <- do.call(rbind, lapply(split(d, d$case), function(cd) {
                cd <- cd[!is.na(cd$outcome), , drop = FALSE]
                runs <- rle(as.character(cd$phase))
                if (length(runs$values) < 2) return(NULL)
                ends <- cumsum(runs$lengths)
                starts <- c(1, head(ends, -1) + 1)
                role <- phase_role(runs$values)
                do.call(rbind, lapply(seq_len(length(runs$values) - 1), function(k) {
                    pre <- tail(cd$outcome[starts[k]:ends[k]], 3)
                    post <- head(cd$outcome[starts[k + 1]:ends[k + 1]], 3)
                    # a withdrawal (intervention -> baseline) is expected to move against the goal,
                    # so its change is reversed: + always means the change the design predicts
                    withdrawal <- isTRUE(role[k] >= 1) && isTRUE(role[k + 1] == 0)
                    expect <- orient * (if (withdrawal) -1 else 1)
                    data.frame(
                        Case = cd$case[1],
                        Transition = paste(runs$values[k], "→", runs$values[k + 1]),
                        `n pre` = length(pre),
                        `Last 3 (pre)` = mean(pre),
                        `n post` = length(post),
                        `First 3 (post)` = mean(post),
                        `Change (+ = expected direction)` = expect * (mean(post) - mean(pre)),
                        check.names = FALSE,
                        stringsAsFactors = FALSE)
                }))
            }))
            shiny::validate(need(!is.null(rows) && nrow(rows) > 0,
                                 "Immediacy needs at least one phase transition per case."))
            rownames(rows) <- NULL
            dt_table(rows, digits = 2, caption = "Immediacy of effect by case")
        })

        if (include_between_phase) {
            va_es_data <- reactive({
                req(values$data)
                req("outcome" %in% names(values$data), "phase" %in% names(values$data),
                    "case" %in% names(values$data))
                es_metrics <- inp("va_es_metrics")
                req(length(es_metrics) > 0)
                d <- values$data
                design <- .design_choice(sel_design(), d)
                pd <- values$processed_data
                if (design == "changing_criterion" && !is.null(pd) &&
                    all(c("criterion_phase", "is_baseline", "case", "outcome") %in% names(pd))) {
                    d <- pd
                    d$phase <- ifelse(d$is_baseline == 1, "A", paste0("C", d$criterion_phase - 1))
                }
                d <- .with_tier(d)
                d$case <- d$.tier
                session_arg <- if ("time" %in% names(d)) rlang::sym("time") else NULL
                if ("time" %in% names(d)) d <- d[order(d$case, d$time), ]
                ccd_option <- inp("ccd_analysis_option") %||% "baseline_last"
                goal <- inp("va_es_improvement") %||% "increase"
                # trend correction assumes a baseline that directly precedes the contrasted phase
                trend_corrected <- "Tau_BC"
                es_pair <- function(dat, base, interv, reversal = FALSE, adjacent = TRUE) {
                    metrics <- if (adjacent) es_metrics else setdiff(es_metrics, trend_corrected)
                    if (length(metrics) == 0) return(NULL)
                    # one call per metric, so an edge case in one index (e.g. Tau-BC with a
                    # one-point baseline) does not drop every other index for the contrast
                    out <- lapply(metrics, function(metric) tryCatch(suppressWarnings(
                        SingleCaseES::batch_calc_ES(
                            dat = dat[dat$phase %in% c(base, interv), ],
                            grouping = case, condition = phase,
                            outcome = outcome, session_number = !!session_arg,
                            baseline_phase = base, intervention_phase = interv,
                            ES = metric,
                            improvement = if (reversal) setdiff(c("increase", "decrease"), goal) else goal,
                            scale = inp("va_es_scale") %||% "other",
                            confidence = inp("va_es_confidence") %||% 0.95,
                            format = "long"
                        )), error = function(e) NULL))
                    out <- dplyr::bind_rows(out)
                    if (!nrow(out)) return(NULL)
                    out %>% mutate(Comparison = trimws(paste(base, "vs", interv, if (reversal) "(reversal)" else "")))
                }
                results <- if (design == "alternating") {
                    conds <- unique(as.character(d$phase[order(d$time %||% seq_len(nrow(d)))]))
                    ref <- inp("atd_reference")
                    if (is.null(ref) || !ref %in% conds) ref <- c(conds[phase_role(conds) %in% 0], conds)[1]
                    others <- setdiff(conds, ref)
                    pairs <- c(lapply(others, function(cn) c(ref, cn)),
                               if (length(others) >= 2) utils::combn(others, 2, simplify = FALSE))
                    lapply(split(d, d$case), function(cd)
                        lapply(pairs, function(p) es_pair(cd, p[1], p[2], adjacent = FALSE)))
                } else {
                    lapply(split(d, d$case), function(cd) {
                        runs <- rle(as.character(cd$phase))
                        n <- length(runs$values)
                        if (n < 2) return(NULL)
                        run_labels <- if (anyDuplicated(runs$values) > 0) {
                            paste0(runs$values, ave(seq_len(n), runs$values, FUN = seq_along))
                        } else runs$values
                        cd$phase <- rep(run_labels, runs$lengths)
                        role <- phase_role(runs$values)
                        is_base <- if (all(is.na(role))) runs$values == runs$values[1] else role %in% 0
                        pairs <- if (design == "reversal") {
                            lapply(which(is_base[-n] != is_base[-1]), function(k) c(k, k + 1))
                        } else if (design == "changing_criterion" && identical(ccd_option, "baseline_last")) {
                            list(c(1, n))
                        } else if (design == "changing_criterion") {
                            lapply(seq_len(n - 1), function(k) c(k, k + 1))
                        } else {
                            list(c(1, 2))
                        }
                        lapply(pairs, function(p) es_pair(cd, run_labels[p[1]], run_labels[p[2]],
                                                          reversal = !is_base[p[1]] && is_base[p[2]],
                                                          adjacent = p[2] - p[1] == 1))
                    })
                }
                out <- dplyr::bind_rows(Filter(Negate(is.null), unlist(results, recursive = FALSE)))
                if (nrow(out) == 0) NULL else out
            })

            output[[id("atdReferenceUI")]] <- renderUI({
                req(values$data, "phase" %in% names(values$data))
                d <- values$data
                if (!identical(.design_choice(sel_design(), d), "alternating")) return(NULL)
                conds <- unique(as.character(d$phase[order(d$time %||% seq_len(nrow(d)))]))
                default_ref <- c(conds[phase_role(conds) %in% 0], conds)[1]
                tagList(
                    selectInput(id("atd_reference"), "Reference condition",
                                choices = conds, selected = default_ref),
                    helpText(info_link("atd_contrast"),
                        "Each condition is compared with the reference and with every other treatment.",
                        "The contrasts ignore session order, so carryover effects are not ruled out."))
            })

            output[[id("reversibilityTable")]] <- renderDT({
                req(values$data)
                d <- values$data
                req(all(c("outcome", "phase", "case") %in% names(d)))
                shiny::validate(need(identical(.design_choice(sel_design(), d), "reversal"),
                                     "Reversibility applies to reversal (withdrawal) designs."))
                d <- .with_tier(d)
                d$case <- d$.tier
                if ("time" %in% names(d)) d <- d[order(d$case, d$time), ]
                goal <- inp("va_es_improvement") %||% "increase"
                rows <- lapply(split(d, d$case), function(cd) {
                    runs <- rle(as.character(cd$phase))
                    idx <- rep(seq_along(runs$values), runs$lengths)
                    base_runs <- which(phase_role(runs$values) %in% 0)
                    if (length(base_runs) < 2) return(NULL)
                    initial <- cd$outcome[idx == base_runs[1]]
                    lapply(base_runs[-1], function(k) {
                        nap <- tryCatch(SingleCaseES::NAP(A_data = initial, B_data = cd$outcome[idx == k],
                                                          improvement = goal)$Est,
                                        error = function(e) NA_real_)
                        data.frame(Case = cd$case[1],
                                   Comparison = sprintf("initial baseline vs baseline run %d", match(k, base_runs)),
                                   NAP = nap,
                                   Reversibility = ifelse(is.na(nap), "n/a",
                                       ifelse(nap <= 0.85, "met: behavior returned to baseline (NAP <= .85)",
                                              "not met: behavior did not return (NAP > .85)")),
                                   check.names = FALSE, stringsAsFactors = FALSE)
                    })
                })
                pieces <- unlist(Filter(Negate(is.null), rows), recursive = FALSE)
                shiny::validate(need(length(pieces) > 0,
                                     "Reversibility needs at least two baseline phases per case."))
                out <- do.call(rbind, pieces)
                rownames(out) <- NULL
                dt_table(out, digits = 3, caption = "Reversibility by case")
            })

            output[[id("ccdPcdTable")]] <- renderDT({
                req(values$data)
                d <- values$data
                crit <- grep("^(crit|criterion)(_(level|value))?$", names(d), ignore.case = TRUE, value = TRUE)
                crit <- Filter(function(v) is.numeric(d[[v]]), crit)
                shiny::validate(need(length(crit) > 0,
                    "PCD needs a numeric criterion column (criterion, crit, criterion_level, or criterion_value)."))
                crit <- crit[1]
                p <- inp("pcd_proportion") %||% 0.5
                d <- .with_tier(d)
                d$case <- d$.tier
                if ("time" %in% names(d)) d <- d[order(d$case, d$time), ]
                rows <- lapply(split(d, d$case), function(cd) {
                    is_bl <- phase_role(cd$phase) %in% 0
                    base_y <- cd$outcome[is_bl]
                    tx <- cd[!is_bl & !is.na(cd[[crit]]), ]
                    if (length(base_y) < 1 || nrow(tx) == 0) return(NULL)
                    seg <- cumsum(c(TRUE, diff(tx[[crit]]) != 0))
                    cl <- tapply(tx[[crit]], seg, `[`, 1)
                    half <- Reduce(function(prev, j) max(0, (abs(cl[j] - cl[j - 1]) - prev) * p),
                                   seq_along(cl)[-1], accumulate = TRUE,
                                   init = diff(range(base_y, na.rm = TRUE)) * p)
                    data.frame(Case = cd$case[1], `Criterion level` = as.numeric(cl),
                               `Half-width` = half,
                               `Lower limit` = as.numeric(cl) - half, `Upper limit` = as.numeric(cl) + half,
                               N = as.integer(table(seg)),
                               `PCD (%)` = 100 * as.numeric(tapply(seq_along(seg), seg, function(i)
                                   mean(abs(tx$outcome[i] - tx[[crit]][i]) <= half[seg[i[1]]], na.rm = TRUE))),
                               check.names = FALSE, stringsAsFactors = FALSE)
                })
                out <- do.call(rbind, Filter(Negate(is.null), rows))
                shiny::validate(need(!is.null(out) && nrow(out) > 0, "No criterion subphases found."))
                rownames(out) <- NULL
                dt_table(out, digits = 2, caption = "Percentage of conforming data by case")
            })

            output[[id("vaEffectSizeTable")]] <- renderDT({
                es_result <- va_es_data()
                shiny::validate(need(!is.null(es_result), "Could not compute effect sizes. Check data format."))
                es_type <- c(
                    LRRd = "Parametric", LRRi = "Parametric",
                    NAP = "Non-parametric", Tau = "Non-parametric", `Tau-BC` = "Non-parametric",
                    PND = "Non-parametric", PEM = "Non-parametric", PAND = "Non-parametric",
                    IRD = "Non-parametric"
                )
                display <- es_result %>%
                    select(case, Comparison, ES, Est, SE, CI_lower, CI_upper) %>%
                    mutate(
                        Metric = es_type[ES],
                        across(where(is.numeric), ~ round(., 3))
                    ) %>%
                    select(Case = case, Comparison, Metric, `Effect Size` = ES, Estimate = Est,
                           `Std Error` = SE, `CI Lower` = CI_lower, `CI Upper` = CI_upper)
                dt_table(display, digits = 3, caption = "Visual-analysis effect sizes by case")
            })
        }
        wwc_check
    }

    wwc_checks <- list(scd  = register_data_outputs("",      include_between_phase = TRUE),
                       meta = register_data_outputs("meta_", include_between_phase = FALSE))

    output$autocorrAlert <- renderUI({
        cases <- if (!is.null(values$validation_result)) values$validation_result$autocorr_cases else character()
        has_flag <- length(cases) > 0
        clr <- if (has_flag) "#b45309" else "#337ab7"
        ico <- if (has_flag) "triangle-exclamation" else "info-circle"
        actionLink("showAR1Guide", icon(ico),
                   style = paste0("color: ", clr, "; font-size: 13px;"),
                   title = "Autocorrelation guide", `aria-label` = "Autocorrelation guide")
    })

    observeEvent(input$showAR1Guide, {
        cases <- if (!is.null(values$validation_result)) values$validation_result$autocorr_cases else character()
        flag_html <- if (length(cases) > 0) {
            paste0("<p style='color: #b45309; margin-top: 12px;'><strong>",
                   icon("triangle-exclamation"),
                   " High autocorrelation (|r| > .5) detected:</strong> ",
                   htmltools::htmlEscape(paste(cases, collapse = ", ")), "</p>")
        } else ""

        showModal(info_popup_modal(
            title = "AR(1) Autocorrelation",
            body = HTML(paste0(
                "<p>AR(1) models correlation between consecutive observations within each case. ",
                "Enable it when adjacent residuals are correlated, for more accurate intervals.</p>",
                flag_html)),
            citation = "Hox, J. J. (2010). <em>Multilevel analysis: Techniques and applications</em> (2nd ed.). Routledge.",
            size = "m"
        ))
    })

    # Design Recommendations
    output$designAiRecommendation <- renderUI({
        req(values$data)
        design <- detect_design_type(values$data)
        analysis <- analyze_design_patterns(values$data)
        sprintf("Detected: %s", design)
    })

    # runs after data load and after the lazy Model tab renders, so its inputs exist
    observeEvent(list(values$data, model_tab_loaded()), {
        req(values$data, model_tab_loaded())
        numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
        updateSelectInput(session, "outcome_var",
                        choices = numeric_vars,
                        selected = if("outcome" %in% numeric_vars) "outcome" else numeric_vars[1])

        all_vars <- names(values$data)
        potential_moderators <- all_vars[!all_vars %in% c("outcome", "phase", "time", "session")]
        updateSelectInput(session, "moderator_vars",
                        choices = potential_moderators)
    })

    # both design selectors follow detection on every data load, on any tab, until the user picks a design
    .design_ids <- c("research_design", "meta_research_design")
    .detected_design <- function(d) {
        det <- tryCatch(detect_design_type(d), error = function(e) "ab")
        if (det %in% c("ab", "reversal", "multiple_baseline", "multiple_probe",
                       "alternating", "changing_criterion")) det else "ab"
    }
    .push_design <- function(ids) {
        target <- isolate(values$design_target)
        if (is.null(target)) return(invisible(NULL))
        lapply(ids, function(sid) updateSelectInput(session, sid, selected = target))
        invisible(NULL)
    }
    observeEvent(values$data_loaded_trigger, {
        values$design_override <- stats::setNames(c(FALSE, FALSE), .design_ids)
    }, priority = 10)
    observeEvent(values$data, {
        values$design_target <- .detected_design(values$data)
        .push_design(Filter(function(sid) !isTRUE(values$design_override[sid]), .design_ids))
    })
    # the first value of a selector is its UI default, not a user choice
    design_seen <- stats::setNames(c(FALSE, FALSE), .design_ids)
    lapply(.design_ids, function(sid) {
        observeEvent(input[[sid]], {
            if (!design_seen[[sid]]) {
                design_seen[[sid]] <<- TRUE
                if (!identical(input[[sid]], isolate(values$design_target))) .push_design(sid)
                return()
            }
            target <- isolate(values$design_target)
            if (is.null(target)) return()
            values$design_override[sid] <- !identical(input[[sid]], target)
        })
    })

    # one intervention direction for the fit, the prior mean, and the non-overlap effect sizes
    observeEvent(input$intervention_goal, {
        goal <- input$intervention_goal %||% "increase"
        if (!identical(input$va_es_improvement, goal)) {
            updateRadioButtons(session, "va_es_improvement", selected = goal)
        }
        if (isTRUE(input$prior_type %in% c("meta_analytic", "informative"))) {
            mu <- abs(as.numeric(input$prior_effect_mean %||% 0))
            updateNumericInput(session, "prior_effect_mean",
                value = if (goal == "decrease") -mu else mu)
        }
    }, ignoreInit = FALSE)

    observeEvent(input$va_es_improvement, {
        if (!identical(input$intervention_goal, input$va_es_improvement)) {
            updateSelectInput(session, "intervention_goal",
                selected = input$va_es_improvement)
        }
    }, ignoreInit = TRUE)

    # outcome_type maps to a default brms family; "other" exposes the full family list
    observeEvent(input$outcome_type, {
        family_for_type <- list(
            continuous = "gaussian",
            count      = "negbinomial",
            binary     = "bernoulli",
            proportion = "beta"
        )
        if (!is.null(input$outcome_type) && input$outcome_type != "other") {
            fam <- family_for_type[[input$outcome_type]]
            if (!is.null(fam)) updateSelectInput(session, "model_family", selected = fam)
        }
        lrr_scale <- switch(input$outcome_type %||% "continuous",
                            count = "count", proportion = "proportion", "other")
        updateSelectInput(session, "va_es_scale", selected = lrr_scale)
    }, ignoreInit = FALSE)

    # Design type change
    .recode_phases <- function(selected) {
        req(values$data, values$model_structure)
        design_map <- list(
            ab = "ab", reversal = "reversal",
            multiple_baseline = "multiple_baseline",
            multiple_probe = "multiple_baseline",
            alternating = "alternating_treatment",
            changing_criterion = "changing_criterion"
        )
        req(isTRUE(selected %in% names(design_map)))
        tryCatch({
            values$processed_data <- .process_phases(values$data, values$model_structure, design_map[[selected]])
            values$processed_data <- compute_autocorrelation_features(values$processed_data)
        }, error = function(e) NULL)
    }
    observeEvent(input$research_design, .recode_phases(input$research_design))
    # the meta tab codes phases by its own design selector
    observeEvent(input$meta_research_design, {
        req(identical(input$main_nav, "meta"))
        .recode_phases(input$meta_research_design)
    }, ignoreInit = TRUE)

    observeEvent(input$reDetectDesign, {
        req(values$data)
        values$design_target <- .detected_design(values$data)
        values$design_override["research_design"] <- FALSE
        .push_design("research_design")
        showNotification(sprintf("Design re-detected: %s", values$design_target),
                         type = "message", duration = 3)
    })

    observeEvent(input$resetSettings, {
        numeric_vars <- if (!is.null(values$data))
            names(values$data)[sapply(values$data, is.numeric)] else character(0)
        default_outcome <- if ("outcome" %in% numeric_vars) "outcome" else numeric_vars[1] %||% character(0)

        values$design_override["research_design"] <- FALSE
        .push_design("research_design")
        updateSelectInput(session, "model_family", selected = "gaussian")
        updateSelectInput(session, "outcome_var", selected = default_outcome)
        updateSelectInput(session, "moderator_vars", selected = character(0))
        updateRadioButtons(session, "formula_mode", selected = "guided")
        updateTextAreaInput(session, "custom_fixed_formula", value = "")
        updateCheckboxInput(session, "cross_level_interactions", value = FALSE)
        updateCheckboxInput(session, "center_moderators", value = FALSE)
        updateCheckboxInput(session, "scale_moderators", value = FALSE)
        updateCheckboxInput(session, "include_ar1", value = FALSE)
        updateCheckboxInput(session, "ar_covariance", value = FALSE)
        updateCheckboxInput(session, "heterogeneous_variance", value = FALSE)
        updateNumericInput(session, "n_chains", value = 4)
        updateNumericInput(session, "n_iter", value = 2000)
        updateNumericInput(session, "n_warmup", value = 1000)
        updateNumericInput(session, "adapt_delta", value = 0.95)

        if (!is.null(input$piecewise_terms))
            updateCheckboxGroupInput(session, "piecewise_terms", selected = character(0))

        values$model_stale <- TRUE
        showNotification("Settings reset to defaults.", type = "message", duration = 3)
    })



    # Distribution recommendations
    output$distributionAiRecommendation <- renderUI({
        req(values$data, input$outcome_var)

        if (is.null(input$outcome_var) || input$outcome_var == "" ||
            !input$outcome_var %in% names(values$data)) {
            return(div(class = "text-muted small", "Select outcome variable first"))
        }

        outcome_data <- values$data[[input$outcome_var]]
        recommendation <- recommend_distribution_family(outcome_data)

        div(
            div(class = "fw-bold", recommendation$recommended),
            div(class = "text-muted mt-1", recommendation$reasoning),
            if(!is.null(recommendation$warning)) {
                div(class = "mt-1", style = "color: #B45309; font-weight: bold;",
                    recommendation$warning)
            }
        )
    })


    # Reactive outputs
    output$modelFitted <- reactive({
        # model_fitted is set when the fit promise resolves
        isTRUE(values$model_fitted)
    })
    outputOptions(output, "modelFitted", suspendWhenHidden = FALSE)

    output$modelRefitting <- reactive({
        isTRUE(values$model_refitting)
    })
    outputOptions(output, "modelRefitting", suspendWhenHidden = FALSE)

    output$modelStale <- reactive({
        isTRUE(values$model_stale)
    })
    outputOptions(output, "modelStale", suspendWhenHidden = FALSE)

    output$hasMultipleStudies <- reactive({
        !is.null(values$data) && "study" %in% names(values$data) &&
        length(unique(values$data$study)) >= 2
    })
    outputOptions(output, "hasMultipleStudies", suspendWhenHidden = FALSE)

    output$metaModelFitted <- reactive({ isTRUE(values$meta_model_fitted) })
    outputOptions(output, "metaModelFitted", suspendWhenHidden = FALSE)

    output$metaModelRefitting <- reactive({ isTRUE(values$meta_model_refitting) })
    outputOptions(output, "metaModelRefitting", suspendWhenHidden = FALSE)

    output$metaModelStale <- reactive({ isTRUE(values$meta_model_stale) })
    outputOptions(output, "metaModelStale", suspendWhenHidden = FALSE)

    # Live meta formula built from the meta-tab inputs (independent of the fit)
    meta_formula_display <- reactive({
        req(values$data)
        model_data <- values$processed_data %||% values$data
        outcome <- input$meta_outcome_var %||% "outcome"
        if (!isTRUE(outcome %in% names(model_data))) return(NULL)

        custom_text <- meta_custom_fixed_text()
        if (identical(input$meta_formula_mode, "custom") && isTRUE(nzchar(custom_text))) {
            fixed <- paste0(outcome, " ~ ", custom_text)
        } else {
            pw <- intersect(input$meta_piecewise_terms %||% character(0), names(model_data))
            if (length(pw) == 0) pw <- .default_fixed_terms(model_data)
            fixed <- paste0(outcome, " ~ ", if (length(pw)) paste(pw, collapse = " + ") else "1")
            mods <- intersect(input$meta_moderator_vars %||% character(0), names(model_data))
            if (length(mods) > 0) {
                fixed <- paste0(fixed, " + ", paste(mods, collapse = " + "))
                tx <- pw[grepl("level_|trend_|treatment|atd_", pw)]
                if (isTRUE(input$meta_cross_level_interactions) && length(tx) > 0) {
                    fixed <- paste0(fixed, " + ",
                                    paste(outer(tx, mods, paste, sep = ":"), collapse = " + "))
                }
            }
        }

        re_formula <- trimws(meta_random_effects_config()$formula %||% "")

        f <- fixed
        if (nzchar(re_formula)) f <- paste0(f, " + ", re_formula)
        if (isTRUE(input$meta_include_ar1)) {
            cov_flag <- .ar_cov_flag(.meta_family(), input$meta_ar_covariance)
            meta_time <- tryCatch(find_time_var(model_data), error = function(e) NULL) %||% "time"
            f <- paste0(f, " + ar(time = ", meta_time, ", gr = ", .ar_group(model_data), cov_flag, ")")
        }
        trials_var <- input$meta_trials_var %||% ""
        if (identical(.meta_family(), "binomial") && trials_var %in% names(model_data)) {
            f <- sub("^[^~]*~", paste0(outcome, " | trials(", trials_var, ") ~"), f)
        }
        .guard_formula(.add_offset(f, model_data, meta = TRUE), model_data)
    })

    output$meta_modelEquation <- renderUI({
        req(values$data)
        f <- meta_formula_display()
        tags$p(style = "font-size: 13px; color: #374151; margin: 4px 0;",
            tags$strong("Formula: "), tags$code(f))
    })

    .formula_equations <- function(meta = FALSE) {
        f <- if (meta) meta_formula_display() else model_formula()
        req(f)
        family <- if (meta) .meta_family() else input$model_family %||% "gaussian"
        hetero <- isTRUE(.input_value("heterogeneous_variance", meta)) &&
            family %in% c("gaussian", "student", "skew_normal")
        app_model_equations(f, family,
            ar = isTRUE(.input_value("include_ar1", meta)),
            ar_covariance = nzchar(.ar_cov_flag(family, .input_value("ar_covariance", meta))),
            sigma_formula = if (hetero) ~ phase else NULL,
            data = values$processed_data %||% values$data)
    }

    .render_equations <- function(eq) {
        withMathJax(tagList(lapply(eq$tex, function(x) tags$div(paste0("$$", x, "$$")))))
    }

    output$meta_modelEquationLatex <- renderUI({
        .render_equations(.formula_equations(TRUE))
    })
    output$meta_priorDisplay <- renderPrint({
        pl <- values$current_prior_list
        if (isTRUE(values$prior_overrides_active) && length(pl)) {
            cat("Queued priors for the next fit (preset, adopted, or edited):\n")
            cat(sprintf("  %-24s %s\n", vapply(pl, function(p) paste0(p$class %||% "",
                            if (length(p$coef)) paste0(" (", paste(p$coef, collapse = ", "), ")") else ""), ""),
                        vapply(pl, function(p) .txt(p$string), "")), sep = "")
            cat("\n")
        }
        if (!is.null(values$meta_model)) {
            tryCatch(print(brms::prior_summary(values$meta_model)),
                     error = function(e) cat("Prior summary unavailable."))
        } else {
            cat("Fit the model to view priors.")
        }
    })

    output$meta_priorsPlot <- renderPlot({
        if (is.null(values$meta_model)) {
            plot.new(); title("Fit the model to view priors")
        } else {
            tryCatch(plot(brms::prior_summary(values$meta_model)),
                     error = function(e) {
                         plot.new(); title("Prior plot unavailable")
                     })
        }
    }, alt = "Prior distributions used in the meta-analysis model")

    output$meta_piecewiseTermsUI <- renderUI({
        model_data <- values$processed_data %||% values$data
        req(model_data)
        choices <- .piecewise_choices(model_data, .design_choice(input$meta_research_design, model_data))
        if (length(choices) == 0) return(NULL)
        checkboxGroupInput("meta_piecewise_terms",
            "Piecewise Terms",
            choices = choices,
            selected = intersect(.default_fixed_terms(model_data), choices), inline = TRUE)
    })

    observe({
        req(values$data)
        num_cols <- names(values$data)[vapply(values$data, is.numeric, logical(1))]
        guess <- grep("^(trials|n_trials|opportunities|intervals)$", num_cols, ignore.case = TRUE, value = TRUE)
        choices <- c("(select)" = "", num_cols)
        pick <- if (length(guess)) guess[1] else ""
        updateSelectInput(session, "trials_var", choices = choices, selected = pick)
        updateSelectInput(session, "meta_trials_var", choices = choices, selected = pick)
        positive <- num_cols[vapply(num_cols, function(v) {
            x <- values$data[[v]]
            !anyNA(x) && all(is.finite(x)) && all(x > 0)
        }, logical(1))]
        exposure_choices <- c("None" = "", positive)
        updateSelectInput(session, "exposure_var", choices = exposure_choices, selected = "")
        updateSelectInput(session, "meta_exposure_var", choices = exposure_choices, selected = "")
    })

    # exposure column for an offset(log()) term; count families only
    .exposure_in <- function(meta) {
        v <- if (meta) input$meta_exposure_var else input$exposure_var
        fam <- if (meta) .meta_family() else input$model_family
        if (isTRUE(fam %in% c("poisson", "negbinomial", "zero_inflated_poisson", "zero_inflated_negbinomial")) &&
            isTRUE(nzchar(v))) v
    }
    .add_offset <- function(f, data, meta) {
        exposure <- .exposure_in(meta)
        if (is.null(exposure) || is.null(f)) return(f)
        tryCatch(add_exposure_offset(f, data, exposure, if (meta) .meta_family() else input$model_family),
                 error = function(e) {
                     showNotification(paste("Exposure offset not added:", conditionMessage(e)),
                                      id = "exposure_offset", type = "error", duration = 8)
                     f
                 })
    }

    observe({
        req(values$data)
        cols <- names(values$data)
        num_cols <- cols[vapply(values$data, is.numeric, logical(1))]
        updateSelectInput(session, "meta_outcome_var",
                          choices = num_cols,
                          selected = isolate(input$meta_outcome_var) %||% num_cols[1])
        updateSelectInput(session, "meta_moderator_vars",
                          choices = cols,
                          selected = isolate(input$meta_moderator_vars))
    })

    # outcome type / measure subset: text columns other than the design columns
    observe({
        req(values$data)
        d <- values$data
        design_cols <- c(find_study_var(d), find_case_var(d), find_phase_var(d))
        cand <- Filter(function(v) {
            n_lev <- length(unique(stats::na.omit(d[[v]])))
            !is.numeric(d[[v]]) && n_lev >= 2 && n_lev <= 20
        }, setdiff(names(d), design_cols))
        current <- isolate(input$meta_measure_var) %||% ""
        updateSelectInput(session, "meta_measure_var",
                          choices = c("(none: all rows)" = "", cand),
                          selected = if (current %in% cand) current else "")
    })

    observeEvent(input$meta_measure_var, {
        v <- input$meta_measure_var %||% ""
        d <- values$data
        vals <- if (nzchar(v) && !is.null(d) && v %in% names(d)) {
            sort(unique(as.character(stats::na.omit(d[[v]]))))
        } else character(0)
        updateSelectInput(session, "meta_measure_value", choices = vals, selected = vals[1])
    }, ignoreNULL = FALSE)

    .meta_family <- function() {
        switch(input$meta_outcome_type %||% "continuous",
               continuous = "gaussian", count = "negbinomial", binary = "bernoulli",
               proportion = "beta", input$meta_model_family %||% "gaussian")
    }

    # fit inputs read the meta_* controls directly during a meta fit; synced SCD inputs may lag
    .input_value <- function(id, meta = FALSE) {
        if (meta) {
            if (identical(id, "model_family")) return(.meta_family())
            v <- input[[paste0("meta_", id)]]
            if (!is.null(v) && !identical(v, "")) return(v)
        }
        input[[id]]
    }

    .fit_in <- function(id) .input_value(id, isTRUE(values$meta_fit_pending))

    .prepare_fit_data <- function(d, outcome, family, meta = FALSE) {
        prepare_fit_data(d, outcome, family,
                         .input_value("moderator_vars", meta),
                         isTRUE(.input_value("center_moderators", meta)),
                         isTRUE(.input_value("scale_moderators", meta)),
                         .exposure_in(meta),
                         outcome_units = .input_value("count_outcome_units", meta) %||% "count")
    }

    .meta_subset <- function(d) {
        v <- input$meta_measure_var %||% ""
        keep <- input$meta_measure_value %||% character(0)
        if (!nzchar(v) || !v %in% names(d) || !length(keep)) return(d)
        d[as.character(d[[v]]) %in% keep, , drop = FALSE]
    }

    # per-tier OLS of the meta piecewise terms; warnings are kept for display, not raised
    .meta_standardize <- function(d, outcome) {
        custom <- identical(input$meta_formula_mode, "custom")
        if (!identical(make.names(outcome), outcome))
            return(simpleError("The outcome column name is not a syntactic R name."))
        pw <- intersect(input$meta_piecewise_terms %||% character(0), names(d))
        pw <- pw[make.names(pw) == pw]
        if (!length(pw)) pw <- .default_fixed_terms(d)
        fixed <- if (custom || !length(pw)) NULL else paste("~", paste(pw, collapse = " + "))
        warn <- character(0)
        res <- withCallingHandlers(
            tryCatch(standardize_by_case(d, formula_fixed = fixed, outcome_var = outcome),
                     error = function(e) e),
            warning = function(w) {
                warn <<- c(warn, conditionMessage(w))
                invokeRestart("muffleWarning")
            })
        if (inherits(res, "error")) return(res)
        list(data = res, table = attr(res, "case_sigma"), formula_fixed = fixed, warning = warn)
    }

    meta_std_preview <- reactive({
        req(values$data, isTRUE(input$meta_standardize))
        d <- .meta_subset(values$processed_data %||% values$data)
        outcome <- input$meta_outcome_var %||% "outcome"
        req(outcome %in% names(d))
        .meta_standardize(d, outcome)
    })

    observe({
        shinyjs::toggleState("meta_standardize", condition = identical(.meta_family(), "gaussian"))
    })

    output$meta_standardizeNotice <- renderUI({
        if (identical(.meta_family(), "gaussian")) return(NULL)
        div(class = "status-sidebar-warning", style = "font-size: 13px;",
            "Within-case standardization needs the Gaussian family (Continuous outcome type);",
            "the fit uses the unstandardized outcome.")
    })

    output$meta_stdWarning <- renderUI({
        res <- meta_std_preview()
        msgs <- if (inherits(res, "error")) conditionMessage(res) else res$warning
        if (!length(msgs)) return(NULL)
        div(class = "status-main-warning", style = "margin: 6px 0;",
            icon("triangle-exclamation"), " ", paste(msgs, collapse = " "))
    })

    output$meta_stdSigmaTable <- renderDT({
        res <- meta_std_preview()
        shiny::validate(need(!inherits(res, "error"), "Residual SDs could not be computed."))
        dt_table(res$table, digits = 3, caption = "Within-case residual SD by case")
    })

    # subset and standardization applied to a meta fit; NULL fields leave the SCD fit path unchanged
    .meta_fit_prep <- function(model_data, formula_str, outcome_var) {
        d <- .meta_subset(model_data)
        subset_label <- if (nrow(d) < nrow(model_data)) {
            sprintf("%s in {%s}", input$meta_measure_var,
                    paste(input$meta_measure_value, collapse = ", "))
        }
        if (!is.null(subset_label)) {
            showNotification(sprintf("Fitting %d of %d rows: %s.", nrow(d), nrow(model_data), subset_label),
                             type = "message", duration = 6)
        }
        plain <- list(data = d, formula = formula_str, outcome = outcome_var, table = NULL,
                      formula_fixed = NULL, subset = subset_label,
                      subset_var = input$meta_measure_var, subset_values = input$meta_measure_value)
        transform <- .prepare_fit_data(d, outcome_var, .meta_family(), TRUE)
        d <- transform$data
        plain$data <- d
        plain$transform <- transform
        if (!isTRUE(input$meta_standardize)) return(plain)
        if (!identical(.meta_family(), "gaussian")) {
            showNotification("Within-case standardization applies to the Gaussian family only; fitting the unstandardized outcome.",
                             type = "warning", duration = 8)
            return(plain)
        }
        res <- .meta_standardize(d, outcome_var)
        if (inherits(res, "error")) {
            showNotification(paste("Standardization failed:", conditionMessage(res)), type = "error", duration = 8)
            return(plain)
        }
        std_col <- paste0(outcome_var, "_std")
        sd_data <- res$data[!is.na(res$data[[std_col]]), , drop = FALSE]
        if (length(res$warning)) showNotification(paste(res$warning, collapse = " "), type = "warning", duration = 8)
        utils::modifyList(plain, list(
            data = sd_data, formula = paste0(std_col, " ~", sub("^[^~]*~", "", formula_str)),
            outcome = std_col, table = res$table, formula_fixed = res$formula_fixed %||% "default"))
    }

    output$meta_rhatMax <- renderText({
        m <- values$meta_model %||% values$model
        if (is.null(m)) return("N/A")
        tryCatch(sprintf("%.4f", max(brms::rhat(m), na.rm = TRUE)),
                 error = function(e) "N/A")
    })
    output$meta_essMin <- renderText({
        if (!is.null(values$diagnostics$basic$ess_bulk) &&
            length(values$diagnostics$basic$ess_bulk) > 0) {
            return(sprintf("%.0f",
                min(values$diagnostics$basic$ess_bulk, na.rm = TRUE)))
        }
        m <- values$meta_model %||% values$model
        if (is.null(m)) return("N/A")
        tryCatch({
            s <- summary(m)
            ess_vals <- c(s$fixed[, "Bulk_ESS"], s$spec_pars[, "Bulk_ESS"])
            for (re in s$random) ess_vals <- c(ess_vals, re[, "Bulk_ESS"])
            sprintf("%.0f", min(ess_vals, na.rm = TRUE))
        }, error = function(e) "N/A")
    })
    output$meta_bayesR2 <- renderText({
        m <- values$meta_model %||% values$model
        if (is.null(m)) return("N/A")
        tryCatch(sprintf("%.3f", mean(brms::bayes_R2(m)[,1])),
                 error = function(e) "N/A")
    })
    output$meta_looic <- renderText({
        loo_res <- .cached_meta_loo()
        if (is.null(loo_res)) return("N/A")
        sprintf("%.1f", loo_res$estimates["looic", "Estimate"])
    })
    output$meta_modelSummary <- renderPrint({
        m <- values$meta_model %||% values$model
        if (is.null(m)) { cat("Fit the model to view the summary."); return() }
        out <- capture.output(summary(m))
        out <- out[!grepl("^={4,}$|^-{4,}$", out)]
        cat(paste(out, collapse = "\n"))
    })

    output$meta_traceParamSelector <- renderUI({
        m <- values$meta_model %||% values$model
        if (is.null(m)) {
            return(tags$p(style = "color: #64748B; font-size: 13px;",
                          "Fit the model to select parameters."))
        }
        params <- tryCatch({
            ps <- brms::variables(m)
            grep("^b_|^sd_|^cor_|^sigma", ps, value = TRUE)
        }, error = function(e) character(0))
        if (length(params) == 0) {
            return(tags$p(style = "color: #64748B; font-size: 13px;",
                          "No parameters available."))
        }
        selectizeInput("meta_traceParam", "Parameters",
                    choices = params,
                    selected = params[seq_len(min(4, length(params)))],
                    multiple = TRUE, options = list(maxItems = 12))
    })

    # once the selector has sent a value, NULL means the user cleared it
    meta_trace_seen <- reactiveVal(FALSE)
    observeEvent(input$meta_traceParam, meta_trace_seen(TRUE), once = TRUE)
    meta_trace_params <- shiny::debounce(reactive({
        m <- values$meta_model %||% values$model
        if (is.null(m)) return(character(0))
        vars <- brms::variables(m)
        picked <- if (meta_trace_seen()) input$meta_traceParam
                  else utils::head(grep("^b_|^sd_|^cor_|^sigma", vars, value = TRUE), 4)
        intersect(utils::head(picked, 12), vars)
    }), 500)
    output$meta_tracePlotsBox <- renderUI({
        plotly_figure("meta_tracePlots", "Trace plots of MCMC draws by iteration for selected parameters",
                      height = paste0(.trace_px(max(length(meta_trace_params()), 1)), "px"))
    })
    output$meta_tracePlots <- plotly::renderPlotly({
        m <- values$meta_model %||% values$model
        if (is.null(m)) return(empty_plot("Fit the model to view trace plots"))
        tryCatch({
            params <- meta_trace_params()
            if (length(params) == 0) return(empty_plot("No parameters selected"))
            posterior <- as.data.frame(brms::as_draws_df(m, variable = params))
            posterior$.draw <- seq_len(nrow(posterior))
            long <- tidyr::pivot_longer(posterior[c(".draw", params)],
                                        cols = -.data$.draw,
                                        names_to = "param", values_to = "value")
            p <- ggplot(long, ggplot2::aes(x = .data$.draw, y = .data$value)) +
                ggplot2::geom_line(alpha = 0.6) +
                ggplot2::facet_wrap(~ param, scales = "free_y", ncol = 1) +
                accessible_plot_theme()
            styled_ggplotly(p, height = .trace_px(length(params)))
        }, error = function(e) empty_plot("Trace plot unavailable"))
    })

    output$meta_residualPlots <- plotly::renderPlotly({
        m <- values$meta_model %||% values$model
        if (is.null(m)) return(empty_plot("Fit the model to view residuals"))
        tryCatch({
            fitted_vals <- as.numeric(brms::fitted(m)[, "Estimate"])
            resid_vals  <- as.numeric(brms::residuals(m)[, "Estimate"])
            p <- ggplot(data.frame(fitted = fitted_vals, residual = resid_vals),
                        ggplot2::aes(x = fitted, y = residual)) +
                ggplot2::geom_point(alpha = 0.6) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
                accessible_plot_theme()
            styled_ggplotly(p)
        }, error = function(e) empty_plot("Residual plot unavailable"))
    })

    # drawn once per (meta fit, draw count) and shared by the four PP sub-tabs
    meta_pp_reps <- reactive(.pp_replicates(req(values$meta_model %||% values$model), input$meta_pp_ndraws %||% 50))
    .meta_pp_plot <- function(build) {
        tryCatch(build(meta_pp_reps()), error = function(e) .meta_pp_note("PP check unavailable"))
    }
    .meta_pp_note <- function(label) ggplot() + annotate("text", x = 0, y = 0, label = label) + theme_void()
    .meta_pp_group <- function(r, var) {
        fd <- r$data
        g <- if (var %in% names(fd)) as.character(fd[[var]])
             else if (var == "phase" && "level_AB" %in% names(fd)) ifelse(fd$level_AB > 0, "intervention", "baseline")
        if (!is.null(g)) g[r$keep]
    }
    .meta_pp_violin <- function(var) {
        .meta_pp_plot(function(r) {
            g <- .meta_pp_group(r, var)
            if (is.null(g)) return(.meta_pp_note(paste("No", var, "variable")))
            bayesplot::ppc_violin_grouped(r$y, r$yrep, group = g) + .pp_theme
        })
    }

    output$meta_ppCheckDensity <- renderPlot({
        req(values$meta_model %||% values$model)
        .meta_pp_plot(function(r) bayesplot::ppc_dens_overlay(r$y, r$yrep) + .pp_theme)
    }, alt = "Posterior predictive density overlay: observed outcome density against replicated draws")
    output$meta_ppCheckStat <- renderPlot({
        req(values$meta_model %||% values$model)
        .meta_pp_plot(function(r) bayesplot::ppc_stat(r$y, r$yrep, stat = "mean") + .pp_theme)
    }, alt = "Posterior predictive histogram of the replicated mean with the observed mean marked")
    output$meta_ppCheckGrouped <- renderPlot({
        req(values$meta_model %||% values$model)
        .meta_pp_violin("phase")
    }, alt = "Posterior predictive violin plots by phase with observed outcomes overlaid")
    output$meta_ppCheckByCase <- renderPlot({
        req(values$meta_model %||% values$model)
        .meta_pp_violin("case")
    }, alt = "Posterior predictive violin plots by case with observed outcomes overlaid")

    # brms hypothesis() alpha at the UI credible level; one-sided (< or >) intervals span 1 - 2 * alpha
    .hyp_alpha <- function(id, h = "") (1 - min(max(.mcmc_num(input[[id]], 95), 50), 99.9) / 100) / (1 + grepl("[<>]", h))

    # Meta Per Study hypothesis-test outputs (mirror scd-side)
    output$meta_hypothesisTestTable <- DT::renderDataTable({
        req(values$meta_model %||% values$model)
        m <- values$meta_model %||% values$model
        term <- tryCatch(.pick_treatment_term(rownames(brms::fixef(m))), error = function(e) NULL)
        shiny::validate(need(!is.null(term), "No treatment coefficient available to test."))
        tryCatch({
            hyp <- brms::hypothesis(m, paste(term, "= 0"), alpha = .hyp_alpha("meta_ci_level"))
            DT::datatable(.drop_na_postprob(hyp$hypothesis), options = list(dom = "t", scrollX = TRUE), rownames = FALSE,
                          caption = .dt_caption("Meta-analysis hypothesis test"))
        }, error = function(e)
            DT::datatable(data.frame(message = "Hypothesis test unavailable"),
                          options = list(dom = "t"), rownames = FALSE,
                          caption = .dt_caption("Meta-analysis hypothesis test (unavailable)")))
    })

    # brms::hypothesis() evaluates its string as R code, so only parameter arithmetic passes
    .hypothesis_ok <- function(m, h) {
        params <- tryCatch(brms::variables(m), error = function(e) character(0))
        if (is_safe_hypothesis(h, params)) return(TRUE)
        showNotification(paste("Hypothesis rejected: use parameter names, numbers, arithmetic,",
                               "and one of =, <, >. Function calls are not allowed."),
                         type = "error", duration = 8)
        FALSE
    }

    meta_custom_hyp <- reactiveVal(NULL)
    observeEvent(input$meta_runCustomHypothesis, {
        m <- values$meta_model %||% values$model
        req(m, nzchar(input$meta_customHypothesis %||% ""))
        if (!.hypothesis_ok(m, input$meta_customHypothesis)) return(meta_custom_hyp(NULL))
        meta_custom_hyp(tryCatch(
            brms::hypothesis(m, input$meta_customHypothesis,
                             alpha = .hyp_alpha("meta_ci_level", input$meta_customHypothesis))$hypothesis,
            error = function(e) data.frame(message = paste("Error:", e$message))))
    })
    output$meta_customHypothesisTable <- DT::renderDataTable({
        req(meta_custom_hyp())
        DT::datatable(.drop_na_postprob(meta_custom_hyp()), options = list(dom = "t", scrollX = TRUE), rownames = FALSE,
                      caption = .dt_caption("Meta-analysis custom hypothesis test"))
    })

    # Meta-Analysis > Studies tab: per-study summary
    output$metaStudyTable <- DT::renderDataTable({
        req(values$data, "study" %in% names(values$data))
        d <- values$data
        case_col <- if ("case" %in% names(d)) "case" else NA_character_
        summary_df <- if (!is.na(case_col)) {
            data.frame(
                study = unique(d$study),
                cases = vapply(unique(d$study),
                               function(s) length(unique(d[[case_col]][d$study == s])),
                               integer(1)),
                observations = vapply(unique(d$study),
                                      function(s) sum(d$study == s),
                                      integer(1)),
                stringsAsFactors = FALSE
            )
        } else {
            data.frame(
                study = unique(d$study),
                observations = vapply(unique(d$study),
                                      function(s) sum(d$study == s),
                                      integer(1)),
                stringsAsFactors = FALSE
            )
        }
        if ("outcome" %in% names(d) && is.numeric(d$outcome)) {
            summary_df$outcome_min <- as.numeric(tapply(d$outcome, d$study, min, na.rm = TRUE)[as.character(summary_df$study)])
            summary_df$outcome_max <- as.numeric(tapply(d$outcome, d$study, max, na.rm = TRUE)[as.character(summary_df$study)])
        }
        DT::datatable(summary_df, options = list(pageLength = 15, scrollX = TRUE),
                      class = "cell-border stripe", rownames = FALSE,
                      caption = .dt_caption("Study characteristics"))
    })

    output$metaComparabilityWarning <- renderUI({
        req(values$data, all(c("study", "outcome") %in% names(values$data)))
        d <- values$data
        req(is.numeric(d$outcome))
        spans <- tapply(d$outcome, d$study, function(y) diff(range(y, na.rm = TRUE)))
        spans <- spans[is.finite(spans) & spans > 0]
        req(length(spans) >= 2)
        ratio <- max(spans) / min(spans)
        if (ratio < 3) return(NULL)
        div(class = "status-main-warning", style = "margin-top: 8px;",
            icon("triangle-exclamation"),
            sprintf(paste("Outcome ranges differ %.1f-fold across studies.",
                          "Use LRR, NAP, or Tau, or standardize within case, before pooling."), ratio))
    })

    output$metaStructureSummary <- renderPrint({
        req(values$data, "study" %in% names(values$data))
        d <- values$data
        n_studies <- length(unique(d$study))
        n_cases <- if ("case" %in% names(d)) length(unique(d$case)) else NA
        n_obs   <- nrow(d)
        cat(sprintf("Studies:      %d\n", n_studies))
        if (!is.na(n_cases)) cat(sprintf("Cases:        %d\n", n_cases))
        cat(sprintf("Observations: %d\n", n_obs))
        cat("\nThree-level structure:\n")
        cat("  observations within cases within studies\n")
    })




    # typed formula text is evaluated by brms and model.frame, so only allowlisted syntax passes
    .rejected_formula_msg <- function(f, data) {
        unknown <- setdiff(tryCatch(all.vars(str2lang(f)), error = function(e) character(0)), names(data))
        if (length(unknown))
            return(sprintf("Formula rejected: column not in the data: %s.", paste(unknown, collapse = ", ")))
        paste("Formula rejected: use column names, operators, and only",
              "ar(), gr(), trials(), or offset(log(column)).")
    }

    .guard_formula <- function(f, data) {
        if (is.null(f) || is.null(data)) return(NULL)
        if (isTRUE(is_safe_formula(f, names(data)))) {
            removeNotification("formula_rejected")
            return(f)
        }
        showNotification(.rejected_formula_msg(f, data), id = "formula_rejected",
                         type = "error", duration = 8)
        NULL
    }

    # a half-typed custom formula is not guarded until typing pauses
    custom_fixed_text <- shiny::debounce(reactive(input$custom_fixed_formula), 800)
    meta_custom_fixed_text <- shiny::debounce(reactive(input$meta_custom_fixed_formula), 800)

    model_formula <- reactive({
        .guard_formula(model_formula_raw(), values$processed_data %||% values$data)
    })

    # Model formula
    model_formula_raw <- reactive({
        req(values$data)

        # Get random effects from the unified module
        re_config <- random_effects_config()

        # Explicitly depend on these inputs for reactivity
        input$outcome_var
        input$formula_mode
        custom_text <- custom_fixed_text()
        input$piecewise_terms
        input$moderator_vars
        input$cross_level_interactions
        input$include_ar1
        input$heterogeneous_variance
        input$model_family
        input$trials_var

        outcome_var <- input$outcome_var
        model_data <- if (!is.null(values$processed_data)) values$processed_data else values$data
        # every column-naming input must be a data column before it enters a formula string
        if (!isTRUE(length(outcome_var) == 1 && outcome_var %in% names(model_data))) return(NULL)
        phase_var <- find_phase_var(model_data)
        case_var <- find_case_var(model_data)

        # filter piecewise terms by user checkbox selection
        selected_pw <- input$piecewise_terms

        model_data <- .center_moderators(model_data)

        # Base fixed effects - check formula mode
        if (input$formula_mode == "custom" && !is.null(custom_text) && custom_text != "") {
            f <- paste0(outcome_var, " ~ ", custom_text)
        } else {
            # a selection left over from earlier data has no columns here, so the defaults apply
            pw_terms <- intersect(selected_pw, names(model_data))
            if (length(pw_terms) == 0) pw_terms <- .default_fixed_terms(model_data)
            treatment_terms <- pw_terms[grepl("^(level|trend|treatment|atd_)", pw_terms)]
            if (length(pw_terms) > 0) {
                f <- paste0(outcome_var, " ~ ", paste(pw_terms, collapse = " + "))
                if (!is.null(input$moderator_vars) && length(input$moderator_vars) > 0) {
                    valid_moderators <- intersect(input$moderator_vars, names(model_data))
                    if (length(valid_moderators) > 0) {
                        if (isTRUE(input$cross_level_interactions) && length(treatment_terms) > 0) {
                            interactions <- paste(
                                outer(treatment_terms, valid_moderators, paste, sep = ":"),
                                collapse = " + "
                            )
                            f <- paste0(f, " + ", paste(valid_moderators, collapse = " + "),
                                        " + ", interactions)
                        } else {
                            f <- paste0(f, " + ", paste(valid_moderators, collapse = " + "))
                        }
                    }
                }
            } else if (!is.null(phase_var) && length(phase_var) > 0) {
                f <- paste0(outcome_var, " ~ ", phase_var)
                if (!is.null(input$moderator_vars) && length(input$moderator_vars) > 0) {
                    valid_moderators <- intersect(input$moderator_vars, names(model_data))
                    if (length(valid_moderators) > 0) {
                        if (isTRUE(input$cross_level_interactions)) {
                            f <- paste0(outcome_var, " ~ ", phase_var, " * (",
                                        paste(valid_moderators, collapse = " + "), ")")
                        } else {
                            f <- paste0(f, " + ", paste(valid_moderators, collapse = " + "))
                        }
                    }
                }
            } else {
                f <- paste0(outcome_var, " ~ 1")
            }
        }

        # the fixed part is allowlisted before terms() or as.formula() evaluates it
        f <- .guard_formula(f, model_data)
        if (is.null(f)) return(NULL)

        # Add random effects from unified module
        re_formula <- if (!is.null(re_config) && !is.null(re_config$formula) && re_config$formula != "") {
            re_config$formula
        } else if (!is.null(case_var) && case_var %in% names(model_data)) {
            default_random_effects(model_data)
        } else {
            NULL
        }
        # drop RE terms for grouping variables with only 1 level
        if (!is.null(re_formula)) {
            re_terms <- Filter(function(term) grepl("|", term, fixed = TRUE), .paren_groups(re_formula))
            re_terms <- Filter(function(term) {
                grp_rhs <- sub(".*\\|\\s*", "", sub("^\\((.*)\\)$", "\\1", term))
                grp_vars <- tryCatch(all.vars(str2lang(grp_rhs)), error = function(e) character(0))
                length(grp_vars) > 0 && all(vapply(grp_vars, function(v) {
                    v %in% names(model_data) && length(unique(model_data[[v]])) >= 2
                }, logical(1)))
            }, re_terms)
            fixed_terms <- tryCatch(attr(stats::terms(stats::as.formula(f)), "term.labels"),
                                    error = function(e) NULL)
            if (!is.null(fixed_terms)) {
                re_terms <- unname(vapply(re_terms, .prune_slopes, character(1), fixed_terms = fixed_terms))
            }
            if (length(re_terms) > 0) {
                f <- paste0(f, " + ", paste(re_terms, collapse = " + "))
            }
        }

        time_var <- tryCatch(find_time_var(model_data), error = function(e) NULL) %||%
                    (if ("time" %in% names(model_data)) "time" else NULL)
        if (isTRUE(input$include_ar1) && !is.null(time_var) && !is.null(case_var)) {
            cov_flag <- .ar_cov_flag(input$model_family %||% "gaussian", input$ar_covariance)
            f <- paste0(f, " + ar(time = ", time_var, ", gr = ", .ar_group(model_data, case_var), cov_flag, ")")
        }
        trials_var <- input$trials_var %||% ""
        if (identical(input$model_family, "binomial") && trials_var %in% names(model_data)) {
            f <- sub("^[^~]*~", paste0(outcome_var, " | trials(", trials_var, ") ~"), f)
        }
        .add_offset(f, model_data, meta = FALSE)
    })

    .ar_warning <- function(data, enabled, outcome_var = "outcome") {
        if (!isTRUE(enabled) || is.null(data)) return(NULL)
        time_var <- tryCatch(find_time_var(data), error = function(e) NULL) %||%
                    (if ("time" %in% names(data)) "time" else NULL)
        if (is.null(time_var) || !is.numeric(data[[time_var]])) return(NULL)
        groups <- intersect(.tier_cols(data), names(data))
        key <- if (length(groups)) interaction(data[groups], drop = TRUE) else factor(rep(1, nrow(data)))
        uneven <- any(tapply(data[[time_var]], key, function(t) {
            steps <- round(diff(sort(unique(t[!is.na(t)]))), 8)
            length(unique(steps)) > 1
        }), na.rm = TRUE)
        has_na <- outcome_var %in% names(data) && anyNA(data[[outcome_var]])
        msgs <- c(
            if (uneven) sprintf(paste("'%s' is unequally spaced within some series; ar() treats",
                                      "consecutive rows as one lag apart, so the gaps are ignored."), time_var),
            if (has_na) "The outcome has missing values; those rows are dropped, so the AR lag spans the gap.")
        if (length(msgs) == 0) return(NULL)
        div(class = "status-main-warning", style = "margin: 6px 0;",
            icon("triangle-exclamation"), " ", paste(msgs, collapse = " "))
    }

    output$arSpacingWarning <- renderUI({
        .ar_warning(values$processed_data %||% values$data, input$include_ar1,
                    input$outcome_var %||% "outcome")
    })
    output$meta_arSpacingWarning <- renderUI({
        .ar_warning(values$processed_data %||% values$data, input$meta_include_ar1,
                    input$meta_outcome_var %||% "outcome")
    })

    # Keep values$formula_display in sync with model_formula()
    # a formula error must not end the session
    observe({
        f <- tryCatch(model_formula(), error = function(e) NULL)
        if (!is.null(f)) values$formula_display <- f
    })

    # Live brms formula string (mirrors the meta tab)
    output$modelFormulaText <- renderUI({
        req(values$data)
        f <- model_formula() %||% values$formula_display
        req(f)
        tags$p(style = "font-size: 13px; color: #374151; margin: 4px 0 8px;",
            tags$strong("Formula: "), tags$code(f))
    })

    # Model equation display
    output$modelEquation <- renderUI({
        .render_equations(.formula_equations())
    })

    observeEvent(input$showEquationGuide, {
        eq <- .formula_equations()
        groups <- vapply(eq$groups, function(g) paste0(g$name, ": ",
            paste(c(if (g$intercept) "intercept", g$terms), collapse = ", ")), "")
        showModal(info_popup_modal(
            title = "Model Equation Notation",
            body = tagList(
                tags$p("Fixed coefficients multiply the formula terms in their displayed order. Factor terms denote their fitted contrast columns."),
                tags$p(paste("Fixed terms:", paste(eq$fixed_terms, collapse = ", "))),
                tags$p(paste("Random coefficients:", paste(groups, collapse = "; "))),
                tags$p("The predictor uses the stated link. Positive coefficients indicate a response increase only for increasing links. Ordinal thresholds replace the fixed location intercept.")),
            citation = "B&uuml;rkner, P.-C. (2017). brms: An R package for Bayesian multilevel models using Stan. <em>Journal of Statistical Software, 80</em>(1), 1&ndash;28. <a href=\"https://doi.org/10.18637/jss.v080.i01\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.18637/jss.v080.i01</a>",
            size = "m"
        ))
    })
    output$convergenceDiagnostics <- renderPrint({
        req(values$diagnostics)

        cat("Convergence Diagnostics\n\n")

        # Basic diagnostics
        if (!is.null(values$diagnostics$basic)) {
            cat("Basic Diagnostics:\n")
            if (!is.null(values$diagnostics$basic$rhat)) {
                max_rhat <- max(values$diagnostics$basic$rhat, na.rm = TRUE)
                cat("  Max R-hat:", round(max_rhat, 4), "\n")
            }
            if (!is.null(values$diagnostics$basic$ess_bulk)) {
                min_ess <- min(values$diagnostics$basic$ess_bulk, na.rm = TRUE)
                cat("  Min ESS Bulk:", round(min_ess), "\n")
            }
            if (!is.null(values$diagnostics$basic$ess_tail)) {
                min_ess_tail <- min(values$diagnostics$basic$ess_tail, na.rm = TRUE)
                cat("  Min ESS Tail:", round(min_ess_tail), "\n")
            }
        }

        if (!is.null(values$diagnostics$rhat_variants)) {
            cat("\nR-hat variants:\n")
            if (!is.na(values$diagnostics$rhat_variants$split_rhat)) {
                cat("  Split R-hat:", round(values$diagnostics$rhat_variants$split_rhat, 4), "\n")
            }
            if (!is.na(values$diagnostics$rhat_variants$rank_rhat)) {
                cat("  Rank R-hat (max of bulk and tail):", round(values$diagnostics$rhat_variants$rank_rhat, 4), "\n")
            }
            if (!is.na(values$diagnostics$rhat_variants$folded_rhat)) {
                cat("  Folded (tail) R-hat:", round(values$diagnostics$rhat_variants$folded_rhat, 4), "\n")
            }
        }

        # Divergences and energy
        if (!is.null(values$diagnostics$divergence)) {
            cat("\nSampling Diagnostics:\n")
            if (!is.null(values$diagnostics$divergence$n_divergent)) {
                cat("  Divergent transitions:", values$diagnostics$divergence$n_divergent, "\n")
            }
            if (!is.null(values$diagnostics$divergence$energy$bfmi)) {
                cat("  BFMI:", round(values$diagnostics$divergence$energy$bfmi, 3), "\n")
            }
        }

        # Overall convergence
        if (!is.null(values$diagnostics$converged)) {
            cat("\nOverall Assessment:\n")
            cv <- values$diagnostics$converged
            status <- if (isTRUE(cv$converged)) "Converged"
                      else if (isFALSE(cv$converged)) "Issues detected"
                      else "Not assessable (no diagnostics could be computed)"
            confidence <- suppressWarnings(as.numeric(cv$confidence))
            cat("  Status:", status, "\n")
            if (length(confidence) == 1 && is.finite(confidence))
                cat("  Diagnostic check score:", paste0(round(confidence * 100, 1), "%"), "\n")
        }

        # Recommendations
        if (!is.null(values$diagnostics$recommendations) && length(values$diagnostics$recommendations) > 0) {
            cat("\nRecommendations:\n")
            for (rec in values$diagnostics$recommendations) {
                cat("  -", rec, "\n")
            }
        }
    })

    # ICCs per posterior draw (bootstrap_variance_components); NA when the family
    # has no single residual SD, rather than a total that leaves the residual out
    .icc_note <- function(m) {
        fam <- tryCatch(brms::family(m)$family, error = function(e) "")
        has_ar <- tryCatch(any(grepl("^ar\\[", brms::variables(m))), error = function(e) FALSE)
        c(if (identical(fam, "student"))
              "Student-t variance includes nu/(nu - 2); ICCs are undefined if any retained draw has nu <= 2.",
          if (isTRUE(has_ar))
              "AR(1) variance includes 1/(1 - ar^2). ICCs refer to random intercepts at the covariate reference point.")
    }
    observe({
        req(values$model)
        m <- values$model
        values$icc_results <- tryCatch({
            bv <- suppressMessages(bootstrap_variance_components(m, n_boot = 1000))
            if (is.null(bv)) NULL else {
                sm <- bv$summary
                cols <- grep("^iccs\\.", colnames(sm), value = TRUE)
                list(table = if (length(cols)) data.frame(
                         group = sub("^iccs\\.", "", cols),
                         mean = sm["mean", cols], lower = sm[3, cols], upper = sm[4, cols],
                         row.names = NULL) else NULL,
                     no_sigma = all(is.na(sm["mean", "sigma2"])),
                     note = .icc_note(m))
            }
        }, error = function(e) NULL)
    })

    output$iccResults <- renderPrint({
        r <- values$icc_results
        if (is.null(r) || is.null(r$table)) {
            cat("Intraclass correlations are available after a model with",
                "random effects has been fit.")
            return()
        }
        if (isTRUE(r$no_sigma)) {
            cat("Intraclass correlations are unavailable because this specification has no finite constant residual response variance.\n",
                "Check the residual model, link, and Student-t degrees of freedom.")
            return()
        }
        cat("Intraclass Correlations (posterior mean, 95% interval)\n\n")
        for (i in seq_len(nrow(r$table))) {
            cat(sprintf("  %-20s %.3f  [%.3f, %.3f]\n", r$table$group[i],
                        r$table$mean[i], r$table$lower[i], r$table$upper[i]))
        }
        if (length(r$note)) cat("\n", paste("Note:", r$note), sep = "\n")
    })


    # Model state
    output$modelRunning <- reactive({
        return(isTRUE(values$model_running))
    })
    outputOptions(output, "modelRunning", suspendWhenHidden = FALSE)

    # Model progress
    output$modelProgress <- renderText({
        if (length(values$model_log) > 0) {
            # Return the last 50 lines of the log
            n_lines <- min(50, length(values$model_log))
            paste(tail(values$model_log, n_lines), collapse = "\n")
        } else {
            ""  # Empty string - no message when no log
        }
    })



    .mcid <- function() {
        v <- suppressWarnings(as.numeric(input$results_mcid))
        if (length(v) != 1 || is.na(v) || v <= 0) NULL else v
    }

    output$posteriorESTable <- renderDT({
        req(values$model)
        tryCatch({
            table_df <- summarize_fixed_effect_decisions(
                values$model, threshold = .mcid(), rope = .default_rope(values$model),
                ci_level = (input$results_ci_level %||% 95) / 100,
                intervention_goal = input$intervention_goal %||% "increase")
            shiny::validate(need(!is.null(table_df) && nrow(table_df) > 0, "No posterior decisions computed."))
            names(table_df) <- .ratio_names(names(table_df), "Typical-case (conditional)")
            dt_table(table_df, digits = 3, caption = "Fixed-effect posterior decisions")
        }, error = function(e) {
            dt_table(data.frame(Message = paste("Error:", e$message)), caption = "Fixed-effect posterior decisions (error)")
        })
    })

    # exp(b) columns are ratios for a case with all random effects at zero, not population averages
    .ratio_names <- function(nm, prefix) {
        base <- sub("_(lower|upper)$", "", nm)
        bound <- ifelse(grepl("_lower$", nm), " lower", ifelse(grepl("_upper$", nm), " upper", ""))
        label <- c(irr = "IRR", or = "OR", mean_ratio = "mean ratio", median_ratio = "median ratio")[base]
        ifelse(is.na(label), nm, paste0(prefix, " ", label, bound))
    }

    .ratio_note <- function(model) {
        link <- tryCatch(stats::family(model)$link, error = function(e) "")
        if (!isTRUE(link %in% c("log", "logit"))) return(NULL)
        helpText(info_link("marginal_contrast"),
            "exp(b) ratios describe a typical case with random effects at zero.",
            "See Marginal contrast for the population average.")
    }

    # Marginal (population-averaged) contrast over new cases
    .marginal_block <- function(model) {
        fam <- tryCatch(stats::family(model)$family, error = function(e) "")
        link <- tryCatch(stats::family(model)$link, error = function(e) "")
        design <- .design_choice(input$research_design, values$processed_data %||% values$data)
        has_offset <- grepl("offset(", paste(deparse(tryCatch(stats::formula(model)$formula, error = function(e) NULL)),
                                             collapse = " "), fixed = TRUE)
        if (has_offset || !is.null(.exposure_in(FALSE)))
            "The marginal contrast does not support models with an exposure offset. Set the exposure column to None and refit to compute it."
        else if (!link %in% c("log", "logit") || fam %in% c("cumulative", "categorical", "sratio", "cratio", "acat"))
            "The marginal contrast applies to log- and logit-link families, where typical-case and population-averaged effects differ."
        else if (!design %in% c("ab", "multiple_baseline") || !"level_AB" %in% names(model$data))
            "The marginal contrast is available for AB and multiple-baseline (multiple-probe) fits with level_AB."
    }

    marginal_state <- reactiveVal(NULL)
    observeEvent(values$model, marginal_state(NULL), ignoreNULL = FALSE)

    output$marginalContrastPanel <- renderUI({
        req(values$model)
        blocked <- .marginal_block(values$model)
        if (!is.null(blocked)) return(tags$p(style = "font-size: 13px; color: #64748B; margin: 12px 0;", blocked))
        tagList(
            helpText(info_link("marginal_contrast"),
                "Treatment versus no treatment at session B, averaged over new cases.",
                "It differs from the typical-case exp(b) ratios."),
            fluidRow(
                column(3, .ab_rule_input("marginal_ab_rule")),
                column(3, numericInput("marginal_duration", "Duration B - A (blank = rule)", value = NULL, min = 1, step = 1)),
                column(3, numericInput("marginal_n_new", "New cases per draw", value = 200, min = 20, step = 20)),
                column(3, numericInput("marginal_ndraws", "Posterior draws", value = 500, min = 50, step = 50))),
            actionButton("runMarginal", "Run marginal contrast", class = "btn-primary mb-3"),
            textOutput("marginalTimes"),
            DT::dataTableOutput("marginalContrastTable"))
    })

    observeEvent(input$runMarginal, {
        model <- values$model
        req(model, is.null(.marginal_block(model)))
        fn <- marginal_contrast
        num <- function(v) {
            v <- suppressWarnings(as.numeric(v))
            if (length(v) == 1 && is.finite(v)) v
        }
        pd <- values$processed_data
        args <- .formal_args(fn, list(model = model,
                                      data = if (!is.null(pd) && "level_AB" %in% names(pd)) pd,
                                      ab_rule = if (isTRUE(input$marginal_ab_rule %in% c("scdhlm", "shortest_treatment")))
                                          input$marginal_ab_rule else "scdhlm",
                                      duration = num(input$marginal_duration),
                                      n_new = num(input$marginal_n_new) %||% 200,
                                      ndraws = num(input$marginal_ndraws) %||% 500,
                                      intervention_goal = input$intervention_goal %||% "increase"))
        args <- args[!vapply(args, is.null, logical(1))]
        marginal_state(list(status = "running"))
        note_id <- showNotification("Computing the marginal contrast...", type = "message", duration = NULL)
        fut <- promises::future_promise(seed = TRUE, scdbayes:::.drop_sampler_files(.session_dir, do.call(fn, args)))
        promises::finally(promises::then(fut,
            onFulfilled = function(res) marginal_state(list(status = "done", result = res)),
            onRejected = function(e) marginal_state(list(status = "failed", message = conditionMessage(e)))),
            onFinally = function() removeNotification(note_id))
        invisible(NULL)
    })

    output$marginalContrastTable <- DT::renderDataTable({
        st <- marginal_state()
        validate(need(!is.null(st), "Click Run marginal contrast."))
        validate(need(!identical(st$status, "running"), "Computing the marginal contrast..."))
        validate(need(!identical(st$status, "failed"), paste("Marginal contrast failed:", st$message)))
        res <- st$result
        df <- res$summary
        lbl <- c(mean_untreated = "Population mean, untreated at B", mean_treated = "Population mean, treated at B",
                 difference = "Marginal difference (treated - untreated)",
                 ratio = "Marginal (population-averaged) ratio",
                 prob_improvement = "P(improvement)")[df$quantity]
        df$quantity <- ifelse(is.na(lbl), df$quantity, lbl)
        dt_table(df, digits = 3, caption = "Population-averaged marginal contrast")
    })

    output$marginalTimes <- renderText({
        st <- marginal_state()
        req(st, identical(st$status, "done"))
        sprintf("Time A = %s, time B = %s", format(st$result$time_A), format(st$result$time_B))
    })

    output$ratioNoteFixed <- renderUI({ req(values$model); .ratio_note(values$model) })
    output$ratioNoteDecisions <- renderUI({ req(values$model); .ratio_note(values$model) })

    output$varianceComponentsTable <- renderDT({
        req(values$model)
        tryCatch({
            table_df <- summarize_variance_components(
                values$model, ci_level = (input$results_ci_level %||% 95) / 100)
            shiny::validate(need(!is.null(table_df) && nrow(table_df) > 0, "No variance components to report."))
            dt_table(table_df, digits = 3, caption = "Variance components")
        }, error = function(e) {
            dt_table(data.frame(Message = paste("Error:", e$message)), caption = "Variance components (error)")
        })
    })

    output$perCaseESTable <- renderDT({
        req(values$results$per_case_es)
        df <- values$results$per_case_es
        shiny::validate(need(nrow(df) > 0, "No per-case decisions computed."))
        digits <- input$results_digits %||% 2
        case_labels <- if (!is.null(values$model)) .case_display(df$case, values$model$data) else df$case
        display_df <- data.frame(Case = case_labels, Term = df$term, stringsAsFactors = FALSE)
        display_df$Mean <- round(df$mean, digits)
        display_df$SD <- round(df$sd, digits)
        display_df$CI_lower <- round(df$lower, digits)
        display_df$CI_upper <- round(df$upper, digits)
        ratio_cols <- grep("^(irr|or|mean_ratio|median_ratio)(_lower|_upper)?$", names(df), value = TRUE)
        display_df[.ratio_names(ratio_cols, "Case-specific (conditional)")] <-
            lapply(df[ratio_cols], function(x) round(x, digits))
        display_df$`P(dir)` <- round(df$pd, 3)
        if (any(!is.na(df$prob_meaningful_change)))
            display_df$`P(> MCID)` <- round(df$prob_meaningful_change, 3)
        DT::datatable(display_df,
                  options = list(dom = 'tp', pageLength = 20, scrollX = TRUE),
                  rownames = FALSE,
                  caption = .dt_caption("Per-case posterior decisions"))
    })

    # Model inputs trigger
    model_inputs_trigger <- reactive({
        list(
            input$research_design,
            input$model_family,
            input$trials_var,
            input$exposure_var,
            input$count_outcome_units,
            input$outcome_var,
            input$formula_mode,
            input$custom_fixed_formula,
            input$piecewise_terms,
            input$moderator_vars,
            input$center_moderators,
            input$scale_moderators,
            input$cross_level_interactions,
            random_effects_config()$formula,
            input$heterogeneous_variance,
            input$include_ar1,
            input$ar_covariance,
            input$ar_prior_sd,
            input$n_chains,
            input$n_iter,
            input$n_warmup,
            input$prior_type,
            input$prior_effect_mean,
            input$prior_effect_sd,
            input$adapt_delta,
            input$max_treedepth,
            input$parallel_chains,
            input$intervention_goal,
            values$prior_overrides_active,
            if (isTRUE(values$prior_overrides_active)) values$current_prior_list
        )
    })

    # stale only when settings differ from those of the fit, so re-sent identical values do not flag it
    observeEvent(model_inputs_trigger(), {
        req(values$model_fitted)
        values$model_stale <- !identical(model_inputs_trigger(), values$fit_inputs)
    }, ignoreInit = TRUE)

    # Run analysis
    observeEvent(input$runModel, { runModelLogic() })
    observeEvent(input$refitFromFit,     { runModelLogic() })
    observeEvent(input$refitFromEffects, { runModelLogic() })
    observeEvent(input$refitFromPerCase, { runModelLogic() })

    sync_guard <- new.env(parent = emptyenv())
    .prior_reset_inputs <- function(meta = FALSE) {
        pick <- function(id) {
            v <- if (meta) input[[paste0("meta_", id)]] else NULL
            if (is.null(v) || identical(v, "")) input[[id]] else v
        }
        lapply(c("prior_type", "prior_effect_mean", "prior_effect_sd", "outcome_var", "model_family"),
               function(id) pick(id) %||% NA)
    }

    # copy meta-tab inputs onto the shared SCD inputs used by the fit and prior preview
    .sync_meta_to_scd_inputs <- function() {
        sync_guard$expect <- .prior_reset_inputs(meta = TRUE)
        d <- values$data
        if (!is.null(d)) {
            numeric_vars <- names(d)[sapply(d, is.numeric)]
            default_outcome <- if ("outcome" %in% numeric_vars) "outcome"
                               else if (length(numeric_vars) > 0) numeric_vars[1] else NULL
            if (!is.null(default_outcome) &&
                (is.null(input$outcome_var) || !nzchar(input$outcome_var))) {
                updateSelectInput(session, "outcome_var",
                                  choices = numeric_vars, selected = default_outcome)
            }
            all_vars <- names(d)
            potential_moderators <- all_vars[!all_vars %in% c("outcome", "phase", "time", "session")]
            updateSelectInput(session, "moderator_vars", choices = potential_moderators)
            model_tab_loaded(TRUE)
        }
        if (!is.null(input$meta_research_design))
            updateSelectInput(session, "research_design",   selected = input$meta_research_design)
        if (!is.null(input$meta_intervention_goal))
            updateSelectInput(session, "intervention_goal", selected = input$meta_intervention_goal)
        if (!is.null(input$meta_outcome_type))
            updateSelectInput(session, "outcome_type",      selected = input$meta_outcome_type)
        updateSelectInput(session, "model_family",          selected = .meta_family())
        if (!is.null(input$meta_trials_var))
            updateSelectInput(session, "trials_var",        selected = input$meta_trials_var)
        if (!is.null(input$meta_outcome_var) && nzchar(input$meta_outcome_var))
            updateSelectInput(session, "outcome_var",       selected = input$meta_outcome_var)
        if (!is.null(input$meta_formula_mode))
            updateRadioButtons(session, "formula_mode",     selected = input$meta_formula_mode)
        if (!is.null(input$meta_custom_fixed_formula))
            updateTextAreaInput(session, "custom_fixed_formula", value = input$meta_custom_fixed_formula)
        if (!is.null(input$meta_moderator_vars))
            updateSelectInput(session, "moderator_vars",    selected = input$meta_moderator_vars)
        if (!is.null(input$meta_cross_level_interactions))
            updateCheckboxInput(session, "cross_level_interactions", value = input$meta_cross_level_interactions)
        if (!is.null(input$meta_center_moderators))
            updateCheckboxInput(session, "center_moderators", value = input$meta_center_moderators)
        if (!is.null(input$meta_scale_moderators))
            updateCheckboxInput(session, "scale_moderators",  value = input$meta_scale_moderators)
        if (!is.null(input$meta_count_outcome_units))
            updateSelectInput(session, "count_outcome_units", selected = input$meta_count_outcome_units)
        if (!is.null(input$meta_include_ar1))
            updateCheckboxInput(session, "include_ar1",     value = input$meta_include_ar1)
        if (!is.null(input$meta_ar_covariance))
            updateCheckboxInput(session, "ar_covariance",   value = input$meta_ar_covariance)
        if (!is.null(input$meta_heterogeneous_variance))
            updateCheckboxInput(session, "heterogeneous_variance", value = input$meta_heterogeneous_variance)
        if (!is.null(input$meta_n_chains))      updateNumericInput(session, "n_chains",      value = input$meta_n_chains)
        if (!is.null(input$meta_n_iter))        updateNumericInput(session, "n_iter",        value = input$meta_n_iter)
        if (!is.null(input$meta_n_warmup))      updateNumericInput(session, "n_warmup",      value = input$meta_n_warmup)
        if (!is.null(input$meta_adapt_delta))   updateNumericInput(session, "adapt_delta",   value = input$meta_adapt_delta)
        if (!is.null(input$meta_max_treedepth)) updateNumericInput(session, "max_treedepth", value = input$meta_max_treedepth)
        if (!is.null(input$meta_parallel_chains))
            updateCheckboxInput(session, "parallel_chains", value = input$meta_parallel_chains)
        if (!is.null(input$meta_prior_type))
            updateRadioButtons(session, "prior_type",       selected = input$meta_prior_type)
        if (!is.null(input$meta_prior_effect_mean))
            updateNumericInput(session, "prior_effect_mean", value = input$meta_prior_effect_mean)
        if (!is.null(input$meta_prior_effect_sd))
            updateNumericInput(session, "prior_effect_sd",   value = input$meta_prior_effect_sd)
        meta_re <- meta_random_effects_config()$formula
        if (!is.null(meta_re) && nzchar(trimws(meta_re)))
            updateTextAreaInput(session, "randomEffects-randomFormula", value = meta_re)
        if (!is.null(input$meta_ar_prior_sd))
            updateNumericInput(session, "ar_prior_sd", value = input$meta_ar_prior_sd)
    }

    .runMetaModelLogic <- function() {
        if (isTRUE(values$model_running)) {
            showNotification("A model fit is already running.", type = "warning", duration = 4)
            return()
        }
        .sync_meta_to_scd_inputs()
        values$pending_meta_fit_inputs <- isolate(meta_inputs_trigger())
        values$meta_model_refitting <- TRUE
        values$meta_fit_pending     <- TRUE
        values$meta_fit_pending_start <- TRUE
        values$model_fitted <- FALSE
    }

    observe({
        req(isTRUE(values$meta_fit_pending_start))
        if (is.null(input$outcome_var) || !nzchar(input$outcome_var %||% "")) {
            d <- isolate(values$data)
            if (!is.null(d)) {
                num_cols <- names(d)[vapply(d, is.numeric, logical(1))]
                pick <- if ("outcome" %in% num_cols) "outcome"
                        else if (length(num_cols) > 0) num_cols[1] else NULL
                if (!is.null(pick)) updateSelectInput(session, "outcome_var",
                                                     choices = num_cols, selected = pick)
            }
            if (is.null(d) || is.null(pick)) {
                showNotification("No numeric outcome column available to fit.", type = "error")
                isolate(.clear_meta_fit_flags())
            }
            return()
        }
        isolate({
            values$meta_fit_pending_start <- FALSE
            runModelLogic()
            if (!isTRUE(values$model_running) && !isTRUE(values$model_fitted))
                .clear_meta_fit_flags()
        })
    })

    .clear_meta_fit_flags <- function() {
        values$meta_model_refitting   <- FALSE
        values$meta_fit_pending       <- FALSE
        values$meta_fit_pending_start <- FALSE
    }

    observeEvent(values$model_running, {
        running <- isTRUE(values$model_running)
        fit_buttons <- c("runModel", "refitFromFit", "refitFromEffects",
                         "refitFromPerCase", "meta_runModel",
                         "meta_refitFromModel", "meta_refitFromFit",
                         "meta_refitFromEffects", "meta_refitFromPerStudy")
        toggle <- if (running) shinyjs::disable else shinyjs::enable
        lapply(fit_buttons, toggle)
        if (!running && isTRUE(values$meta_fit_pending) && !isTRUE(values$model_fitted))
            .clear_meta_fit_flags()
    }, ignoreNULL = FALSE)

    observe({
        req(isTRUE(values$meta_fit_pending))
        if (isTRUE(values$model_fitted)) {
            values$meta_model           <- values$model %||% values$current_model
            values$meta_model_loo       <- NULL
            values$meta_fit_brms_priors <- values$fit_brms_priors
            values$meta_results         <- values$results
            values$meta_diagnostics     <- values$diagnostics
            values$meta_fit_scope       <- values$fit_scope
            values$meta_model_fitted    <- TRUE
            values$meta_fit_inputs      <- values$pending_meta_fit_inputs
            values$meta_model_stale     <- !identical(isolate(meta_inputs_trigger()), values$meta_fit_inputs)
            values$meta_model_refitting <- FALSE
            values$meta_fit_pending     <- FALSE
        }
    })
    observeEvent(input$meta_runModel,        { .runMetaModelLogic() })
    observeEvent(input$meta_refitFromModel,  { .runMetaModelLogic() })
    observeEvent(input$meta_refitFromFit,    { .runMetaModelLogic() })
    observeEvent(input$meta_refitFromEffects,{ .runMetaModelLogic() })
    observeEvent(input$meta_refitFromPerStudy,{ .runMetaModelLogic() })

    meta_inputs_trigger <- reactive({
        list(input$meta_research_design, input$meta_outcome_type, input$meta_model_family,
             input$meta_outcome_var, input$meta_formula_mode, input$meta_custom_fixed_formula,
             input$meta_moderator_vars, input$meta_cross_level_interactions,
             input$meta_center_moderators, input$meta_scale_moderators,
             input$meta_piecewise_terms,
             meta_random_effects_config()$formula,
             input$meta_heterogeneous_variance,
             input$meta_include_ar1, input$meta_ar_prior_sd, input$meta_n_chains,
             input$meta_n_iter, input$meta_n_warmup, input$meta_prior_type,
             input$meta_standardize, input$meta_measure_var, input$meta_measure_value,
             input$meta_exposure_var, input$meta_count_outcome_units, input$meta_trials_var,
             input$meta_ar_covariance, input$meta_prior_effect_mean, input$meta_prior_effect_sd,
             input$meta_adapt_delta, input$meta_max_treedepth, input$meta_parallel_chains,
             input$meta_intervention_goal, values$prior_overrides_active,
             if (isTRUE(values$prior_overrides_active)) values$current_prior_list)
    })
    observeEvent(meta_inputs_trigger(), {
        req(values$meta_model_fitted)
        values$meta_model_stale <- !identical(meta_inputs_trigger(), values$meta_fit_inputs)
    }, ignoreInit = TRUE)

    observeEvent(input$meta_resetSettings, {
        values$design_override["meta_research_design"] <- FALSE
        .push_design("meta_research_design")
        updateSelectInput(session, "meta_outcome_type",    selected = "continuous")
        updateSelectInput(session, "meta_measure_var",     selected = "")
        updateCheckboxInput(session, "meta_standardize",   value = FALSE)
        showNotification("Meta-Analysis settings reset.", type = "message", duration = 3)
    })

    # Model fitting
    runModelLogic <- function() {
        req(values$data)
        if (isTRUE(values$model_running)) {
            showNotification("A model fit is already running.", type = "warning", duration = 4)
            return()
        }
        # settings at the click, so changes made while the fit runs still mark it stale
        values$pending_fit_inputs <- isolate(model_inputs_trigger())
        if (!.size_gate(values$processed_data %||% values$data, "fit")) return()

        # Input validation
        validation_errors <- character()

        n_iter_in <- .fit_in("n_iter") %||% 2000
        n_warmup_in <- .fit_in("n_warmup") %||% 1000
        if (!is.numeric(n_iter_in) || !is.numeric(n_warmup_in) ||
            is.na(n_iter_in) || is.na(n_warmup_in) || n_warmup_in >= n_iter_in) {
            validation_errors <- c(validation_errors,
                "Warmup must be smaller than total iterations")
        }

        # Check if outcome variable is selected
        if (is.null(.fit_in("outcome_var")) || .fit_in("outcome_var") == "") {
            validation_errors <- c(validation_errors, "Please select an outcome variable")
        }

        # Check if research design is selected
        if (is.null(.fit_in("research_design")) || .fit_in("research_design") == "") {
            validation_errors <- c(validation_errors, "Please select a research design")
        }

        # Check if moderators are properly selected (not empty strings)
        if (!is.null(.fit_in("moderator_vars")) && length(.fit_in("moderator_vars")) > 0) {
            if (any(.fit_in("moderator_vars") == "")) {
                validation_errors <- c(validation_errors, "Remove empty moderator selections")
            }
        }

        # If there are validation errors, show them and stop
        if (length(validation_errors) > 0) {
            showNotification(
                HTML(paste("<b>Cannot run model:</b><br/>",
                          paste("- ", validation_errors, collapse = "<br/>"))),
                type = "error",
                duration = 10
            )
            return()
        }

        # small-sample advisories on identifiability and sampler stability
        guard_data <- values$processed_data %||% values$data
        guard_case <- if ("case" %in% names(guard_data)) "case" else
                      (values$model_structure$case_vars %||% character(0))[1]
        if (!is.null(guard_case) && guard_case %in% names(guard_data)) {
            n_cases <- length(unique(guard_data[[guard_case]]))
            nest_hit <- find_nest_var(guard_data, exclude = c(guard_case, "study", "study_id"), case_var = guard_case)
            nest_n <- if (!is.null(nest_hit)) length(unique(guard_data[[nest_hit]])) else 0
            has_nest <- !is.null(nest_hit) && nest_n >= 2

            if (n_cases <= 1 && has_nest) {
                showNotification(
                    paste0("One case with ", nest_n, " levels of ", nest_hit,
                           ". Fitting random effects nested within the participant."),
                    type = "message", duration = 6)
            } else if (n_cases <= 1) {
                showNotification(
                    "Single-case data detected. Fitting without group-level variance.",
                    type = "message", duration = 6)
            } else if (has_nest) {
                msg <- paste0(n_cases, " cases and ", nest_n, " levels of ", nest_hit,
                              ". Fitting 3-level model (", nest_hit,
                              " nested within case).")
                if (n_cases <= 3) {
                    msg <- paste0(msg, " Case-level slope variance is prior-dominated.")
                }
                showNotification(msg, type = "message", duration = 8)
            } else if (n_cases <= 3) {
                showNotification(
                    paste0("Only ", n_cases, " cases. Random-slope variance posterior will be ",
                           "prior-dominated; treat between-case heterogeneity as exploratory."),
                    type = "message", duration = 8)
            }
            if ("phase" %in% names(guard_data)) {
                phase_n <- as.numeric(tapply(rep(1, nrow(guard_data)),
                                             list(guard_data[[guard_case]], guard_data$phase), sum))
                min_phase_n <- suppressWarnings(min(phase_n, na.rm = TRUE))
                if (is.finite(min_phase_n) && min_phase_n < 5) {
                    showNotification(
                        "Short phases detected (fewer than 5 observations per phase). Sampler will use adapt_delta = 0.99.",
                        type = "message", duration = 6)
                }
            }
        }

        # Initialize model running state
        fit_id <- fit_ids$run
        values$model_running <- TRUE
        values$model_log <- character()
        values$iteration_count <- 0
        values$model_start_time <- Sys.time()

        # Initialize progress bar
        shinyWidgets::updateProgressBar(session, id = "mcmc_progress", value = 0, title = "Initializing...")

        # Brief status at bottom right
        shiny::showNotification(
            "Running MCMC sampling...",
            type = "message", duration = NULL, id = "mcmc_status_notif"
        )

        # Full progress popup - use a plain pre element, updated via shinyjs::html()
        shiny::showModal(
            shiny::modalDialog(
                title = "Bayesian Analysis Progress",
                tags$div(
                    id = "mcmc_log_container",
                    style = "max-height: 500px; overflow-y: auto;",
                    tags$pre(id = "mcmc_log_text",
                             style = "font-size: 13px; margin: 0; white-space: pre-wrap;",
                             "Initializing analysis...")
                ),
                size = "l",
                easyClose = TRUE,
                footer = modalButton("Close")
            )
        )

        # Log helper: writes to console + pushes directly to modal DOM via shinyjs
        add_log <- function(msg) {
            clean_msg <- gsub("^\\[DEBUG\\]\\s*", "", msg)
            values$model_log <- c(values$model_log, clean_msg)
            cat(paste0(clean_msg, "\n"))
            shinyjs::html("mcmc_log_text", htmltools::htmlEscape(paste(values$model_log, collapse = "\n")))
            shinyjs::runjs("var el = document.getElementById('mcmc_log_container'); if(el) el.scrollTop = el.scrollHeight;")
        }

        add_log(sprintf("Data: %d obs, %d vars | Design: %s | Family: %s",
            nrow(values$data), ncol(values$data),
            .fit_in("research_design") %||% "ab",
            .fit_in("model_family") %||% "gaussian"))

        tryCatch({
            if (requireNamespace("brms", quietly = TRUE)) {
                shinyWidgets::updateProgressBar(session, id = "mcmc_progress", value = 5, title = "Preparing model...")

                # Use processed_data if available, otherwise use raw data
                model_data <- if (!is.null(values$processed_data)) values$processed_data else values$data

                # Get actual variable names from input
                outcome_var <- .fit_in("outcome_var")
                phase_var <- find_phase_var(model_data)
                case_var <- find_case_var(model_data)

                # required variables check
                if (!isTRUE(length(outcome_var) == 1 && outcome_var %in% names(model_data))) {
                    add_log("No outcome variable selected")
                    stop("Select an outcome column from the data.")
                }

                # a meta fit uses the formula of the meta piecewise checkboxes
                meta_fit <- isTRUE(values$meta_fit_pending)
                formula_str <- if (meta_fit) meta_formula_display() else model_formula()

                if (is.null(formula_str) && (identical(.fit_in("formula_mode"), "custom") ||
                                             meta_fit || !is.null(model_formula_raw()))) {
                    add_log("Formula rejected: only data columns, operators, and ar()/gr()/trials()/offset(log()) are allowed.")
                    stop("Formula rejected: check the custom or random-effects formula for columns not in the data or function calls.")
                }
                # Fallback if model_formula returns NULL
                if (is.null(formula_str)) {
                    add_log("WARNING: Using fallback formula generation")
                    if (is.null(phase_var) || length(phase_var) == 0) {
                        formula_str <- paste0(outcome_var, " ~ 1")
                    } else {
                        formula_str <- paste0(outcome_var, " ~ ", phase_var)
                    }
                    if (!is.null(case_var) && length(case_var) > 0) {
                        formula_str <- paste0(formula_str, " + (1|", case_var, ")")
                    }
                }

                meta_prep <- if (isTRUE(values$meta_fit_pending)) .meta_fit_prep(model_data, formula_str, outcome_var)
                if (!is.null(meta_prep)) {
                    model_data <- meta_prep$data
                    formula_str <- meta_prep$formula
                    outcome_var <- meta_prep$outcome
                    if (!is.null(meta_prep$table)) add_log(sprintf("Outcome standardized within %d tier(s) by residual SD.", nrow(meta_prep$table)))
                }
                transform <- if (!is.null(meta_prep)) meta_prep$transform else
                    .prepare_fit_data(model_data, outcome_var, .fit_in("model_family") %||% "gaussian")
                if (is.null(meta_prep)) model_data <- transform$data
                if (!is_safe_formula(formula_str, names(model_data)))
                    stop(.rejected_formula_msg(formula_str, model_data))

                add_log(paste("Formula:", formula_str))

                # Update formula display for UI
                values$formula_display <- formula_str
                local_fit_design <- .design_choice(.fit_in("research_design"), values$data)
                local_fit_scope <- .scope_inputs(isTRUE(values$meta_fit_pending))

                # Get total iterations for progress tracking
                mc <- .mcmc_settings(.fit_in("n_chains"), .fit_in("n_iter"), .fit_in("n_warmup"),
                                     .fit_in("adapt_delta"), .fit_in("max_treedepth"))
                n_chains <- mc$chains
                n_iter <- mc$iter
                n_warmup <- mc$warmup
                parallel_chains <- .fit_in("parallel_chains") %||% TRUE
                adapt_delta_val <- mc$adapt_delta
                max_treedepth_val <- mc$max_treedepth

                # Short-phase guardrail: bump adapt_delta when minimum phase length is small
                guard_data2 <- values$processed_data %||% values$data
                if (!is.null(guard_data2) && "phase" %in% names(guard_data2) && "case" %in% names(guard_data2)) {
                    phase_n2 <- as.numeric(tapply(rep(1, nrow(guard_data2)),
                                                  list(guard_data2$case, guard_data2$phase), sum))
                    min_phase_n2 <- suppressWarnings(min(phase_n2, na.rm = TRUE))
                    if (is.finite(min_phase_n2) && min_phase_n2 < 5 && adapt_delta_val < 0.99) {
                        adapt_delta_val <- 0.99
                    }
                }

                total_iterations <- n_chains * n_iter

                add_log(sprintf("MCMC: %d chains x %d iter (%d warmup), adapt_delta=%.3g, max_treedepth=%d",
                    n_chains, n_iter, n_warmup, adapt_delta_val, max_treedepth_val))

                # Calculate and store effect sizes immediately (before running model)
                outcome_var_name <- .fit_in("outcome_var") %||% "outcome"
                effects_calc <- tryCatch({
                    summarize_phase_descriptives(model_data,
                                                 phase_var = "phase",
                                                 outcome_var = outcome_var_name)
                }, error = function(e) {
                    summarize_phase_descriptives(data.frame())
                })

                case_col <- tryCatch(find_case_var(model_data), error = function(e) NULL) %||%
                            (if ("case" %in% names(model_data)) "case" else names(model_data)[1])
                values$results <- list(
                    data_summary = list(
                        n_observations = nrow(model_data),
                        n_cases = length(unique(model_data[[case_col]]))
                    ),
                    model = NULL,
                    effects = effects_calc
                )

                # Validate outcome type matches family
                model_family_value <- .fit_in("model_family") %||% "gaussian"
                outcome_vals <- model_data[[outcome_var]]
                if (!is.null(transform$exposure))
                    add_log(sprintf("Outcome converted to counts as round(outcome * %s).", transform$exposure))
                if (!is.null(transform$beta_n))
                    showNotification("Boundary values rescaled for Beta regression with fitted observation count.",
                                     type = "warning", duration = 8)
                if (model_family_value == "bernoulli" && !all(outcome_vals >= 0 & outcome_vals <= 1, na.rm = TRUE)) {
                    add_log("Bernoulli family requires outcomes in [0, 1].")
                    stop("Bernoulli family requires outcome values between 0 and 1.")
                }
                if (model_family_value == "binomial") {
                    trials_var <- .fit_in("trials_var") %||% ""
                    if (!trials_var %in% names(model_data)) {
                        stop("The binomial family needs a trials column; select it under Specific distribution.")
                    }
                    n_trials <- model_data[[trials_var]]
                    if (any(outcome_vals < 0 | outcome_vals > n_trials | outcome_vals != floor(outcome_vals), na.rm = TRUE)) {
                        stop("Binomial outcomes must be whole-number successes between 0 and the trials column.")
                    }
                }
                if (model_family_value == "Gamma" && any(outcome_vals <= 0, na.rm = TRUE)) {
                    add_log("Gamma family requires strictly positive outcomes.")
                    stop("Gamma family requires outcome values > 0.")
                }

                shinyWidgets::updateProgressBar(session, id = "mcmc_progress", value = 15, title = "Compiling Stan model...")
                add_log("Compiling Stan model...")

                shinyWidgets::updateProgressBar(session, id = "mcmc_progress", value = 20, title = "Starting MCMC sampling...")
                add_log("Starting MCMC sampling...")

                tryCatch({
                    # orientation (positive beta = improvement for a decrease goal) is applied in the decision layer via intervention_goal, not by reflecting the outcome
                    goal <- .fit_in("intervention_goal") %||% "increase"
                    prior_type_value <- .fit_in("prior_type") %||% "default"
                    fit_priors <- .scd_fit_priors(model_data, formula_str, model_family_value,
                                                  outcome_var)
                    prior_list <- fit_priors$prior_list
                    values$current_prior_list <- prior_list
                    local_prior_sources <- if (isTRUE(values$prior_overrides_active)) values$prior_sources
                    local_prior_type <- .fit_in("prior_type") %||% "default"
                    brms_priors <- fit_priors$brms_priors
                    add_log(sprintf("Prior: %s | b ~ %s",
                        prior_type_value, prior_list$treatment$string))

                    add_log("Fitting model (this may take a minute)...")
                    shinyWidgets::updateProgressBar(session, id = "mcmc_progress", value = 30, title = "Sampling...")

                    # Snapshot inputs for the async context
                    local_formula <- fit_priors$formula
                    if (fit_priors$hetero) add_log("Heterogeneous variance: sigma ~ phase")
                    if (isTRUE(.fit_in("heterogeneous_variance")) && !fit_priors$hetero) {
                        add_log(sprintf("Heterogeneous variance unavailable for %s (no residual sigma or phase column); fitting homogeneous model.", model_family_value))
                    }
                    local_has_ar <- grepl("\\bar\\(", formula_str)
                    local_data <- model_data
                    local_transform <- transform
                    local_family <- brms::brmsfamily(model_family_value)
                    local_priors <- brms_priors
                    local_n_chains <- n_chains
                    local_n_iter <- n_iter
                    local_n_warmup <- n_warmup
                    local_cores <- if (isTRUE(parallel_chains)) min(n_chains, parallel::detectCores()) else 1
                    local_threads <- if (local_has_ar) NULL else brms_threads
                    local_adapt <- adapt_delta_val
                    local_treedepth <- max_treedepth_val
                    local_cache_dir <- brms_cache_dir
                    local_processed <- if (!is.null(values$processed_data)) values$processed_data else values$data
                    local_outcome_var <- .fit_in("outcome_var") %||% "outcome"
                    local_cache_file <- sub("\\.rds$", "", .session_model_file())
                    # file_refit compares only Stan code and data, so new sampler settings need a fresh file
                    unlink(.session_model_file())
                    local_stan_args <- .stan_model_args()

                    promises::future_promise(seed = TRUE, scdbayes:::.drop_sampler_files(.session_dir, {
                        brms::brm(
                            formula = local_formula,
                            data = local_data,
                            family = local_family,
                            prior = local_priors,
                            chains = local_n_chains,
                            iter = local_n_iter,
                            warmup = local_n_warmup,
                            cores = local_cores,
                            threads = local_threads,
                            backend = brms_backend,
                            stan_model_args = local_stan_args,
                            file = local_cache_file,
                            file_refit = "on_change",
                            refresh = 0,
                            silent = 2,
                            control = list(adapt_delta = local_adapt, max_treedepth = local_treedepth)
                        )
                    })) %...>% (function(fitted_model) {
                        # a fit that ends after the session closed leaves its file behind
                        if (session$isClosed()) return(unlink(.session_model_file()))
                        # data loaded after launch: this fit describes other data
                        if (!identical(fit_ids$run, fit_id)) {
                            values$model_running <- FALSE
                            try(removeNotification("mcmc_status_notif"), silent = TRUE)
                            return(invisible(NULL))
                        }
                        values$model <- fitted_model
                        values$meta_fit_prep <- meta_prep
                        values$fit_design <- local_fit_design
                        values$fit_scope <- local_fit_scope
                        values$fit_prior_list <- prior_list
                        values$fit_prior_sources <- local_prior_sources
                        values$fit_prior_type <- local_prior_type
                        values$fit_brms_priors <- local_priors
                        values$fit_hetero_sigma <- inherits(local_formula, "brmsformula")
                        values$model_loo <- NULL

                        add_log("MCMC sampling completed!")
                        shinyWidgets::updateProgressBar(session, id = "mcmc_progress", value = 80, title = "Running diagnostics...")

                        values$diagnostics <- tryCatch(
                            compute_convergence_diagnostics(fitted_model, local_data),
                            error = function(e) list(basic = list(rhat = numeric(0), ess_bulk = numeric(0)), recommendations = list(), converged = NULL)
                        )

                        effects_calc <- tryCatch(
                            summarize_phase_descriptives(local_processed,
                                                         phase_var = "phase",
                                                         outcome_var = local_outcome_var),
                            error = function(e) summarize_phase_descriptives(data.frame())
                        )

                        local_case_col <- tryCatch(find_case_var(local_processed), error = function(e) NULL) %||%
                                          (if ("case" %in% names(local_processed)) "case" else names(local_processed)[1])
                        if (is.null(values$results)) values$results <- list()
                        values$results$data_summary <- list(
                            n_observations = nrow(local_processed),
                            n_cases = length(unique(local_processed[[local_case_col]])))
                        values$results$model <- fitted_model
                        if (!is.null(effects_calc$raw_difference) && !is.na(effects_calc$raw_difference)) {
                            values$results$effects <- effects_calc
                        }

                        ci_level <- (input$results_ci_level %||% 95) / 100
                        values$results$posterior_es <- tryCatch(
                            summarize_fixed_effect_decisions(fitted_model, threshold = .mcid(),
                                                             rope = .default_rope(fitted_model),
                                                             ci_level = ci_level, intervention_goal = goal),
                            error = function(e) NULL
                        )

                        values$results$per_case_es <- tryCatch(
                            summarize_per_case_effects(fitted_model, case_var = local_case_col,
                                                       threshold = .mcid(),
                                                       rope = .default_rope(fitted_model),
                                                       ci_level = ci_level, intervention_goal = goal),
                            error = function(e) NULL
                        )

                        values$fit_transform <- local_transform
                        values$fit_sampler <- list(adapt_delta = local_adapt, max_treedepth = local_treedepth)
                        values$model_fitted <- TRUE
                        values$model_refitting <- FALSE
                        values$model_running <- FALSE
                        # a meta fit syncs these inputs after the click, so its baseline is read at completion
                        values$fit_inputs <- if (isTRUE(values$meta_fit_pending)) isolate(model_inputs_trigger()) else values$pending_fit_inputs
                        values$model_stale <- !identical(isolate(model_inputs_trigger()), values$fit_inputs)
                        shinyWidgets::updateProgressBar(session, id = "mcmc_progress", value = 100, title = "Complete!")

                        add_log("Analysis complete! Check Results tab.")
                        try(removeNotification("mcmc_status_notif"), silent = TRUE)
                        shiny::showNotification("Model fitted successfully.", type = "message", duration = 5)
                    }) %...!% (function(e) {
                        add_log(paste("Analysis failed:", .short_error(e)))
                        values$model_running <- FALSE
                        try(removeNotification("mcmc_status_notif"), silent = TRUE)
                        shiny::showNotification("Model fitting failed.", type = "error", duration = 10)
                    })

                }, error = function(e) {
                    add_log(paste("Model setup failed:", .short_error(e)))
                    values$model_running <- FALSE
                    try(removeNotification("mcmc_status_notif"), silent = TRUE)
                    shiny::showNotification("Model setup failed.", type = "error", duration = 10)
                })

            } else {
                add_log("brms package not available, using fallback analysis")

                # Use the processed data for calculating results
                model_data_final <- if (!is.null(values$processed_data)) values$processed_data else values$data

                # Calculate effect sizes for fallback analysis
                outcome_var_name <- input$outcome_var %||% "outcome"
                effects_calc <- tryCatch({
                    summarize_phase_descriptives(model_data_final,
                                                 phase_var = "phase",
                                                 outcome_var = outcome_var_name)
                }, error = function(e) {
                    add_log(paste("Warning: Could not summarize phase descriptives:", e$message))
                    summarize_phase_descriptives(data.frame())
                })

                if (!is.null(effects_calc$raw_difference) && !is.na(effects_calc$raw_difference)) {
                    add_log("Phase descriptives:")
                    add_log(sprintf("  Raw mean difference (B - A): %.3f", effects_calc$raw_difference))
                    add_log(sprintf("  Phase A mean: %.3f (n = %d)", effects_calc$phase_a_mean, effects_calc$phase_a_n))
                    add_log(sprintf("  Phase B mean: %.3f (n = %d)", effects_calc$phase_b_mean, effects_calc$phase_b_n))
                }

                values$results <- run_scd_analysis(
                    values$data,
                    options = list(
                        quick_test = TRUE,
                        expected_effect_size = 0.5
                    )
                )

                # Merge effect sizes into results
                if (!is.null(values$results)) {
                    values$results$effects <- effects_calc
                }

                if (is.null(values$model) && !is.null(values$results$model)) {
                    values$model <- values$results$model
                    values$model_loo <- NULL
                }

                # Complete for synchronous fallback path only
                shinyWidgets::updateProgressBar(session, id = "mcmc_progress", value = 100, title = "Complete!")
                shinyWidgets::updateProgressBar(session, id = "mcmc_progress", value = 100, title = "Analysis complete!")
                values$model_running <- FALSE
                values$model_fitted <- TRUE
                values$model_refitting <- FALSE
                # a meta fit syncs these inputs after the click, so its baseline is read at completion
                values$fit_inputs <- if (isTRUE(values$meta_fit_pending)) isolate(model_inputs_trigger()) else values$pending_fit_inputs
                values$model_stale <- !identical(isolate(model_inputs_trigger()), values$fit_inputs)
                add_log("Analysis complete! Check Results tab for output.")
                try(removeNotification("mcmc_status_notif"), silent = TRUE)
                shiny::showNotification("Model fitted successfully.", type = "message", duration = 5)
            }

        }, error = function(e) {
            add_log(paste("Analysis failed:", .short_error(e)))
            values$model_running <- FALSE
            try(removeNotification("mcmc_status_notif"), silent = TRUE)
            shiny::showNotification("Analysis failed.", type = "error", duration = 10)
        })
    } # End runModelLogic function



    # Results-tab metrics from convergence diagnostics
    output$samplerHealthBanner <- renderUI({
        m <- values$model
        if (is.null(m)) return(NULL)
        np <- tryCatch(brms::nuts_params(m), error = function(e) NULL)
        if (is.null(np)) return(NULL)
        n_div <- sum(np$Value[np$Parameter == "divergent__"] > 0, na.rm = TRUE)
        max_td <- max(np$Value[np$Parameter == "treedepth__"], na.rm = TRUE)
        max_treedepth_val <- .mcmc_clamp(input$max_treedepth, 8, 20, 12)
        msgs <- character()
        if (isTRUE(n_div > 0))
            msgs <- c(msgs, sprintf("%d divergent transition(s) — raise adapt_delta or reparameterize", n_div))
        if (isTRUE(max_td >= max_treedepth_val))
            msgs <- c(msgs, sprintf("max_treedepth %d hit — raise max_treedepth", max_treedepth_val))
        if (length(msgs) == 0) {
            return(div(class = "status-main-success",
                style = "padding: 6px 12px; margin: 4px 0;",
                icon("check-circle"), " Sampler diagnostics clean."))
        }
        div(class = "status-main-warning",
            style = "padding: 6px 12px; margin: 4px 0;",
            icon("triangle-exclamation"),
            tags$strong(" Sampler warnings: "),
            paste(msgs, collapse = "; "))
    })
    outputOptions(output, "samplerHealthBanner", suspendWhenHidden = FALSE)

    output$rhatMax <- renderText({
        if (!is.null(values$diagnostics) && !is.null(values$diagnostics$basic$rhat)) {
            max_rhat <- max(values$diagnostics$basic$rhat, na.rm = TRUE)
            sprintf("%.4f", max_rhat)
        } else if (!is.null(values$model)) {
            tryCatch({
                rhat_vals <- brms::rhat(values$model)
                sprintf("%.4f", max(rhat_vals, na.rm = TRUE))
            }, error = function(e) "N/A")
        } else {
            "N/A"
        }
    })

    output$essMin <- renderText({
        if (!is.null(values$diagnostics) && !is.null(values$diagnostics$basic$ess_bulk)) {
            min_ess <- min(values$diagnostics$basic$ess_bulk, na.rm = TRUE)
            sprintf("%.0f", min_ess)
        } else if (!is.null(values$model)) {
            tryCatch({
                ess_vals <- posterior::summarise_draws(brms::as_draws_array(values$model), "ess_bulk")$ess_bulk
                sprintf("%.0f", min(ess_vals, na.rm = TRUE))
            }, error = function(e) "N/A")
        } else {
            "N/A"
        }
    })

    output$bayesR2 <- renderText({
        if (!is.null(values$model)) {
            tryCatch({
                r2 <- brms::bayes_R2(values$model)
                sprintf("%.3f", mean(r2[,1]))
            }, error = function(e) "N/A")
        } else {
            "N/A"
        }
    })

    output$looic <- renderText({
        loo_res <- .cached_loo()
        if (is.null(loo_res)) return("N/A")
        sprintf("%.1f", loo_res$estimates["looic", "Estimate"])
    })

    # Model Summary for Results tab
    output$modelSummary <- renderPrint({
        if (!is.null(values$model)) {
            out <- capture.output(summary(values$model))
            out <- out[!grepl("^={4,}$|^-{4,}$", out)]
            cat(paste(out, collapse = "\n"))
        } else if (!is.null(values$results)) {
            cat("Model Results Summary\n\n")
            cat("Data Summary:\n")
            cat(sprintf("  Observations: %d\n", values$results$data_summary$n_observations))
            cat(sprintf("  Cases: %d\n", values$results$data_summary$n_cases))
            cat("\nPhase descriptives:\n")
            if (!is.null(values$results$effects)) {
                ef <- values$results$effects
                cat(sprintf("  Raw mean difference (B - A): %.3f\n", ef$raw_difference))
                if (!is.null(ef$phase_a_mean)) cat(sprintf("  Phase A mean: %.3f (n = %d)\n", ef$phase_a_mean, ef$phase_a_n))
                if (!is.null(ef$phase_b_mean)) cat(sprintf("  Phase B mean: %.3f (n = %d)\n", ef$phase_b_mean, ef$phase_b_n))
            }
        }
    })

    # Prior Display output
    .prior_rules <- c(
        "Weakly informative: normal(0, 2.5 x SD(outcome)); normal(0, 2.5) on log/logit links.",
        "Informative: normal(elicited mean, half the weakly informative SD).",
        "Meta-analytic: normal(mean, SD) as entered.",
        "Diffuse: normal(0, 2 x the weakly informative SD).")

    output$priorDisplay <- renderPrint({
        prior_type <- input$prior_type %||% "default"
        d <- values$processed_data %||% values$data
        family_val <- input$model_family %||% "gaussian"
        type_label <- switch(prior_type,
            default = "Weakly informative", informative = "Informative",
            meta_analytic = "Meta-analytic", sensitivity = "Diffuse (sensitivity)",
            "Weakly informative")
        adaptive <- if (is.null(d)) NULL else tryCatch(
            .adaptive_priors(d, values$model_structure, family_val, input$outcome_var, .exposure_in(FALSE)),
            error = function(e) NULL)
        cat(sprintf("Prior type: %s\n\n", type_label))
        if (is.null(adaptive)) {
            cat(paste0("  ", .prior_rules, "\n"), sep = "")
            cat("\nLoad a dataset to preview the exact prior strings.\n")
            return(invisible(NULL))
        }
        preview_formula <- model_formula() %||% ""
        fp <- tryCatch(.scd_fit_priors(d, preview_formula, family_val, input$outcome_var),
                       error = function(e) NULL)
        if (is.null(fp)) {
            cat("Priors cannot be built for the current formula.\n")
            return(invisible(NULL))
        }
        pl <- fp$prior_list
        if (isTRUE(values$prior_overrides_active)) cat("Active overrides (preset or edited priors) replace the type's priors.\n\n")
        bp <- as.data.frame(fp$brms_priors)
        field <- function(x) ifelse(is.na(x) | !nzchar(x), "", x)
        target <- paste0(bp$class, ifelse(nzchar(field(bp$coef)), paste0(" (", bp$coef, ")"), ""),
                         ifelse(nzchar(field(bp$dpar)), paste0(" [", bp$dpar, "]"), ""))
        bounds <- ifelse(nzchar(field(bp$lb)) | nzchar(field(bp$ub)),
                         sprintf("  bounds [%s, %s]", field(bp$lb), field(bp$ub)), "")
        cat(sprintf("  %-24s %s%s\n", target, bp$prior, bounds), sep = "")
        if (prior_type == "meta_analytic" && !is.null(attr(pl, "treatment_sd"))) {
            sd_use <- attr(pl, "treatment_sd")
            cat(sprintf("\nMeta-analytic on b: mean=%.2f, sd=%.2f.\n", attr(pl, "treatment_loc"), sd_use))
            if (family_val %in% c("poisson", "negbinomial", "zero_inflated_poisson", "Gamma")) {
                band <- exp(c(-1.96, 1.96) * sd_use)
                cat(sprintf("Response-scale 95%% band: x%.2f to x%.2f.\n", band[1], band[2]))
            } else if (family_val %in% c("bernoulli", "binomial", "beta", "cumulative")) {
                band <- stats::plogis(c(-1.96, 1.96) * sd_use)
                cat(sprintf("Response-scale 95%% band (prob): %.2f to %.2f.\n", band[1], band[2]))
            }
        }
        if (prior_type == "sensitivity") {
            cat("\nA single diffuse prior is fit. For a full sensitivity check, tick 'Sensitivity analyses' on the Per Case tab.\n")
        }
    })

    register_info_popups(input)

    .showPriorGuideModal <- function(fam) {
        showModal(info_popup_modal(
            title = "Prior choice",
            body = tagList(
                tags$p("Choose priors on the fitted coefficient scale and check plausible outcome values."),
                tags$p("Preview prior compares prior-simulated outcomes with the data. Repeat the analysis with alternative plausible priors to assess sensitivity.")
            ),
            citation = "Stan Development Team. (n.d.). <em>Prior choice recommendations</em>. <a href=\"https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations\" target=\"_blank\" rel=\"noopener noreferrer\">https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations</a>"
        ))
    }
    observeEvent(input$showPriorGuide,      { .showPriorGuideModal(input$model_family) })
    observeEvent(input$meta_showPriorGuide, { .showPriorGuideModal(input$meta_model_family) })

    observeEvent(input$showPriorWizard, {
        showModal(modalDialog(
            title = "Help me pick a prior",
            radioButtons("wizard_prior_evidence",
                "1. Do prior single-case studies inform an expected effect?",
                choices = list("Yes — meta-analytic mean and SD" = "meta",
                               "Yes — direction only, magnitude unclear" = "informative",
                               "No prior studies" = "none"),
                selected = "none"),
            radioButtons("wizard_small_n",
                "2. Number of cases?",
                choices = list("≤ 3" = "small",
                               "4-10" = "medium",
                               "10+" = "large"),
                selected = "medium"),
            radioButtons("wizard_sensitivity",
                "3. Run sensitivity comparison?",
                choices = list("No" = "no", "Yes — default/narrow/diffuse treatment priors" = "yes"),
                selected = "no"),
            footer = tagList(
                actionButton("applyPriorWizard", "Apply", class = "btn-primary"),
                modalButton("Cancel")
            ), easyClose = TRUE
        ))
    })

    observeEvent(input$applyPriorWizard, {
        choice <- input$wizard_prior_evidence
        new_type <- switch(choice,
            meta = "meta_analytic",
            informative = "informative",
            "default")
        updateRadioButtons(session, "prior_type", selected = new_type)
        if (choice == "meta") {
            updateNumericInput(session, "prior_effect_mean",
                value = if (identical(input$intervention_goal, "decrease")) -0.5 else 0.5)
            updateNumericInput(session, "prior_effect_sd",
                value = if (input$wizard_small_n == "small") 0.2 else 0.3)
        }
        if (input$wizard_sensitivity == "yes") {
            updateCheckboxInput(session, "run_sensitivity", value = TRUE)
            updateCheckboxGroupInput(session, "sensitivity_types", selected = "prior")
        }
        removeModal()
        showNotification(sprintf("Prior set to '%s'.", new_type),
                         type = "message", duration = 4)
    })

    .showPriorOverrideModal <- function() {
        pl <- values$current_prior_list
        if (is.null(pl)) {
            showNotification("Run a fit first so the active prior list is populated.",
                             type = "warning", duration = 5)
            return(invisible(NULL))
        }
        rows <- lapply(names(pl), function(nm) {
            p <- pl[[nm]]
            tag <- if (!is.null(p$dpar)) sprintf("%s (%s)", p$class, p$dpar) else p$class
            fluidRow(
                column(4, tags$div(style = "padding-top: 8px;", tags$strong(nm),
                                   tags$br(), tags$small(tag, style = "color: #64748B;"))),
                column(8, textInput(paste0("override_", nm), NULL,
                                    value = p$string, width = "100%"))
            )
        })
        showModal(modalDialog(
            title = "Edit individual priors",
            tags$p(style = "color: #64748B; font-size: 13px;",
                "Edit any prior string in Stan syntax (e.g., normal(0, 1.5)).",
                "Empty entries fall back to the assembled default."),
            do.call(tagList, rows),
            footer = tagList(
                actionButton("applyPriorOverride", "Apply", class = "btn-primary"),
                modalButton("Cancel")
            ), size = "l", easyClose = TRUE
        ))
    }
    observeEvent(input$showPriorOverride,      { .showPriorOverrideModal() })
    observeEvent(input$meta_showPriorOverride, { .showPriorOverrideModal() })

    # Meta prior wizard (updates the meta_ prior inputs)
    observeEvent(input$meta_showPriorWizard, {
        showModal(modalDialog(
            title = "Help me pick a prior",
            radioButtons("meta_wizard_prior_evidence",
                "1. Do prior single-case studies inform an expected effect?",
                choices = list("Yes — meta-analytic mean and SD" = "meta",
                               "Yes — direction only, magnitude unclear" = "informative",
                               "No prior studies" = "none"),
                selected = "none"),
            radioButtons("meta_wizard_small_n",
                "2. Number of studies?",
                choices = list("≤ 3" = "small", "4-10" = "medium", "10+" = "large"),
                selected = "medium"),
            radioButtons("meta_wizard_sensitivity",
                "3. Run sensitivity comparison?",
                choices = list("No" = "no", "Yes — default/narrow/diffuse treatment priors" = "yes"),
                selected = "no"),
            footer = tagList(
                actionButton("meta_applyPriorWizard", "Apply", class = "btn-primary"),
                modalButton("Cancel")
            ), easyClose = TRUE
        ))
    })

    observeEvent(input$meta_applyPriorWizard, {
        choice <- input$meta_wizard_prior_evidence
        new_type <- switch(choice,
            meta = "meta_analytic", informative = "informative", "default")
        updateRadioButtons(session, "meta_prior_type", selected = new_type)
        if (choice == "meta") {
            updateNumericInput(session, "meta_prior_effect_mean",
                value = if (identical(input$meta_intervention_goal, "decrease")) -0.5 else 0.5)
            updateNumericInput(session, "meta_prior_effect_sd",
                value = if (input$meta_wizard_small_n == "small") 0.2 else 0.3)
        }
        if (input$meta_wizard_sensitivity == "yes") {
            updateCheckboxInput(session, "meta_run_sensitivity", value = TRUE)
            updateCheckboxGroupInput(session, "meta_sensitivity_types", selected = "prior")
        }
        removeModal()
        showNotification(sprintf("Prior set to '%s'.", new_type),
                         type = "message", duration = 4)
    })

    # Meta prior predictive (mirrors the SCD runPriorPredictive on meta_ inputs)
    output$meta_priorPredictiveReady <- reactive({ isTRUE(values$meta_prior_predictive_ready) })
    outputOptions(output, "meta_priorPredictiveReady", suspendWhenHidden = FALSE)

    observeEvent(input$meta_runPriorPredictive, {
        req(values$data)
        if (isTRUE(values$meta_prior_pred_running))
            return(showNotification("Prior predictive is already running.", type = "warning", duration = 4))
        if (!.size_gate(values$processed_data %||% values$data, "fit")) return()
        showNotification("Sampling from prior only...", type = "message", duration = 5)
        family_val <- .meta_family()
        formula_str <- meta_formula_display()
        req(formula_str)
        # same subset and standardization as the meta fit
        prep <- .meta_fit_prep(values$processed_data %||% values$data, formula_str,
                               input$meta_outcome_var %||% "outcome")
        model_data <- prep$data
        formula_str <- prep$formula
        fp <- tryCatch(.scd_fit_priors(model_data, formula_str, family_val, prep$outcome, TRUE),
                       error = function(e) NULL)
        if (is.null(fp)) {
            showNotification("Adaptive prior generation failed.",
                             type = "error", duration = 5); return()
        }
        brms_priors <- fp$brms_priors
        preview_formula <- fp$formula
        family_obj <- brms::brmsfamily(family_val)
        stan_args <- .stan_model_args()
        values$meta_prior_pred_running <- TRUE
        promises::finally(promises::future_promise(seed = TRUE, scdbayes:::.drop_sampler_files(.session_dir, {
            brms::brm(preview_formula, data = model_data,
                family = family_obj, prior = brms_priors,
                sample_prior = "only", chains = 2, iter = 1000,
                refresh = 0, silent = 2,
                backend = brms_backend,
                stan_model_args = stan_args)
        })) %...>% (function(fit_prior) {
            values$meta_prior_predictive_model <- fit_prior
            values$meta_prior_predictive_ready <- TRUE
            showNotification("Prior predictive ready.", type = "message", duration = 4)
        }) %...!% (function(e) {
            showNotification(paste("Prior predictive failed:", e$message),
                type = "error", duration = 10)
        }), onFinally = function() values$meta_prior_pred_running <- FALSE)
    })

    output$meta_priorPredictivePlot <- renderPlot({
        req(values$meta_prior_predictive_model)
        tryCatch(brms::pp_check(values$meta_prior_predictive_model,
                                ndraws = 50, type = "dens_overlay"),
                 error = function(e) { plot.new(); title("PP check unavailable") })
    }, alt = "Prior predictive density overlay: observed outcome density against draws from the prior")

    observeEvent(list(input$meta_prior_type, input$meta_prior_effect_mean,
                      input$meta_prior_effect_sd, input$meta_outcome_var,
                      input$meta_model_family, input$meta_outcome_type,
                      input$meta_standardize, input$meta_measure_var, input$meta_measure_value), {
        values$meta_prior_predictive_ready <- FALSE
        values$meta_prior_predictive_model <- NULL
    }, ignoreInit = TRUE)

    observeEvent(input$showPowerGuide, {
        guide_path <- file.path("markdown", "guides", "power_guide.html")
        showModal(info_popup_modal(
            title = "Power analysis",
            body = if (file.exists(guide_path)) HTML(paste(readLines(guide_path, warn = FALSE), collapse = "\n"))
                   else "Guide not found.",
            size = "m"
        ))
    })

    values$power_results <- NULL

    output$powerStatus <- renderText({
        if (is.null(values$power_results)) return("Power depends on the specified assumptions. Simulation not run.")
        plan <- tryCatch(scdbayes:::.power_plan(input), error = function(e) NULL)
        previous <- attr(values$power_results, "requested_planning_args")
        if (is.null(plan) || !identical(previous, plan$args))
            return("Settings changed. Run simulation again to update the displayed estimates.")
        sprintf("Completed simulation: %s target, %d studies, %d cases per study, %d replicates per effect.",
                plan$target, plan$args$n_studies, plan$args$n_cases,
                attr(values$power_results, "planning_args")$n_sims)
    })
    output$powerReady <- reactive({ !is.null(values$power_results) })
    outputOptions(output, "powerReady", suspendWhenHidden = FALSE)

    # count families put the effect SD on the log-rate scale, where 0.7 means about a 2x spread in case rates
    observeEvent(input$power_family, {
        count <- input$power_family %in% c("poisson", "negbinomial")
        shinyjs::toggleState("power_autocorr", condition = !count)
        if (count) updateNumericInput(session, "power_autocorr", value = 0)
        # a value the user typed is kept; only the other family's default is swapped
        if (isTRUE(input$power_slope_sd %in% c(0.3, 0.7)))
            updateNumericInput(session, "power_slope_sd",
                value = if (input$power_family %in% c("poisson", "negbinomial")) 0.3 else 0.7)
    }, ignoreInit = TRUE)

    observeEvent(input$runPowerSim, {
        if (isTRUE(values$power_running)) {
            showNotification("A power simulation is already running.", type = "warning", duration = 4)
            return()
        }
        plan <- tryCatch(scdbayes:::.power_plan(input), error = function(e) e)
        if (inherits(plan, "error")) {
            showNotification(conditionMessage(plan), type = "error", duration = 8)
            return()
        }
        args <- plan$args
        es <- args$effect_sizes
        if (.scdbayes_hosted && args$n_sims * length(es) > 500) {
            args$n_sims <- as.integer(500 %/% length(es))
            showNotification(sprintf("Hosted limit: replicates lowered to %d (500 fits total).", args$n_sims),
                             type = "warning", duration = 6)
        }
        args <- .formal_args(run_power_simulation, args)
        reset_power <- function() {
            values$power_running <- FALSE
            removeNotification("power_running")
            shinyjs::enable("runPowerSim")
        }
        values$power_running <- TRUE
        shinyjs::disable("runPowerSim")
        start_msg <- sprintf(
            "Running %d replicates per effect size (%d total fits). Results appear when all fits finish.",
            args$n_sims, args$n_sims * length(es))
        showNotification(start_msg, id = "power_running", type = "message", duration = NULL)
        .announce(start_msg)
        # warnings are raised in the worker process, so collect them there and return them
        fut <- tryCatch(
            promises::future_promise(scdbayes:::.drop_sampler_files(.session_dir, {
                warn <- character(0)
                out <- withCallingHandlers(
                    do.call(run_power_simulation, args),
                    warning = function(w) {
                        warn <<- c(warn, conditionMessage(w))
                        invokeRestart("muffleWarning")
                    })
                attr(out, "warnings") <- unique(warn)
                out
            }),
                seed = TRUE,
                globals = c(".session_dir", "run_power_simulation", "args")),
            error = function(e) {
                reset_power()
                showNotification(paste("Power simulation could not start:", conditionMessage(e)),
                                 type = "error", duration = 10)
                NULL
            })
        if (is.null(fut)) return()
        promises::finally(promises::then(fut,
            onFulfilled = function(res) {
                attr(res, "planning_args") <- args
                attr(res, "requested_planning_args") <- plan$args
                values$power_results <- res
                warn <- attr(res, "warnings") %||% character(0)
                showNotification("Power simulation complete.",
                                 type = "message", duration = 5)
                if (length(warn))
                    showNotification(paste(warn, collapse = " "), type = "warning", duration = NULL)
                .announce(paste(c("Power simulation complete. Results are shown in the Power Analysis tab.",
                                  warn), collapse = " "))
            },
            onRejected = function(e) {
                fail_msg <- paste("Power simulation failed:", e$message)
                showNotification(fail_msg, type = "error", duration = 10)
                .announce(fail_msg)
            }),
            onFinally = reset_power)
    })

    output$powerPlot <- plotly::renderPlotly({
        req(values$power_results)
        df <- values$power_results
        mcse_bars <- if ("mc_se" %in% names(df)) {
            df$lo <- df$power_lo %||% pmax(0, df$power - 2 * df$mc_se)
            df$hi <- df$power_hi %||% pmin(1, df$power + 2 * df$mc_se)
            geom_errorbar(data = df, aes(ymin = lo, ymax = hi), width = 0.05, color = "#2E5090")
        }
        df$valid_label <- .power_valid_label(df)
        p <- ggplot(df, aes(x = effect_size, y = power)) +
            geom_hline(yintercept = 0.80, linetype = "dashed", color = "#15803d") +
            geom_hline(yintercept = 0.50, linetype = "dotted", color = "#b45309") +
            mcse_bars +
            geom_line(color = "#2E5090", linewidth = 1) +
            # lower bound on power: non-converged fits counted as misses
            (if ("power_all" %in% names(df)) geom_line(aes(y = power_all), linetype = "dashed", color = "#64748b")) +
            # text is a plotly tooltip aesthetic that ggplot2 does not know
            suppressWarnings(geom_point(aes(text = paste("valid fits:", valid_label)), size = 3, color = "#2E5090")) +
            geom_text(aes(label = valid_label), nudge_y = 0.06, size = 3, color = "#334155") +
            scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.2)) +
            labs(x = if (any(df$true_beta < 0, na.rm = TRUE)) "Effect size (magnitude of the decrease)" else "Effect size",
                 y = "Conditional power") +
            accessible_plot_theme()
        styled_ggplotly(p)
    })

    output$powerTable <- DT::renderDataTable({
        req(values$power_results)
        df <- values$power_results
        df$valid_of_sims <- .power_valid_label(df)
        front <- intersect(c("effect_size", "true_beta", "power", "mc_se", "power_all", "mc_se_all", "valid_of_sims"),
                           names(df))
        dt_table(df[c(front, setdiff(names(df), front))], digits = 3,
                 caption = paste("Power by effect size (power over converged fits; power_all counts non-converged",
                                 "fits as misses, so at effect size 0 it understates the Type I rate)"))
    })

    output$downloadPowerPlan <- downloadHandler(
        filename = function() "power-simulation.json",
        content = function(file) {
            req(values$power_results)
            jsonlite::write_json(list(
                specification = attr(values$power_results, "planning_args"),
                requested_specification = attr(values$power_results, "requested_planning_args"),
                engine = "brms/Stan", package_version = as.character(utils::packageVersion("scdbayes")),
                results = values$power_results, warnings = attr(values$power_results, "warnings")),
                file, pretty = TRUE, auto_unbox = TRUE, na = "null")
        })

    # power uses converged fits only, so the denominator is shown beside it
    .power_valid_label <- function(df) {
        if (!all(c("n_valid", "n_sims") %in% names(df))) return(rep("", nrow(df)))
        sprintf("%d / %d", as.integer(df$n_valid), as.integer(df$n_sims))
    }

    # replication prior: normal(earlier estimate, inflation x earlier SE) on the treatment coefficient
    replication_scope <- reactiveVal("scd")
    .replication_string <- function() {
        est <- .mcmc_num(input$rep_estimate, NA_real_)
        se <- .mcmc_num(input$rep_se, NA_real_)
        infl <- .mcmc_num(input$rep_inflation, NA_real_)
        if (!is.finite(est)) return(list(error = "Enter the earlier study's effect estimate."))
        if (!is.finite(se) || se <= 0) return(list(error = "The earlier study's SE must be greater than 0."))
        if (!is.finite(infl) || infl < 1) return(list(error = "The inflation factor must be at least 1."))
        list(string = sprintf("normal(%s, %s)", format(signif(est, 4)), format(signif(se * infl, 4))))
    }

    .replication_modal <- function() {
        modalDialog(
            title = "Replication prior",
            tags$p("Centers the treatment prior on an earlier study's estimate, with its SE inflated.",
                   "Enter values on the link scale."),
            numericInput("rep_estimate", "Earlier study's effect estimate", value = NULL, step = 0.1),
            numericInput("rep_se", "Earlier study's standard error (greater than 0)", value = NULL, min = 0, step = 0.1),
            numericInput("rep_inflation", "SE inflation factor", value = 2, min = 1, step = 0.5),
            tags$p(tags$strong("Prior on the treatment coefficient: "),
                   textOutput("rep_preview", inline = TRUE)),
            footer = tagList(actionButton("rep_apply", "Apply", class = "btn-primary"),
                             modalButton("Cancel")))
    }

    output$rep_preview <- renderText({
        r <- .replication_string()
        r$string %||% r$error
    })

    .apply_replication_preset <- function(meta, string) {
        s <- .scope_inputs(meta)
        d <- values$processed_data %||% values$data
        tcoef <- if (!is.null(d) && !is.null(s$formula))
            tryCatch(treatment_coef(d, s$formula), error = function(e) NULL)
        if (is.null(tcoef))
            return(showNotification("No treatment term found; replication prior not applied.",
                                    type = "warning", duration = 6))
        # the SCD prior-type observer clears overrides unless the guard expects this change
        if (!meta && !identical(input$prior_type, "informative")) {
            sync_guard$expect <- .prior_reset_inputs()
            sync_guard$expect[[1]] <- "informative"
        }
        pl <- values$current_prior_list %||% .default_prior_list(meta) %||% list()
        pl <- pl[!grepl("^sd_", names(pl))]
        if (!is.null(pl$treatment) && is.null(pl$treatment$coef))
            pl$b_default <- pl$b_default %||% list(class = "b", string = pl$treatment$string)
        pl$treatment <- list(class = "b", string = string, coef = tcoef)
        updateRadioButtons(session, if (meta) "meta_prior_type" else "prior_type", selected = "informative")
        values$current_prior_list <- pl
        values$prior_overrides_active <- TRUE
        values$prior_sources["treatment"] <- "replication preset"
        showNotification(sprintf("Replication prior %s on %s. Takes effect on the next fit.",
                                 string, tcoef), type = "message", duration = 6)
    }

    observeEvent(input$applyReplicationPreset, {
        replication_scope("scd")
        showModal(.replication_modal())
    })

    observeEvent(input$meta_applyReplicationPreset, {
        replication_scope("meta")
        showModal(.replication_modal())
    })

    observeEvent(input$rep_apply, {
        r <- .replication_string()
        if (!is.null(r$error)) return(showNotification(r$error, type = "error", duration = 6))
        .apply_replication_preset(identical(replication_scope(), "meta"), r$string)
        removeModal()
    })

    # dialog and Ask AI share provider settings; transcripts survive a provider switch
    dialog_state <- reactiveVal(list())
    dialog_log <- reactiveVal(list())
    dialog_scope <- reactiveVal("scd")
    dialog_proposal <- reactiveVal(NULL)
    dialog_turn_meta <- reactiveVal(NULL)
    ask_history <- reactiveVal(list())

    # the demo server offers Gemini only; other providers run with the local package
    .llm_choices <- if (.scdbayes_hosted) c("Gemini (cloud)" = "gemini")
        else c("Gemini (cloud)" = "gemini", "OpenAI (cloud)" = "openai",
               "Anthropic API (cloud)" = "anthropic",
               "Local model (OpenAI-compatible server)" = "local")
    llm_state <- reactiveValues(
        provider = if (is.na(.llm_default)) "gemini" else .llm_default,
        model = llm_default_model(if (is.na(.llm_default)) "gemini" else .llm_default),
        base_url = llm_base_url())

    .txt <- function(x) paste(as.character(unlist(x)), collapse = " ")

    # LLM JSON: only list candidates carrying class and prior_string are usable
    .usable_candidates <- function(cands) {
        cands <- if (is.list(cands)) cands else list()
        Filter(function(cd) is.list(cd) && length(cd$class) == 1 && length(cd$prior_string) == 1 &&
                   is.character(unlist(cd$class)) && is.character(unlist(cd$prior_string)), cands)
    }

    # a typed key is bound to the provider selected when it was typed and never sent to another
    .llm_keys <- new.env(parent = emptyenv())

    .llm_settings <- function(prefix) {
        prov <- input[[paste0(prefix, "_provider")]] %||% llm_state$provider
        if (!prov %in% .llm_choices) prov <- "gemini"
        model <- trimws(input[[paste0(prefix, "_model")]] %||% llm_state$model)
        if (!nzchar(model)) model <- llm_default_model(prov)
        entry <- .llm_keys[[prefix]]
        list(provider = prov, model = model,
             base_url = if (identical(prov, "local"))
                 trimws(input[[paste0(prefix, "_base_url")]] %||% llm_state$base_url),
             user_key = if (identical(entry$provider, prov)) entry$key else "")
    }

    .llm_mode <- function(s) {
        if (!identical(s$provider, "local")) "cloud"
        else if (llm_is_local_host(s$base_url)) "local" else "local (remote host)"
    }

    # a personal key wins; the shared server key covers Gemini only and never reaches the browser
    .llm_key <- function(s) {
        if (nzchar(s$user_key)) return(list(key = s$user_key, shared = FALSE))
        shared <- if (identical(s$provider, "gemini")) Sys.getenv("GEMINI_API_KEY", "") else ""
        list(key = shared, shared = nzchar(shared))
    }

    # NULL when a request can be sent, else the reason shown in the dialog
    .llm_setup_error <- function(s, k) {
        if (identical(s$provider, "local") || nzchar(k$key)) return(NULL)
        if (identical(s$provider, "gemini"))
            paste("No AI provider available: this server has no shared Gemini key.",
                  "Enter a personal API key, or choose Local model with a running local server.")
        else sprintf("%s needs an API key. Enter one, or choose Gemini or Local model.",
                     names(.llm_choices)[.llm_choices == s$provider])
    }

    # shared key only: 20 calls/hour per session, plus process-wide, per-client, and text caps in global.R
    .llm_calls <- new.env(parent = emptyenv())
    .llm_calls$times <- numeric(0)
    # a client can forge X-Forwarded-For; only a trusted proxy's appended last hop counts
    .llm_client <- local({
        req <- session$request
        remote <- if (is.null(req)) "" else req$REMOTE_ADDR %||% ""
        xff <- if (is.null(req)) "" else req$HTTP_X_FORWARDED_FOR %||% ""
        hops <- trimws(strsplit(xff, ",", fixed = TRUE)[[1]])
        hops <- hops[nzchar(hops)]
        trust <- .env_flag("scdbayes.trust_proxy", "SCDBAYES_TRUST_PROXY")
        addr <- if (trust && length(hops)) hops[length(hops)]
                else if (nzchar(remote)) remote else "unknown"
        if (requireNamespace("digest", quietly = TRUE))
            substr(digest::digest(paste0(.llm_shared_salt, addr), algo = "sha256"), 1, 16)
        else "anon"
    })
    # NULL when the call is allowed (and counted), else the reason
    .gate_llm <- function(k, n_chars) {
        if (!isTRUE(k$shared)) return(NULL)
        now <- as.numeric(Sys.time())
        .llm_calls$times <- .llm_calls$times[now - .llm_calls$times < 3600]
        msg <- if (length(.llm_calls$times) >= 20)
            "This session reached 20 shared-key AI requests this hour. Enter a personal key, choose Local model, or wait."
        else .llm_shared_gate(.llm_client, n_chars, now)
        if (is.null(msg)) .llm_calls$times <- c(.llm_calls$times, now)
        msg
    }

    # LLM HTTP runs in-process on curl's async pool polled by later, so calls never queue behind brms future workers
    # a synchronous stop (no model, no key, refused host) becomes a rejected promise so onFinally still runs
    .llm_run <- function(fun, args) {
        if (isTRUE(getOption("scdbayes.llm_async", TRUE))) {
            out <- tryCatch(do.call(fun, c(args, list(async = TRUE))),
                            error = function(e) promises::promise_reject(e))
            return(if (promises::is.promise(out)) out else promises::promise_resolve(out))
        }
        promises::promise(function(resolve, reject)
            tryCatch(resolve(do.call(fun, args)), error = function(e) reject(e)))
    }

    .scope_inputs <- function(meta) {
        f <- tryCatch(if (meta) meta_formula_display() else model_formula(), error = function(e) NULL)
        list(
            family = if (meta) .meta_family() else input$model_family %||% "gaussian",
            design = (if (meta) input$meta_research_design else input$research_design) %||% "ab",
            goal = (if (meta) input$meta_intervention_goal else input$intervention_goal) %||% "increase",
            outcome_var = (if (meta) input$meta_outcome_var else input$outcome_var) %||% "outcome",
            prior_type = (if (meta) input$meta_prior_type else input$prior_type) %||% "default",
            prior_effect_mean = if (meta) input$meta_prior_effect_mean else input$prior_effect_mean,
            prior_effect_sd = if (meta) input$meta_prior_effect_sd else input$prior_effect_sd,
            hetero = if (meta) input$meta_heterogeneous_variance else input$heterogeneous_variance,
            formula = f)
    }

    # same assembly as the fit path, so adopting before the first fit keeps adaptive defaults
    .default_prior_list <- function(meta) {
        s <- .scope_inputs(meta)
        d <- values$processed_data %||% values$data
        if (is.null(d) || is.null(s$formula)) return(NULL)
        tryCatch(assemble_brms_priors(
            prior_type = s$prior_type,
            prior_effect_mean = s$prior_effect_mean,
            prior_effect_sd = s$prior_effect_sd,
            heterogeneous_variance = s$hetero,
            model_family_value = s$family,
            adaptive_priors = .adaptive_priors(d, values$model_structure, s$family, s$outcome_var,
                                               .exposure_in(meta)),
            treatment_coef = treatment_coef(d, s$formula),
            has_random_slope = has_random_slope(s$formula),
            formula_str = s$formula), error = function(e) NULL)
    }

    .prior_scale <- function(x) {
        e <- tryCatch(str2lang(x), error = function(e) NULL)
        if (!is.call(e) || !is.symbol(e[[1]])) return(NA_real_)
        pos <- c(normal = 3, student_t = 4, cauchy = 3, logistic = 3,
                 double_exponential = 3, skew_normal = 3)[as.character(e[[1]])]
        if (is.na(pos) || length(e) < pos || !is.numeric(e[[pos]])) return(NA_real_)
        abs(e[[pos]])
    }

    .ai_notice <- function(prefix, always, sharing) {
        if (identical(prefix, "ask")) return(NULL)
        tier <- switch(tolower(Sys.getenv("SCDBAYES_AI_SHARED_TIER", "")),
            paid = "The shared server Gemini key is on a paid tier.",
            free = "The shared server Gemini key is on the free (unpaid) tier, so the Gemini free-tier terms apply to shared-key use.",
            "Whether the shared server Gemini key is billed is not stated: check with the server administrator.")
        tagList(
            if (is.na(.llm_default)) tags$p(class = "status-main-warning",
                "No AI provider is configured. Enter a cloud API key,",
                "or start a local model server and choose Local model."),
            tags$details(
                tags$summary("Privacy details"),
                tags$div(style = "color: #64748B; font-size: 13px;",
                    tags$p(tags$strong("Estimates come only from the brms model; AI text is advisory.")),
                    tags$p(tags$strong("Always sent: "), always),
                    tags$p(tags$strong("Sent only when sharing is on: "), sharing),
                    uiOutput(paste0(prefix, "_privacy")),
                    tags$p(tier, " Leave sharing off for restricted-use data."))))
    }

    # one-line pending/error status inside each AI dialog
    ai_status <- reactiveValues(ask = NULL, dialog = NULL)
    .ai_status_ui <- function(prefix)
        tags$div(class = "ai-status", role = "status", `aria-live` = "polite",
                 uiOutput(paste0(prefix, "_status"), inline = TRUE))

    .llm_settings_ui <- function(prefix) {
        prov <- llm_state$provider
        tagList(
            fluidRow(
                column(4, selectInput(paste0(prefix, "_provider"), "AI provider",
                                      choices = .llm_choices, selected = prov)),
                column(4, textInput(paste0(prefix, "_model"), "Model", value = llm_state$model)),
                column(4, conditionalPanel(sprintf("input.%s_provider == 'local'", prefix),
                    textInput(paste0(prefix, "_base_url"), "Local base URL", value = llm_state$base_url)))),
            .password_input(paste0(prefix, "_api_key"),
                "API key (optional; kept for this session and the selected provider only)",
                placeholder = "Blank uses the shared server key (Gemini only)", value = ""),
            div(style = "margin-bottom: 8px;",
                actionButton(paste0(prefix, "_test"), "Test connection", icon = icon("plug"),
                             class = "btn-sm"),
                uiOutput(paste0(prefix, "_test_result"), inline = TRUE)))
    }

    lapply(c("ask", "dialog"), function(prefix) {
        observeEvent(input[[paste0(prefix, "_provider")]], {
            prov <- input[[paste0(prefix, "_provider")]]
            if (!identical(.llm_keys[[prefix]]$provider, prov)) {
                .llm_keys[[prefix]] <- NULL
                updateTextInput(session, paste0(prefix, "_api_key"), value = "")
            }
            if (identical(prov, llm_state$provider)) return()
            llm_state$provider <- prov
            llm_state$model <- llm_default_model(prov)
            updateTextInput(session, paste0(prefix, "_model"), value = llm_state$model)
            other <- setdiff(c("ask", "dialog"), prefix)
            updateSelectInput(session, paste0(other, "_provider"), selected = prov)
            updateTextInput(session, paste0(other, "_model"), value = llm_state$model)
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0(prefix, "_api_key")]], {
            key <- trimws(input[[paste0(prefix, "_api_key")]])
            prov <- input[[paste0(prefix, "_provider")]] %||% llm_state$provider
            .llm_keys[[prefix]] <- if (nzchar(key)) list(provider = prov, key = key)
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0(prefix, "_model")]], {
            m <- trimws(input[[paste0(prefix, "_model")]])
            if (nzchar(m)) llm_state$model <- m
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0(prefix, "_base_url")]], {
            u <- trimws(input[[paste0(prefix, "_base_url")]])
            if (nzchar(u)) llm_state$base_url <- u
        }, ignoreInit = TRUE)
        output[[paste0(prefix, "_privacy")]] <- renderUI(tags$p(
            llm_privacy_note(llm_state$provider, llm_state$base_url)))
        output[[paste0(prefix, "_label")]] <- renderUI(tags$span(
            llm_model_label(llm_state$provider, llm_state$model)))
        output[[paste0(prefix, "_status")]] <- renderUI({
            st <- ai_status[[prefix]]
            if (!is.null(st)) tags$span(class = if (isTRUE(st$error)) "ai-status-error", st$text)
        })
        observeEvent(input[[paste0(prefix, "_test")]], {
            s <- .llm_settings(prefix)
            k <- .llm_key(s)
            msg <- .llm_setup_error(s, k) %||% .gate_llm(k, 100)
            if (!is.null(msg))
                return(output[[paste0(prefix, "_test_result")]] <- renderUI(tags$span(
                    style = "color: #b91c1c;", paste0(" ", msg))))
            output[[paste0(prefix, "_test_result")]] <- renderUI(tags$span(" Testing..."))
            .llm_run(llm_test_connection, list(s$provider, s$model, k$key, s$base_url, 15)) %...>%
                (function(r) output[[paste0(prefix, "_test_result")]] <- renderUI(tags$span(
                    style = if (isTRUE(r$ok)) "color: #15803d;" else "color: #b91c1c;",
                    sprintf(" %s %s: %s (%s ms)", if (isTRUE(r$ok)) "OK" else "Failed",
                            r$model, r$message, r$latency_ms)))) %...!%
                (function(e) output[[paste0(prefix, "_test_result")]] <- renderUI(tags$span(
                    style = "color: #b91c1c;", paste(" Failed:", conditionMessage(e)))))
            NULL
        })
    })

    .dialog_context <- function() {
        d <- values$data
        meta <- identical(dialog_scope(), "meta")
        s <- .scope_inputs(meta)
        shared <- isTRUE(input$dialog_share_context)
        y <- if (!is.null(d) && s$outcome_var %in% names(d)) d[[s$outcome_var]]
        outcome_summary <- if (!shared) "not shared"
            else if (is.numeric(y)) {
                q <- stats::quantile(y, c(0.1, 0.5, 0.9), na.rm = TRUE, names = FALSE)
                sprintf("10%%/50%%/90%% quantiles %.2f / %.2f / %.2f, SD %.2f",
                        q[1], q[2], q[3], stats::sd(y, na.rm = TRUE))
            } else if (!is.null(y)) sprintf("non-numeric, %d levels", length(unique(y)))
            else "no outcome data"
        pl <- if (shared) .default_prior_list(meta)
        prior_lines <- if (is.null(pl)) "not shared" else paste(vapply(pl, function(p)
            sprintf("%s %s", .txt(p$class), .txt(p$string)), ""), collapse = "; ")
        unit_col <- if (meta) "study" else "case"
        n_units <- if (!is.null(d) && unit_col %in% names(d)) length(unique(d[[unit_col]])) else NA
        paste(c(
            sprintf("Scope: %s", if (meta) "meta-analysis" else "single-case study"),
            sprintf("Design: %s", s$design),
            sprintf("Outcome family: %s", s$family),
            sprintf("Outcome: outcome (%s)", outcome_summary),
            sprintf("%s: %s", if (meta) "Studies" else "Cases",
                    if (is.na(n_units)) "unknown" else as.character(n_units)),
            sprintf("Intervention goal: %s", s$goal),
            sprintf("Current prior type: %s", s$prior_type),
            sprintf("Default priors: %s", prior_lines)), collapse = "\n")
    }

    .render_ai_json <- function(txt) {
        p <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.list(p) || !isTRUE(p$phase %in% c("elicit", "propose")))
            return(tags$div(style = "white-space: pre-wrap;", "Reply could not be parsed:", tags$br(), txt))
        if (identical(p$phase, "elicit")) return(tagList(
            tags$ol(lapply(p$questions %||% list(), function(q) tags$li(.txt(q)))),
            if (nzchar(.txt(p$rationale))) tags$p(tags$em(.txt(p$rationale)))))
        tagList(
            tags$ul(lapply(.usable_candidates(p$candidates), function(cd) tags$li(
                tags$strong(sprintf("%s [%s]: ", .txt(cd$label), .txt(cd$class))),
                tags$code(.txt(cd$prior_string)),
                if (!is.null(cd$rationale)) tags$div(.txt(cd$rationale)),
                if (!is.null(cd$tradeoffs))
                    tags$div(style = "color: #64748B;", "Trade-off: ", .txt(cd$tradeoffs))))),
            if (!is.null(p$clarifying_question))
                tags$p(tags$strong("Question: "), .txt(p$clarifying_question)),
            if (!is.null(p$next_action_hint))
                tags$p(style = "color: #64748B;", .txt(p$next_action_hint)))
    }

    output$dialogTranscript <- renderUI({
        msgs <- dialog_log()
        if (length(msgs) == 0)
            return(div(style = "color: #64748B; font-style: italic;",
                "Press Send to start; the assistant asks a few short questions first."))
        tagList(lapply(msgs, function(m) {
            mine <- identical(m$role, "researcher")
            tags$div(style = sprintf(
                "margin: 6px 0; padding: 8px 12px; border-radius: 6px; background: %s;",
                if (mine) "#eef4fb" else "#f5f5f0"),
                tags$strong(if (mine) "Researcher: " else "Assistant: "),
                tags$div(style = "font-size: 13px;",
                    if (mine) tags$div(style = "white-space: pre-wrap;", m$text)
                    else .render_ai_json(m$text)))
        }))
    })

    .adopt_classes <- c("Intercept", "b", "sd", "sigma", "cor", "shape", "phi",
                        "nu", "sds", "ar", "ma")

    output$dialogCandidates <- renderUI({
        p <- dialog_proposal()
        if (is.null(p)) return(NULL)
        cands <- .usable_candidates(p$candidates)
        cls <- vapply(cands, function(cd) .txt(cd$class), "")
        bad <- unique(cls[!cls %in% .adopt_classes])
        tags$div(style = "border-top: 1px solid #e2e8f0; padding-top: 8px;",
            tags$h3("Choose the priors to adopt (one per class)"),
            lapply(unique(cls[cls %in% .adopt_classes]), function(cl) {
                idx <- which(cls == cl)
                labels <- vapply(cands[idx], function(cd)
                    sprintf("%s: %s", .txt(cd$label), .txt(cd$prior_string)), "")
                radioButtons(paste0("dialog_pick_", cl), sprintf("Class %s", cl),
                    choices = c(stats::setNames(as.character(idx), labels), "Do not adopt" = "none"),
                    selected = if (length(idx) == 1) as.character(idx) else character(0))
            }),
            if (length(bad)) tags$p(style = "color: #b91c1c; font-size: 13px;",
                sprintf("Not adoptable (unknown class): %s", paste(bad, collapse = ", "))),
            actionButton("dialogAdopt", "Adopt selected priors", class = "btn-success btn-sm",
                         icon = icon("check"),
                         title = "Validate and apply the selected candidates; they take effect on the next fit"))
    })

    .ai_priors_modal <- function() {
        modalDialog(
            title = tagList(icon("wand-magic-sparkles"), "Co-design priors (AI)"),
            tags$p(style = "color: #64748B; font-size: 13px;",
                HTML(paste0("Prior elicitation with ", as.character(uiOutput("dialog_label", inline = TRUE)),
                            "; each proposal is checked before use."))),
            uiOutput("dialogTranscript"),
            .ai_status_ui("dialog"),
            uiOutput("dialogCandidates"),
            textAreaInput("dialog_user_input", tags$span(class = "sr-only", "Message to the assistant"),
                placeholder = "Optional note to start, then short replies (Ctrl+Enter sends)",
                rows = 2, width = "100%"),
            div(class = "ask-panel-actions",
                actionButton("dialogStart", "Send", class = "btn-primary btn-sm", icon = icon("paper-plane")),
                actionButton("dialogReset", "Reset", class = "btn-sm", icon = icon("arrow-rotate-left"))),
            checkboxInput("dialog_share_context",
                "Share outcome quantiles (10/50/90%), SD, and default prior strings",
                value = FALSE),
            tags$details(
                tags$summary("Settings"),
                .llm_settings_ui("dialog")),
            .ai_notice("dialog",
                paste("design, outcome family, intervention goal, number of cases or studies,",
                      "prior type, typed notes and replies, and earlier turns of this dialog.",
                      "The outcome name is replaced by 'outcome'; data rows are never sent."),
                paste("outcome 10/50/90% quantiles and SD, and the default prior strings",
                      "(their scales reflect the outcome SD).")),
            tags$details(
                tags$summary("Context summary (exactly what is sent)"),
                uiOutput("dialogContextPreview")),
            footer = modalButton("Close"), size = "l", easyClose = FALSE
        )
    }

    output$dialogContextPreview <- renderUI(tags$pre(.dialog_context()))

    observeEvent(input$askAiPriors, {
        dialog_scope("scd")
        showModal(.ai_priors_modal())
        if (isTRUE(ai_status$dialog$busy)) shinyjs::disable("dialogStart")
    })

    observeEvent(input$meta_askAiPriors, {
        dialog_scope("meta")
        showModal(.ai_priors_modal())
        if (isTRUE(ai_status$dialog$busy)) shinyjs::disable("dialogStart")
    })

    # a proposal for one scope must not be adopted into the other; a reply to an earlier dialog is dropped
    dialog_ids <- new.env(parent = emptyenv())
    dialog_ids$run <- 0
    observeEvent(dialog_scope(), {
        dialog_ids$run <- dialog_ids$run + 1
        dialog_state(list())
        dialog_log(list())
        dialog_proposal(NULL)
        dialog_turn_meta(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$dialogReset, {
        dialog_ids$run <- dialog_ids$run + 1
        if (!isTRUE(ai_status$dialog$busy)) ai_status$dialog <- NULL
        dialog_state(list())
        dialog_log(list())
        dialog_proposal(NULL)
        dialog_turn_meta(NULL)
    })

    observeEvent(input$dialogStart, {
        s <- .llm_settings("dialog")
        k <- .llm_key(s)
        user_text <- .redact_columns(sanitize_llm_text(input$dialog_user_input %||% ""))
        state <- dialog_state()
        ctx <- .dialog_context()
        msg <- (if (length(state) > 0 && !nzchar(trimws(user_text)))
                    "Type a reply to the questions before sending.") %||%
               .llm_setup_error(s, k) %||%
               .gate_llm(k, nchar(ctx) + nchar(user_text) + sum(nchar(unlist(state))))
        if (!is.null(msg)) return(ai_status$dialog <- list(text = msg, error = TRUE))
        scope <- dialog_scope()
        turn_id <- dialog_ids$run
        ai_status$dialog <- list(text = sprintf("%s is thinking...", llm_model_label(s$provider, s$model)),
                                 busy = TRUE)
        shinyjs::disable("dialogStart")
        promises::finally(
            .llm_run(llm_dialog_turn, list(state, user_text, ctx, s$provider, s$model,
                                           k$key, s$base_url)) %...>%
            (function(out) {
                ai_status$dialog <- NULL
                if (isTRUE(k$shared)) .llm_shared_charge(nchar(out$raw_text))
                if (!identical(dialog_ids$run, turn_id) || !identical(dialog_scope(), scope)) return(invisible(NULL))
                phase <- if (is.null(out$parsed)) "unparsed" else out$parsed$phase
                .audit_add(if (is.null(out$parsed)) "dialog_parse_failure" else "prior_dialog_turn",
                           scope = scope, provider = s$provider, model_id = out$model,
                           served_model = out$served_model, mode = .llm_mode(s), phase = phase, context = ctx,
                           user_text = user_text, response = out$raw_text, problem = out$problem,
                           request = .ai_request_meta("dialog", s$provider))
                dialog_state(out$next_state)
                dialog_log(c(dialog_log(),
                    if (nzchar(user_text)) list(list(role = "researcher", text = user_text)),
                    list(list(role = "ai", text = out$raw_text))))
                dialog_proposal(if (identical(phase, "propose")) out$parsed)
                dialog_turn_meta(list(provider = s$provider, model = out$model, served_model = out$served_model,
                                      mode = .llm_mode(s),
                                      raw = out$raw_text, phase = phase, problem = out$problem))
                updateTextAreaInput(session, "dialog_user_input", value = "")
                if (identical(phase, "unparsed"))
                    ai_status$dialog <- list(text = paste("Proposal could not be parsed.", .txt(out$problem),
                        "Send a reply asking for the expected format."), error = TRUE)
            }) %...!%
            (function(e) ai_status$dialog <- list(text = paste("AI error:", conditionMessage(e)), error = TRUE)),
            onFinally = function() shinyjs::enable("dialogStart"))
        NULL
    })

    # in-session audit trail of AI turns and adoptions, exported for a manuscript supplement
    ai_audit <- reactiveVal(list())
    .audit_add <- function(event, ...) {
        entry <- c(list(
            time    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
            event   = event,
            package_version = as.character(utils::packageVersion("scdbayes"))
        ), list(...))
        ai_audit(c(ai_audit(), list(entry)))
    }

    # request settings per event; the instruction text and schema are recorded as SHA-256 hashes
    .ai_request_meta <- function(kind, provider, instructions = NULL) {
        ns_call <- function(f) tryCatch(get(f, envir = asNamespace("scdbayes"))(), error = function(e) NULL)
        sha <- function(txt) if (is.null(txt) || !requireNamespace("digest", quietly = TRUE)) NA_character_
                             else digest::digest(txt, algo = "sha256", serialize = FALSE)
        dialog <- identical(kind, "dialog")
        schema <- if (dialog) ns_call(".llm_dialog_schema")
        used <- if (isTRUE(nzchar(trimws(instructions %||% "")))) instructions else llm_default_instructions()
        ask_prompt <- if (!dialog) paste0(used, "\n\n", ns_call(".llm_conventions"))
        c(if (!dialog) list(instructions_edited = !identical(trimws(used), trimws(llm_default_instructions()))), list(
            provider = provider,
            # mirrors llm_dialog_turn(): temperature 0 for gemini and local, provider default otherwise
            temperature = if (dialog && provider %in% c("gemini", "local")) 0 else "provider default",
            json_schema = if (dialog) "scdbayes prior dialog (elicit/propose)" else "none (free text)",
            json_schema_sha256 = if (!is.null(schema))
                sha(as.character(jsonlite::toJSON(schema, auto_unbox = TRUE))) else NA_character_,
            system_prompt_sha256 = sha(if (dialog) ns_call(".llm_dialog_prompt") else ask_prompt),
            system_prompt_note = "Hash of the instruction text before the per-turn context, which is stored in 'context'."))
    }

    # non-canonical identifiers become generic tokens so restricted variable names stay local
    # exact token matching, so outcome names with regex metacharacters cannot break redaction
    .redact_formula <- function(f, outcome_var) {
        if (is.null(f) || !nzchar(f)) return("(unknown)")
        pat <- "[A-Za-z.][A-Za-z0-9_.]*"
        toks <- regmatches(f, gregexpr(pat, f))[[1]]
        canon <- grepl(paste0("^(outcome|phase|case|study|time|time_.*|level_.*|",
                              "trend_.*|treatment.*|atd_.*|phase_num|ar|cov|gr|trials|sd|nest|",
                              "tier|Intercept|TRUE|FALSE|offset|log)$"), toks)
        is_out <- toks %in% c(outcome_var, paste0(outcome_var, "_std"))
        user_cols <- unique(toks[!canon & !is_out])
        regmatches(f, gregexpr(pat, f)) <- list(ifelse(is_out, "outcome",
            ifelse(canon, toks, paste0("covariate_", match(toks, user_cols)))))
        f
    }

    # redacted aggregate context, sent only when the user opts in; meta inputs on the meta tab
    .ask_context <- function() {
        if (!isTRUE(input$ask_share_context))
            return("Context sharing is off. No analysis details were shared; answer from general Bayesian SCD methodology only.")
        main <- input$main_nav %||% "home"
        if (identical(main, "power"))
            return(scdbayes:::.power_context(input, values$power_results, values$power_running))
        meta <- identical(main, "meta")
        s <- .scope_inputs(meta)
        pre <- if (meta) "meta_" else ""
        model <- values[[paste0(pre, "model")]]
        fitted <- isTRUE(values[[paste0(pre, "model_fitted")]]) && !is.null(model)
        scope <- values[[paste0(pre, "fit_scope")]]
        s <- if (fitted) scope %||% list(design = "unknown", goal = "unknown") else s
        d <- if (fitted) model$data else values$data
        outcome <- if (fitted) all.vars(model$formula$formula)[1] else s$outcome_var
        outcome <- outcome %||% "outcome"
        if (!is.null(d) && outcome %in% names(d)) d$outcome <- d[[outcome]]
        step <- if (meta) sub("^meta_", "", input$meta_nav %||% "meta_data") else input$scd_nav %||% "import"
        tab_names <- c(home = "Home", scd = "Single-Case Design", meta = "Meta-Analysis", power = "Power Analysis",
                       import = "Data", data = "Data", setup = "Model", model = "Model", fit = "Diagnostics",
                       effects = "Decisions", percase = "Per Case", perstudy = "Per Study", export = "Export")
        inner_ids <- c(import = "data_tabs", data = "data_tabs", setup = "model_tabs", model = "model_tabs",
                       fit = "fit_tabs", effects = "effectsTabset", percase = "percaseTabset",
                       perstudy = "perstudyTabset")
        location <- c(tab_names[main], if (main %in% c("scd", "meta"))
                          c(tab_names[step], input[[paste0(pre, inner_ids[step])]]))
        unit_col <- if (meta) "study" else "case"
        n_units <- if (!is.null(d) && unit_col %in% names(d))
            length(unique(if (meta) d$study else scdbayes:::.std_key(d, intersect(c("study", "nest", "cluster", "case"), names(d))))) else NA
        n_obs <- if (!is.null(d)) nrow(d) else 0
        # per-phase run summaries pooled across cases; slope is the within-case OLS slope
        phase_lines <- if (!is.null(d) && all(c("outcome", "phase", "case") %in% names(d)) && is.numeric(d$outcome)) {
            # free-text phase labels may name an intervention; standard labels (A, B2) stay as they are
            lab <- as.character(d$phase)
            std <- grepl("^[A-H]{1,2}[0-9]*$", lab)
            # free-text labels are lettered by first appearance in time, so a later phase never becomes A
            onset <- tapply(if ("time" %in% names(d)) d$time else seq_along(lab), lab, min)
            ordered <- names(sort(onset[unique(lab[!std])]))
            free <- c(setdiff(LETTERS, substr(lab[std], 1, 1)), paste0("Z", seq_len(100)))
            lab[!std] <- free[match(lab[!std], ordered)]
            pd <- data.frame(unit = scdbayes:::.std_key(d, intersect(c("study", "nest", "cluster", "case"), names(d))),
                             phase = lab,
                             y = d$outcome,
                             t = if ("time" %in% names(d)) d$time else ave(seq_len(nrow(d)), d$case, FUN = seq_along))
            atd <- identical(s$design, "alternating")
            ps <- pd[order(pd$unit, pd$t), ] %>%
                group_by(unit) %>%
                mutate(run = cumsum(c(TRUE, phase[-1] != phase[-n()]))) %>%
                group_by(unit, phase) %>%
                mutate(k = dense_rank(run)) %>%
                group_by(phase) %>%
                mutate(label = if (!atd && max(k) > 1) paste0(phase, k) else phase) %>%
                ungroup() %>%
                filter(!is.na(y), !is.na(t)) %>%
                group_by(label) %>%
                summarise(order = min(run), n = n(), m = mean(y), sd = stats::sd(y),
                          b = sum((t - ave(t, unit)) * (y - ave(y, unit))) / sum((t - ave(t, unit))^2),
                          .groups = "drop") %>%
                arrange(order, label)
            utils::head(sprintf("Phase %s: n=%d, mean=%.2f, SD=%.2f, slope=%s", ps$label, ps$n, ps$m, ps$sd,
                                ifelse(is.finite(ps$b), sprintf("%+.2f/session", ps$b), "NA")), 8)
        }
        wwc <- if (!fitted && !is.null(d)) tryCatch(wwc_checks[[if (meta) "meta" else "scd"]](), error = function(e) e)
        lines <- c(
            sprintf("Location: %s", paste(stats::na.omit(location), collapse = " > ")),
            sprintf("Scope: %s", if (meta) "meta-analysis" else "single-case study"),
            sprintf("Design: %s", s$design),
            sprintf("Outcome family: %s", if (fitted) stats::family(values[[paste0(pre, "model")]])$family else s$family),
            sprintf("Intervention goal: %s", s$goal),
            sprintf("%s: %s; observations: %d", if (meta) "Studies" else "Cases",
                    ifelse(is.na(n_units), "unknown", as.character(n_units)), n_obs),
            if (meta && !is.null(d) && "case" %in% names(d))
                sprintf("Cases: %d; nesting: observations within cases within studies",
                        length(unique(scdbayes:::.std_key(d, intersect(c("study", "nest", "cluster", "case"), names(d)))))),
            phase_lines,
            if (!is.null(wwc) && !inherits(wwc, "error"))
                sprintf("WWC screening: %s (screened as %s)", wwc$rating, gsub("_", " ", wwc$design)))
        if (fitted) {
            diagnostics <- values[[paste0(pre, "diagnostics")]]
            b <- diagnostics$basic
            rhat <- if (any(!is.na(b$rhat))) max(b$rhat, na.rm = TRUE) else NA_real_
            ess  <- if (any(!is.na(b$ess_bulk))) min(b$ess_bulk, na.rm = TRUE) else NA_real_
            ndiv <- diagnostics$divergence$n_divergent %||% NA
            lines <- c(lines,
                sprintf("Model: FITTED. Formula: %s",
                        .redact_formula(paste(deparse(model$formula$formula), collapse = " "), outcome)),
                sprintf("Diagnostics: max R-hat %s, min bulk-ESS %s, divergences %s",
                        ifelse(is.na(rhat), "NA", formatC(rhat, format = "f", digits = 4)),
                        ifelse(is.na(ess), "NA", as.character(round(ess))),
                        ifelse(is.na(ndiv), "NA", as.character(ndiv))),
                if (isTRUE(values[[paste0(pre, "model_stale")]])) "Model: stale (inputs changed since the fit)")
        } else {
            lines <- c(lines, "Model: not yet fitted.")
        }
        paste(lines, collapse = "\n")
    }

    output$askTranscript <- renderUI({
        pairs <- ask_history()
        if (length(pairs) == 0)
            return(NULL)
        bubble <- function(mine, body) tags$div(style = sprintf(
            "margin: 6px 0; padding: 8px 12px; border-radius: 6px; background: %s;",
            if (mine) "#eef4fb" else "#f5f5f0"),
            tags$strong(if (mine) "Question: " else "Assistant: "),
            tags$div(body))
        tagList(lapply(pairs, function(t) tagList(
            bubble(TRUE, tags$div(style = "white-space: pre-wrap;", t$q)),
            bubble(FALSE, tryCatch(HTML(render_llm_markdown(t$a)),
                error = function(e) tags$div(style = "white-space: pre-wrap;", t$a))))))
    })

    # suggested questions picked by rules from the current tab and analysis state, no model call
    .ask_suggestions <- function() {
        main <- input$main_nav %||% "home"
        step <- input$scd_nav %||% "import"
        sub_tab <- input$data_tabs %||% ""
        has_data <- !is.null(values$data)
        fitted <- isTRUE(values$model_fitted)
        b <- values$diagnostics$basic
        bad_fit <- fitted && (isTRUE(any(b$rhat >= 1.01, na.rm = TRUE)) ||
                              isTRUE(any(b$ess_bulk <= 400, na.rm = TRUE)) ||
                              isTRUE((values$diagnostics$divergence$n_divergent %||% 0) > 0))
        picks <- if (main == "home") c("What can scdbayes do?", "What data format does scdbayes need?")
            else if (main == "power") c("How many cases and sessions are enough?", "What effect size should I plan for?")
            else if (main == "meta") c("How do I prepare data for a meta-analysis?",
                                       "Should I use a one-stage or two-stage model?")
            else if (!has_data) c("What data format does scdbayes need?", "Which example matches my design?")
            else if (step == "import") switch(sub_tab,
                "WWC Check" = c("Does this design meet WWC standards?", "What is missing for a higher rating?"),
                "Baseline Trend" = c("Is the baseline stable enough?", "Does the baseline trend affect the effect?"),
                "Plot" = c("What do the plots show about level and trend?", "Is there overlap between phases?"),
                c("Is the baseline stable enough?", "What should I check before fitting?"))
            else if (!fitted) c("Which outcome family fits this outcome?", "Are the default priors reasonable?",
                                "Should I add AR(1) or random slopes?")
            else if (bad_fit) c("The model did not converge. What should I change?", "Can I trust these estimates?")
            else switch(step,
                fit = c("Did the model converge?", "Do the predictive checks look acceptable?"),
                effects = c("What does the level effect mean here?", "How should I read pd and ROPE?"),
                percase = c("Do the effects agree across cases?", "Which case differs most and why?"),
                export = c("What should I report from this analysis?", "How do I cite the methods?"),
                c("Did the model converge?", "What does the level effect mean here?"))
        c("What should I do next?", picks)
    }

    .ask_chip_prompts <- list(
        "What should I do next?" = c("Next step", "Explain the next step for the current analysis. Identify the settings or checks needed before proceeding and briefly explain their purpose."),
        "How many cases and sessions are enough?" = c("Sample size", "Explain how to compare case counts and baseline/intervention observation counts for a single study, or study counts and category allocations for meta-analysis. Describe how the selected assumptions affect power; do not invent a required sample size."),
        "What effect size should I plan for?" = c("Effect size", "Explain how to select a plausible intervention-effect magnitude from previous research or a meaningful behavioral change. Explain the selected outcome scale, increase/decrease goal, and sensitivity checks across several magnitudes.")
    )

    output$askSuggestions <- renderUI(tagList(lapply(.ask_suggestions(), function(q) {
        prompt <- .ask_chip_prompts[[q]] %||% c(q, paste(q, "Explain the relevant settings, assumptions, and checks for the current step. State when information is missing."))
        tags$button(type = "button", class = "btn btn-xs ask-chip",
                    `data-q` = prompt[[2]], title = prompt[[2]], prompt[[1]])
    })))

    # the user's own column names (e.g. in pasted error text) become generic tokens before sending
    .redact_columns <- function(text) {
        # canonical and common single-case words stay readable even when a column carries that name
        keep <- c("case", "time", "phase", "outcome", "study", "session", "behavior", "behaviour", "tier",
                  "condition", "measure", "setting", "baseline", "treatment", "intervention", "participant")
        cols <- unique(c(values$upload_names, names(values$data)))
        cols <- cols[!tolower(cols) %in% keep]
        # names under 3 characters match ordinary words, so they are replaced only when quoted as in R errors
        pattern <- ifelse(nchar(cols) >= 3,
                          paste0("(?i)(?<![\\w.])\\Q", cols, "\\E(?!\\w|\\.\\w)"),
                          paste0("(?<=['\"`\u2018\u2019])\\Q", cols, "\\E(?=['\"`\u2018\u2019])"))
        Reduce(function(txt, i) gsub(pattern[i], paste0("column_", i), txt, perl = TRUE),
               seq_along(cols), text)
    }

    output$askContextPreview <- renderUI(tags$pre(.ask_context()))
    output$askFunding <- renderUI({
        if (!identical(llm_state$provider, "gemini")) return(NULL)
        info_popup_link("showGeminiInfo", "Gemini information")
    })
    observeEvent(input$showGeminiInfo, {
        showModal(info_popup_modal(
            title = "Gemini",
            body = tagList(
                tags$p("Gemini API credits funded by Google Cloud."),
                tags$ul(
                    tags$li("Chat and instructions are sent to the selected provider."),
                    tags$li("Context sharing starts off; data rows and case IDs are excluded."),
                    tags$li("Provider terms apply. Session data and keys clear on exit."))
            )
        ))
    })

    # side panel beside the analysis: chat on top, settings folded below
    .ask_panel_ui <- function() {
        tagList(
            tags$p(class = "ask-panel-note",
                tags$span(class = "ask-model-label", uiOutput("ask_label", inline = TRUE)),
                uiOutput("askFunding", inline = TRUE)),
            uiOutput("askTranscript", class = "ask-transcript"),
            .ai_status_ui("ask"),
            uiOutput("askSuggestions", class = "ask-suggestions"),
            textAreaInput("ask_user_input", tags$span(class = "sr-only", "Question for the assistant"),
                placeholder = "Ask about this step (Ctrl+Enter sends)",
                rows = 3, width = "100%"),
            div(class = "ask-panel-actions",
                actionButton("askSend", "Send", class = "btn-primary btn-sm", icon = icon("paper-plane")),
                actionButton("askReset", "Clear", class = "btn-sm", icon = icon("arrow-rotate-left"))),
            checkboxInput("ask_share_context",
                "Share context (no data rows)",
                value = FALSE),
            tags$details(
                tags$summary("Settings"),
                .llm_settings_ui("ask")),
            .ai_notice("ask",
                "the typed question, earlier questions and answers in this chat (last 8), and the assistant instructions.",
                paste("the current app tab, design, outcome family, intervention goal, case and observation counts,",
                      "per-phase n, mean, SD, and slope pooled across cases, the WWC screening rating, the model",
                      "formula with non-standard variable names replaced, and convergence diagnostics.",
                      "Data rows, case IDs, and posterior estimates are never sent.")),
            tags$details(
                tags$summary("Preview what the assistant receives"),
                uiOutput("askContextPreview")),
            tags$details(
                tags$summary("Edit instructions"),
                textAreaInput("ask_instructions", tags$span(class = "sr-only", "Assistant instructions"),
                    value = isolate(input$ask_instructions) %||% llm_default_instructions(),
                    rows = 8, width = "100%"),
                actionLink("ask_instructions_reset", "Reset", style = "font-size: 13px;")))
    }

    ask_panel_mounted <- reactiveVal(FALSE)
    observeEvent(input$askAiFab, {
        if (isolate(ask_panel_mounted())) return()
        insertUI("#ask-panel-body", "beforeEnd", .ask_panel_ui())
        ask_panel_mounted(TRUE)
    })

    observeEvent(input$ask_instructions_reset,
        updateTextAreaInput(session, "ask_instructions", value = llm_default_instructions()))

    observeEvent(input$askReset, {
        ask_history(list())
        if (!isTRUE(ai_status$ask$busy)) ai_status$ask <- NULL
    })

    observeEvent(input$askSend, {
        s <- .llm_settings("ask")
        k <- .llm_key(s)
        user_text <- .redact_columns(sanitize_llm_text(input$ask_user_input %||% ""))
        history <- ask_history()
        ctx <- .ask_context()
        shared <- isTRUE(input$ask_share_context)
        instructions <- .redact_columns(sanitize_llm_text(input$ask_instructions %||% "", 4000))
        msg <- (if (!nzchar(trimws(user_text))) "Type a question before sending.") %||%
               .llm_setup_error(s, k) %||%
               .gate_llm(k, nchar(ctx) + nchar(user_text) + nchar(instructions) + sum(nchar(unlist(history))))
        if (!is.null(msg)) return(ai_status$ask <- list(text = msg, error = TRUE))
        ai_status$ask <- list(text = sprintf("%s is thinking...", llm_model_label(s$provider, s$model)),
                              busy = TRUE)
        shinyjs::disable("askSend")
        promises::finally(
            .llm_run(llm_ask, list(history, user_text, ctx, s$provider, s$model,
                                   k$key, s$base_url, instructions = instructions)) %...>%
            (function(out) {
                ai_status$ask <- list(text = "Answer received.")
                if (isTRUE(k$shared)) .llm_shared_charge(nchar(out$text))
                .audit_add("ask_turn",
                           provider = s$provider,
                           model_id = out$model,
                           served_model = out$served_model,
                           mode = .llm_mode(s),
                           context_shared = shared,
                           context = ctx,
                           user_text = user_text,
                           response = out$text,
                           request = .ai_request_meta("ask", s$provider, instructions))
                ask_history(out$next_history)
                updateTextAreaInput(session, "ask_user_input", value = "")
            }) %...!%
            (function(e) ai_status$ask <- list(text = paste("AI error:", conditionMessage(e)), error = TRUE)),
            onFinally = function() shinyjs::enable("askSend"))
        NULL
    })

    .adopt_selected <- function() {
        p <- dialog_proposal()
        if (is.null(p)) {
            phase <- dialog_turn_meta()$phase %||% "none"
            showNotification(switch(phase,
                elicit = "Latest turn is an elicitation question, not a proposal.",
                unparsed = paste("Proposal could not be parsed; there is nothing to adopt.",
                                 .txt(dialog_turn_meta()$problem)),
                "No proposal to adopt yet."), type = "warning", duration = 5)
            return()
        }
        cands <- .usable_candidates(p$candidates)
        if (length(cands) == 0) {
            showNotification("No usable candidate priors (class and prior string) in the latest proposal.",
                             type = "warning", duration = 4); return()
        }
        meta <- identical(dialog_scope(), "meta")
        s <- .scope_inputs(meta)
        model_data <- values$processed_data %||% values$data
        formula_str <- s$formula %||% values$formula_display
        req(model_data, formula_str)
        cls <- vapply(cands, function(cd) .txt(cd$class), "")
        picks <- vapply(cls, function(cl)
            if (cl %in% .adopt_classes) input[[paste0("dialog_pick_", cl)]] %||% NA_character_
            else NA_character_, "", USE.NAMES = FALSE)
        pending <- unique(cls[cls %in% .adopt_classes & is.na(picks)])
        if (length(pending)) {
            showNotification(sprintf("Select one candidate, or Do not adopt, for class: %s.",
                                     paste(pending, collapse = ", ")),
                             type = "warning", duration = 6)
            return()
        }
        # LLM output: allowlisted class, user-selected, and a distribution allowed for that class
        status <- vapply(seq_along(cands), function(i) {
            if (!cls[i] %in% .adopt_classes) "unknown class"
            else if (identical(picks[i], "none")) "researcher chose not to adopt this class"
            else if (!identical(picks[i], as.character(i))) "another candidate selected for this class"
            else if (!is_prior_string(.txt(cands[[i]]$prior_string), cls[i]))
                "prior string not allowed for this class"
            else "adopted"
        }, "")
        tcoef <- treatment_coef(model_data, formula_str)
        describe <- function(i) list(label = .txt(cands[[i]]$label), class = cls[i],
                                     coef = if (identical(cls[i], "b")) tcoef,
                                     prior_string = .txt(cands[[i]]$prior_string),
                                     status = status[i])
        turn <- dialog_turn_meta()
        audit_base <- list(scope = dialog_scope(), provider = turn$provider, mode = turn$mode,
                           model_id = turn$model, served_model = turn$served_model, source_response = turn$raw,
                           request = .ai_request_meta("dialog", turn$provider %||% NA_character_),
                           not_adopted = lapply(which(status != "adopted"), describe))
        adopted_idx <- which(status == "adopted")
        if (length(adopted_idx) == 0) {
            do.call(.audit_add, c(list("prior_adopt_rejected", reason = "no candidate passed"), audit_base))
            showNotification("No selected candidate passed validation (unknown class or prior string not allowed for its class).",
                             type = "warning", duration = 6); return()
        }
        defaults <- .default_prior_list(meta)
        key_of <- c(Intercept = "intercept", b = "treatment", cor = "correlation")
        pl <- values$current_prior_list %||% defaults %||% list()
        pl <- pl[!grepl("^sd_", names(pl))]
        pl <- Reduce(function(acc, i) {
            cl <- cls[i]
            key <- if (cl %in% names(key_of)) key_of[[cl]] else tolower(cl)
            if (key == "treatment" && !is.null(tcoef) && is.null(acc$treatment$coef) &&
                !is.null(acc$treatment)) {
                acc$b_default <- acc$b_default %||% list(class = "b", string = acc$treatment$string)
            }
            acc[[key]] <- list(class = cl, string = .txt(cands[[i]]$prior_string),
                               coef = if (key == "treatment") tcoef else NULL)
            acc
        }, adopted_idx, pl)
        family_val <- s$family
        check_formula <- if (isTRUE(s$hetero) && "phase" %in% names(model_data) &&
                             family_val %in% c("gaussian", "student", "skew_normal")) {
            brms::bf(as.formula(formula_str), sigma ~ phase)
        } else as.formula(formula_str)
        check_msg <- tryCatch({
            brms::validate_prior(priors_to_brms(pl), check_formula, data = model_data,
                                 family = brms::brmsfamily(family_val))
            NULL
        }, error = function(e) conditionMessage(e))
        if (!is.null(check_msg)) {
            do.call(.audit_add, c(list("prior_adopt_rejected", reason = paste("validate_prior:", check_msg),
                                       selected = lapply(adopted_idx, describe)), audit_base))
            showNotification(paste("Selected priors do not fit the current model:", check_msg),
                             type = "error", duration = 8); return()
        }
        ref_key <- c(Intercept = "intercept", b = "treatment", sd = "sd", sigma = "sigma", ar = "ar")
        scale_warn <- unlist(lapply(adopted_idx[cls[adopted_idx] %in% names(ref_key)], function(i) {
            ps <- .txt(cands[[i]]$prior_string)
            ref_sd <- .prior_scale(defaults[[ref_key[[cls[i]]]]]$string %||% "")
            r <- .prior_scale(ps) / ref_sd
            c(if (cls[i] %in% c("sd", "sigma") && startsWith(ps, "inv_gamma"))
                  sprintf("%s: inv_gamma on %s rules out values near zero.", ps, cls[i]),
              if (is.finite(r) && (r > 10 || r < 0.1))
                  sprintf("%s: scale is %.3g times the adaptive default %s scale (%.3g).", ps, r, cls[i], ref_sd))
        }))
        values$current_prior_list <- pl
        values$prior_overrides_active <- TRUE
        adopted_keys <- vapply(cls[adopted_idx], function(cl)
            if (cl %in% names(key_of)) key_of[[cl]] else tolower(cl), "", USE.NAMES = FALSE)
        sm <- turn$served_model %||% NA_character_
        served <- if (isTRUE(nzchar(sm) && sm != turn$model)) paste0("; served ", sm)
        values$prior_sources[adopted_keys] <- sprintf("AI-adopted (%s, %s%s)", turn$provider, turn$model, served %||% "")
        do.call(.audit_add, c(list("prior_adopt", adopted = lapply(adopted_idx, describe),
                                   scale_warnings = as.list(scale_warn)), audit_base))
        if (length(scale_warn))
            showNotification(paste("Check the prior scale.", paste(scale_warn, collapse = " ")),
                             type = "warning", duration = 12)
        showNotification(sprintf("Adopted %d prior(s); %d candidate(s) not adopted. They take effect on the next fit.",
                                 length(adopted_idx), sum(status != "adopted")),
                         type = "message", duration = 6)
        removeModal()
    }

    observeEvent(input$dialogAdopt, tryCatch(.adopt_selected(), error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)
        showNotification(paste("Could not adopt the proposal:", conditionMessage(e)),
                         type = "error", duration = 8)
    }))

    observeEvent(input$applyPriorOverride, {
        pl <- values$current_prior_list
        req(pl)
        vals <- lapply(names(pl), function(nm) input[[paste0("override_", nm)]])
        ok <- vapply(seq_along(pl), function(i) is_prior_string(vals[[i]], pl[[i]]$class), logical(1))
        typed <- vapply(vals, function(v) is.character(v) && nzchar(trimws(v)), logical(1))
        changed <- ok & vapply(seq_along(pl), function(i)
            !identical(trimws(vals[[i]] %||% ""), trimws(pl[[i]]$string %||% "")), logical(1))
        pl[ok] <- Map(function(p, v) utils::modifyList(p, list(string = v)), pl[ok], vals[ok])
        values$current_prior_list <- pl
        values$prior_overrides_active <- TRUE
        values$prior_sources[names(pl)[changed]] <- "override"
        removeModal()
        bad <- names(pl)[typed & !ok]
        showNotification(if (length(bad))
            sprintf("Prior overrides queued; not allowed for its class and kept unchanged: %s.", paste(bad, collapse = ", "))
            else "Prior overrides queued for the next fit.",
            type = if (length(bad)) "warning" else "message", duration = 6)
    })

    # fit and prior preview share one prior builder, so the preview samples the priors the fit uses
    .scd_fit_priors <- function(model_data, formula_str, family, outcome_var,
                                meta = isTRUE(values$meta_fit_pending)) {
        pick <- function(id) .input_value(id, meta)
        hetero <- isTRUE(pick("heterogeneous_variance")) && "phase" %in% names(model_data) &&
            family %in% c("gaussian", "student", "skew_normal")
        prior_list <- if (isTRUE(values$prior_overrides_active) &&
                          !is.null(values$current_prior_list)) {
            values$current_prior_list
        } else {
            assemble_brms_priors(
                prior_type = pick("prior_type") %||% "default",
                prior_effect_mean = pick("prior_effect_mean"),
                prior_effect_sd = pick("prior_effect_sd"),
                heterogeneous_variance = hetero,
                model_family_value = family,
                adaptive_priors = .adaptive_priors(model_data, values$model_structure,
                                                   family, outcome_var,
                                                   .exposure_in(meta)),
                treatment_coef = treatment_coef(model_data, formula_str),
                has_random_slope = has_random_slope(formula_str),
                formula_str = formula_str)
        }
        brms_priors <- priors_to_brms(prior_list)
        ar_sd <- suppressWarnings(as.numeric(pick("ar_prior_sd") %||% 0.5))
        # an adopted ar prior keeps the stationarity bounds; the default ar prior fills in only when none is set
        is_ar <- brms_priors$class %in% "ar"
        if (any(is_ar)) {
            brms_priors$lb[is_ar] <- "-1"
            brms_priors$ub[is_ar] <- "1"
        } else if (grepl("\\bar\\(", formula_str)) {
            brms_priors <- c(brms_priors,
                brms::prior_string(sprintf("normal(0, %.2f)", if (isTRUE(ar_sd > 0)) ar_sd else 0.5),
                                   class = "ar", lb = -1, ub = 1))
        }
        list(prior_list = prior_list, brms_priors = brms_priors, hetero = hetero,
             formula = tryCatch(if (hetero) brms::bf(as.formula(formula_str), sigma ~ phase)
                                else as.formula(formula_str), error = function(e) NULL))
    }

    output$priorPredictiveReady <- reactive({ isTRUE(values$prior_predictive_ready) })
    outputOptions(output, "priorPredictiveReady", suspendWhenHidden = FALSE)

    observeEvent(input$runPriorPredictive, {
        req(values$data, input$outcome_var)
        if (isTRUE(values$prior_pred_running))
            return(showNotification("Prior predictive is already running.", type = "warning", duration = 4))
        if (!.size_gate(values$processed_data %||% values$data, "fit")) return()
        showNotification("Sampling from prior only...", type = "message", duration = 5)
        model_data <- values$processed_data %||% values$data
        family_val <- input$model_family %||% "gaussian"
        formula_str <- model_formula()
        req(formula_str)
        model_data <- .prepare_fit_data(model_data, input$outcome_var, family_val)$data
        fp <- tryCatch(.scd_fit_priors(model_data, formula_str, family_val, input$outcome_var, FALSE),
                       error = function(e) NULL)
        if (is.null(fp)) {
            showNotification("Adaptive prior generation failed.",
                             type = "error", duration = 5); return()
        }
        brms_priors <- fp$brms_priors
        preview_formula <- fp$formula
        family_obj <- brms::brmsfamily(family_val)
        stan_args <- .stan_model_args()
        values$prior_pred_running <- TRUE
        promises::finally(promises::future_promise(seed = TRUE, scdbayes:::.drop_sampler_files(.session_dir, {
            brms::brm(preview_formula, data = model_data,
                family = family_obj, prior = brms_priors,
                sample_prior = "only", chains = 2, iter = 1000,
                refresh = 0, silent = 2,
                backend = brms_backend,
                stan_model_args = stan_args)
        })) %...>% (function(fit_prior) {
            values$prior_predictive_model <- fit_prior
            values$prior_predictive_ready <- TRUE
            showNotification("Prior predictive ready.", type = "message", duration = 4)
        }) %...!% (function(e) {
            showNotification(paste("Prior predictive failed:", e$message),
                type = "error", duration = 10)
        }), onFinally = function() values$prior_pred_running <- FALSE)
    })

    output$priorPredictivePlot <- renderPlot({
        req(values$prior_predictive_model)
        tryCatch(brms::pp_check(values$prior_predictive_model,
                                ndraws = 50, type = "dens_overlay"),
                 error = function(e) { plot.new(); title("PP check unavailable") })
    }, alt = "Prior predictive density overlay: observed outcome density against draws from the prior")

    # same caveat prior_predictive_check() returns as $note for a Gaussian ar() term
    .prior_pred_note <- function(m) {
        if (is.null(m)) return(NULL)
        ar_gauss <- tryCatch(isTRUE(get(".gaussian_ar", envir = asNamespace("scdbayes"))(m)),
                             error = function(e) FALSE)
        if (ar_gauss) tags$p(class = "status-main-warning", style = "font-size: 13px; margin-top: 6px;",
            paste("Gaussian ar() term: prior predictions lean on the observed series.",
                  "The check looks better than the prior alone warrants."))
    }
    output$priorPredictiveNote <- renderUI(.prior_pred_note(values$prior_predictive_model))
    output$meta_priorPredictiveNote <- renderUI(.prior_pred_note(values$meta_prior_predictive_model))

    # a programmatic meta-to-SCD sync must not discard adopted priors; only user changes do
    observeEvent(list(input$prior_type, input$prior_effect_mean,
                      input$prior_effect_sd, input$outcome_var,
                      input$model_family), {
        values$prior_predictive_ready <- FALSE
        values$prior_predictive_model <- NULL
        synced <- !is.null(sync_guard$expect) &&
            isTRUE(all.equal(.prior_reset_inputs(), sync_guard$expect, check.attributes = FALSE))
        sync_guard$expect <- NULL
        if (!synced) {
            values$prior_overrides_active <- FALSE
            values$prior_sources <- character(0)
        }
    }, ignoreInit = TRUE)

    # Prior visualization plot: the treatment prior assemble_brms_priors() builds for the loaded data
    output$priorsPlot <- renderPlot({
        prior_type <- input$prior_type %||% "default"
        d <- values$processed_data %||% values$data
        s <- if (is.null(d)) NA_real_ else tryCatch(
            .adaptive_priors(d, values$model_structure, input$model_family %||% "gaussian",
                             input$outcome_var, .exposure_in(FALSE))$treatment$sd,
            error = function(e) NA_real_)
        if (length(s) != 1 || !is.finite(s)) {
            plot.new()
            text(0.5, 0.5, "Load data to plot the treatment prior", cex = 1.2)
            return(invisible(NULL))
        }
        num <- function(x, default) {
            v <- suppressWarnings(as.numeric(x))
            if (length(v) != 1 || !is.finite(v)) default else v
        }
        m <- num(input$prior_effect_mean, 0)
        spec <- switch(prior_type,
            informative   = list(Informative = c(m, 0.5 * s)),
            meta_analytic = list(`Meta-analytic` = c(m, num(input$prior_effect_sd, 0.3))),
            sensitivity   = list(Diffuse = c(0, 2 * s)),
            list(`Weakly informative` = c(0, s)))
        center <- vapply(spec, `[`, numeric(1), 1)
        width <- vapply(spec, `[`, numeric(1), 2)
        x <- seq(min(center) - 3.5 * max(width), max(center) + 3.5 * max(width), length.out = 600)
        dens <- vapply(spec, function(p) stats::dnorm(x, p[1], p[2]), numeric(length(x)))
        cols <- c("#2E5090", "#b45309", "#15803d")[seq_along(spec)]
        graphics::matplot(x, dens, type = "l", lty = 1, lwd = 2, col = cols,
                          xlab = "Treatment coefficient (model scale)", ylab = "Density",
                          main = "Treatment-effect prior")
        graphics::abline(v = 0, lty = 2, col = "gray")
        graphics::legend("topright", legend = sprintf("%s N(%.2f, %.2f)", names(spec), center, width),
                         col = cols, lwd = 2, bg = "white", cex = 0.85)
    }, alt = "Density curves of the selected prior distributions for the treatment effect")

    # Visualization tab outputs
    output$individualTrajectories <- plotly::renderPlotly({
        if (!is.null(values$data)) {
            # Check if required columns exist
            if (all(c("time", "outcome", "phase", "case") %in% names(values$data))) {
                p <- ggplot(values$data, aes(x = time, y = outcome, color = phase)) +
                    geom_point(size = 2) +
                    geom_line(aes(group = interaction(case, phase)), alpha = 0.5) +
                    facet_wrap(~case, scales = "free_y", ncol = .phone_ncol()) +
                    .plot_theme +
                    theme(legend.position = "top") +
                    labs(x = "Time", y = "Outcome")

                styled_ggplotly(p, height = .facet_px(length(unique(values$data$case)), extra = 120,
                                                      ncol = .phone_ncol())) %>%
                    plotly::layout(
                        margin = list(t = 50)
                    )
            } else {
                # Use plot_ly for empty plot with message
                empty_plot("Required columns (time, outcome, phase, case) not found")
            }
        } else {
            empty_plot("No data available")
        }
    })

    output$phaseEffects <- plotly::renderPlotly({
        if (!is.null(values$data)) {
            if (all(c("phase", "outcome") %in% names(values$data))) {
                p <- ggplot(values$data, aes(x = phase, y = outcome, fill = phase)) +
                    geom_boxplot(alpha = 0.7) +
                    geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
                    .plot_theme +
                    theme(legend.position = "none") +
                    labs(x = "Phase", y = "Outcome")

                styled_ggplotly(p) %>%
                    plotly::layout(
                        margin = list(t = 50),
                        showlegend = FALSE
                    )
            } else {
                empty_plot("Required columns (phase, outcome) not found")
            }
        } else {
            empty_plot("No data available")
        }
    })

    output$crossLevelPanel <- renderUI({
        empty_state("interactions",
          'Add moderator variables in <strong style="color: #0c1f4a;">Model &gt; Fixed Effects</strong> and refit to see cross-level interactions.')
    })

    output$posteriorPredictionsPanel <- renderUI({
        empty_state("predict",
          'Posterior predictive plots will appear here after fitting a model.')
    })


    # Results > Hypothesis Tests
    output$hypothesisTestTable <- renderDT({
        req(values$model)

        model_vars <- names(brms::fixef(values$model)[, 1])
        hypotheses <- list()

        # intervention slope = baseline slope + slope change
        if (all(c("time_A", "trend_AB") %in% model_vars)) {
            hypotheses[["Intervention slope"]] <- "time_A + trend_AB = 0"
        }
        # immediate level change
        if ("level_AB" %in% model_vars) {
            hypotheses[["Immediate effect (level change)"]] <- "level_AB = 0"
        }
        # slope change
        if ("trend_AB" %in% model_vars) {
            hypotheses[["Slope change (B vs A)"]] <- "trend_AB = 0"
        }
        # maintenance level
        if ("level_BM" %in% model_vars) {
            hypotheses[["Maintenance effect (level)"]] <- "level_BM = 0"
        }
        # maintenance slope
        if (all(c("time_A", "trend_AB", "trend_BM") %in% model_vars)) {
            hypotheses[["Maintenance slope"]] <- "time_A + trend_AB + trend_BM = 0"
        }
        # reversal: one level-change test per phase transition
        rev_levels <- grep("^level_", reversal_terms(model_vars), value = TRUE)
        rev_hyp <- lapply(rev_levels, function(lv) {
            st <- .step_roles(lv, model_vars)
            labels <- scdbayes:::.step_labels(lv)
            if (is.null(labels)) return(NULL)
            kind <- if (isTRUE(st[2] == 0 && st[1] >= 1)) "Withdrawal" else "Intervention"
            stats::setNames(paste(lv, "= 0"), sprintf("%s effect (%s to %s)", kind, labels[1], labels[2]))
        })
        hypotheses <- c(hypotheses, as.list(unlist(rev_hyp)))

        shiny::validate(need(length(hypotheses) > 0, "No testable hypotheses for this model specification."))

        rows <- lapply(names(hypotheses), function(label) {
            h <- tryCatch(
                brms::hypothesis(values$model, hypotheses[[label]], alpha = .hyp_alpha("results_ci_level")),
                error = function(e) NULL
            )
            if (is.null(h)) return(NULL)
            data.frame(
                Contrast = label,
                Hypothesis = hypotheses[[label]],
                Estimate = h$hypothesis$Estimate,
                `Std Error` = h$hypothesis$Est.Error,
                `CI Lower` = h$hypothesis$CI.Lower,
                `CI Upper` = h$hypothesis$CI.Upper,
                `Post Prob` = h$hypothesis$Post.Prob,
                Star = h$hypothesis$Star,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )
        })

        result <- do.call(rbind, Filter(Negate(is.null), rows))
        shiny::validate(need(!is.null(result) && nrow(result) > 0, "Could not compute hypothesis tests."))
        dt_table(.drop_na_postprob(result), digits = 3, caption = "Hypothesis test")
    })

    # Custom hypothesis test
    custom_hypothesis_result <- reactiveVal(NULL)
    observeEvent(input$runCustomHypothesis, {
        req(values$model, input$customHypothesis)
        if (!.hypothesis_ok(values$model, input$customHypothesis)) return(custom_hypothesis_result(NULL))
        h <- tryCatch(
            brms::hypothesis(values$model, input$customHypothesis,
                             alpha = .hyp_alpha("results_ci_level", input$customHypothesis)),
            error = function(e) {
                showNotification(paste("Hypothesis error:", e$message), type = "error", duration = 5)
                NULL
            }
        )
        if (is.null(h)) {
            custom_hypothesis_result(NULL)
            return()
        }
        custom_hypothesis_result(data.frame(
            Hypothesis = input$customHypothesis,
            Estimate = h$hypothesis$Estimate,
            `Std Error` = h$hypothesis$Est.Error,
            `CI Lower` = h$hypothesis$CI.Lower,
            `CI Upper` = h$hypothesis$CI.Upper,
            `Post Prob` = h$hypothesis$Post.Prob,
            Star = h$hypothesis$Star,
            check.names = FALSE,
            stringsAsFactors = FALSE
        ))
    })

    output$customHypothesisTable <- renderDT({
        result <- custom_hypothesis_result()
        shiny::validate(need(!is.null(result), "Run a hypothesis test to see results."))
        dt_table(.drop_na_postprob(result), digits = 3, caption = "Custom hypothesis test")
    })

    # Results > Fixed Effects
    output$fixedEffectsTable <- renderDT({
        req(values$model)
        tryCatch({
            ci_level <- (input$results_ci_level %||% 95) / 100
            probs <- c((1 - ci_level) / 2, 1 - (1 - ci_level) / 2)
            fe <- as.data.frame(brms::fixef(values$model, probs = probs))
            fe$Parameter <- rownames(fe)
            ci_cols <- grep("^Q", names(fe), value = TRUE)
            fam <- tryCatch(family(values$model)$family, error = function(e) "")
            link <- tryCatch(family(values$model)$link, error = function(e) "")
            is_cumulative <- identical(fam, "cumulative")
            is_logit <- identical(link, "logit")
            is_log <- identical(link, "log")
            # reversed thresholds: a positive coefficient raises P(Y > k | X)
            if (is_cumulative) {
                thr <- grepl("^Intercept\\[", fe$Parameter) | grepl("^Intercept$", fe$Parameter)
                if (any(thr)) {
                    fe[thr, "Estimate"] <- -fe[thr, "Estimate"]
                    lo <- fe[thr, ci_cols[1]]
                    fe[thr, ci_cols[1]] <- -fe[thr, ci_cols[2]]
                    fe[thr, ci_cols[2]] <- -lo
                }
            }
            fe <- fe[, c("Parameter", "Estimate", "Est.Error", ci_cols)]
            pct_lo <- sprintf("%.1f%%", probs[1] * 100)
            pct_hi <- sprintf("%.1f%%", probs[2] * 100)
            names(fe) <- c("Parameter", "Estimate", "SE",
                           paste0("CI Lower (", pct_lo, ")"),
                           paste0("CI Upper (", pct_hi, ")"))
            if (is_logit) {
                label <- if (identical(fam, "beta")) "Typical-case mean-proportion odds ratio" else
                    "Typical-case (conditional) OR"
                fe[[label]] <- exp(fe$Estimate)
            }
            if (is_log) {
                label <- if (identical(fam, "Gamma")) "Typical-case (conditional) mean ratio" else
                    "Typical-case (conditional) IRR"
                fe[[label]] <- exp(fe$Estimate)
            }
            d <- input$results_digits %||% 3
            fe[] <- lapply(fe, function(x) if (is.numeric(x)) round(x, d) else x)
            dt_table(fe, pageLength = 20, digits = d, caption = "Fixed-effect estimates")
        }, error = function(e) {
            dt_table(data.frame(Message = "Fixed effects not available"), caption = "Fixed-effect estimates (unavailable)")
        })
    })

    output$fixedEffectsPlot <- plotly::renderPlotly({
        req(values$model)
        tryCatch({
            ci_level <- (input$results_ci_level %||% 95) / 100
            probs <- c((1 - ci_level) / 2, 1 - (1 - ci_level) / 2)
            fe <- as.data.frame(brms::fixef(values$model, probs = probs))
            fe$Parameter <- rownames(fe)
            fe <- fe[fe$Parameter != "Intercept", , drop = FALSE]
            if (nrow(fe) == 0) {
                return(empty_plot("No non-intercept fixed effects"))
            }
            ci_cols <- grep("^Q", names(fe), value = TRUE)
            p <- ggplot(fe, aes(x = Estimate, y = reorder(Parameter, Estimate))) +
                geom_point(size = 3, color = "#2E5090") +
                geom_errorbar(aes(xmin = .data[[ci_cols[1]]], xmax = .data[[ci_cols[2]]]),
                              width = 0.2, color = "#2E5090", orientation = "y") +
                geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
                .plot_theme +
                labs(x = sprintf("Estimate (%g%% CI)", input$results_ci_level %||% 95), y = "Parameter")
            styled_ggplotly(p)
        }, error = function(e) {
            empty_plot(paste("Error:", e$message))
        })
    })

    # Results > Variance Components
    output$varianceComponents <- renderPrint({
        req(values$model)
        tryCatch({
            vc <- brms::VarCorr(values$model)
            cat("Variance Components\n\n")
            re_groups <- setdiff(names(vc), "residual__")
            for (group in re_groups) {
                label <- switch(group,
                    case = "Between-case", study = "Between-study",
                    participant = "Between-participant", group)
                cat(sprintf("%s (%s)\n", label, group))
                for (i in seq_len(nrow(vc[[group]]$sd))) {
                    param <- rownames(vc[[group]]$sd)[i]
                    tau <- vc[[group]]$sd[i, "Estimate"]
                    cat(sprintf("  tau (%s): %.3f  [tau-sq: %.3f]\n", param, tau, tau^2))
                }
                cat("\n")
            }
            sigma_val <- tryCatch(summary(values$model)$spec_pars["sigma", "Estimate"], error = function(e) NA)
            if (!is.na(sigma_val)) {
                cat(sprintf("Within-case (sigma): %.3f  [sigma-sq: %.3f]\n", sigma_val, sigma_val^2))
            }
            if (length(re_groups) > 0 && !is.na(sigma_val)) {
                total_var <- sum(sapply(re_groups, function(g) vc[[g]]$sd[1, "Estimate"]^2)) + sigma_val^2
                cat("\nICC:\n")
                for (group in re_groups) {
                    tau_sq <- vc[[group]]$sd[1, "Estimate"]^2
                    cat(sprintf("  %s: %.3f (%.1f%%)\n", group, tau_sq / total_var, tau_sq / total_var * 100))
                }
            }
        }, error = function(e) cat("Variance components not available:", e$message))
    })

    output$varianceDecomposition <- plotly::renderPlotly({
        req(values$model)
        tryCatch({
            vc <- brms::VarCorr(values$model)
            re_groups <- setdiff(names(vc), "residual__")
            var_data <- data.frame(Component = character(), Variance = numeric(), stringsAsFactors = FALSE)
            for (group in re_groups) {
                label <- switch(group,
                    case = "Between-case (tau)", study = "Between-study (tau)",
                    participant = "Between-participant (tau)", paste0(group, " (tau)"))
                var_data <- rbind(var_data, data.frame(Component = label, Variance = vc[[group]]$sd[1, "Estimate"]^2))
            }
            sigma_val <- tryCatch(summary(values$model)$spec_pars["sigma", "Estimate"], error = function(e) NA)
            if (!is.na(sigma_val)) {
                var_data <- rbind(var_data, data.frame(Component = "Within-case (sigma)", Variance = sigma_val^2))
            }
            var_data$Proportion <- var_data$Variance / sum(var_data$Variance) * 100

            p <- ggplot(var_data, aes(x = reorder(Component, Proportion), y = Proportion, fill = Component)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                .plot_theme +
                labs(x = "Component", y = "Proportion of Variance (%)") +
                theme(legend.position = "none")
            styled_ggplotly(p)
        }, error = function(e) {
            empty_plot("Variance decomposition not available")})
    })

    # Results > Diagnostics > Model Fit
    output$modelFitStats <- renderPrint({
        req(values$model)
        tryCatch({
            cat("Model Fit Statistics\n")
            cat("\n")
            r2 <- tryCatch({ r2_val <- brms::bayes_R2(values$model); sprintf("%.3f", mean(r2_val[,1])) }, error = function(e) "N/A")
            cat(sprintf("Bayes R-squared: %s\n", r2))
            loo_res <- .cached_loo()
            if (!is.null(loo_res)) {
                cat(sprintf("LOO-IC: %.1f (SE: %.1f)\n", loo_res$estimates["looic", "Estimate"], loo_res$estimates["looic", "SE"]))
                cat(sprintf("ELPD: %.1f (SE: %.1f)\n", loo_res$estimates["elpd_loo", "Estimate"], loo_res$estimates["elpd_loo", "SE"]))
                # sample-size-dependent Pareto k threshold (loo >= 2.7)
                k_max <- min(1 - 1 / log10(posterior::ndraws(values$model)), 0.7)
                bad_k <- sum(loo_res$diagnostics$pareto_k > k_max)
                if (bad_k > 0) cat(sprintf("\nWarning: %d observations with Pareto k > %.2f\n", bad_k, k_max))
            }
            waic_res <- tryCatch(brms::waic(values$model), error = function(e) NULL)
            if (!is.null(waic_res)) {
                cat(sprintf("WAIC: %.1f (SE: %.1f)\n", waic_res$estimates["waic", "Estimate"], waic_res$estimates["waic", "SE"]))
            }
        }, error = function(e) cat("Model fit statistics not available:", e$message))
    })

    # Results > Diagnostics: parameter selector
    output$traceParamSelector <- renderUI({
        req(values$model)
        vars <- brms::variables(values$model)
        all_params <- vars[!grepl("^lp__|^lprior", vars)]
        # exclude individual random effect draws (r_case[...], z_1[...]) to keep list manageable
        main_params <- all_params[!grepl("^r_|^z_", all_params)]
        if (length(main_params) == 0) main_params <- all_params[1:min(4, length(all_params))]
        default_sel <- main_params[1:min(4, length(main_params))]
        checkboxGroupInput("trace_params", tags$span(class = "sr-only", "Parameters to trace"),
            choices = main_params, selected = default_sel)
    })

    # Results > Diagnostics plots
    .trace_px <- function(n) min(max(180 * n, 300), 2400)
    trace_params <- shiny::debounce(reactive(input$trace_params), 500)
    output$tracePlotsBox <- renderUI({
        plotly_figure("tracePlots", "Trace plots of MCMC draws by iteration for selected parameters",
                      height = paste0(.trace_px(min(max(length(trace_params()), 1), 12)), "px"))
    })
    output$tracePlots <- plotly::renderPlotly({
        req(values$model, trace_params())
        shiny::validate(need(length(trace_params()) <= 12, "Select at most 12 parameters."))
        tryCatch({
            param_cols <- intersect(trace_params(), brms::variables(values$model))
            req(length(param_cols) > 0)
            draws <- posterior::as_draws_df(values$model, variable = param_cols)
            px <- .trace_px(length(param_cols))
            plots <- lapply(param_cols, function(param) {
                vals <- draws[[param]]
                plotly::plot_ly(x = seq_along(vals), y = vals, type = "scatter", mode = "lines",
                        name = param, line = list(width = 0.5), height = px) %>%
                    plotly::layout(xaxis = list(title = "Iteration"), yaxis = list(title = param))
            })
            plotly::subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE) %>%
                plotly::layout(title = "Trace Plots", showlegend = TRUE)
        }, error = function(e) {
            empty_plot(paste("Trace plots error:", e$message))
        })
    })




    output$residualPlots <- plotly::renderPlotly({
        req(values$model)
        tryCatch({
            resid_vals <- residuals(values$model)[, "Estimate"]
            fitted_vals <- fitted(values$model)[, "Estimate"]
            df <- data.frame(Fitted = fitted_vals, Residual = resid_vals)
            p <- ggplot(df, aes(x = Fitted, y = Residual)) +
                geom_point(alpha = 0.5) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "blue", alpha = 0.2) +
                .plot_theme +
                labs(x = "Fitted Values", y = "Residuals")
            styled_ggplotly(p)
        }, error = function(e) {
            empty_plot("Residual plot not available")})
    })

    # Results > Effect Size Plot
    output$effectSizePlot <- plotly::renderPlotly({
        req(values$data)
        tryCatch({
            if (!all(c("case", "phase", "outcome") %in% names(values$data))) {
                return(empty_plot("Required columns not found"))
            }
            phases <- unique(values$data$phase)
            if (length(phases) < 2) {
                return(empty_plot("Need at least 2 phases"))
            }
            case_effects <- values$data %>%
                group_by(case) %>%
                summarise(
                    mean_a = mean(outcome[phase == phases[1]], na.rm = TRUE),
                    mean_b = mean(outcome[phase == phases[2]], na.rm = TRUE),
                    sd_a = sd(outcome[phase == phases[1]], na.rm = TRUE),
                    n_a = sum(phase == phases[1]),
                    n_b = sum(phase == phases[2]),
                    .groups = "drop"
                ) %>%
                mutate(
                    effect = mean_b - mean_a,
                    se = sd_a / sqrt(pmin(n_a, n_b)),
                    ci_lower = effect - 1.96 * se,
                    ci_upper = effect + 1.96 * se
                )
            p <- ggplot(case_effects, aes(x = effect, y = reorder(case, effect))) +
                geom_point(size = 3, color = "#2E5090") +
                geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.2, color = "#2E5090", orientation = "y") +
                geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
                .plot_theme +
                labs(x = "Raw phase difference", y = "Case")
            styled_ggplotly(p)
        }, error = function(e) {
            empty_plot(paste("Error:", e$message))
        })
    })

    # treatment coefficient among model terms; the intercept is excluded
    .pick_treatment_term <- function(terms) {
        terms <- grep("^Intercept|^sigma_|^disc_|^ar(\\[|$)|\\[[0-9]+\\]$", terms,
                      value = TRUE, invert = TRUE)
        if (length(terms) == 0) return(NULL)
        onset <- tryCatch(treatment_coef(as.data.frame(as.list(stats::setNames(numeric(length(terms)), terms)),
                                                       check.names = FALSE)),
                          error = function(e) NULL)
        if (!is.null(onset)) return(onset)
        pri <- c("level_AB", grep("^level_", terms, value = TRUE),
                 "condition", "treatment", grep("^treatment", terms, value = TRUE),
                 grep("^trend_", terms, value = TRUE))
        hit <- pri[pri %in% terms]
        if (length(hit) > 0) return(hit[1])
        return(terms[1])
    }

    .meta_goal <- function() {
        input$meta_intervention_goal %||% input$intervention_goal %||% "increase"
    }

    .per_study_effects <- function(model) {
        study_group <- find_study_var(model$data) %||% "study"
        re <- tryCatch(brms::ranef(model, summary = FALSE), error = function(e) NULL)
        if (is.null(re) || is.null(re[[study_group]])) return(list(note = "No study-level random effects in this model."))
        term <- .pick_treatment_term(dimnames(re[[study_group]])[[3]])
        fixed <- tryCatch(brms::fixef(model, summary = FALSE), error = function(e) NULL)
        if (is.null(term) || is.null(fixed) || !term %in% colnames(fixed)) {
            return(list(note = "Study grouping is intercept-only; no study-level treatment effect estimated."))
        }
        goal <- scdbayes:::.coefficient_goal(model, term, .meta_goal(), colnames(fixed))
        sign <- if (goal == "decrease") -1 else 1
        alpha <- (1 - (input$meta_ci_level %||% 95) / 100) / 2
        studies <- dimnames(re[[study_group]])[[2]]
        rows <- lapply(studies, function(s) {
            d <- sign * (fixed[, term] + re[[study_group]][, s, term])
            q <- stats::quantile(d, c(alpha, 1 - alpha), names = FALSE)
            data.frame(Study = s, est = mean(d), se = stats::sd(d), lo = q[1], hi = q[2],
                       stringsAsFactors = FALSE)
        })
        list(term = term, df = do.call(rbind, rows))
    }

    output$perStudyEffectsTable <- DT::renderDataTable({
        m <- values$meta_model %||% values$model
        req(m, "study" %in% names(values$data %||% data.frame()))
        res <- .per_study_effects(m)
        if (!is.null(res$note)) {
            return(DT::datatable(data.frame(Note = res$note),
                                 rownames = FALSE, options = list(dom = "t"),
                                 caption = .dt_caption("Per-study effects (note)")))
        }
        df <- data.frame(
            Study     = res$df$Study,
            Term      = res$term,
            Estimate  = round(res$df$est, 3),
            Est.Error = round(res$df$se, 3),
            Lower     = round(res$df$lo, 3),
            Upper     = round(res$df$hi, 3),
            stringsAsFactors = FALSE
        )
        DT::datatable(df, options = list(pageLength = 20, scrollX = TRUE),
                      class = "cell-border stripe", rownames = FALSE,
                      caption = "Study effect = fixed effect + study deviation, oriented so positive values mean improvement.")
    })

    output$perStudyEffectsPlot <- plotly::renderPlotly({
        m <- values$meta_model %||% values$model
        req(m, "study" %in% names(values$data %||% data.frame()))
        res <- .per_study_effects(m)
        if (!is.null(res$note)) return(empty_plot(res$note))
        term <- res$term
        df <- res$df
        df <- df[order(df$est), ]
        df$Study <- factor(df$Study, levels = df$Study)
        plotly::plot_ly(df, type = "scatter", mode = "markers",
                        x = ~est, y = ~Study, height = .facet_px(nrow(df), per_row = 28, ncol = 1, extra = 140),
                        error_x = list(array = ~(hi - est), arrayminus = ~(est - lo)),
                        marker = list(size = 10, color = "#0c1f4a")) %>%
            plotly::layout(
                title = sprintf("Per-study intervention effect (%s)", term),
                xaxis = list(title = "Posterior mean (study treatment effect, positive = improvement)",
                             zeroline = TRUE, zerolinecolor = "#9CA3AF"),
                yaxis = list(title = ""),
                margin = list(l = 100, r = 30, t = 50, b = 50)
            )
    })

    # Results > Meta-Analysis
    # per-case effects depend only on model, CI level and goal, so sort/height changes reuse them
    forest_effects <- reactive({
        m <- values$meta_model %||% values$model
        ci_level <- (input$meta_ci_level %||% 95) / 100
        goal <- .meta_goal()
        pce <- tryCatch(summarize_per_case_effects(m, ci_level = ci_level,
                                                   intervention_goal = goal),
                        error = function(e) NULL)
        term <- if (!is.null(pce) && nrow(pce) > 0) .pick_treatment_term(unique(pce$term)) else NULL

        if (!is.null(term)) {
            df <- pce[pce$term == term, c("case", "mean", "lower", "upper")]
            names(df) <- c("Case", "Overall", "Lower", "Upper")
            effect_label <- sprintf("per-case intervention effect: %s", term)
            effect_scale <- "link"
        } else {
            # intercept-only case effects: response-scale contrast from processed_data
            pc <- tryCatch(compute_phase_contrast(m,
                                                  data = values$processed_data %||% values$data,
                                                  per_case = TRUE,
                                                  intervention_goal = goal,
                                                  ci_level = ci_level),
                           error = function(e) NULL)
            if (is.null(pc) || is.null(pc$per_case_contrast)) return(NULL)
            df <- pc$per_case_contrast[, c("case", "mean", "lower", "upper")]
            names(df) <- c("Case", "Overall", "Lower", "Upper")
            effect_label <- "per-case contrast (no case-varying slope)"
            effect_scale <- "response"
        }
        df$Case <- .case_display(df$Case, m$data)
        native_goal <- if (identical(effect_scale, "link"))
            scdbayes:::.coefficient_goal(m, term, goal) else goal
        list(df = df, label = effect_label, scale = effect_scale, goal = native_goal)
    })
    output$forestPlot <- plotly::renderPlotly({
        m <- values$meta_model %||% values$model
        req(m, values$data)
        tryCatch({
            if (!"case" %in% names(values$data)) {
                return(empty_plot("No case variable for forest plot"))
            }
            fe <- forest_effects()
            if (is.null(fe)) return(empty_plot("No per-case treatment effect available for forest plot"))
            df <- fe$df
            effect_label <- fe$label
            sort_by <- input$meta_forest_sort %||% "effect"
            y_var <- if (sort_by == "name") reorder(df$Case, rev(seq_len(nrow(df)))) else reorder(df$Case, df$Overall)

            p <- ggplot(df, aes(x = Overall, y = y_var)) +
                geom_point(size = 3, color = "#2E5090") +
                geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2, color = "#2E5090", orientation = "y") +
                geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
                .plot_theme +
                labs(x = sprintf("Estimate (%g%% CrI) — %s%s", (input$meta_ci_level %||% 95), effect_label,
                                 paste0("\n", forest_scale_text(fe$scale,
                                     tryCatch(stats::family(m)$link, error = function(e) ""), fe$goal))),
                     y = "Case")
            # the slider sets a minimum; each case row needs about 28 px
            height_px <- .facet_px(nrow(df), per_row = 28, ncol = 1, extra = 140,
                                   min_px = .mcmc_num(input$meta_forest_height, 500))
            styled_ggplotly(p, height = height_px)
        }, error = function(e) {
            empty_plot(paste("Forest plot error:", e$message))
        })
    })

    .plot_theme <- theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank(),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 8)),
              axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
              strip.text = element_text(size = 13, face = "bold"),
              legend.text = element_text(size = 13),
              plot.title = element_text(size = 15, face = "bold"))

    .pp_theme <- .plot_theme

    # Visual-analysis posterior predictive checks
    ppc_result <- reactiveVal(NULL)
    ppc_features <- function() ppc_result() %||% list()
    .ppc_msg <- function(default) {
        note <- ppc_features()$note %||% ""
        if (nzchar(note)) note else default
    }
    observeEvent(values$model, {
        ppc_result(NULL)
        shinyjs::toggleState("runPpcFeatures", condition = !is.null(values$model))
    }, ignoreNULL = FALSE)

    observeEvent(input$runPpcFeatures, {
        req(values$model)
        stats_sel <- input$ppc_stats
        if (length(stats_sel) == 0) return(ppc_result(NULL))
        goal <- input$intervention_goal %||% "increase"
        mode <- input$ppc_mode %||% "same"
        # fitted columns (rounded counts, squeezed proportions) over the processed rows that kept time/phase/case
        pd <- values$processed_data %||% values$data
        fd <- values$model$data
        shared <- intersect(names(fd), names(pd))
        keep <- stats::complete.cases(pd[, shared, drop = FALSE])
        ppc_data <- if (length(shared) > 0 && sum(keep) == nrow(fd)) {
            out <- pd[keep, , drop = FALSE]
            out[shared] <- fd[shared]
            out
        } else pd
        # the design the model was fit with, so the checks do not re-derive a different phase structure
        ppc_design <- values$fit_design %||% .design_choice(input$research_design, values$data)
        feature_args <- c(list(model = values$model, data = ppc_data, statistics = stats_sel,
                               n_draws = input$ppc_ndraws %||% 500, mode = mode,
                               intervention_goal = goal, design = ppc_design),
                          .formal_args(posterior_predictive_features,
                                       list(ar_method = input$ppc_ar_method, pool = input$ppc_pool)))
        # package errors (such as latent AR) and result notes are shown in place of an empty panel
        notes <- character(0)
        capture <- function(expr) withCallingHandlers(
            tryCatch(expr, error = function(e) { notes <<- c(notes, .short_error(e)); NULL }),
            message = function(m) { notes <<- c(notes, trimws(conditionMessage(m))); invokeRestart("muffleMessage") })
        withProgress(message = "Running posterior predictive checks", value = 0.4, {
            features <- capture(do.call(posterior_predictive_features, feature_args))
            overlay <- capture(ppc_overlay_draws(values$model, data = ppc_data, n_lines = 20,
                                                 mode = mode, design = ppc_design))
            notes <- unique(c(notes, attr(features, "message"),
                              if (is.list(features)) c(features$message, features$note),
                              if (is.list(overlay)) overlay$note, attr(overlay, "message")))
            ppc_result(list(features = features, overlay = overlay, note = paste(notes, collapse = " ")))
        })
    })

    output$ppcOptionsUI <- renderUI({
        tagList(
            selectInput("ppc_ar_method", "Autocorrelation estimator",
                choices = c("Method of moments" = "moment", "GLS" = "gls"), selected = "moment"),
            selectInput("ppc_pool", "Pooled row",
                choices = c("Mean of case values" = "case_means", "All observations" = "all_obs"),
                selected = "case_means"))
    })

    output$ppcCaseStatUI <- renderUI({
        res <- ppc_features()$features
        stats_avail <- unique(res$summary$statistic)
        stats_avail <- stats_avail[!grepl("^immediacy_t", stats_avail)]
        if (!length(stats_avail)) return(NULL)
        selectInput("ppc_case_stat", "Feature for the per-case panels", choices = stats_avail,
                    selected = isolate(input$ppc_case_stat) %||% stats_avail[1])
    })

    .ppc_case_order <- function(case) {
        lv <- unique(as.character(case))
        factor(case, levels = c(sort(setdiff(lv, "pooled")), intersect("pooled", lv)))
    }

    output$ppcCaseDensity <- renderPlot({
        res <- ppc_features()$features
        stat <- input$ppc_case_stat
        validate(need(!is.null(res) && nrow(res$draws) > 0 && !is.null(stat),
                      .ppc_msg("Click Run checks to generate the per-case feature checks.")))
        dr <- res$draws[res$draws$statistic == stat, , drop = FALSE]
        sm <- res$summary[res$summary$statistic == stat, , drop = FALSE]
        validate(need(nrow(dr) > 0, "No draws for this feature."))
        dr$phase <- ifelse(is.na(dr$phase), "all", dr$phase)
        sm$phase <- ifelse(is.na(sm$phase), "all", sm$phase)
        dr$case <- .ppc_case_order(dr$case)
        sm$case <- factor(sm$case, levels = levels(dr$case))
        sm$label <- sprintf("Pr=%.2f", sm$p_value)
        n_rows <- nlevels(dr$case)
        ggplot(dr, aes(value)) +
            geom_density(fill = "#93c5fd", color = "#3b82f6", alpha = 0.6) +
            geom_vline(data = sm, aes(xintercept = observed), color = "#dc2626", linewidth = 0.7) +
            geom_text(data = sm, aes(x = Inf, y = Inf, label = label), hjust = 1.1, vjust = 1.4,
                      size = 3.6, color = "#374151") +
            facet_grid(case ~ phase, scales = "free") +
            labs(title = sprintf("%s by case and phase; bottom row pooled across cases (observed in red)", stat),
                 caption = if (.facet_capped(n_rows, per_row = 130, min_px = 320, ncol = 1, extra = 0))
                     "Plot height capped at 2400 px; the Summary CSV lists every case.",
                 x = NULL, y = NULL) +
            .pp_theme
    }, height = function() {
        res <- ppc_features()$features
        .facet_px(length(unique(res$draws$case)), per_row = 130, min_px = 320, ncol = 1, extra = 0)
    }, alt = "Per-case posterior-predictive densities of the selected feature by phase, with a pooled row and observed values marked")

    output$ppcImmediacy <- renderPlot({
        res <- ppc_features()$features
        validate(need(!is.null(res) && any(grepl("^immediacy_t", res$draws$statistic)),
                      .ppc_msg("Select Immediacy (TSTE) and click Run checks.")))
        dr <- res$draws[grepl("^immediacy_t", res$draws$statistic), , drop = FALSE]
        sm <- res$summary[grepl("^immediacy_t", res$summary$statistic), , drop = FALSE]
        dr$k <- factor(sub("^immediacy_t", "", dr$statistic))
        sm$k <- factor(sub("^immediacy_t", "", sm$statistic), levels = levels(dr$k))
        panel_of <- function(d) paste(d$case, ifelse(is.na(d$phase), "", d$phase))
        lv <- unique(panel_of(dr))
        pooled <- lv[startsWith(lv, "pooled ")]
        dr$panel <- factor(panel_of(dr), levels = c(sort(setdiff(lv, pooled)), sort(pooled)))
        sm$panel <- factor(panel_of(sm), levels = levels(dr$panel))
        ggplot(dr, aes(k, value)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "#9CA3AF") +
            geom_violin(fill = "#93c5fd", color = "#3b82f6", alpha = 0.6, scale = "width") +
            geom_point(data = sm, aes(k, observed), color = "#dc2626", size = 2.4) +
            facet_wrap(~ panel, scales = "free_y", ncol = .phone_ncol()) +
            labs(title = "Time-specific treatment effects after treatment onset (observed in red)",
                 x = "Sessions since treatment onset", y = "TSTE (improvement direction)") +
            .pp_theme
    }, height = function() {
        dr <- ppc_features()$features$draws
        imm <- dr[grepl("^immediacy_t", dr$statistic %||% character(0)), , drop = FALSE]
        .facet_px(nrow(unique(imm[intersect(c("case", "phase"), names(imm))])), ncol = .phone_ncol())
    }, alt = "Posterior-predictive distributions of time-specific treatment effects for the first five intervention sessions per case, with observed values")

    output$downloadPpcSummary <- downloadHandler(
        filename = function() "scdbayes_ppc_summary.csv",
        content = function(file) {
            res <- ppc_features()$features
            if (is.null(res) || !nrow(res$summary)) return(.download_unavailable(file, "Run the posterior predictive checks first."))
            write.csv(res$summary, file, row.names = FALSE)
        }
    )

    output$downloadPpcDraws <- downloadHandler(
        filename = function() "scdbayes_ppc_draws.csv",
        content = function(file) {
            res <- ppc_features()$features
            if (is.null(res) || !nrow(res$draws)) return(.download_unavailable(file, "Run the posterior predictive checks first."))
            write.csv(res$draws, file, row.names = FALSE)
        }
    )

    output$ppcOverlay <- renderPlot({
        ov <- ppc_features()$overlay
        validate(need(!is.null(ov), .ppc_msg("Click Run checks to generate posterior-predictive draws.")))
        # one line per phase occurrence, so an A1-B1-A2-B2 series is not joined across phases
        piece <- function(d) if ("phase" %in% names(d)) d$phase else d$condition
        ov$replicates$.piece <- piece(ov$replicates)
        ov$observed$.piece <- piece(ov$observed)
        ggplot() +
            geom_line(data = ov$replicates,
                      aes(time, outcome, group = interaction(draw, .piece), color = condition),
                      alpha = 0.15) +
            geom_line(data = ov$observed,
                      aes(time, outcome, group = .piece), color = "#111827", linewidth = 0.8) +
            geom_point(data = ov$observed, aes(time, outcome), color = "#111827", size = 1.4) +
            facet_wrap(~ case, scales = "free", ncol = .phone_ncol()) +
            scale_color_manual(values = c(baseline = "#e879a8", intervention = "#34d399"), name = NULL) +
            labs(title = "Posterior-predictive overlay (20 draws)", x = "Time", y = "Outcome") +
            .pp_theme
    }, height = function() .facet_px(length(unique(ppc_features()$overlay$observed$case)), ncol = .phone_ncol()),
    alt = "Observed outcome series per case overlaid on 20 posterior-predictive replicate series by phase")

    output$ppcFeatureDensity <- renderPlot({
        res <- ppc_features()$features
        validate(need(!is.null(res) && nrow(res$draws) > 0,
                      .ppc_msg("Click Run checks to generate the feature checks.")))
        panel_key <- function(s, p) ifelse(is.na(p), s, paste(s, p))
        sm <- res$summary[res$summary$case == "pooled", , drop = FALSE]
        sm$k <- panel_key(sm$statistic, sm$phase)
        sm$panel <- sprintf("%s  (Pr[T(yrep) ≥ T(y)]=%.3f%s)", sm$k, sm$p_value, ifelse(sm$flag, " *", ""))
        lut <- stats::setNames(sm$panel, sm$k)
        dr <- res$draws[res$draws$case == "pooled", , drop = FALSE]
        dr$panel <- lut[panel_key(dr$statistic, dr$phase)]
        ggplot(dr, aes(value)) +
            geom_density(fill = "#93c5fd", color = "#3b82f6", alpha = 0.6) +
            geom_vline(data = sm, aes(xintercept = observed), color = "#dc2626", linewidth = 0.7) +
            facet_wrap(~ panel, scales = "free") +
            labs(title = "Observed (red) vs posterior-predictive distribution, pooled across cases",
                 subtitle = if (nzchar(ppc_features()$note %||% "")) strwrap(ppc_features()$note, 110, simplify = FALSE)[[1]] |> paste(collapse = "\n"),
                 x = NULL, y = NULL) +
            .pp_theme
    }, alt = "Posterior-predictive densities of visual-analysis features with observed values marked; panel titles give p-values")

    output$ppcFeatureTable <- DT::renderDataTable({
        res <- ppc_features()$features
        validate(need(!is.null(res) && nrow(res$summary) > 0,
                      .ppc_msg("Click Run checks to view posterior predictive p-values.")))
        sm <- res$summary
        if (!"p_tie" %in% names(sm)) sm$p_tie <- NA_real_
        df <- sm[, c("statistic", "case", "phase", "observed", "p_value", "p_tie", "flag")]
        df$phase <- ifelse(is.na(df$phase), "", df$phase)
        df$observed <- round(df$observed, 3)
        df$p_value <- round(df$p_value, 3)
        df$p_tie <- round(df$p_tie, 3)
        df$flag <- ifelse(df$flag, "discrepant", ifelse(!is.na(df$p_tie) & df$p_tie >= 0.5, "ties (not flagged)", ""))
        names(df) <- c("Feature", "Case", "Phase", "Observed", "Pr[T(yrep) ≥ T(y)]", "Pr[T(yrep) = T(y)]", "Flag")
        DT::datatable(df, rownames = FALSE, class = "cell-border stripe",
                      options = list(pageLength = 15, scrollX = TRUE),
                      caption = .dt_caption("Posterior predictive checks on visual-analysis features")) %>%
            DT::formatStyle("Flag", target = "row",
                            backgroundColor = DT::styleEqual("discrepant", "#fef2f2"))
    })

    # generic checks use the package replicates, which redraw observation-level effects
    .pp_replicates <- function(m, ndraws) {
        fd <- m$data
        yrep <- ppc_replicates(m, fd, mode = "same", n_draws = min(ndraws, brms::ndraws(m)))
        y <- as.numeric(fd[[all.vars(stats::formula(m)$formula)[1]]])
        keep <- is.finite(y) & colSums(is.finite(yrep)) == nrow(yrep)
        list(y = y[keep], yrep = yrep[, keep, drop = FALSE], keep = keep, data = fd)
    }

    # drawn once per (model, draw count) and shared, so switching sub-tabs does not refit
    pp_reps_density <- reactive(.pp_replicates(values$model, input$pp_ndraws %||% 50))
    pp_reps_stat <- reactive(.pp_replicates(values$model, (input$pp_ndraws %||% 50) * 10))

    output$ppCheckDensity <- renderPlot({
        req(values$model)
        tryCatch({
            r <- pp_reps_density()
            bayesplot::ppc_dens_overlay(r$y, r$yrep) + .pp_theme
        }, error = function(e) ggplot() + annotate("text", x = 0, y = 0, label = "Density overlay not available") + theme_void())
    }, alt = "Posterior predictive density overlay: observed outcome density against replicated draws")

    output$ppCheckStat <- renderPlot({
        req(values$model)
        tryCatch({
            r <- pp_reps_stat()
            bayesplot::ppc_stat(r$y, r$yrep, stat = "sd") + .pp_theme
        }, error = function(e) ggplot() + annotate("text", x = 0, y = 0, label = "Stat check not available") + theme_void())
    }, alt = "Posterior predictive histogram of the replicated outcome SD with the observed SD marked")

    output$ppCheckGrouped <- renderPlot({
        req(values$model)
        tryCatch({
            # the model frame has piecewise terms, not phase: phase is matched from the processed rows the fit kept
            m <- values$model
            fd <- m$data
            pd <- values$processed_data %||% values$data
            shared <- intersect(names(fd), names(pd))
            keep <- if (length(shared) && "phase" %in% names(pd)) stats::complete.cases(pd[, shared, drop = FALSE])
            group <- if (!is.null(keep) && sum(keep) == nrow(fd)) as.character(pd$phase[keep])
                     else if ("level_AB" %in% names(fd)) ifelse(fd$level_AB > 0, "intervention", "baseline")
            if (is.null(group)) {
                ggplot() + annotate("text", x = 0, y = 0, label = "No phase variable") + theme_void()
            } else {
                r <- pp_reps_stat()
                bayesplot::ppc_stat_grouped(r$y, r$yrep, group = group[r$keep], stat = "sd") + .pp_theme
            }
        }, error = function(e) ggplot() + annotate("text", x = 0, y = 0, label = "Grouped check not available") + theme_void())
    }, alt = "Posterior predictive histograms of the outcome SD by phase with observed SDs marked")

    output$ppCheckByCase <- renderPlot({
        req(values$model)
        tryCatch({
            r <- pp_reps_stat()
            if ("case" %in% names(r$data)) {
                bayesplot::ppc_stat_grouped(r$y, r$yrep, group = as.character(r$data$case)[r$keep],
                                            stat = "sd", facet_args = list(ncol = .phone_ncol())) + .pp_theme
            } else {
                ggplot() + annotate("text", x = 0, y = 0, label = "No case variable") + theme_void()
            }
        }, error = function(e) ggplot() + annotate("text", x = 0, y = 0, label = "By-case check not available") + theme_void())
    }, height = function() .facet_px(length(unique((values$processed_data %||% values$data)$case)), ncol = .phone_ncol()),
    alt = "Posterior predictive histograms of the outcome SD by case with observed SDs marked")


    output$heterogeneityResults <- renderPrint({
        m <- values$meta_model %||% values$model
        req(m)
        tryCatch({
            cat("Heterogeneity Assessment\n")
            cat("\n")
            vc <- brms::VarCorr(m)
            # residual__ is the level-1 SD, not a grouping factor
            for (group in setdiff(names(vc), "residual__")) {
                tau <- vc[[group]]$sd[1, "Estimate"]
                cat(sprintf("Group: %s\n", group))
                cat(sprintf("  tau (SD): %.3f\n", tau))
                cat(sprintf("  tau-squared: %.3f\n", tau^2))
            }
            bv <- suppressMessages(bootstrap_variance_components(m, n_boot = 1000))
            cols <- if (!is.null(bv)) grep("^iccs\\.", colnames(bv$summary), value = TRUE) else character(0)
            if (length(cols) && !all(is.na(bv$summary["mean", cols]))) {
                for (cc in cols) {
                    icc <- bv$summary["mean", cc]
                    cat(sprintf("\n  ICC (%s): %.3f (%.1f%% of total variance; posterior mean)\n",
                                sub("^iccs\\.", "", cc), icc, icc * 100))
                }
                for (nt in .icc_note(m)) cat("\n  Note:", nt, "\n")
            } else {
                cat("\n  ICC not reported: this family has no single residual SD on the response scale.\n")
            }
        }, error = function(e) cat("Heterogeneity results not available:", e$message))
    })

    # Results > Sensitivity: refits run in a worker so the session stays responsive
    sensitivity_runs <- reactiveValues(scd = NULL, meta = NULL)
    # a result from a superseded launch is dropped
    sensitivity_ids <- new.env(parent = emptyenv())

    .launch_sensitivity <- function(slot, model, priors, types) {
        run_id <- (sensitivity_ids[[slot]] %||% 0) + 1
        sensitivity_ids[[slot]] <- run_id
        types <- intersect(types %||% character(0), c("prior", "outlier"))
        if (is.null(model) || length(types) == 0) {
            sensitivity_runs[[slot]] <- NULL
            return(invisible(NULL))
        }
        set_run <- function(value) if (identical(sensitivity_ids[[slot]], run_id)) sensitivity_runs[[slot]] <- value
        # refitting the brmsfit keeps its sigma submodel and distributional priors
        args <- list(data = model$data,
                     base_model_formula = paste(deparse(stats::formula(model)$formula), collapse = " "),
                     vary_priors = "prior" %in% types, vary_outliers = "outlier" %in% types,
                     family = stats::family(model)$family,
                     priors = priors, model = model)
        sensitivity_runs[[slot]] <- list(status = "running")
        tryCatch(
            promises::future_promise(seed = TRUE, scdbayes:::.drop_sampler_files(.session_dir, do.call(run_sensitivity_analysis, args))) %...>%
                (function(res) set_run(list(status = "done", arms = res))) %...!%
                (function(e) set_run(list(status = "failed", message = .short_error(e)))),
            error = function(e) set_run(list(status = "failed", message = .short_error(e))))
        invisible(NULL)
    }

    observeEvent(list(values$model, input$run_sensitivity, input$sensitivity_types), {
        req(isTRUE(input$run_sensitivity))
        .launch_sensitivity("scd", values$model, values$fit_brms_priors, input$sensitivity_types)
    }, ignoreInit = TRUE)

    observeEvent(list(values$meta_model, input$meta_run_sensitivity, input$meta_sensitivity_types), {
        req(isTRUE(input$meta_run_sensitivity))
        .launch_sensitivity("meta", values$meta_model, values$meta_fit_brms_priors, input$meta_sensitivity_types)
    }, ignoreInit = TRUE)

    .print_sensitivity <- function(run, model) {
        if (is.null(run)) {
            cat("Tick Prior and/or Student-t to refit the model.\n\nCurrent model fixed effects:\n")
            tryCatch(print(brms::fixef(model)), error = function(e) cat("Not available\n"))
            return(invisible(NULL))
        }
        if (identical(run$status, "running")) return(cat("Sensitivity refits are running...\n"))
        if (identical(run$status, "failed")) return(cat("Sensitivity analysis failed:", run$message, "\n"))
        if (length(run$arms) == 0) return(cat("No sensitivity results generated.\n"))
        labels <- c(prior_default = "Default treatment prior N(0, s)",
                    prior_narrow = "Narrow treatment prior N(0, s/2)",
                    prior_diffuse = "Diffuse treatment prior N(0, 2s)",
                    prior_sd_small = "Smaller random-effect SD prior t3(0, s_y / sqrt(10))",
                    student_t = "Student-t likelihood")
        cat("Sensitivity Analysis Results (s = weakly informative treatment-prior SD)\n\n")
        invisible(lapply(names(run$arms), function(nm) {
            cat(sprintf("-- %s --\n", if (nm %in% names(labels)) labels[[nm]] else gsub("_", " ", nm)))
            tryCatch(print(round(run$arms[[nm]], 3)), error = function(e) cat("  Not available\n"))
            cat("\n")
        }))
    }

    output$sensitivityResults <- renderPrint({
        req(values$model)
        if (!isTRUE(input$run_sensitivity)) {
            cat("Enable 'Sensitivity analyses' in the Model Comparison sidebar to generate results.\n\n")
            cat("Current model fixed effects:\n")
            tryCatch(print(brms::fixef(values$model)), error = function(e) cat("Not available\n"))
            return(invisible())
        }
        .print_sensitivity(sensitivity_runs$scd, values$model)
    })

    output$sensitivityPlots <- plotly::renderPlotly({
        if (!isTRUE(input$run_sensitivity)) {
            return(empty_plot("Enable sensitivity analyses in the Model Comparison sidebar"))
        }
        req(values$model)
        if (!"prior" %in% (input$sensitivity_types %||% character(0))) {
            return(empty_plot("Tick the Prior box to overlay the default / narrow / diffuse posteriors"))
        }
        .prior_triplet_plot(sensitivity_runs$scd)
    })

    # Meta-Analysis > Decisions > Sensitivity (mirrors the single-case sensitivity on meta_ inputs)
    output$meta_sensitivityResults <- renderPrint({
        req(values$meta_model)
        if (!isTRUE(input$meta_run_sensitivity)) {
            cat("Enable 'Sensitivity analyses' in the sidebar to generate results.\n\n")
            cat("Current model fixed effects:\n")
            tryCatch(print(brms::fixef(values$meta_model)), error = function(e) cat("Not available\n"))
            return(invisible())
        }
        .print_sensitivity(sensitivity_runs$meta, values$meta_model)
    })

    output$meta_sensitivityPlots <- plotly::renderPlotly({
        if (!isTRUE(input$meta_run_sensitivity)) {
            return(empty_plot("Enable sensitivity analyses in the sidebar"))
        }
        req(values$meta_model)
        if (!"prior" %in% (input$meta_sensitivity_types %||% character(0))) {
            return(empty_plot("Tick the Prior box to overlay the default / narrow / diffuse posteriors"))
        }
        .prior_triplet_plot(sensitivity_runs$meta)
    })

    # plots the prior arms of the sensitivity run itself, so the plot and the table share fits and scale
    .prior_triplet_plot <- function(run) {
        if (is.null(run) || identical(run$status, "running")) return(empty_plot("Sensitivity refits are running..."))
        if (identical(run$status, "failed")) return(empty_plot(paste("Sensitivity analysis failed:", run$message)))
        arms <- run$arms[intersect(c("prior_default", "prior_narrow", "prior_diffuse"), names(run$arms))]
        if (length(arms) == 0) return(empty_plot("No prior-arm refits available"))
        term <- attr(run$arms, "treatment_coef") %||% .pick_treatment_term(rownames(arms[[1]]))
        s <- attr(run$arms, "treatment_sd")
        if (is.null(term) || !all(vapply(arms, function(a) term %in% rownames(a), logical(1)))) {
            return(empty_plot("No treatment coefficient in the refits"))
        }
        mult <- c(prior_default = 1, prior_narrow = 0.5, prior_diffuse = 2)[names(arms)]
        label <- c(prior_default = "Default", prior_narrow = "Narrow", prior_diffuse = "Diffuse")[names(arms)]
        df <- data.frame(
            prior = if (is.null(s)) label else sprintf("%s N(0, %.3f)", label, mult * s),
            estimate = vapply(arms, function(a) a[term, "Estimate"], numeric(1)),
            lower = vapply(arms, function(a) a[term, "Q2.5"], numeric(1)),
            upper = vapply(arms, function(a) a[term, "Q97.5"], numeric(1)))
        df$prior <- factor(df$prior, levels = df$prior)
        p <- ggplot(df, aes(x = estimate, y = prior, color = prior)) +
            geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
            geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, orientation = "y") +
            geom_point(size = 3) +
            labs(x = sprintf("Posterior mean and 95%% CrI of %s", term), y = "Treatment prior") +
            accessible_plot_theme() + theme(legend.position = "none")
        styled_ggplotly(p)
    }

    # Visual Analysis tab: piecewise term checkboxes by design
    output$piecewiseTermsUI <- renderUI({
        req(values$processed_data %||% values$data)
        md <- values$processed_data %||% values$data
        # the default selection equals the package default formula
        choices <- .piecewise_choices(md, .design_choice(input$research_design, md))
        selected <- intersect(.default_fixed_terms(md), choices)
        if (length(choices) == 0) return(NULL)

        vr <- values$validation_result
        has_trend <- !is.null(vr) && !is.null(vr$trend_cases) && length(vr$trend_cases) > 0
        clr <- if (has_trend) "#b45309" else "#337ab7"
        ico <- if (has_trend) "triangle-exclamation" else "info-circle"
        trend_icon <- actionLink("showPiecewiseGuide", icon(ico),
                                 style = paste0("color: ", clr, "; font-size: 13px;"),
                                 title = "Piecewise coding guide", `aria-label` = "Piecewise coding guide")

        tagList(
            div(style = "display: flex; align-items: center; gap: 6px;",
                tags$label("Piecewise terms", class = "control-label"),
                trend_icon),
            checkboxGroupInput("piecewise_terms", tags$span(class = "sr-only", "Piecewise terms"),
                               choices = choices, selected = selected)
        )
    })

    # changing-criterion: treatment = sum(level_crit*), so keep only the per-criterion terms
    observeEvent(input$piecewise_terms, {
        md <- values$processed_data %||% values$data
        if (is.null(md)) return()
        if (.design_choice(input$research_design, md) != "changing_criterion") return()
        sel <- input$piecewise_terms
        collapsed <- intersect(sel, c("treatment", "time_treatment"))
        per_crit  <- grep("^(level|trend)_(crit|ret)[0-9]+$", sel, value = TRUE)
        if (length(collapsed) > 0 && length(per_crit) > 0) {
            updateCheckboxGroupInput(session, "piecewise_terms", selected = per_crit)
            override_msg <- sprintf(
                "Collapsed and per-criterion terms are collinear; unselected %s and kept the per-criterion terms.",
                paste(collapsed, collapse = ", "))
            showNotification(override_msg, type = "warning", duration = 8)
            .announce(override_msg)
        }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    observeEvent(input$showPiecewiseGuide, {
        cases <- if (!is.null(values$validation_result)) values$validation_result$trend_cases else character()
        flag_html <- if (length(cases) > 0) {
            paste0("<p style='color: #b45309; margin-top: 12px;'><strong>",
                   icon("triangle-exclamation"),
                   " Baseline trend (p < .05) detected:</strong> ",
                   htmltools::htmlEscape(paste(cases, collapse = ", ")),
                   ". Consider including a time predictor.</p>")
        } else ""

        showModal(info_popup_modal(
            title = "Piecewise Regression Terms",
            body = HTML(paste0(
                "<p>Piecewise terms split each series into a baseline trend plus level and trend changes. ",
                "Reversal and changing-criterion designs add a step per phase; alternating designs use one dummy per condition.</p>",
                "<ul><li><strong>Baseline trend</strong>: slope across sessions</li>",
                "<li><strong>Level change</strong>: jump at a phase change</li>",
                "<li><strong>Trend change</strong>: slope change after it</li></ul>",
                flag_html)),
            citation = "Pustejovsky, J. E., Hedges, L. V., &amp; Shadish, W. R. (2014). Design-comparable effect sizes in multiple baseline designs: A general modeling framework. <em>Journal of Educational and Behavioral Statistics, 39</em>(5), 368&ndash;393. <a href=\"https://doi.org/10.3102/1076998614547577\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.3102/1076998614547577</a>"
        ))
    })

    # Model Comparison
    observeEvent(input$runModelComparison, {
        if (is.null(values$model)) {
            showNotification("No brms model available. Fit a Bayesian model first.", type = "warning", duration = 5)
            return()
        }
        model_data <- values$processed_data %||% values$data
        if (!.size_gate(model_data, "comparison")) return()
        showNotification("Running model comparison (fits multiple models - may take 5-15 min)...", type = "message", duration = 15)
        outcome_var <- input$outcome_var %||% "outcome"
        if (!outcome_var %in% names(model_data) || !identical(make.names(outcome_var), outcome_var)) {
            showNotification("Select an outcome column of the loaded data before comparing models.",
                             type = "error", duration = 8)
            return()
        }
        phase_var <- find_phase_var(model_data)
        case_var <- find_case_var(model_data)
        time_var <- find_time_var(model_data)
        has_autocor <- "autocor_A" %in% names(model_data)

        if (is.null(phase_var) || is.null(case_var)) {
            showNotification("Need phase and case variables for comparison", type = "error")
            return()
        }

        formulas <- build_comparison_models(outcome_var, phase_var, case_var, time_var, has_autocor)
        family_val <- input$model_family %||% "gaussian"
        existing_model <- values$model
        cmp_dir <- .session_dir
        fut <- promises::future_promise(scdbayes:::.drop_sampler_files(.session_dir, {
            dir.create(cmp_dir, showWarnings = FALSE, recursive = TRUE)
            local({
                old <- options(scdbayes.cache_dir = cmp_dir)
                on.exit(options(old))
                run_model_comparison(model_data, formulas, family = family_val,
                                     existing_model = existing_model)
            })
        }), seed = TRUE, globals = c(".session_dir", "run_model_comparison", "model_data", "formulas",
            "family_val", "existing_model", "brms_threads", "brms_backend",
            "brms_stanc_options", "brms_cache_dir", "cmp_dir"))
        promises::then(fut,
            onFulfilled = function(res) {
                values$model_comparison <- res
                if (!is.null(res)) showNotification("Model comparison complete. See Per Case > Model Comparison.", type = "message")
                else showNotification("Model comparison returned no results.", type = "warning")
            },
            onRejected = function(e) {
                showNotification(paste("Comparison failed:", e$message), type = "error")
            }
        )
    })

    output$modelComparisonResults <- renderPrint({
        req(values$model_comparison)
        cat("LOO Model Comparison: see table below\n")
    })

    output$modelComparisonTable <- renderDT({
        req(values$model_comparison)
        comp <- as.data.frame(values$model_comparison$comparison)
        # newer loo returns names in a model column and drops row names
        comp$Model <- if ("model" %in% names(comp)) as.character(comp$model) else rownames(comp)
        comp <- comp[, c("Model", setdiff(names(comp), c("Model", "model")))]
        comp[sapply(comp, is.numeric)] <- lapply(comp[sapply(comp, is.numeric)], round, 2)
        dt_table(comp, pageLength = 10, digits = 2, caption = "Model comparison by LOO")
    })

    # Sequential Updating
    observeEvent(input$runSequentialUpdate, {
        if (is.null(values$model)) {
            showNotification("No brms model available. Fit a Bayesian model first.", type = "warning", duration = 5)
            return()
        }
        # refit on the data the model was fitted to, so rate offsets and transformed outcomes carry over
        seq_data <- values$model$data %||% values$processed_data %||% values$data
        case_var <- find_case_var(seq_data) %||% "case"
        # default priors are rebuilt on the first cases; only priors the user chose carry over from the fit
        seq_priors <- if (identical(input$prior_type %||% "default", "default")) NULL else values$model$prior
        if (!.size_gate(seq_data, "sequential")) return()
        showNotification("Running sequential updating (refits per case - may take 5-15 min)...", type = "message", duration = 15)
        formula_str <- values$formula_display %||% "outcome ~ phase + (1|case)"
        family_val <- input$model_family %||% "gaussian"
        fut <- promises::future_promise(scdbayes:::.drop_sampler_files(.session_dir, {
            tryCatch(
                run_sequential_update(seq_data, case_var = case_var, formula_str = formula_str,
                                      family = family_val, priors = seq_priors),
                error = function(e) list(error = e$message)
            )
        }), seed = TRUE, globals = c(".session_dir", "run_sequential_update", "seq_data", "case_var", "seq_priors", "formula_str",
            "family_val", "brms_threads", "brms_backend", "brms_stanc_options",
            "brms_cache_dir", "%||%"))
        promises::then(fut,
            onFulfilled = function(res) {
                if (!is.null(res$error)) {
                    showNotification(paste("Sequential update error:", res$error), type = "error", duration = 15)
                } else if (!is.null(res) && !is.null(res$tracking)) {
                    values$sequential_results <- res
                    showNotification("Sequential updating complete! See Per Case > Model Comparison.", type = "message")
                } else {
                    showNotification("Sequential update returned no results.", type = "warning")
                }
            },
            onRejected = function(e) {
                showNotification(paste("Sequential update failed:", e$message), type = "error", duration = 15)
            }
        )
    })

    output$sequentialUpdatePlot <- plotly::renderPlotly({
        req(values$sequential_results)
        df <- values$sequential_results$tracking
        p <- ggplot(df, aes(x = n_cases, y = estimate)) +
            geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#2E5090") +
            geom_line(color = "#2E5090", linewidth = 1) +
            geom_point(color = "#2E5090", size = 3) +
            geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
            .plot_theme +
            labs(x = "Cumulative Cases", y = "Phase Effect Estimate (95% CrI)")
        styled_ggplotly(p)
    })

    output$sequentialUpdateTable <- renderDT({
        req(values$sequential_results)
        df <- values$sequential_results$tracking
        df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, 3) else x)
        dt_table(df, pageLength = 20, digits = 3, caption = "Sequential Bayesian updating")
    })

    # Posterior variance components / ICC
    observeEvent(input$runBootstrap, {
        if (is.null(values$model)) {
            showNotification("No brms model available. Fit a Bayesian model first.", type = "warning", duration = 5)
            return()
        }
        if (!.size_gate(values$processed_data %||% values$data, "bootstrap")) return()
        showNotification("Summarizing posterior variance components / ICC...", type = "message", duration = 8)
        boot_model <- values$model
        boot_n <- input$boot_n %||% 200
        fut <- promises::future_promise(scdbayes:::.drop_sampler_files(.session_dir, {
            bootstrap_variance_components(boot_model, n_boot = boot_n)
        }), seed = TRUE, globals = c(".session_dir", "bootstrap_variance_components", "boot_model", "boot_n"))
        promises::then(fut,
            onFulfilled = function(res) {
                values$bootstrap_results <- res
                if (!is.null(res)) showNotification("Variance components ready. See Per Case > Model Comparison.", type = "message")
                else showNotification("Variance components returned no results.", type = "warning")
            },
            onRejected = function(e) {
                showNotification(paste("Variance components failed:", e$message), type = "error")
            }
        )
    })

    output$bootstrapResults <- renderPrint({
        req(values$bootstrap_results)
        cat("Posterior Variance Components / ICC\n\n")
        cat(sprintf("Posterior draws used: %d / %d\n\n",
            values$bootstrap_results$n_successful, values$bootstrap_results$n_attempted))

        summ <- values$bootstrap_results$summary
        for (param in colnames(summ)) {
            cat(sprintf("%s:\n", param))
            cat(sprintf("  Mean: %.4f\n", summ["mean", param]))
            cat(sprintf("  SD: %.4f\n", summ["sd", param]))
            cat(sprintf("  95%% CI: [%.4f, %.4f]\n\n", summ["ci_lower.2.5%", param], summ["ci_upper.97.5%", param]))
        }
    })

    # Export > Download Handlers
    # fit-dependent downloads stay disabled until a fit exists
    observe({
        fitted <- isTRUE(values$model_fitted) && !is.null(values$model)
        for (id in c("downloadModel", "downloadReport", "downloadEffects", "downloadCode"))
            shinyjs::toggleState(id, condition = fitted)
        # disabled links keep aria-disabled and tabindex unless reset here
        shinyjs::runjs(paste0("$('#downloadModel, #downloadReport, #downloadEffects, #downloadCode')",
                              if (fitted) ".removeAttr('aria-disabled tabindex').removeClass('shinyjs-disabled');"
                              else ".attr({'aria-disabled': 'true', tabindex: '-1'}).addClass('shinyjs-disabled');"))
    })

    output$downloadModel <- downloadHandler(
        filename = function() "scdbayes_model.rds",
        content = function(file) {
            req(values$model)
            saveRDS(values$model, file)
        }
    )

    output$downloadData <- downloadHandler(
        filename = function() "scdbayes_data.csv",
        content = function(file) {
            if (is.null(values$data)) return(.download_unavailable(file, "Load data before downloading it."))
            write.csv(values$data, file, row.names = FALSE)
        }
    )

    # AI adoption audit trail — reproducibility record for a manuscript supplement
    output$downloadAiAudit <- downloadHandler(
        filename = function() paste0("scdbayes-ai-audit-",
            format(Sys.time(), "%Y%m%d-%H%M%S"), ".json"),
        content = function(file) {
            log <- ai_audit()
            payload <- list(
                tool = "scdbayes Ask AI / prior co-design",
                exported = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                package_version = as.character(utils::packageVersion("scdbayes")),
                note = paste("Each event records the package version, provider, model, local/cloud mode,",
                             "and under 'request' the temperature, JSON schema name and SHA-256, and the",
                             "SHA-256 of the system instruction text."),
                n_events = length(log),
                events = log)
            jsonlite::write_json(payload, file, auto_unbox = TRUE,
                                 pretty = TRUE, null = "null", na = "null")
        }
    )

    output$downloadEffects <- downloadHandler(
        filename = function() "scdbayes_decisions.csv",
        content = function(file) {
            req(values$model)
            es <- tryCatch(
                summarize_fixed_effect_decisions(values$model, threshold = .mcid(),
                    rope = .default_rope(values$model),
                    intervention_goal = input$intervention_goal %||% "increase"),
                error = function(e) NULL
            )
            if (!is.null(es) && nrow(es) > 0) {
                write.csv(es, file, row.names = FALSE)
            } else {
                write.csv(data.frame(Message = "No posterior decisions available"), file, row.names = FALSE)
            }
        }
    )

    # same screen as the package baseline-trend check: OLS slope p < .05 on >= 4 baseline points
    .baseline_trend_flag <- function(y, t) {
        ok <- is.finite(y) & is.finite(t)
        if (sum(ok) < 4 || length(unique(y[ok])) < 2) return(FALSE)
        p <- tryCatch(summary(stats::lm(y[ok] ~ t[ok]))$coefficients[2, 4], error = function(e) NA_real_)
        isTRUE(p < 0.05)
    }

    # every baseline run directly followed by an intervention run is one A_k -> B_k contrast
    .ab_pairs <- function(labels) {
        role <- phase_role(labels)
        n <- length(labels)
        pairs <- which(role[-n] %in% 0 & role[-1] %in% 1)
        if (length(pairs)) return(lapply(pairs, function(b) c(b, b + 1)))
        b <- c(which(role %in% 0), 1)[1]
        i <- c(which(role %in% 1 & seq_along(role) > b), b + 1)[1]
        if (i > n) list() else list(c(b, i))
    }

    .es_trend_caveat <- paste(
        "LRR, NAP, and Tau assume no time trend.",
        "Rows with baseline_trend_flag = TRUE may misstate the change.")

    # first-stage, unpooled per-contrast effect sizes per series; LRR1 omits and LRR2 applies the small-sample bias correction
    .first_stage_es <- function(d, goal, scale, measure_var = NULL, moderators = character(0)) {
        req_cols <- c("study", "case", "phase", "outcome")
        if (!all(req_cols %in% names(d))) return(NULL)
        measure_var <- intersect(measure_var, setdiff(names(d), req_cols))[1]
        measure_var <- measure_var[!is.na(measure_var)]
        nesting <- .bcsmd_nesting(d, measure_var)
        series_var <- nesting$series
        cluster_var <- nesting$cluster
        # a case ID repeated across clusters or series names separate tiers
        tiers <- intersect(unique(c("study", tryCatch(.tier_cols(d), error = function(e) NULL),
                                    "case", series_var, measure_var)), names(d))
        moderators <- intersect(moderators, setdiff(names(d), c(req_cols, "time", tiers)))
        if ("time" %in% names(d)) d <- d[order(d$study, d$case, d$time), ]
        rows <- lapply(split(d, lapply(tiers, function(v) d[[v]]), drop = TRUE), function(cd) {
            goal <- .bcsmd_goal(cd, goal)
            # SingleCaseES LRRd is negative for a decrease, so its sign is reversed to make positive = improvement
            lrr <- if (goal == "decrease") "LRRd" else "LRRi"
            flip <- if (goal == "decrease") -1 else 1
            runs <- rle(as.character(cd$phase))
            run_id <- rep(seq_along(runs$values), runs$lengths)
            t <- if ("time" %in% names(cd)) as.numeric(cd$time) else seq_len(nrow(cd))
            pairs <- .ab_pairs(runs$values)
            run_label <- function(i) {
                v <- runs$values[i]
                repeated <- sum(runs$values == v) > 1 && !grepl("[0-9]$", v)
                if (repeated) paste0(v, sum(runs$values[seq_len(i)] == v)) else v
            }
            do.call(rbind, lapply(seq_along(pairs), function(k) {
                p <- pairs[[k]]
                keep <- run_id %in% p
                es_args <- list(condition = ifelse(run_id[keep] == p[1], "A", "B"), outcome = cd$outcome[keep],
                                baseline_phase = "A", intervention_phase = "B",
                                improvement = goal, scale = scale, format = "long")
                calc <- function(es, bc) tryCatch(suppressWarnings(do.call(SingleCaseES::calc_ES,
                    c(es_args, list(ES = es, bias_correct = bc)))), error = function(e) NULL)
                # LRR needs a ratio scale with a true zero (Pustejovsky, 2018): only for
                # count or proportion outcomes with no negative values
                lrr_ok <- scale %in% c("count", "proportion", "percentage", "rate") &&
                    all(cd$outcome[keep] >= 0, na.rm = TRUE)
                es1 <- calc(c(if (lrr_ok) lrr, "NAP", "Tau"), FALSE)
                es2 <- if (lrr_ok) calc(lrr, TRUE)
                es <- rbind(es1, es2)
                if (is.null(es) || nrow(es) == 0) return(NULL)
                es$ES <- c(ifelse(es1$ES == lrr, "LRR1", as.character(es1$ES)), rep("LRR2", NROW(es2)))
                is_lrr <- es$ES %in% c("LRR1", "LRR2")
                s <- ifelse(is_lrr, flip, 1)
                lower <- ifelse(s < 0, -es$CI_upper, es$CI_lower)
                upper <- ifelse(s < 0, -es$CI_lower, es$CI_upper)
                direction <- ifelse(is_lrr & flip < 0, paste("positive = improvement (decrease);", lrr, "sign reversed"),
                                    paste0("positive = improvement (", goal, ")"))
                out <- data.frame(study_id = as.character(cd$study[1]), case = as.character(cd$case[1]),
                                  contrast_id = k, contrast = paste(run_label(p[1]), "vs", run_label(p[2])),
                                  ES = es$ES, improvement = goal, direction = direction,
                                  yi = s * es$Est, sei = es$SE, vi = es$SE^2,
                                  ci_lower = lower, ci_upper = upper,
                                  baseline_trend_flag = .baseline_trend_flag(cd$outcome[run_id == p[1]], t[run_id == p[1]]),
                                  stringsAsFactors = FALSE)
                out$cluster <- if (length(cluster_var)) as.character(cd[[cluster_var]][1])
                out$series <- if (length(series_var)) as.character(cd[[series_var]][1])
                out$measure <- if (length(measure_var)) as.character(cd[[measure_var]][1])
                out[moderators] <- lapply(cd[keep, moderators, drop = FALSE], function(v) v[!is.na(v)][1])
                out
            }))
        })
        res <- do.call(rbind, rows)
        if (is.null(res)) return(NULL)
        # a zero mean (LRR without a truncation constant) or a one-point phase (NAP/Tau SE)
        # gives an infinite or undefined estimate or variance; such rows cannot be pooled
        bad <- !is.finite(res$yi) | !is.finite(res$vi)
        n_bad <- sum(bad)
        res <- res[!bad, , drop = FALSE]
        front <- intersect(c("study_id", "cluster", "case", "series", "measure", "contrast_id", "contrast"), names(res))
        rownames(res) <- NULL
        res <- res[, c(front, setdiff(names(res), front))]
        attr(res, "n_dropped") <- n_bad
        res
    }

    .meta_es_table <- function() {
        d <- values$data
        if (is.null(d)) return(NULL)
        lrr_scale <- switch(input$meta_outcome_type %||% "continuous",
                            count = "count", proportion = "proportion", "other")
        .first_stage_es(d, .meta_goal(), lrr_scale,
                        measure_var = input$meta_measure_var %||% "",
                        moderators = input$meta_moderator_vars %||% character(0))
    }

    output$downloadMetaES <- downloadHandler(
        filename = function() "scdbayes_meta_es.csv",
        content = function(file) {
            d <- values$data
            if (is.null(d) || !all(c("study", "case", "phase", "outcome") %in% names(d))) {
                return(.download_unavailable(file, "Load a multi-study dataset with study, case, phase, and outcome columns."))
            }
            es_df <- .meta_es_table()
            if (is.null(es_df) || nrow(es_df) == 0) {
                return(.download_unavailable(file, "No first-stage effect sizes could be computed."))
            }
            if (any(es_df$baseline_trend_flag)) {
                showNotification(.es_trend_caveat, type = "warning", duration = 12)
            }
            if (isTRUE(attr(es_df, "n_dropped") > 0)) {
                showNotification(sprintf(paste("%d effect size row(s) left out: the estimate or its variance was",
                                               "not finite (a zero phase mean for LRR, or a one-point phase)."),
                                         attr(es_df, "n_dropped")), type = "warning", duration = 12)
            }
            if (!(input$meta_outcome_type %||% "continuous") %in% c("count", "proportion")) {
                showNotification("LRR is exported only for count or proportion outcomes (ratio scale); this file has NAP and Tau.",
                                 type = "message", duration = 10)
            }
            write.csv(es_df, file, row.names = FALSE)
        }
    )

    # cluster (CMB) and within-case series (RMBB) columns, when the data carry them
    .bcsmd_nesting <- function(d, measure_var = character(0)) {
        cl <- find_cluster_var(d)
        cl <- cl[length(cl) && length(unique(d[[cl]])) > 1]
        nest <- setdiff(tryCatch(tier_vars(d), error = function(e) character(0)),
                        c("study", cl, "case", measure_var))[1]
        list(cluster = cl, series = nest[!is.na(nest)])
    }

    # long data with a baseline/treatment indicator in the layout scdhlm reads
    .scdhlm_data <- function(d, measure_var = NULL) {
        if (is.null(d) || !all(c("case", "phase", "outcome") %in% names(d))) return(NULL)
        has_study <- "study" %in% names(d)
        measure_var <- intersect(measure_var, setdiff(names(d), c("case", "phase", "outcome", "study")))[1]
        measure_var <- measure_var[!is.na(measure_var)]
        nesting <- .bcsmd_nesting(d, measure_var)
        key <- paste(if (has_study) d$study else "",
                     if (length(nesting$cluster)) d[[nesting$cluster]] else "", d$case,
                     if (length(nesting$series)) d[[nesting$series]] else "",
                     if (length(measure_var)) d[[measure_var]] else "", sep = "\r")
        d$session <- if ("time" %in% names(d)) as.numeric(d$time) else stats::ave(seq_len(nrow(d)), key, FUN = seq_along)
        ord <- order(key, d$session)
        d <- d[ord, , drop = FALSE]
        key <- key[ord]
        coded <- do.call(rbind, lapply(split(as.character(d$phase), factor(key, levels = unique(key))), function(p) {
            runs <- rle(p)
            role <- phase_role(runs$values)
            role <- ifelse(is.na(role), as.integer(seq_along(role) > 1), role)
            data.frame(phase_pair = rep(pmax(1L, cumsum(role == 0)), runs$lengths),
                       treatment = rep(ifelse(role >= 1, "treatment", "baseline"), runs$lengths),
                       ab_phase = rep(role <= 1, runs$lengths),
                       stringsAsFactors = FALSE)
        }))
        out <- data.frame(case = as.character(d$case), session = d$session,
                          phase = as.character(d$phase), phase_pair = coded$phase_pair,
                          treatment = coded$treatment, outcome = d$outcome, stringsAsFactors = FALSE)
        out$measure <- if (length(measure_var)) as.character(d[[measure_var]])
        out$study <- if (has_study) as.character(d$study)
        out$cluster <- if (length(nesting$cluster)) as.character(d[[nesting$cluster]])
        out$series <- if (length(nesting$series)) as.character(d[[nesting$series]])
        # maintenance and generalization runs (roles 2 and 3) are outside the BC-SMD baseline/treatment contrast
        out <- out[coded$ab_phase, , drop = FALSE]
        front <- intersect(c("study", "cluster", "case", "series", "measure"), names(out))
        out[, c(front, setdiff(names(out), front))]
    }

    # per study and measure, A = last session of the shortest baseline; B follows ab_rule as in bc_smd_bayes()
    # scdhlm: A + first-treatment span of the series with that shortest baseline; shortest_treatment: A + shortest
    # first-treatment run across series; a benchmark duration sets B = A + duration
    .scdhlm_times <- function(sd, duration = NULL, ab_rule = "scdhlm") {
        study <- if ("study" %in% names(sd)) sd$study else rep("all", nrow(sd))
        measure <- if ("measure" %in% names(sd)) sd$measure else rep("", nrow(sd))
        tier <- paste(study, if ("cluster" %in% names(sd)) sd$cluster else "", sd$case,
                      if ("series" %in% names(sd)) sd$series else "", measure, sep = "\r")
        per_tier <- do.call(rbind, lapply(split(sd, tier), function(cd) {
            first_trt <- which(cd$treatment == "treatment")[1]
            if (is.na(first_trt) || first_trt == 1) return(NULL)
            trt <- cd$session[cd$phase_pair == cd$phase_pair[first_trt] & cd$treatment == "treatment"]
            base_last <- cd$session[first_trt - 1]
            data.frame(study = if ("study" %in% names(cd)) cd$study[1] else "all",
                       measure = if ("measure" %in% names(cd)) cd$measure[1] else "",
                       base_last = base_last, trt_span = max(trt) - min(trt) + 1,
                       trt_follow = max(trt) - base_last, stringsAsFactors = FALSE)
        }))
        if (is.null(per_tier)) return(NULL)
        do.call(rbind, lapply(split(per_tier, paste(per_tier$study, per_tier$measure, sep = "\r"), drop = TRUE), function(s) {
            A <- min(s$base_last)
            rule_len <- if (identical(ab_rule, "shortest_treatment")) min(s$trt_follow) else min(s$trt_span[s$base_last == A])
            data.frame(study = s$study[1], measure = s$measure[1], A = A, B = A + (duration %||% rule_len),
                       stringsAsFactors = FALSE)
        }))
    }

    .ab_rule_text <- function(opts) {
        if (!is.null(opts$duration)) return(sprintf("benchmark duration, B = A + %s sessions", format(opts$duration)))
        if (identical(opts$ab_rule, "shortest_treatment")) return("shortest treatment, B = A + shortest treatment duration across series")
        "scdhlm default_times(), B = A + treatment duration of the series with the shortest baseline"
    }

    output$downloadScdhlmData <- downloadHandler(
        filename = function() "scdbayes_scdhlm_data.csv",
        content = function(file) {
            sd <- .scdhlm_data(values$data, input$meta_measure_var %||% "")
            if (is.null(sd) || nrow(sd) == 0) {
                return(.download_unavailable(file, "Load data with case, phase, and outcome columns."))
            }
            opts <- .bcsmd_opts()
            ab <- .scdhlm_times(sd, opts$duration, opts$ab_rule)
            ab_lines <- if (is.null(ab)) "# Suggested times unavailable: no tier has a baseline followed by treatment."
                        else sprintf("# %s%s: A = %s, B = %s", .md_cell(ab$study),
                                     ifelse(nzchar(ab$measure), paste0(" / ", .md_cell(ab$measure)), ""),
                                     format(ab$A), format(ab$B))
            writeLines(c(
                "# scdhlm-ready data exported by scdbayes. Read with read.csv(file, comment.char = '#').",
                "# treatment = baseline/treatment indicator; phase_pair = A_k/B_k pair index for treatment-reversal designs.",
                "# Suggested BC-SMD times: A = last session of the shortest baseline;",
                paste("# B rule:", .ab_rule_text(opts)),
                ab_lines,
                "# Optional frequentist comparison: scdhlm estimates the BC-SMD by REML with a small-sample",
                "# correction; scdbayes estimates it in a Bayesian model instead."), file)
            suppressWarnings(utils::write.table(sd, file, sep = ",", row.names = FALSE,
                                                col.names = TRUE, append = TRUE, qmethod = "double"))
        }
    )

    output$downloadMetaPosteriorES <- downloadHandler(
        filename = function() "scdbayes_posterior_case_effects.csv",
        content = function(file) {
            m <- values$meta_model %||% values$model
            if (is.null(m)) return(.download_unavailable(file, "Fit a model before downloading posterior case effects."))
            fit_data <- m$data
            case_var <- find_case_var(fit_data) %||% "case"
            study_var <- find_study_var(fit_data)
            per_case <- tryCatch(
                summarize_per_case_effects(m, case_var = case_var,
                                           ci_level = (input$meta_ci_level %||% 95) / 100,
                                           intervention_goal = .meta_goal()),
                error = function(e) NULL)
            if (is.null(per_case) || nrow(per_case) == 0) {
                return(.download_unavailable(file, "No posterior case effects available."))
            }
            es_df <- per_case
            es_df$case <- as.character(es_df$case)
            if (!is.null(study_var)) {
                uniq <- unique(fit_data[, c(study_var, case_var)])
                study_chr <- as.character(uniq[[study_var]])
                case_chr <- as.character(uniq[[case_var]])
                bare <- !case_chr %in% case_chr[duplicated(case_chr)]
                keys <- c(paste(study_chr, case_chr, sep = "_"), case_chr[bare])
                study_of <- stats::setNames(c(study_chr, study_chr[bare]), keys)
                case_of <- stats::setNames(c(case_chr, case_chr[bare]), keys)
                es_df$study_id <- unname(study_of[es_df$case])
                es_df$case <- unname(ifelse(is.na(case_of[es_df$case]), es_df$case, case_of[es_df$case]))
            }
            es_df$note <- "descriptive: shrunken posterior case effects; do not re-pool"
            front <- intersect(c("study_id", "case", "term"), names(es_df))
            write.csv(es_df[, c(front, setdiff(names(es_df), front))], file, row.names = FALSE)
        }
    )

    .meta_template_header <- function(title) {
        paste0(
            "# ", title, "\n",
            "# Input: scdbayes_meta_es.csv, unpooled first-stage effect sizes from SingleCaseES for every\n",
            "# A_k -> B_k contrast (contrast_id) of every series: LRR1 (no small-sample correction), LRR2\n",
            "# (corrected), NAP, and Tau, with sampling variances vi = SE^2. Every yi is oriented so that\n",
            "# positive = improvement (see the direction column; LRRd is sign-reversed for decrease goals).\n",
            "# Shrunken posterior case effects are not re-pooled: they are correlated and already\n",
            "# partially pooled. Within-case SMD is not computed.\n",
            "# Generated by scdbayes on ", Sys.Date(), "\n\n")
    }

    .template_trend_check <- function() {
        paste0("if (any(es$baseline_trend_flag)) message('", .es_trend_caveat, "')\n\n")
    }

    .template_mods <- function() {
        m <- intersect(input$meta_moderator_vars %||% character(0), names(values$data %||% data.frame()))
        if (length(m)) paste(m, collapse = " + ") else "moderator"
    }

    output$downloadMetaBrmsScript <- downloadHandler(
        filename = function() "scdbayes_brms_meta_template.R",
        content = function(file) {
            writeLines(paste0(
                .meta_template_header("Bayesian second-stage meta-analysis of first-stage single-case effect sizes"),
                "library(brms)\n\n",
                "es <- read.csv('scdbayes_meta_es.csv')\n",
                "es$case_id <- paste(es$study_id, es$case, sep = '/')\n",
                .template_trend_check(),
                "# LRR1: log ratio with known sampling SDs; several contrasts or outcomes per case add an effect-level SD\n",
                "lrr <- subset(es, ES == 'LRR1')\n",
                "multi <- anyDuplicated(lrr$case_id) > 0\n",
                "f_lrr <- if (multi) bf(yi | se(sei, sigma = TRUE) ~ 1 + (1 | study_id / case_id)) else\n",
                "         bf(yi | se(sei) ~ 1 + (1 | study_id / case_id))\n",
                "fit_lrr <- brm(f_lrr, data = lrr,\n",
                "               prior = c(prior(normal(0, 1), class = Intercept),\n",
                "                         prior(student_t(3, 0, 0.5), class = sd)),\n",
                "               chains = 4, iter = 4000, warmup = 2000,\n",
                "               control = list(adapt_delta = 0.95), seed = 1)\n",
                "summary(fit_lrr)\n",
                "# positive LRR = improvement for both increase and decrease goals\n",
                "hypothesis(fit_lrr, 'Intercept > 0')\n\n",
                "# Tau (= 2 * NAP - 1, 0 = no effect): unweighted, because sampling-variance weights\n",
                "# correlate with non-overlap estimates; supplementary to BC-SMD and LRR\n",
                "tau <- subset(es, ES == 'Tau')\n",
                "fit_tau <- brm(yi ~ 1 + (1 | study_id / case_id), data = tau,\n",
                "               prior = c(prior(normal(0, 0.5), class = Intercept),\n",
                "                         prior(student_t(3, 0, 0.5), class = sd),\n",
                "                         prior(student_t(3, 0, 0.5), class = sigma)),\n",
                "               chains = 4, iter = 4000, warmup = 2000,\n",
                "               control = list(adapt_delta = 0.95), seed = 1)\n",
                "summary(fit_tau)\n",
                "hypothesis(fit_tau, 'Intercept > 0')\n\n",
                "# Moderator model\n",
                "# fit_mod <- update(fit_lrr, formula. = ~ . + ", .template_mods(), ", newdata = lrr)\n"
            ), file)
        }
    )

    output$downloadMetaScript <- downloadHandler(
        filename = function() "scdbayes_metafor_comparison.R",
        content = function(file) {
            writeLines(paste0(
                .meta_template_header("Optional frequentist comparison: MLMA with RVE and OLS with RVE"),
                "library(metafor)\n",
                "library(clubSandwich)\n\n",
                "es <- read.csv('scdbayes_meta_es.csv')\n",
                "es$case_id <- paste(es$study_id, es$case, sep = '/')\n",
                "es$es_id <- seq_len(nrow(es))\n",
                "# several series (behaviors, settings) per case get distinct observation ids\n",
                "es$obs_id <- if ('series' %in% names(es)) paste(es$series, es$contrast_id) else es$contrast_id\n",
                .template_trend_check(),
                "# LRR1 with MLMA (cases within studies) and RVE clustered by study\n",
                "lrr <- subset(es, ES == 'LRR1')\n",
                "# Working correlation rho among contrasts and series (obs = obs_id) and outcomes (type = measure)\n",
                "# within a case; rho is unknown, so the estimate is checked across values\n",
                "mlma_rve <- function(dat, rho) {\n",
                "  # with outcomes, rho = c(within-outcome, between-outcome) correlation\n",
                "  V <- if ('measure' %in% names(dat))\n",
                "    vcalc(vi, cluster = case_id, obs = obs_id, type = measure, data = dat, rho = c(rho, rho))\n",
                "  else vcalc(vi, cluster = case_id, obs = obs_id, data = dat, rho = rho)\n",
                "  levels <- if (anyDuplicated(dat$case_id)) ~ 1 | study_id / case_id / es_id else ~ 1 | study_id / case_id\n",
                "  robust(rma.mv(yi, V = V, random = levels, data = dat), cluster = study_id, clubSandwich = TRUE)\n",
                "}\n",
                "rho_values <- c(0.2, 0.5, 0.8)\n",
                "fits <- lapply(rho_values, function(rho) mlma_rve(lrr, rho))\n",
                "data.frame(rho = rho_values, estimate = sapply(fits, function(f) f$beta[1]),\n",
                "           se = sapply(fits, function(f) f$se[1]),\n",
                "           ci_lower = sapply(fits, function(f) f$ci.lb[1]),\n",
                "           ci_upper = sapply(fits, function(f) f$ci.ub[1]))\n",
                "summary(fits[[2]])\n\n",
                "# OLS with RVE for NAP and Tau, and for LRR2\n",
                "ols_rve <- function(dat) {\n",
                "  fit <- lm(yi ~ 1, data = dat)\n",
                "  coef_test(fit, vcov = 'CR2', cluster = dat$study_id)\n",
                "}\n",
                "ols_rve(subset(es, ES == 'Tau'))\n",
                "ols_rve(subset(es, ES == 'NAP'))\n",
                "ols_rve(subset(es, ES == 'LRR2'))\n\n",
                "# Moderator analysis\n",
                "# mod <- rma.mv(yi, V = vi, mods = ~ ", .template_mods(), ",\n",
                "#               random = ~ 1 | study_id / case_id, data = lrr)\n",
                "# robust(mod, cluster = study_id, clubSandwich = TRUE)\n"
            ), file)
        }
    )

    output$downloadPubBiasScript <- downloadHandler(
        filename = function() "scdbayes_pub_bias.R",
        content = function(file) {
            writeLines(paste0(
                .meta_template_header("Optional frequentist comparison: publication bias analysis"),
                "library(metafor)\n",
                "library(clubSandwich)\n",
                "# library(PublicationBias)  # for sensitivity analysis\n\n",
                "es <- read.csv('scdbayes_meta_es.csv')\n",
                "es$case_id <- paste(es$study_id, es$case, sep = '/')\n",
                "es$es_id <- seq_len(nrow(es))\n",
                "es <- subset(es, ES == 'LRR1')\n",
                "levels <- if (anyDuplicated(es$case_id)) ~ 1 | study_id / case_id / es_id else ~ 1 | study_id / case_id\n\n",
                "# MLMA (study/case) with RVE clustered by study\n",
                "meta <- rma.mv(yi, V = vi, random = levels, data = es)\n",
                "robust(meta, cluster = study_id, clubSandwich = TRUE)\n\n",
                "# Egger-type regression test for small-study effects (sandwich SEs clustered by study)\n",
                "egger <- rma.mv(yi, V = vi, mods = ~ sei, random = levels, data = es)\n",
                "robust(egger, cluster = study_id, clubSandwich = TRUE)\n\n",
                "# Contour funnel plot\n",
                "funnel(meta, level = c(0.90, 0.95, 0.99),\n",
                "       shade = c('white', 'gray75', 'gray95'), refline = 0)\n\n",
                "# PET-PEESE (precision-effect test), sandwich SEs clustered by study\n",
                "pet <- robust(rma.mv(yi, V = vi, mods = ~ sei, random = levels, data = es),\n",
                "              cluster = study_id, clubSandwich = TRUE)\n",
                "peese <- robust(rma.mv(yi, V = vi, mods = ~ vi, random = levels, data = es),\n",
                "                cluster = study_id, clubSandwich = TRUE)\n",
                "# Use PET if the PET intercept is significant, otherwise PEESE\n\n",
                "# Sensitivity analysis (Mathur & VanderWeele)\n",
                "# sval <- svalue(yi = es$yi, vi = es$vi, q = 0, model_type = 'robust', clustervar = es$study_id)\n"
            ), file)
        }
    )

    # Two-stage Bayesian BC-SMD: design, participant count, and skip reason per study
    .bcsmd_plan <- function(raw) {
        dt <- tryCatch(detect_design_type(raw), error = function(e) "unknown")
        nesting <- .bcsmd_nesting(raw)
        mb <- dt %in% c("multiple_baseline", "ab")
        design <- if (identical(dt, "reversal")) "TR"
                  else if (mb && length(nesting$series)) "RMBB"
                  else if (mb && length(nesting$cluster)) "CMB"
                  else if (mb) "MBP"
                  else NA_character_
        # participants as bc_smd_bayes() counts them: series within a participant do not add cases
        n_cases <- nrow(unique(raw[c(nesting$cluster, "case")]))
        reason <- if (is.na(design)) sprintf("unsupported design (%s)", gsub("_", " ", dt))
                  else if (n_cases < 3) sprintf("fewer than 3 participants (%d); series within a participant count once", n_cases)
                  else NA_character_
        probe <- identical(input$meta_research_design, "multiple_probe") && mb
        model_design <- if (identical(design, "TR")) "reversal" else if (probe) "multiple_probe" else "multiple_baseline"
        list(design = model_design, scdhlm_design = design, detected = dt, n_cases = n_cases, reason = reason)
    }

    .bcsmd_groups <- function(d) {
        study <- if ("study" %in% names(d)) as.character(d$study) else rep("all", nrow(d))
        mv <- intersect(input$meta_measure_var %||% "", setdiff(names(d), c("study", "case", "phase", "outcome")))
        measure <- if (length(mv)) as.character(d[[mv]]) else rep(NA_character_, nrow(d))
        idx <- split(seq_len(nrow(d)), paste(study, measure, sep = "\r"), drop = TRUE)
        lapply(unname(idx), function(i) c(list(study_id = study[i[1]], measure = measure[i[1]],
                                               goal = .bcsmd_goal(d[i, , drop = FALSE]),
                                               raw = d[i, , drop = FALSE]),
                                          .bcsmd_plan(d[i, , drop = FALSE])))
    }

    # a study or measure codes its own direction in an 'improvement' column; otherwise the meta goal applies
    .bcsmd_goal <- function(raw, default = .meta_goal()) {
        g <- if ("improvement" %in% names(raw)) unique(tolower(trimws(stats::na.omit(as.character(raw$improvement)))))
        if (length(g) == 1 && g %in% c("increase", "decrease")) g else default
    }

    .bcsmd_args <- function(fn, g, opts) {
        .formal_args(fn, c(list(data = g$raw, design = g$design, intervention_goal = g$goal), opts))
    }

    # only arguments the installed function declares are passed
    .formal_args <- function(fn, args) {
        args[names(args) %in% names(formals(fn))]
    }

    .bcsmd_opts <- function() {
        num <- function(v) {
            v <- suppressWarnings(as.numeric(v))
            if (length(v) == 1 && is.finite(v)) v
        }
        list(duration = num(input$bcsmd_duration),
             ab_rule = if (isTRUE(input$bcsmd_ab_rule %in% c("scdhlm", "shortest_treatment"))) input$bcsmd_ab_rule else "scdhlm",
             trend = input$bcsmd_trend %||% "none", ar = TRUE,
             random_effect = !isTRUE(input$bcsmd_common_effect),
             iter = .mcmc_clamp(input$n_iter, 200, .llm_limit("SCDBAYES_MAX_ITER", 20000), 2000),
             chains = .mcmc_clamp(input$n_chains, 1, 8, 4))
    }

    .non_gaussian_outcome <- function() {
        isTRUE(input$meta_outcome_type %in% c("count", "binary", "proportion")) ||
            isTRUE(input$meta_model_family %in% c("poisson", "negbinomial", "zero_inflated_poisson",
                                                  "bernoulli", "binomial", "cumulative", "beta"))
    }

    bcsmd_state <- reactiveVal(NULL)
    bcsmd_pool <- reactiveVal(NULL)
    # a result from a superseded launch or a data reset is dropped
    bcsmd_ids <- new.env(parent = emptyenv())
    bcsmd_ids$run <- 0
    bcsmd_ids$pool <- 0

    .bcsmd_buttons <- function(enabled) {
        toggle <- if (enabled) shinyjs::enable else shinyjs::disable
        toggle("runBCSMD")
        toggle("runBCSMDPool")
    }

    .bcsmd_reset <- function() {
        bcsmd_ids$run <- bcsmd_ids$run + 1
        bcsmd_ids$pool <- bcsmd_ids$pool + 1
        bcsmd_state(NULL)
        bcsmd_pool(NULL)
        .bcsmd_buttons(TRUE)
    }

    observeEvent(values$data, .bcsmd_reset())

    observeEvent(input$runBCSMD, {
        d <- values$data
        req(d, all(c("case", "phase", "outcome") %in% names(d)))
        if (!.size_gate(d, "fit")) return()
        fn <- bc_smd_bayes
        groups <- .bcsmd_groups(d)
        runnable <- Filter(function(g) is.na(g$reason), groups)
        opts <- .bcsmd_opts()
        .bcsmd_reset()
        if (!length(runnable)) {
            bcsmd_state(list(status = "done", groups = groups, fits = list()))
            showNotification("No study can be estimated; see the reasons in the table.", type = "warning")
            return(invisible(NULL))
        }
        run_id <- bcsmd_ids$run
        current <- function() identical(bcsmd_ids$run, run_id)
        set_state <- function(value) if (current()) bcsmd_state(value)
        bcsmd_state(list(status = "running", groups = groups))
        .bcsmd_buttons(FALSE)
        note_id <- showNotification(sprintf("Estimating Bayesian BC-SMD for %d stud%s...", length(runnable),
                                            if (length(runnable) == 1) "y" else "ies"),
                                    type = "message", duration = NULL)
        calls <- lapply(runnable, function(g) .bcsmd_args(fn, g, opts))
        keys <- vapply(runnable, function(g) paste(g$study_id, g$measure), "")
        fut <- promises::future_promise(seed = TRUE, scdbayes:::.drop_sampler_files(.session_dir, {
            lapply(calls, function(a) tryCatch(do.call(fn, a), error = function(e) e))
        }))
        promises::finally(promises::then(fut,
            onFulfilled = function(res) {
                set_state(list(status = "done", groups = groups, fits = stats::setNames(res, keys)))
            },
            onRejected = function(e) {
                set_state(list(status = "failed", groups = groups, message = conditionMessage(e)))
            }),
            onFinally = function() {
                removeNotification(note_id)
                if (current()) .bcsmd_buttons(TRUE)
            })
        invisible(NULL)
    })

    .bcsmd_rows <- function() {
        st <- bcsmd_state()
        if (is.null(st) || is.null(st$groups)) return(NULL)
        do.call(rbind, lapply(st$groups, function(g) {
            fit <- st$fits[[paste(g$study_id, g$measure)]]
            ok <- is.list(fit) && !inherits(fit, "error") && is.data.frame(fit$summary) && nrow(fit$summary) > 0
            s <- if (ok) fit$summary[1, , drop = FALSE] else NULL
            reason <- if (!is.na(g$reason)) g$reason
                      else if (inherits(fit, "error")) paste("estimation failed:", .short_error(fit))
                      else if (identical(st$status, "running")) "running"
                      else if (!ok) "no estimate returned" else ""
            data.frame(study_id = g$study_id, measure = g$measure,
                       direction = paste0("positive = improvement (", g$goal %||% .meta_goal(), ")"),
                       design = if (is.na(g$scdhlm_design)) "" else g$design,
                       scdhlm_design = if (is.na(g$scdhlm_design)) "" else g$scdhlm_design,
                       detected = g$detected, n_cases = g$n_cases,
                       time_A = if (ok) fit$time_A %||% NA_real_ else NA_real_,
                       time_B = if (ok) fit$time_B %||% NA_real_ else NA_real_,
                       median = if (ok) s$estimate else NA_real_, sd = if (ok) s$sd else NA_real_,
                       lower = if (ok) s$lower else NA_real_, upper = if (ok) s$upper else NA_real_,
                       status = if (ok) "estimated" else "skipped", reason = reason,
                       stringsAsFactors = FALSE)
        }))
    }

    output$bcsmdOutcomeNote <- renderUI({
        if (!.non_gaussian_outcome()) return(NULL)
        div(class = "status-main-warning",
            "BC-SMD assumes approximately normal outcomes.",
            "For count, binary, proportion, or ordinal outcomes, use the one-stage GLMM (Meta-Analysis > Model).")
    })

    output$bcsmdTable <- DT::renderDataTable({
        st <- bcsmd_state()
        validate(need(!is.null(st), "Click Estimate BC-SMD to fit the per-study Bayesian models."))
        validate(need(!identical(st$status, "failed"), paste("BC-SMD estimation failed:", st$message)))
        df <- .bcsmd_rows()
        df <- df[, setdiff(names(df), if (all(is.na(df$measure))) "measure" else character(0)), drop = FALSE]
        names(df)[names(df) %in% c("median", "sd", "lower", "upper")] <-
            c("Posterior median", "Posterior SD", "2.5%", "97.5%")
        dt_table(df, digits = 3, caption = "Bayesian BC-SMD by study")
    })

    observeEvent(input$runBCSMDPool, {
        if (!.size_gate(req(values$data), "fit")) return()
        rows <- .bcsmd_rows()
        est <- if (!is.null(rows)) rows[rows$status == "estimated", , drop = FALSE]
        if (is.null(est) || nrow(est) < 2) {
            showNotification("Pooling needs at least two studies with a BC-SMD estimate.", type = "warning")
            return(invisible(NULL))
        }
        fn <- meta_bayes
        es <- data.frame(study_id = est$study_id, es_id = seq_len(nrow(est)),
                         yi = est$median, sei = est$sd, stringsAsFactors = FALSE)
        multi <- anyDuplicated(es$study_id) > 0
        args <- .formal_args(fn, list(es_data = es, yi = "yi", sei = "sei", study = "study_id",
                                      case = if (multi) "es_id", chains = .mcmc_clamp(input$n_chains, 1, 8, 4)))
        args <- args[!vapply(args, is.null, logical(1))]
        bcsmd_ids$pool <- bcsmd_ids$pool + 1
        pool_id <- bcsmd_ids$pool
        current <- function() identical(bcsmd_ids$pool, pool_id)
        set_pool <- function(value) if (current()) bcsmd_pool(value)
        bcsmd_pool(list(status = "running"))
        .bcsmd_buttons(FALSE)
        note_id <- showNotification("Pooling BC-SMD estimates (Bayesian random effects)...",
                                    type = "message", duration = NULL)
        fut <- promises::future_promise(seed = TRUE, scdbayes:::.drop_sampler_files(.session_dir, do.call(fn, args)))
        promises::finally(promises::then(fut,
            onFulfilled = function(res) set_pool(list(status = "done", result = res)),
            onRejected = function(e) set_pool(list(status = "failed", message = conditionMessage(e)))),
            onFinally = function() {
                removeNotification(note_id)
                if (current()) .bcsmd_buttons(TRUE)
            })
        invisible(NULL)
    })

    # pooled mean, tau, prediction interval, shrunken study estimates, and directional probabilities
    .bcsmd_pool_summary <- function(pool) {
        res <- pool$result
        sm <- res$summary
        fit <- if (inherits(res$model, "brmsfit")) res$model
        q3 <- function(name) {
            r <- sm[sm$quantity == name, , drop = FALSE]
            if (nrow(r)) c(r$estimate[1], r$lower[1], r$upper[1]) else rep(NA_real_, 3)
        }
        labels <- c(mu = "Pooled mean BC-SMD", tau = "Tau (between-study SD)",
                    tau_within = "Within-study SD", prediction = "Prediction interval")
        keep <- intersect(names(labels), sm$quantity)
        tab <- data.frame(quantity = unname(labels[keep]),
                          median = vapply(keep, function(k) q3(k)[1], numeric(1)),
                          lower = vapply(keep, function(k) q3(k)[2], numeric(1)),
                          upper = vapply(keep, function(k) q3(k)[3], numeric(1)),
                          stringsAsFactors = FALSE, row.names = NULL)
        mu <- if (!is.null(fit)) as.data.frame(fit)$b_Intercept
        mcid <- suppressWarnings(as.numeric(input$bcsmd_mcid))
        probs <- if (!is.null(mu)) data.frame(
            quantity = c("P(improvement)", if (isTRUE(mcid > 0)) sprintf("P(> MCID = %g)", mcid)),
            median = c(mean(mu > 0), if (isTRUE(mcid > 0)) mean(mu > mcid)),
            lower = NA_real_, upper = NA_real_, stringsAsFactors = FALSE)
        se <- res$study_effects
        shrunk <- if (is.data.frame(se) && nrow(se)) data.frame(study_id = as.character(se$study), median = se$estimate,
                                                                lower = se$lower, upper = se$upper,
                                                                stringsAsFactors = FALSE)
        cv <- res$convergence
        # meta_bayes() warns inside the worker, where the app cannot show it
        conv <- if (isFALSE(cv$converged)) data.frame(
            quantity = sprintf("Not converged: max R-hat %.4f, min bulk/tail ESS %.0f/%.0f, divergences %s",
                               cv$max_rhat %||% NA_real_, cv$min_ess_bulk %||% NA_real_,
                               cv$min_ess_tail %||% NA_real_, cv$n_divergent %||% NA),
            median = NA_real_, lower = NA_real_, upper = NA_real_, stringsAsFactors = FALSE)
        list(table = rbind(tab, probs, conv), shrunk = shrunk, mu = q3("mu"), pred = q3("prediction"), fit = fit)
    }

    bcsmd_pool_summary <- reactive({
        pool <- bcsmd_pool()
        req(pool, identical(pool$status, "done"))
        .bcsmd_pool_summary(pool)
    })

    output$bcsmdPoolTable <- DT::renderDataTable({
        pool <- bcsmd_pool()
        validate(need(!is.null(pool), "Click Pool (Bayesian) after the per-study estimates finish."))
        validate(need(!identical(pool$status, "running"), "Pooling is running..."))
        validate(need(!identical(pool$status, "failed"), paste("Pooling failed:", pool$message)))
        dt_table(bcsmd_pool_summary()$table, digits = 3, caption = "Pooled Bayesian BC-SMD")
    })

    output$bcsmdPoolPriors <- renderPrint({
        pool <- bcsmd_pool()
        req(pool, identical(pool$status, "done"))
        pr <- pool$result$priors %||% tryCatch(brms::prior_summary(pool$result$model), error = function(e) NULL)
        req(pr)
        cat("Priors of the pooled model:\n")
        print(pr)
    })

    output$bcsmdForest <- renderPlot({
        rows <- .bcsmd_rows()
        validate(need(!is.null(rows) && any(rows$status == "estimated"), "No per-study BC-SMD estimates yet."))
        est <- rows[rows$status == "estimated", , drop = FALSE]
        # one row per study and measure, so several measures of a study do not overlap
        label <- ifelse(is.na(est$measure), est$study_id, paste(est$study_id, "/", est$measure))
        stage1 <- data.frame(study_id = label, median = est$median, lower = est$lower,
                             upper = est$upper, estimate = "Per study (stage 1)", stringsAsFactors = FALSE)
        ps <- tryCatch(bcsmd_pool_summary(), error = function(e) NULL)
        shrunk <- if (!is.null(ps$shrunk)) cbind(ps$shrunk, estimate = "Shrunken (pooled model)")
        pooled <- if (!is.null(ps)) data.frame(study_id = c("Pooled mean", "Prediction interval"),
                                               median = c(ps$mu[1], ps$pred[1]),
                                               lower = c(ps$mu[2], ps$pred[2]),
                                               upper = c(ps$mu[3], ps$pred[3]),
                                               estimate = "Pooled", stringsAsFactors = FALSE)
        df <- rbind(stage1, shrunk, pooled)
        df <- df[is.finite(df$median), , drop = FALSE]
        df$study_id <- factor(df$study_id, levels = rev(unique(c(sort(unique(c(stage1$study_id, shrunk$study_id))),
                                                                  "Pooled mean", "Prediction interval"))))
        ggplot(df, aes(median, study_id, color = estimate, shape = estimate)) +
            geom_vline(xintercept = 0, linetype = "dashed", color = "#9CA3AF") +
            geom_pointrange(aes(xmin = lower, xmax = upper), position = position_dodge(width = 0.5)) +
            scale_color_manual(values = c("Per study (stage 1)" = "#2E5090",
                                          "Shrunken (pooled model)" = "#d97706", "Pooled" = "#111827"),
                               name = NULL) +
            scale_shape_manual(values = c("Per study (stage 1)" = 16, "Shrunken (pooled model)" = 17,
                                          "Pooled" = 18), name = NULL) +
            labs(x = "BC-SMD (positive = improvement; posterior median, 95% CrI)", y = NULL) +
            .plot_theme
    }, height = function() {
        rows <- .bcsmd_rows()
        n <- if (is.null(rows)) 0 else sum(rows$status == "estimated", na.rm = TRUE)
        .facet_px(n + 2, per_row = 32, min_px = 320, ncol = 1, extra = 120)
    }, alt = "Forest plot of per-study posterior BC-SMD estimates, shrunken estimates from the pooled model, the pooled mean, and the prediction interval")

    output$downloadBCSMD <- downloadHandler(
        filename = function() "scdbayes_bcsmd_bayes.csv",
        content = function(file) {
            rows <- .bcsmd_rows()
            if (is.null(rows)) return(.download_unavailable(file, "Estimate the per-study BC-SMD first."))
            rows$es_id <- seq_len(nrow(rows))
            rows$yi <- rows$median
            rows$sei <- rows$sd
            write.csv(rows[, c("study_id", "es_id", setdiff(names(rows), c("study_id", "es_id")))], file, row.names = FALSE)
        }
    )

    output$downloadBCSMDCode <- downloadHandler(
        filename = function() "scdbayes_bcsmd_two_stage.R",
        content = function(file) {
            st <- bcsmd_state()
            groups <- st$groups %||% tryCatch(.bcsmd_groups(values$data), error = function(e) list())
            opts <- .bcsmd_opts()
            runnable <- Filter(function(g) is.na(g$reason), groups)
            one_line <- function(x) gsub("[\r\n]", " ", paste(x, collapse = " "))
            fit_formula <- function(g) {
                f <- st$fits[[paste(g$study_id, g$measure)]]
                m <- if (is.list(f) && !inherits(f, "error")) f$model
                if (inherits(m, "brmsfit")) paste("  # brms formula:", one_line(deparse(stats::formula(m)$formula)))
            }
            mv <- intersect(input$meta_measure_var %||% "", names(values$data %||% data.frame()))
            has_study <- "study" %in% names(values$data %||% data.frame())
            # every literal is deparsed, so data-derived ids cannot alter the script
            call_line <- function(g, last) {
                rows <- c(if (has_study) sprintf("as.character(dat$study) == %s", deparse(g$study_id)),
                          if (!is.na(g$measure) && length(mv)) sprintf("as.character(dat[[%s]]) == %s", deparse(mv), deparse(g$measure)))
                sel <- if (length(rows)) paste(rows, collapse = " & ") else "TRUE"
                a <- .bcsmd_args(bc_smd_bayes, g, opts)
                a <- a[setdiff(names(a), "data")]
                a <- a[!vapply(a, is.null, logical(1))]
                arg_txt <- paste(names(a), vapply(a, function(v) one_line(deparse(v)), ""), sep = " = ", collapse = ", ")
                c(fit_formula(g),
                  sprintf("  bc_smd_bayes(dat[%s, , drop = FALSE], %s)%s", sel, arg_txt, if (last) "" else ","))
            }
            stage1 <- unlist(Map(call_line, runnable, seq_along(runnable) == length(runnable)))
            ids <- function(field) paste(vapply(runnable, function(g) deparse(as.character(g[[field]] %||% NA)), ""),
                                         collapse = ", ")
            pooled <- tryCatch(bcsmd_pool_summary()$fit, error = function(e) NULL)
            # study and measure ids stay character, so selections such as '01' match
            id_cols <- c(if (has_study) "study", mv)
            id_cols <- stats::setNames(rep("character", length(id_cols)), id_cols)
            writeLines(c(
                "# Two-stage Bayesian synthesis of the design-comparable BC-SMD (generated by scdbayes)",
                "# Stage 1: Bayesian BC-SMD per study and measure; Stage 2: Bayesian random-effects meta-analysis.",
                "# Time A is the last session of the shortest baseline in each study.",
                paste("# B rule:", .ab_rule_text(opts)),
                "# BC-SMD assumes approximately normal outcomes. Within-case SMD is not computed.",
                "# Estimates are oriented so that positive values mean improvement.",
                sprintf("# Generated on %s", Sys.Date()), "",
                "library(scdbayes)", "library(brms)", "",
                if (length(id_cols)) sprintf("dat <- read.csv('scdbayes_data.csv', colClasses = %s)", one_line(deparse(id_cols)))
                else "dat <- read.csv('scdbayes_data.csv')", "",
                "stage1 <- list(", stage1, ")",
                sprintf("study_id <- c(%s)", ids("study_id")),
                sprintf("measure <- c(%s)", ids("measure")), "",
                "es <- data.frame(study_id = study_id, measure = measure, es_id = seq_along(stage1),",
                "                 yi = sapply(stage1, function(r) r$summary$estimate[1]),",
                "                 sei = sapply(stage1, function(r) r$summary$sd[1]))", "",
                if (!is.null(pooled)) paste("# Pooled model formula in the app:", one_line(deparse(stats::formula(pooled)$formula))),
                "# several measures per study add a within-study effect, as in the app: (1 | study_id) + (1 | study_id:es_id)",
                "f <- if (anyDuplicated(es$study_id) > 0) bf(yi | se(sei) ~ 1 + (1 | study_id) + (1 | study_id:es_id)) else",
                "  bf(yi | se(sei) ~ 1 + (1 | study_id))",
                "# fixed weakly informative intercept prior, as in meta_bayes(): normal(0, 2) on the SMD scale",
                "mu_sd <- 2",
                "fit <- brm(f, data = es,",
                "           prior = c(prior_string(sprintf('normal(0, %g)', mu_sd), class = 'Intercept'),",
                "                     prior(student_t(3, 0, 0.5), class = sd)),",
                "           chains = 4, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99), seed = 1)",
                "summary(fit)",
                "draws <- as_draws_df(fit)",
                "mean(draws$b_Intercept > 0)  # posterior probability of a positive pooled BC-SMD"), file)
        }
    )

    output$downloadBCSMDFreqScript <- downloadHandler(
        filename = function() "scdbayes_bcsmd_scdhlm_comparison.R",
        content = function(file) {
            groups <- tryCatch(.bcsmd_groups(values$data), error = function(e) list())
            runnable <- Filter(function(g) is.na(g$reason), groups)
            opts <- .bcsmd_opts()
            # time trend -> scdhlm baseline/treatment terms (0 = level, c(0, 1) = level + trend); RE_trt mirrors random_effect
            fe_base <- if (opts$trend == "none") "0" else "c(0, 1)"
            re_base <- if (opts$trend == "random") "c(0, 1)" else "0"
            fe_trt <- if (opts$trend == "none") "0" else "c(0, 1)"
            re_trt <- if (isTRUE(opts$random_effect)) ", RE_trt = 0" else ""
            re_trt_2 <- if (isTRUE(opts$random_effect)) ", RE_trt_2 = 0" else ""
            sd_all <- .scdhlm_data(values$data, input$meta_measure_var %||% "")
            ab <- if (!is.null(sd_all)) .scdhlm_times(sd_all, opts$duration, opts$ab_rule)
            spec_row <- function(g) {
                m <- if (is.na(g$measure)) "" else g$measure
                t <- if (!is.null(ab)) ab[ab$study == g$study_id & ab$measure == m, , drop = FALSE]
                sprintf("  data.frame(study = %s, measure = %s, design = %s, goal = %s, A = %s, B = %s)",
                        deparse(g$study_id), deparse(m), deparse(g$scdhlm_design), deparse(g$goal),
                        if (NROW(t)) format(t$A[1]) else "NA", if (NROW(t)) format(t$B[1]) else "NA")
            }
            spec_rows <- vapply(runnable, spec_row, "")
            writeLines(c(
                "# Optional frequentist comparison: REML BC-SMD with scdhlm, pooled with metafor (export only)",
                "# scdhlm applies a small-sample correction; scdbayes estimates the BC-SMD in a Bayesian model.",
                paste("# B rule:", .ab_rule_text(opts)),
                sprintf("# Treatment effect: %s", if (isTRUE(opts$random_effect)) "varies across cases (RE_trt = 0, as MB2)"
                                              else "common across cases (sensitivity)"),
                sprintf("# Generated by scdbayes on %s", Sys.Date()), "",
                "library(scdhlm)", "library(metafor)", "library(clubSandwich)", "",
                "# maintenance and generalization sessions are already excluded from the exported data",
                "dat <- read.csv('scdbayes_scdhlm_data.csv', comment.char = '#', colClasses = c(study = 'character', measure = 'character'))",
                "if (is.null(dat$study)) dat$study <- 'all'",
                "if (is.null(dat$measure)) dat$measure <- ''",
                "dat$measure[is.na(dat$measure)] <- ''",
                "# one row per study and measure: scdhlm design, goal, and times A and B",
                "specs <- rbind(", paste(spec_rows, collapse = ",\n"), ")",
                "specs$measure[is.na(specs$measure)] <- ''", "",
                "bcsmd <- do.call(rbind, lapply(seq_len(nrow(specs)), function(k) {",
                "  s <- specs[k, ]",
                "  d <- dat[dat$study == s$study & dat$measure == s$measure, ]",
                "  args <- list(design = s$design, case = d$case, phase = d$treatment, session = d$session,",
                "               outcome = d$outcome, treatment_name = 'treatment', corStruct = 'AR1')",
                "  # scdhlm accepts only its default level-only terms for treatment reversal; a time trend is not mirrored",
                paste0("  terms <- if (s$design == 'TR') list(FE_base = 0, RE_base = 0, FE_trt = 0", re_trt, ")"),
                sprintf("           else list(FE_base = %s, RE_base = %s, FE_trt = %s%s, A = s$A, B = s$B)", fe_base, re_base, fe_trt, re_trt),
                sprintf("  outer <- list(RE_base_2 = 0%s)", re_trt_2),
                "  if (s$design == 'RMBB') args <- c(args, list(series = d$series), outer)",
                "  if (s$design == 'CMB') args <- c(args, list(cluster = d$cluster), outer)",
                "  r <- tryCatch(do.call(calc_BCSMD, c(args, terms)), error = function(e) conditionMessage(e))",
                "  failed <- is.character(r)",
                "  # scdhlm estimates are positive for an increase; decrease-goal studies are sign-reversed",
                "  sign <- if (s$goal == 'decrease') -1 else 1",
                "  num <- function(name) if (failed) NA_real_ else as.numeric(r[[name]])",
                "  data.frame(study = s$study, measure = s$measure, design = s$design, goal = s$goal,",
                "             yi = sign * num('BC-SMD estimate'), vi = num('Std. Error')^2,",
                "             df = num('Degrees of freedom'), A = num('Initial treatment time'),",
                "             B = num('Follow-up time'), converged = if (failed) NA_character_ else as.character(r[['Converged']]),",
                "             error = if (failed) r else '')",
                "}))",
                "bcsmd", "",
                "ok <- subset(bcsmd, is.finite(yi) & is.finite(vi))",
                "if (anyDuplicated(ok$study) == 0) {",
                "  rma.uni(yi, vi, data = ok, test = 'knha')",
                "} else {",
                "  ok$es_id <- seq_len(nrow(ok))",
                "  robust(rma.mv(yi, V = vi, random = ~ 1 | study / es_id, data = ok),",
                "         cluster = study, clubSandwich = TRUE)",
                "}"), file)
        }
    )

    output$downloadNetworkMetaScript <- downloadHandler(
        filename = function() "scdbayes_network_meta.R",
        content = function(file) {
            writeLines(paste0(
                .meta_template_header("Network meta-analysis template"),
                "# library(gemtc)    # Bayesian NMA\n",
                "# library(multinma)  # Bayesian NMA with IPD\n",
                "library(netmeta)    # frequentist NMA\n\n",
                "es <- read.csv('scdbayes_meta_es.csv')\n",
                "es <- subset(es, ES == unique(es$ES)[1])\n\n",
                "# Each row needs: study, treatment1, treatment2, TE, seTE\n",
                "# For SCD: treatment1 = baseline, treatment2 = intervention (merge an intervention column)\n\n",
                "# pw <- data.frame(\n",
                "#   studlab = es$study_id,\n",
                "#   treat1 = 'baseline',\n",
                "#   treat2 = es$intervention,\n",
                "#   TE = es$yi,\n",
                "#   seTE = es$sei\n",
                "# )\n",
                "# net <- netmeta(TE, seTE, treat1, treat2, studlab, data = pw)\n",
                "# summary(net)\n",
                "# netgraph(net)\n",
                "# forest(net)\n\n",
                "# Example with gemtc (Bayesian)\n",
                "# network <- mtc.network(data.re = pw_gemtc_format)\n",
                "# model <- mtc.model(network, linearModel = 'random')\n",
                "# results <- mtc.run(model)\n",
                "# summary(results)\n"
            ), file)
        }
    )

    output$downloadMLMetaScript <- downloadHandler(
        filename = function() "scdbayes_ml_moderators.R",
        content = function(file) {
            writeLines(paste0(
                .meta_template_header("Machine learning moderator analysis template"),
                "library(metafor)\n",
                "# library(metaforest)  # random forest for meta-analysis\n\n",
                "es <- read.csv('scdbayes_meta_es.csv')\n",
                "es <- subset(es, ES == unique(es$ES)[1])\n\n",
                "# MetaForest: random forest for identifying important moderators\n",
                "# Requires moderator columns merged into the exported CSV\n",
                "# mf <- MetaForest(\n",
                "#   yi ~ moderator1 + moderator2 + moderator3,\n",
                "#   data = es,\n",
                "#   vi = 'vi',\n",
                "#   study = 'study_id',\n",
                "#   whichweights = 'random',\n",
                "#   num.trees = 1000\n",
                "# )\n",
                "# print(mf)\n",
                "# VarImpPlot(mf)\n\n",
                "# Partial dependence plots\n",
                "# PartialDependence(mf, vars = 'moderator1')\n\n",
                "# MetaCART: meta-analytic classification and regression trees\n",
                "# library(metacart)\n",
                "# mc <- metacart(yi ~ moderator1 + moderator2,\n",
                "#                vi = vi, data = es)\n",
                "# summary(mc)\n",
                "# plot(mc)\n"
            ), file)
        }
    )

    output$liveCodeDisplay <- renderPrint({
        req(values$data)
        formula_str <- model_formula()
        req(formula_str)
        family <- input$model_family %||% "gaussian"
        outcome <- input$outcome_var %||% "outcome"
        prep <- .prepare_fit_data(values$processed_data %||% values$data, outcome, family)
        fp <- .scd_fit_priors(prep$data, formula_str, family, outcome, FALSE)
        mc <- .mcmc_settings(input$n_chains, input$n_iter, input$n_warmup,
                             input$adapt_delta, input$max_treedepth)
        formula_code <- if (fp$hetero) sprintf("bf(%s, sigma ~ phase)", formula_str) else deparse(formula_str)
        family_code <- if (family == "beta") "Beta()" else paste0(family, "()")
        cat(sprintf(paste0("library(brms)\nlibrary(scdbayes)\n\n",
            "data <- read.csv(\"data.csv\")\n",
            "data <- process_scd_phases(data, list(), scd_type = %s)\n%s\n%s",
            "model <- brm(formula = %s, data = data, family = %s, prior = priors,\n",
            "  chains = %d, iter = %d, warmup = %d, backend = \"cmdstanr\",\n",
            "  control = list(adapt_delta = %s, max_treedepth = %d))\n\n",
            "summary(model)\npp_check(model)\n"),
            deparse(.design_choice(input$research_design, values$data)), fit_transform_code(prep),
            .prior_code(fp$brms_priors), formula_code, family_code,
            mc$chains, mc$iter, mc$warmup, mc$adapt_delta, mc$max_treedepth))
    })
    observeEvent(input$copyCodeResult, {
        ok <- identical(input$copyCodeResult$status, "ok")
        msg <- if (ok) "Code copied to clipboard." else
            "Copy blocked by the browser. Select the code and copy it manually."
        showNotification(msg, type = if (ok) "message" else "warning", duration = 4)
        .announce(msg)
    })

    .download_unavailable <- function(file, msg) {
        showNotification(msg, type = "warning", duration = 6)
        writeLines(msg, file)
    }

    .prior_code <- function(bp) {
        if (is.null(bp) || NROW(bp) == 0) return(NULL)
        fields <- intersect(c("class", "coef", "group", "resp", "dpar", "nlpar", "lb", "ub"), names(bp))
        calls <- vapply(seq_len(nrow(bp)), function(i) {
            vals <- vapply(fields, function(f) as.character(bp[[f]][i]), character(1))
            vals <- vals[!is.na(vals) & nzchar(vals)]
            args <- paste0(", ", names(vals), " = ", vapply(vals, deparse, character(1)), collapse = "")
            paste0("prior_string(", deparse(as.character(bp$prior[i])), args, ")")
        }, character(1))
        paste0("priors <- c(\n  ", paste(calls, collapse = ",\n  "), "\n)\n\n")
    }

    output$downloadCode <- downloadHandler(
        filename = function() paste0("scdbayes_analysis_", Sys.Date(), ".R"),
        content = function(file) {
            req(values$model)
            formula_str <- values$formula_display %||% "outcome ~ phase + (1|case)"
            mc <- .mcmc_settings(input$n_chains, input$n_iter, input$n_warmup, input$adapt_delta, input$max_treedepth)
            # family and sampler sizes describe the fitted model, not the current inputs
            fitted <- brms::is.brmsfit(values$model)
            if (fitted) formula_str <- paste(deparse(values$model$formula$formula), collapse = " ")
            family_val <- if (fitted) values$model$family$family else input$model_family %||% "gaussian"
            chains <- if (fitted) values$model$fit@sim$chains else mc$chains
            iter <- if (fitted) values$model$fit@sim$iter else mc$iter
            warmup <- if (fitted) values$model$fit@sim$warmup else mc$warmup
            adapt <- values$fit_sampler$adapt_delta %||% mc$adapt_delta
            outcome <- all.vars(as.formula(formula_str)[[2]])[1]
            count_code <- fit_transform_code(values$fit_transform)
            used_priors <- values$fit_brms_priors %||%
                tryCatch(priors_to_brms(values$current_prior_list), error = function(e) NULL)
            prior_block <- .prior_code(used_priors)
            rope_used <- if (is.null(values$model)) NULL else .default_rope(values$model)
            formula_code <- if (isTRUE(values$fit_hetero_sigma)) {
                sprintf("bf(%s, sigma ~ phase)", formula_str)
            } else deparse(formula_str)
            prep <- values$meta_fit_prep
            subset_code <- if (!is.null(prep$subset)) {
                sprintf("data <- data[as.character(data[[%s]]) %%in%% %s, ]\n\n",
                        deparse(prep$subset_var), paste(deparse(prep$subset_values), collapse = ""))
            } else ""
            std_code <- if (!is.null(prep$table)) {
                fixed_arg <- if (identical(prep$formula_fixed, "default")) "NULL" else deparse(prep$formula_fixed)
                paste0(
                    "# Scale harmonization: outcome divided by the per-tier OLS residual SD\n",
                    "# Sensitivity analysis only: the residual SD ignores autocorrelation\n",
                    sprintf("data <- standardize_by_case(data, formula_fixed = %s, outcome_var = %s)\n",
                            fixed_arg, deparse(sub("_std$", "", prep$outcome))),
                    "attr(data, 'case_sigma')\n",
                    sprintf("data <- data[!is.na(data$%s), ]\n\n", prep$outcome))
            } else ""
            # the fit used the coded piecewise columns, so the script codes the phases first
            phase_code <- sprintf("data <- process_scd_phases(data, list(), %s)\n\n",
                                  deparse(values$fit_design %||% .design_choice(input$research_design, values$data)))

            code <- paste0(
                "# Bayesian Single-Case Design Analysis\n",
                "# Generated by scdbayes (", Sys.Date(), ")\n\n",
                "library(brms)\nlibrary(ggplot2)\nlibrary(dplyr)\nlibrary(scdbayes)\n\n",
                "# Load data\n",
                "data <- read.csv('scdbayes_data.csv')\n\n",
                "# Piecewise phase coding\n",
                phase_code,
                subset_code,
                count_code,
                std_code,
                if (!is.null(prior_block)) paste0("# Priors used in the app fit\n", prior_block) else "",
                "# Fit Bayesian multilevel piecewise regression model\n",
                "model <- brm(\n",
                "  formula = ", formula_code, ",\n",
                "  data = data,\n",
                "  family = ", deparse(family_val), ",\n",
                if (!is.null(prior_block)) "  prior = priors,\n" else "",
                "  chains = ", chains, ", iter = ", iter, ", warmup = ", warmup, ",\n",
                "  cores = parallel::detectCores(),\n",
                "  control = list(adapt_delta = ", adapt,
                ", max_treedepth = ", values$fit_sampler$max_treedepth %||% mc$max_treedepth, "),\n",
                "  seed = 123\n",
                ")\n\n",
                "# Model summary\n",
                "summary(model)\n",
                "fixef(model)\n",
                "ranef(model)\n",
                "VarCorr(model)\n\n",
                "# Diagnostics\n",
                "plot(model)\n",
                "pp_check(model, type = 'dens_overlay', ndraws = 50)\n",
                "pp_check(model, type = 'stat', stat = 'sd', ndraws = 500)\n",
                "loo(model)\n",
                "bayes_R2(model)\n\n",
                "# Hypothesis tests\n",
                "# hypothesis(model, 'level_AB > 0')  # immediate effect\n",
                "# hypothesis(model, 'time_A + trend_AB = 0')  # intervention slope\n\n",
                "# Posterior decisions\n",
                if (!is.null(rope_used)) {
                    sprintf("summarize_fixed_effect_decisions(model, rope = c(%s), intervention_goal = \"%s\")  # pd, ROPE, IRR/OR per term\n",
                            paste(signif(rope_used, 4), collapse = ", "), input$intervention_goal %||% "increase")
                } else {
                    sprintf("summarize_fixed_effect_decisions(model, intervention_goal = \"%s\")  # pd, IRR/OR per term\n",
                            input$intervention_goal %||% "increase")
                },
                "# See scdbayes_decisions.csv for full results\n"
            )
            writeLines(code, file)
        }
    )

    .report_format <- function() {
        fmt <- input$report_format %||% "html"
        can_pandoc <- requireNamespace("rmarkdown", quietly = TRUE) &&
            rmarkdown::pandoc_available()
        if (fmt %in% c("pdf", "docx") && !can_pandoc) "md" else fmt
    }

    # backticks, dollar signs, and pipes are stripped so logged text cannot become R or LaTeX in the report
    .ai_audit_appendix <- function() {
        clean <- function(x) .md_cell(.txt(x %||% ""))
        rows <- vapply(ai_audit(), function(e) sprintf("| %s | %s | %s | %s | %s | %s |",
            clean(e$time), clean(e$event), clean(e$provider), clean(e$model_id), clean(e$mode),
            clean(paste(vapply(e$adopted %||% list(), function(a)
                paste(.txt(a$class), .txt(a$prior_string)), ""), collapse = "; "))), "")
        c("", "# Appendix: AI audit log", "",
          paste("AI text is advisory; estimates come only from the brms model.",
                "Full prompts and responses are in the AI audit log (JSON) download."), "",
          "| Time | Event | Provider | Model | Mode | Adopted priors |",
          "|---|---|---|---|---|---|", rows)
    }

    # markdown table of the last posterior predictive check run; empty when none was run
    .ppc_appendix <- function() {
        sm <- isolate(ppc_features()$features$summary)
        if (is.null(sm) || !nrow(sm)) return(character(0))
        cell <- function(x) ifelse(is.na(x), "", if (is.numeric(x)) sprintf("%.3f", x) else .md_cell(x))
        rows <- sprintf("| %s | %s | %s | %s | %s | %s |", cell(sm$statistic), cell(sm$case), cell(sm$phase),
                        cell(sm$observed), cell(sm$p_value), ifelse(sm$flag, "discrepant", ""))
        c("", "# Appendix: Posterior predictive checks", "",
          paste("Visual-analysis features of replicated data;",
                "the p-value is Pr[T(yrep) >= T(y)]."), "",
          "| Feature | Case | Phase | Observed | Pr[T(yrep) >= T(y)] | Flag |",
          "|---|---|---|---|---|---|", rows)
    }

    # priors of the fitted model with where each came from: brms default, app default or preset, AI-adopted, override
    .report_priors <- function() {
        ps <- tryCatch(as.data.frame(brms::prior_summary(values$model)), error = function(e) NULL)
        if (is.null(ps) || !nrow(ps)) return(NULL)
        ps <- ps[nzchar(ps$prior), , drop = FALSE]
        pl <- values$fit_prior_list %||% list()
        src <- values$fit_prior_sources %||% character(0)
        squash <- function(x) gsub("\\s+", "", as.character(x %||% ""))
        key <- vapply(seq_len(nrow(ps)), function(i) {
            hit <- names(pl)[vapply(pl, function(p) identical(p$class, ps$class[i]) &&
                identical(squash(p$coef), squash(ps$coef[i])) &&
                identical(squash(p$string), squash(ps$prior[i])), logical(1))]
            c(hit, NA_character_)[1]
        }, "")
        type <- values$fit_prior_type %||% "default"
        app_label <- if (identical(type, "default")) "default" else sprintf("default (%s prior type)", type)
        source <- ifelse(ps$source == "default", "default (brms)",
                  ifelse(!is.na(key) & key %in% names(src), unname(src[key]), app_label))
        data.frame(class = ps$class, coef = ps$coef, group = ps$group, prior = ps$prior,
                   source = source, stringsAsFactors = FALSE)
    }

    output$downloadReport <- downloadHandler(
        filename = function() paste0("scdbayes_report_", Sys.Date(), ".", .report_format()),
        content = function(file) {
            req(values$model)
            fmt <- .report_format()
            if (!identical(fmt, input$report_format %||% "html")) {
                showNotification(
                    paste0(toupper(input$report_format), " export requires the rmarkdown package and pandoc; downloading markdown instead."),
                    type = "warning", duration = 8)
            }
            format_map <- list(html = "html_document", pdf = "pdf_document",
                               docx = "word_document", md = "md_document")
            out_format <- format_map[[fmt]] %||% "html_document"
            sections <- input$report_sections %||% c("methods", "results", "tables", "figures", "diagnostics")

            template_path <- system.file("report_template.Rmd", package = "scdbayes")
            if (template_path == "") template_path <- file.path(getwd(), "inst", "report_template.Rmd")

            render_dir <- tempfile("scdbayes-report-")
            dir.create(render_dir)
            on.exit(unlink(render_dir, recursive = TRUE), add = TRUE)

            rendered <- if (file.exists(template_path) && requireNamespace("rmarkdown", quietly = TRUE)) {
                tmp_rmd <- file.path(render_dir, "scdbayes_report.Rmd")
                file.copy(template_path, tmp_rmd, overwrite = TRUE)
                if ("ai_audit" %in% sections && length(ai_audit()))
                    cat(.ai_audit_appendix(), file = tmp_rmd, sep = "\n", append = TRUE)
                cat(.ppc_appendix(), file = tmp_rmd, sep = "\n", append = TRUE)
                tryCatch(
                    rmarkdown::render(tmp_rmd, output_format = out_format,
                        output_dir = render_dir, intermediates_dir = render_dir,
                        params = list(
                            model = values$model,
                            results = values$results,
                            data = values$data,
                            formula_str = values$formula_display %||% "outcome ~ phase + (1|case)",
                            family = stats::family(values$model)$family,
                            sections = sections,
                            priors = .report_priors()
                        ),
                        envir = new.env(parent = globalenv()),
                        quiet = TRUE
                    ),
                    error = function(e) {
                        showNotification(paste("Report rendering failed:", e$message),
                                         type = "warning", duration = 8)
                        NULL
                    })
            }
            if (!is.null(rendered) && file.exists(rendered)) {
                file.copy(rendered, file, overwrite = TRUE)
                return()
            }

            if (fmt %in% c("html", "md")) {
                .write_fallback_report(file, values$model, values$results,
                                       sections = sections, format = fmt,
                                       formula_str = values$formula_display, priors = .report_priors())
                if (fmt == "md" && "ai_audit" %in% sections && length(ai_audit()))
                    cat(.ai_audit_appendix(), file = file, sep = "\n", append = TRUE)
                if (fmt == "md") cat(.ppc_appendix(), file = file, sep = "\n", append = TRUE)
                return()
            }

            fallback_md <- file.path(render_dir, "scdbayes_report_fallback.md")
            .write_fallback_report(fallback_md, values$model, values$results,
                                   sections = sections, format = "md",
                                   formula_str = values$formula_display, priors = .report_priors())
            if ("ai_audit" %in% sections && length(ai_audit()))
                cat(.ai_audit_appendix(), file = fallback_md, sep = "\n", append = TRUE)
            cat(.ppc_appendix(), file = fallback_md, sep = "\n", append = TRUE)
            converted <- fmt == "docx" && isTRUE(tryCatch({
                rmarkdown::pandoc_convert(fallback_md, to = "docx", output = file)
                file.exists(file)
            }, error = function(e) FALSE))
            if (!converted) {
                showNotification(
                    paste(toupper(fmt), "report could not be produced. Choose HTML or Markdown format instead."),
                    type = "error", duration = 10)
                writeLines(c(sprintf("The %s report could not be rendered on this server.", toupper(fmt)),
                             "Download the report in HTML or Markdown format instead."), file)
            }
        }
    )
})
