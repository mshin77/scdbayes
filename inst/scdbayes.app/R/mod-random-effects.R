# baseline trend, onset level change and its trend change all vary over cases and studies
.effect_terms <- function(data) {
  onset <- tryCatch(treatment_coef(data), error = function(e) NULL)
  if (is.null(onset)) return(character(0))
  base_trend <- c("time_A", reversal_terms(names(data))[1])
  intersect(c(base_trend, onset, sub("^level_", "trend_", onset)), names(data))
}

# grouping columns ordered outer to inner by containment (study/cluster/case/nest)
.tier_cols <- function(data) {
  tiers <- tryCatch(tier_vars(data), error = function(e) NULL)
  if (!is.null(tiers)) return(tiers)
  case_var <- find_case_var(data)
  outer <- unique(c(find_study_var(data), find_cluster_var(data), case_var))
  c(outer, find_nest_var(data, exclude = outer, case_var = case_var))
}

# user slopes plus the design's effect terms, after the random intercept
.slope_lhs <- function(slopes, data) {
  user_slopes <- if (length(slopes) > 0 && slopes[1] != "") slopes else character(0)
  slope_terms <- setdiff(unique(c(user_slopes, .effect_terms(data))), "")
  if (length(slope_terms)) paste0("1 + ", paste(slope_terms, collapse = " + ")) else "1"
}

.tier_order <- function(grps, data) {
  tiers <- .tier_cols(data)
  c(intersect(tiers, grps), setdiff(grps, tiers))
}

unifiedRandomEffectsServer <- function(id, data_reactive, formula_reactive = NULL) {
  moduleServer(id, function(input, output, session) {

    values <- reactiveValues(
      detected_structure = NULL
    )

    # Update variable choices when data changes
    observe({
      req(data_reactive())
      data <- data_reactive()
      vars <- names(data)

      coded_cols <- c("outcome", "phase", "phase_numeric",
                      grep("^(level|trend|time|treatment|atd)_", vars, value = TRUE))
      grouping_candidates <- setdiff(vars[sapply(data, function(x) {
        (is.factor(x) || is.character(x) || length(unique(x)) < length(x) / 2) &&
        length(unique(x)) >= 2
      })], coded_cols)

      cardinalities <- sapply(grouping_candidates, function(v) length(unique(data[[v]])))
      grouping_candidates <- grouping_candidates[order(cardinalities)]
      time_cols <- c("time", "session", "day", "week", "trial", "observation",
                     tryCatch(find_time_var(data), error = function(e) NULL))
      group_pool <- setdiff(grouping_candidates, time_cols)
      tiers <- intersect(.tier_cols(data), group_pool)
      known_groups <- intersect(c("id", "subject", "participant", "case",
                                  "wave", "item", "tier", "unit",
                                  "teacher", "class", "classroom", "cluster",
                                  "school", "site", "district", "study"), group_pool)
      auto_selected <- if (length(tiers) > 0) tiers else if (length(known_groups) > 0) {
        known_groups
      } else {
        if (length(group_pool) > 0) group_pool[1] else character(0)
      }
      updateSelectInput(session, "groupingVars",
        choices = grouping_candidates,
        selected = auto_selected)

      numeric_vars <- vars[sapply(data, is.numeric)]
      slope_choices <- c("None" = "")
      if ("phase" %in% vars) slope_choices <- c(slope_choices, "Phase" = "phase")
      if ("time" %in% numeric_vars) slope_choices <- c(slope_choices, "Time" = "time")
      if ("treatment" %in% vars) slope_choices <- c(slope_choices, "Treatment" = "treatment")
      other_numeric <- setdiff(numeric_vars, c("outcome", "time"))
      if (length(other_numeric) > 0) {
        slope_choices <- c(slope_choices, stats::setNames(other_numeric, other_numeric))
      }

      updateSelectInput(session, "slopeVars", choices = slope_choices, selected = NULL)
      auto_formula_dirty(TRUE)
    })

    auto_formula_dirty <- reactiveVal(TRUE)
    observeEvent(input$randomFormula, {
      auto_formula_dirty(FALSE)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent(list(input$groupingVars, input$slopeVars), {
      auto_formula_dirty(TRUE)
    }, ignoreInit = TRUE)

    observe({
      req(isTRUE(auto_formula_dirty()))
      grps <- input$groupingVars
      slopes <- input$slopeVars
      req(grps)
      data <- data_reactive()
      user_slopes <- if (length(slopes) > 0 && slopes[1] != "") slopes else character(0)
      lhs <- .slope_lhs(slopes, data)
      grps <- .tier_order(grps, data)
      tiers <- .tier_cols(data)
      package_default <- !length(user_slopes) && setequal(grps, intersect(tiers, names(data)))
      formula <- if (package_default) {
        tryCatch(default_random_effects(data), error = function(e) NULL) %||%
          paste0("(", lhs, " | ", paste(grps, collapse = "/"), ")")
      } else if (length(grps) == 1) {
        paste0("(", lhs, " | ", grps[1], ")")
      } else if ("study" %in% grps || all(grps %in% tiers)) {
        paste0("(", lhs, " | ", paste(grps, collapse = "/"), ")")
      } else {
        paste(paste0("(", lhs, " | ", grps, ")"), collapse = " + ")
      }
      isolate(updateTextAreaInput(session, "randomFormula", value = formula))
    })

    # Build nested formula (supports 2+ levels: outermost / ... / innermost)
    observeEvent(input$buildNested, {
      req(input$groupingVars)
      grps <- input$groupingVars
      if (length(grps) >= 2) {
        nesting_path <- paste(.tier_order(grps, data_reactive()), collapse = "/")
        formula <- paste0("(", .slope_lhs(input$slopeVars, data_reactive()), " | ", nesting_path, ")")
        updateTextAreaInput(session, "randomFormula", value = formula)
      } else {
        showNotification("Need at least 2 variables for nested structure", type = "warning")
      }
    })

    # Build cross-classified formula
    observeEvent(input$buildCrossClass, {
      req(input$groupingVars)
      if (length(input$groupingVars) >= 2) {
        lhs <- .slope_lhs(input$slopeVars, data_reactive())
        formula <- paste(paste0("(", lhs, " | ", input$groupingVars, ")"), collapse = " + ")
        updateTextAreaInput(session, "randomFormula", value = formula)
      } else {
        showNotification("Need at least 2 variables for cross-classified structure", type = "warning")
      }
    })

    # RE syntax guide modal
    observeEvent(input$showREGuide, {
      items <- list(
        list(sym = "<code>(1|group)</code>", meaning = "Random intercepts by grouping variable"),
        list(sym = "<code>(1+x|group)</code>", meaning = "Random intercepts and slopes (correlated)"),
        list(sym = "<code>(1|g1) + (1|g2)</code>", meaning = "Cross-classified random effects"),
        list(sym = "<code>(1|g1/g2)</code>", meaning = "Nested random effects (g2 nested within g1)"),
        list(sym = "<code>(0+x|group)</code>", meaning = "Random slopes only (no intercept)"),
        list(sym = "<code>(1+x||group)</code>", meaning = "Random intercepts and slopes (uncorrelated)")
      )
      rows <- paste0(sapply(items, function(r) {
        paste0("<tr><td style='padding: 8px; border-bottom: 1px solid #e5e7eb; white-space: nowrap;'>",
               r$sym, "</td><td style='padding: 8px; border-bottom: 1px solid #e5e7eb;'>",
               r$meaning, "</td></tr>")
      }), collapse = "")
      html <- paste0(
        "<table style='width: 100%; font-size: 15px; border-collapse: collapse;'>",
        "<thead><tr><th style='padding: 8px; border-bottom: 2px solid #0c1f4a; text-align: left;'>Syntax</th>",
        "<th style='padding: 8px; border-bottom: 2px solid #0c1f4a; text-align: left;'>Meaning</th></tr></thead>",
        rows, "</table>",
        "<p style='margin-top: 12px;'>By default every level change varies across cases. ",
        "Omitting a real random slope can inflate Type I error to about 40%.</p>")
      showModal(info_popup_modal(
        title = "Random Effects Formula Syntax",
        body = HTML(html),
        citation = paste(
          "Moeyaert et al. (2020, p. 38, Eq. 3); Rodr&iacute;guez-Prada, Olmos, &amp;",
          "Mart&iacute;nez-Huertas (n.d., poster).")
      ))
    })

    # Auto-detect structure from formula
    observeEvent(input$detectStructure, {
      req(input$randomFormula)
      formula_text <- input$randomFormula
      structure <- detect_formula_structure(formula_text)
      values$detected_structure <- structure
      showNotification(paste("Detected:", structure$description),
        type = if (length(structure$warnings) > 0) "warning" else "default",
        duration = 5)
    })

    detect_formula_structure <- function(formula_text) {
      structure <- list(type = NULL, description = "", warnings = list())

      if (grepl("/", formula_text)) {
        structure$type <- "nested"
        n_levels <- length(strsplit(formula_text, "/")[[1]])
        structure$description <- paste0("Nested hierarchical structure (", n_levels, " levels)")
      } else if (grepl("mm\\(", formula_text)) {
        structure$type <- "multiple_membership"
        structure$description <- "Multiple membership structure"
      } else {
        separate_terms <- trimws(strsplit(formula_text, "\\+")[[1]])
        group_terms <- grep("\\|", separate_terms, value = TRUE)
        if (length(group_terms) > 1) {
          structure$type <- "cross_classified"
          structure$description <- paste("Cross-classified with", length(group_terms), "grouping factors")
        } else if (length(group_terms) == 1) {
          structure$type <- "hierarchical"
          structure$description <- "Simple hierarchical structure"
        }
      }
      structure
    }

    # Formula warnings
    output$formulaWarnings <- renderUI({
      req(input$randomFormula)
      warnings <- list()
      formula_text <- input$randomFormula

      if (grepl("\\|\\s*1\\s*\\)", formula_text)) {
        warnings <- append(warnings, "Formula contains (|1), which is invalid. Did the spec mean (1|group)?")
      }
      n_terms <- length(strsplit(formula_text, "\\+")[[1]])
      if (n_terms > 8) {
        warnings <- append(warnings, "Complex model with many random effects may have convergence issues")
      }

      if (length(warnings) > 0) {
        div(class = "status-main-warning mt-3",
          tags$ul(style = "margin: 0; padding-left: 20px;",
            lapply(warnings, tags$li)))
      }
    })

    # Structure alert
    output$structureAlert <- renderUI({
      if (!is.null(values$detected_structure)) {
        structure <- values$detected_structure
        alert_class <- if (length(structure$warnings) > 0) "status-sidebar-warning" else "status-sidebar-success"
        div(class = alert_class, structure$description)
      }
    })

    return(reactive({
      list(
        formula = input$randomFormula,
        detected_structure = values$detected_structure
      )
    }))
  })
}
