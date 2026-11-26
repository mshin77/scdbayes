.power_plan <- function(input) {
  number <- function(id, default, lower, upper = Inf, integer = FALSE) {
    value <- input[[id]] %||% default
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value) ||
        value < lower || value > upper) stop(paste(id, "requires a finite value in range."), call. = FALSE)
    if (integer && value != floor(value)) stop(paste(id, "requires an integer."), call. = FALSE)
    value
  }
  target <- match.arg(input$power_target %||% "single", c("single", "pooled", "category"))
  text <- input$power_effect_sizes %||% "0, 0.5, 1.0"
  effects <- suppressWarnings(as.numeric(strsplit(trimws(text), "[,;\\s]+", perl = TRUE)[[1]]))
  if (!length(effects) || length(effects) > 10 || any(!is.finite(effects)) || any(effects < 0))
    stop("Effect magnitudes require up to 10 finite nonnegative numbers.", call. = FALSE)
  allocation <- if (target == "category") c(number("power_category_a", 4, 1, 100, TRUE),
    number("power_category_b", 4, 1, 100, TRUE)) else c(4, 4)
  if (target == "category" && any(allocation < 2)) stop("Each category requires at least 2 studies.", call. = FALSE)
  studies <- switch(target, single = 1, pooled = number("power_n_studies", 4, 2, 100, TRUE), category = sum(allocation))
  family <- match.arg(input$power_family %||% "gaussian", c("gaussian", "poisson", "negbinomial"))
  threshold <- number("power_prob_threshold", 0.975, 0.5, 0.999)
  if (threshold <= 0.5) stop("Posterior threshold must exceed 0.5.", call. = FALSE)
  autocorr <- number("power_autocorr", if (family == "gaussian") 0.2 else 0, 0, 0.9)
  if (family != "gaussian" && autocorr != 0) stop("Count planning does not support serial dependence; set AR(1) to 0.", call. = FALSE)
  args <- list(n_sims = number("power_n_sims", 200, 20, 500, TRUE),
    n_cases = number("power_n_cases", 5, 1, 30, TRUE), n_studies = studies,
    n_phase_a = number("power_n_phase_a", 5, 3, 30, TRUE),
    n_phase_b = number("power_n_phase_b", 5, 3, 30, TRUE), effect_sizes = effects,
    autocorr = autocorr, icc = number("power_icc", 0.2, 0, 0.95),
    study_sd = if (target == "single") 0 else number("power_study_sd", 0.2, 0, 5),
    slope_sd = number("power_slope_sd", 0, 0, 2),
    iter = number("power_iter", 2000, 500, 4000, TRUE),
    design = match.arg(input$power_design %||% "multiple_baseline", c("ab", "reversal", "multiple_baseline")),
    family = family, coefficient = match.arg(input$power_coefficient %||% "level", c("level", "trend", "slope_change")),
    intervention_goal = match.arg(input$power_goal %||% "increase", c("increase", "decrease")),
    reversal_decision = match.arg(input$power_reversal_decision %||% "all_transitions", c("all_transitions", "first_transition")),
    decision_rule = match.arg(input$power_decision_rule %||% "prob_meaningful", c("prob_meaningful", "prob_direction", "rope")),
    prob_threshold = threshold, rope = number("power_rope", 0.1, 0.001, 5),
    seed = number("power_seed", 2026, 1, .Machine$integer.max, TRUE),
    planning_target = if (target == "category") "category" else "pooled", studies_per_category = allocation,
    common_effect = if (target == "category") number("power_common_effect", 0.5, 0, 5) else 0.5)
  list(target = target, args = args)
}

.power_context <- function(input, results = NULL, running = FALSE) {
  plan <- tryCatch(.power_plan(input), error = function(e) e)
  if (inherits(plan, "error")) return(paste("Location: Power Analysis\nInvalid planning specification:", conditionMessage(plan)))
  a <- plan$args
  scope <- switch(plan$target, single = "single-study planning", pooled = "pooled meta-analysis planning", category = "category-difference planning")
  status <- if (isTRUE(running)) "running" else if (is.null(results)) "not run" else "completed"
  lines <- c("Location: Power Analysis", paste("Scope:", scope), paste("Design:", a$design),
    paste("Outcome family:", a$family), paste("Intervention goal:", a$intervention_goal),
    sprintf("Studies: %d; cases per study: %d", a$n_studies, a$n_cases),
    sprintf("Baseline observations per case: %d; intervention observations per case: %d", a$n_phase_a, a$n_phase_b),
    "Multiple baseline adds one baseline session per successive case; reversal repeats A and B phases.",
    paste("Target coefficient:", a$coefficient), paste("Effect magnitude grid:", paste(a$effect_sizes, collapse = ", ")),
    if (plan$target == "category") sprintf("Study allocation A/B: %d/%d; common category-A effect: %g; target: category B minus A interaction", a$studies_per_category[1], a$studies_per_category[2], a$common_effect),
    sprintf("AR(1): %g; ICC: %g; between-case effect SD: %g; between-study effect SD: %g", a$autocorr, a$icc, a$slope_sd, a$study_sd),
    sprintf("Decision: %s; posterior threshold: %g; meaningful-effect threshold or ROPE half-width: %g", a$decision_rule, a$prob_threshold, a$rope),
    "prob_meaningful requires benefit in the goal direction; prob_direction and rope are two-sided for a single contrast.",
    sprintf("Replicates per effect: %d; iterations: %d; seed: %d", a$n_sims, a$iter, a$seed),
    "Conditional power under fixed generating parameters; assurance is not computed.",
    "Gaussian effect magnitudes and effect SDs are in units of the marginal within-case residual SD, excluding between-case intercept variance; count effects use the log-rate scale. Ordinal planning and dependent-effect two-stage meta-analysis planning are unsupported.",
    "Analysis priors use the simulation engine defaults unless supplied through the R API; generating effects do not set analysis priors.",
    "Generating defaults: baseline mean 50; Gaussian marginal residual SD 10; negative-binomial shape 3. Count ICC parameter controls a latent log-intercept SD surrogate, not an observed count ICC.",
    "With multiple studies, intercept variance is split equally between studies and cases; effect deviations are independent across studies.",
    paste("Simulation:", status))
  if (!is.null(results)) {
    specification <- attr(results, "requested_planning_args") %||% attr(results, "planning_args")
    stale <- !is.null(specification) && !identical(specification, a)
    lines <- c(lines,
      if (!is.null(attr(results, "planning_args"))) paste("Completed-run replicates per effect:", attr(results, "planning_args")$n_sims),
      if (stale) "Results belong to a previous specification; current settings were changed.",
      paste("Last simulation results:", paste(capture.output(print(as.data.frame(results))), collapse = "\n")))
  }
  paste(lines, collapse = "\n")
}
