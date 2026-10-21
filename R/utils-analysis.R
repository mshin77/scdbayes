#' Print method for scd_analysis objects
#'
#' @param x An scd_analysis object.
#' @param ... Additional arguments (ignored).
#'
#' @return x invisibly.
#'
#' @keywords internal
#' @export
print.scd_analysis <- function(x, ...) {
  cat(sprintf("Bayesian SCD Analysis Results\n\nCases: %d\nObservations: %d\nApproximate power (design-effect t approximation): %.1f%%\n",
              x$data_summary$n_cases, x$data_summary$n_observations, x$power$power * 100))

  if (!is.null(x$effects)) {
    cat(sprintf("\nPhase descriptives:\n  Raw mean difference (B - A): %.3f\n",
                x$effects$raw_difference))
    if (!is.null(x$effects$phase_a_mean)) {
      cat(sprintf("  Phase A mean: %.3f (n = %d)\n",
                  x$effects$phase_a_mean, x$effects$phase_a_n))
    }
    if (!is.null(x$effects$phase_b_mean)) {
      cat(sprintf("  Phase B mean: %.3f (n = %d)\n",
                  x$effects$phase_b_mean, x$effects$phase_b_n))
    }
  }

  if (!is.null(x$diagnostics)) {
    cat(sprintf("\nConverged: %s\n", x$diagnostics$converged))
  }

  invisible(x)
}

#' Detect hierarchy levels in SCD data
#'
#' @param data A data frame.
#'
#' @return A list describing detected levels: n_levels, level variables, and
#'   unit counts.
#'
#' @examples
#' d <- data.frame(case = rep(c("A", "B"), each = 5), time = rep(1:5, 2),
#'                 outcome = rnorm(10))
#' detect_hierarchy_levels(d)
#'
#' @export
detect_hierarchy_levels <- function(data) {
  levels_detected <- list(
    n_levels = 2,
    level1_var = "time",
    level2_var = "case",
    level3_var = NULL,
    level4_var = NULL,
    structure = "two_level"
  )

  t_hit <- find_time_var(data)
  if (!is.null(t_hit)) levels_detected$level1_var <- t_hit

  c_hit <- find_case_var(data)
  if (!is.null(c_hit)) levels_detected$level2_var <- c_hit

  cluster_hit <- find_cluster_var(data)
  if (!is.null(cluster_hit) && length(unique(data[[cluster_hit]])) >= 2) {
    levels_detected$level3_var <- cluster_hit
    levels_detected$n_levels <- 3
    levels_detected$structure <- "three_level"
  }

  study_hit <- find_study_var(data)
  if (!is.null(study_hit) && length(unique(data[[study_hit]])) >= 2) {
    if (is.null(levels_detected$level3_var)) {
      levels_detected$level3_var <- study_hit
      levels_detected$n_levels <- 3
      levels_detected$structure <- "three_level_no_cluster"
    } else {
      levels_detected$level4_var <- study_hit
      levels_detected$n_levels <- 4
      levels_detected$structure <- "four_level"
    }
  }

  levels_detected$n_level1 <- nrow(data)
  levels_detected$n_level2 <- length(unique(data[[levels_detected$level2_var]]))

  if (!is.null(levels_detected$level3_var)) {
    levels_detected$n_level3 <- length(unique(data[[levels_detected$level3_var]]))
  }
  if (!is.null(levels_detected$level4_var)) {
    levels_detected$n_level4 <- length(unique(data[[levels_detected$level4_var]]))
  }

  return(levels_detected)
}

#' Detect SCD design type
#'
#' Phase sequences are built per tier (see [tier_vars()]) from time-ordered
#' run lengths (\code{rle}), so repeated phases such as A-B-A-B are preserved
#' rather than collapsed to A-B, and a case identifier repeated across studies
#' is read as separate series.
#'
#' @details Checks run in this order: alternating treatments (two or more
#'   conditions alternating in short runs after an optional baseline phase),
#'   changing criterion with a numeric criterion column that changes two or
#'   more times, up or down, between adjacent intervention sessions of a
#'   tier (so A-B-B-B-A-B with stepped criteria is not read as a reversal),
#'   reversal (three or more runs alternating between baseline-role and
#'   intervention-role phases, with Withdrawal, Return, Reversal, or a
#'   numbered baseline in the baseline role and Reinstatement in the
#'   intervention role), sequential non-recurring designs (A-B, A-B-M,
#'   A-B1-B2-M, A-B-M-G with onsets that differ across tiers), and changing
#'   criterion (four or more conditions
#'   after the first whose labels carry the intervention role or no known
#'   role, such as B1/B2/B3, possibly returning to an earlier criterion).
#'   Maintenance or generalization phases rule out changing criterion: a
#'   single A-B-M-G series is \code{"ab"}.
#'
#' @param data A data frame with case and phase columns.
#'
#' @return A character string: "ab", "reversal", "multiple_baseline",
#'   "alternating", "changing_criterion", or "unknown".
#'
#' @examples
#' d <- data.frame(
#'   case = rep(c("C1", "C2"), each = 10),
#'   phase = rep(c("A", "B"), 10),
#'   time = rep(1:10, 2),
#'   outcome = rnorm(20)
#' )
#' detect_design_type(d)
#'
#' @export
detect_design_type <- function(data) {
  case_var <- find_case_var(data)
  phase_var <- find_phase_var(data)

  if (is.null(case_var) || is.null(phase_var)) return("unknown")

  data <- data[!.is_blank(data[[phase_var]]), , drop = FALSE]
  if (!nrow(data)) return("unknown")
  time_var <- find_time_var(data)
  phase_col <- as.character(data[[phase_var]])
  vars <- tier_vars(data, time_var)
  key <- .std_factor(data, vars)

  tiers <- lapply(split(seq_len(nrow(data)), key), function(idx) {
    ord <- if (!is.null(time_var)) idx[order(data[[time_var]][idx])] else idx
    r <- rle(phase_col[ord])
    onset <- if (is.null(time_var) || length(r$values) < 2) NA_real_
             else as.numeric(data[[time_var]][ord][r$lengths[1] + 1])
    list(runs = r, role = .phase_numeric(r$values, seq_along(r$values)),
         known = phase_role(r$values), onset = onset)
  })
  n_conditions <- vapply(tiers, function(x) length(unique(x$runs$values)), integer(1))
  n_runs <- vapply(tiers, function(x) length(x$runs$values), integer(1))

  # ATD: 2+ conditions alternating in short runs, after an optional baseline phase
  is_atd <- vapply(tiers, function(x) {
    r <- x$runs
    lead_bl <- isTRUE(x$known[1] == 0) && r$lengths[1] > 2
    vals <- if (lead_bl) r$values[-1] else r$values
    lens <- if (lead_bl) r$lengths[-1] else r$lengths
    length(unique(vals)) >= 2 && length(vals) >= 6 && mean(lens <= 2) >= 0.75
  }, logical(1))
  # a strict majority of tiers must alternate
  if (mean(is_atd) > 0.5) return("alternating")

  crit_col <- .criterion_col(data)
  # two or more criterion changes (up or down) between adjacent intervention sessions mark
  # criterion subphases; a criterion constant within each tier (one goal per tier) has none
  crit_steps <- !is.null(crit_col) && any(vapply(split(seq_len(nrow(data)), key), function(idx) {
    ord <- if (!is.null(time_var)) idx[order(data[[time_var]][idx])] else idx
    tx <- !phase_role(phase_col[ord]) %in% 0
    cr <- data[[crit_col]][ord]
    sum(tx[-1] & tx[-length(tx)] & cr[-1] != cr[-length(cr)], na.rm = TRUE) >= 2
  }, logical(1)))
  if (crit_steps) return("changing_criterion")

  # adjacent intervention-role runs with different labels are criterion subphases, not a withdrawal
  label_steps <- vapply(tiers, function(x) {
    k <- x$known
    any(k[-1] %in% 1 & k[-length(k)] %in% 1)
  }, logical(1))

  # reversal / withdrawal: >= 3 runs alternating baseline vs intervention roles
  is_reversal <- vapply(tiers, function(x) {
    on_off <- rle(x$role >= 1)$values
    length(on_off) >= 3 && !anyNA(on_off)
  }, logical(1)) & !label_steps
  if (any(is_reversal)) return("reversal")

  # sequential A-B(-M-G) with staggered onsets; a fourth phase must carry a known role
  is_sequential <- vapply(tiers, function(x) {
    n <- length(x$runs$values)
    # roles never fall (A-B1-B2, A-B1-B2-M); four phases need an M or G role, so A-B1-B2-B3 stays a CCD
    roles_ok <- isTRUE(x$role[1] == 0) && isFALSE(is.unsorted(x$role)) &&
      (n <= 3 || !anyNA(x$known) && max(x$role) >= 2)
    n >= 2 && n <= 4 && n == length(unique(x$runs$values)) && roles_ok
  }, logical(1))
  onsets <- vapply(tiers, `[[`, numeric(1), "onset")
  if (all(is_sequential) && length(tiers) >= 2 &&
      length(unique(stats::na.omit(onsets))) > 1) {
    return("multiple_baseline")
  }

  # criterion subphases share the intervention role or carry unknown labels; M/G roles mean A-B-M-G
  criterion_roles <- vapply(tiers, function(x) !any(x$known %in% c(2, 3)), logical(1)) &
    (vapply(tiers, function(x) all(x$known[-1] %in% c(1, NA)), logical(1)) | label_steps)
  # returning to an earlier criterion repeats a label, so runs may exceed distinct labels
  is_ccd <- n_runs >= 4 & n_conditions >= 4 & !is_atd & criterion_roles
  if (all(is_ccd)) return("changing_criterion")

  return("ab")
}

#' Analyze design patterns
#'
#' @param data A data frame with case and phase columns.
#'
#' @return A list with phase_patterns, temporal_structure, baseline_length,
#'   classification_logic, statistical_implications, and recommendations.
#'
#' @examples
#' d <- data.frame(
#'   case = rep(c("C1", "C2"), each = 10),
#'   phase = rep(c("A", "B"), 10),
#'   time = rep(1:10, 2),
#'   outcome = rnorm(20)
#' )
#' analyze_design_patterns(d)
#'
#' @export
analyze_design_patterns <- function(data) {
  if (!all(c("case", "phase") %in% names(data))) {
    return(list(
      confidence = 0.3,
      phase_patterns = "Unknown - missing required columns",
      temporal_structure = "Cannot analyze",
      baseline_length = "N/A",
      classification_logic = "Insufficient data structure for analysis.",
      statistical_implications = list("Data structure too thin for reliable analysis"),
      stat_agent_rec = "Ensure case, phase, and outcome variables are properly defined.",
      method_agent_rec = "Review data format and column naming conventions.",
      validity_agent_rec = "Current data structure may compromise internal validity assessment.",
      optim_agent_rec = "Consider data restructuring before proceeding with analysis."
    ))
  }

  time_sort <- intersect(c("session", "time"), names(data))
  if (length(time_sort) > 0) {
    data <- dplyr::arrange(data, case, !!rlang::sym(time_sort[1]))
  }

  # run-length sequence preserves phase repeats (A-B-A-B), unlike unique()
  phase_analysis <- data %>%
    group_by(case) %>%
    summarise(
      phases = paste(rle(as.character(phase))$values, collapse = "-"),
      n_phases = dplyr::n_distinct(phase),
      n_obs = dplyr::n(),
      phase_sequence = paste(phase, collapse = ""),
      .groups = "drop"
    )

  pattern_consistency <- length(unique(phase_analysis$phases)) == 1
  obs_adequacy <- all(phase_analysis$n_obs >= 6)

  common_pattern <- names(sort(table(phase_analysis$phases), decreasing = TRUE))[1]
  phase_patterns <- sprintf("%s (in %d/%d cases)",
                            common_pattern,
                            sum(phase_analysis$phases == common_pattern),
                            nrow(phase_analysis))

  has_time <- any(c("time", "session") %in% names(data))
  is_staggered <- FALSE
  if (has_time) {
    time_var <- if ("session" %in% names(data)) "session" else "time"
    if (length(unique(data$phase)) >= 2) {
      intervention_phase <- unique(data$phase)[2]
      intervention_starts <- data %>%
        filter(phase == intervention_phase) %>%
        group_by(case) %>%
        summarise(start_time = min(!!rlang::sym(time_var)), .groups = "drop")
      is_staggered <- length(unique(intervention_starts$start_time)) > 1
      temporal_structure <- if (is_staggered) "Staggered baseline (multiple baseline design indicators)"
                            else "Concurrent phase changes"
    } else {
      temporal_structure <- "Single phase data"
    }

    if ("A" %in% data$phase || "baseline" %in% data$phase) {
      baseline_phase <- if ("A" %in% data$phase) "A" else "baseline"
      baseline_lengths <- data %>%
        filter(phase == baseline_phase) %>%
        group_by(case) %>%
        summarise(baseline_n = dplyr::n(), .groups = "drop")
      baseline_length <- sprintf("Mean: %.1f sessions (range: %d-%d)",
                                 mean(baseline_lengths$baseline_n),
                                 min(baseline_lengths$baseline_n),
                                 max(baseline_lengths$baseline_n))
    } else {
      baseline_length <- "No clear baseline phase identified"
    }
  } else {
    temporal_structure <- "No time variable detected"
    baseline_length <- "Cannot calculate without time data"
  }

  n_cases <- length(unique(data$case))
  n_phases <- length(unique(data$phase))

  if (n_phases == 2 && pattern_consistency) {
    classification_logic <- sprintf(
      "Detected 2-phase design across %d cases with consistent %s pattern. This suggests a simple AB comparison design suitable for basic treatment effect analysis.",
      n_cases, common_pattern
    )
  } else if (grepl("A-B-A", common_pattern)) {
    classification_logic <- sprintf(
      "Reversal pattern detected (%s) suggesting ABAB design. This allows for demonstration of experimental control through phase reversals.",
      common_pattern
    )
  } else if (is_staggered) {
    classification_logic <- "Staggered intervention timing across cases suggests multiple baseline design with systematic replication across tiers."
  } else {
    classification_logic <- sprintf(
      "Complex or inconsistent phase patterns detected. %d unique patterns across %d cases may indicate alternating treatment or mixed design elements.",
      length(unique(phase_analysis$phases)), n_cases
    )
  }

  statistical_implications <- list()
  if (n_cases < 5) {
    statistical_implications <- append(statistical_implications,
                                       "Small sample size (< 5 cases) may limit generalizability and power")
  }
  if (n_phases == 2) {
    statistical_implications <- append(statistical_implications,
                                       "Two-phase design allows for level and trend analysis")
  }
  if (pattern_consistency) {
    statistical_implications <- append(statistical_implications,
                                       "Consistent phase patterns support pooled analysis across cases")
  } else {
    statistical_implications <- append(statistical_implications,
                                       "Inconsistent patterns may require case-specific modeling approaches")
  }

  stat_agent_rec <- sprintf(
    "With %d cases and %s pattern, recommend %s analysis approach. Report family-native posterior decisions (probability of direction, ROPE, probability of meaningful change).",
    n_cases, common_pattern,
    if (n_cases >= 5) "multilevel Bayesian" else "individual case-level"
  )

  method_agent_rec <- sprintf(
    "Design classification: %s. %s Ensure adequate baseline stability before intervention phases.",
    detect_design_type(data),
    if (!pattern_consistency) "Inconsistent phase patterns across cases; verify design coding." else ""
  )

  validity_agent_rec <- if (has_time && is_staggered) {
    "Staggered baselines provide experimental control. Monitor for history and maturation threats."
  } else if (grepl("A-B-A", common_pattern)) {
    "Reversal design supports internal validity through experimental control demonstration. Assess reversibility of target behavior."
  } else {
    "Consider strengthening internal validity: add baseline phases, implement systematic replication, or use multiple baseline across cases."
  }

  optim_agent_rec <- if (!pattern_consistency || !obs_adequacy) {
    "Verify phase coding and ensure adequate observations per phase (>= 6)."
  } else if (n_cases < 5) {
    "Consider expanding to at least 5 cases for multilevel modeling."
  } else {
    "Proceed with Bayesian hierarchical modeling to account for case-level heterogeneity."
  }

  list(
    phase_patterns = phase_patterns,
    temporal_structure = temporal_structure,
    baseline_length = baseline_length,
    classification_logic = classification_logic,
    statistical_implications = statistical_implications,
    stat_agent_rec = stat_agent_rec,
    method_agent_rec = method_agent_rec,
    validity_agent_rec = validity_agent_rec,
    optim_agent_rec = optim_agent_rec
  )
}

#' Recommend distribution family based on outcome data
#'
#' @param outcome_data A numeric vector of outcome values.
#'
#' @return A list with recommended family, reasoning, and optional warning.
#'
#' @examples
#' recommend_distribution_family(c(0, 1, 0, 1, 1, 0))
#' recommend_distribution_family(rnorm(50))
#' recommend_distribution_family(rpois(50, lambda = 3))
#'
#' @export
recommend_distribution_family <- function(outcome_data) {
  clean_data <- outcome_data[!is.na(outcome_data)]

  if (length(clean_data) == 0) {
    return(list(
      recommended = "Gaussian (default)",
      reasoning = "No valid data points available for analysis",
      warning = "Check for missing data issues"
    ))
  }

  n_unique <- length(unique(clean_data))
  min_val <- min(clean_data)
  max_val <- max(clean_data)
  mean_val <- mean(clean_data)
  sd_val <- stats::sd(clean_data)

  is_binary <- n_unique == 2 && all(clean_data %in% c(0, 1))
  is_proportion <- all(clean_data >= 0 & clean_data <= 1) && n_unique > 10
  is_count <- all(clean_data >= 0) && all(clean_data == floor(clean_data))
  is_positive_continuous <- all(clean_data > 0) && !is_count

  skewness_val <- if (sd_val > 0) moments::skewness(clean_data) else 0
  is_skewed <- abs(skewness_val) > 1
  is_heavy_tailed <- moments::kurtosis(clean_data) > 4

  if (is_binary) {
    list(recommended = "Bernoulli (binary)",
         reasoning = sprintf("Binary outcome with values %s. Use Bernoulli (logistic) link.",
                             paste(sort(unique(clean_data)), collapse = ", ")),
         warning = NULL)
  } else if (is_proportion && min_val > 0 && max_val < 1) {
    list(recommended = "Beta (proportion)",
         reasoning = sprintf("Continuous proportions (range: %.3f-%.3f). Beta distribution handles bounded data well.",
                             min_val, max_val),
         warning = if (any(clean_data == 0 | clean_data == 1)) "Consider zero-one-inflated beta for boundary values")
  } else if (is_count) {
    overdispersion <- stats::var(clean_data) / mean_val
    if (overdispersion > 1.2) {
      list(recommended = "Negative Binomial",
           reasoning = sprintf("Count data (range: %d-%d) with overdispersion (%.2f). Negative binomial handles excess variance.",
                               as.integer(min_val), as.integer(max_val), overdispersion),
           warning = NULL)
    } else {
      list(recommended = "Poisson (count)",
           reasoning = sprintf("Count data (range: %d-%d) with mean %.2f. Low overdispersion suggests Poisson is appropriate.",
                               as.integer(min_val), as.integer(max_val), mean_val),
           warning = if (overdispersion > 1.5) "Monitor for potential overdispersion")
    }
  } else if (is_positive_continuous) {
    if (is_skewed && skewness_val > 1) {
      list(recommended = "Gamma (positive)",
           reasoning = sprintf("Positive continuous data with right skew (%.2f). Gamma distribution appropriate for positive, skewed outcomes.",
                               skewness_val),
           warning = NULL)
    } else {
      list(recommended = "Gaussian (continuous)",
           reasoning = sprintf("Positive continuous data with low skew (%.2f). Gaussian with log-link could work; Gamma is a heavier-tailed alternative.",
                               skewness_val),
           warning = "Consider log transformation or Gamma family")
    }
  } else if (is_heavy_tailed || abs(skewness_val) > 0.8) {
    list(recommended = "Student-t (heavy-tailed)",
         reasoning = sprintf("Heavy tails (kurtosis: %.2f) or skewness (%.2f). Student-t tolerates outliers.",
                             moments::kurtosis(clean_data), skewness_val),
         warning = NULL)
  } else if (is_skewed && skewness_val != 0) {
    list(recommended = "Skew-normal",
         reasoning = sprintf("Moderate skewness detected (%.2f). Skew-normal accommodates asymmetric distributions.",
                             skewness_val),
         warning = NULL)
  } else {
    list(recommended = "Gaussian (continuous)",
         reasoning = sprintf("Approximately normal distribution (skew: %.2f, n=%d). Gaussian family is appropriate.",
                             skewness_val, length(clean_data)),
         warning = if (n_unique < 10) "Few unique values - consider ordinal modeling")
  }
}

#' Run sensitivity analysis with different priors and outlier handling
#'
#' @details The prior arms refit the model with the treatment-coefficient
#'   prior at the adaptive default SD (see [generate_adaptive_priors()]), at
#'   half of it (narrow), and at twice it (diffuse), each centered on 0 and
#'   targeted at the treatment coefficient. For a model with group-level
#'   terms the \code{prior_sd_small} arm keeps the base priors but sets every
#'   group-level SD prior to \code{student_t(3, 0, s / sqrt(10))}, where
#'   \code{s} is the default SD scale: a tenfold smaller variance scale, for
#'   the small individual-variation case where the variance priors drive the
#'   sensitivity. The outlier arm keeps every observation and refits with a
#'   Student-t likelihood, whose heavy tails downweight outlying points
#'   instead of deleting them; it runs only for Gaussian models, since a
#'   Student-t base fit already has that likelihood.
#'
#' @param data A data frame; with \code{model}, defaults to \code{model$data}.
#' @param base_model_formula Formula string or formula object for the base
#'   model; ignored when \code{model} is supplied.
#' @param vary_priors Logical; test different prior widths.
#' @param vary_outliers Logical; add a Student-t refit.
#' @param family brms family name of the base model (default
#'   \code{"gaussian"}).
#' @param priors Optional priors of the fitted base model, as a
#'   \code{brmsprior} or as the list returned by [assemble_brms_priors()];
#'   prior arms replace only its treatment-coefficient prior (or its SD
#'   priors for \code{prior_sd_small}).
#' @param iter,chains Sampler settings for each refit.
#' @param model Optional fitted base \code{brmsfit}. Each arm then refits it
#'   with \code{update(model, prior = ...)} (and \code{family = student()}
#'   for the outlier arm), which keeps its full \code{brms::bf()} formula,
#'   including distributional submodels such as \code{sigma ~ phase}; its
#'   formula, family, and user-set priors replace \code{base_model_formula},
#'   \code{family}, and \code{priors}. Without \code{model}, each arm is
#'   fitted from \code{data} and the mean formula only.
#'
#' @return A named list of \code{brms::fixef()} matrices, one per arm
#'   (\code{prior_default}, \code{prior_narrow}, \code{prior_diffuse},
#'   \code{prior_sd_small}, \code{student_t}), with attributes
#'   \code{treatment_sd} (the default treatment-prior SD \code{s} of the
#'   arms) and \code{treatment_coef} (the targeted coefficient).
#'
#' @examples
#' if (interactive()) {
#' dat <- process_scd_phases(simulate_scd_data(), list(), "AB")
#' run_sensitivity_analysis(dat, "outcome ~ time_A + level_AB + trend_AB + (1 | case)")
#' fit <- brms::brm(brms::bf(outcome ~ time_A + level_AB + trend_AB + (1 | case),
#'                           sigma ~ phase), data = dat, refresh = 0)
#' run_sensitivity_analysis(model = fit)
#' }
#'
#' @export
run_sensitivity_analysis <- function(data = NULL, base_model_formula = NULL,
                                     vary_priors = TRUE, vary_outliers = TRUE,
                                     family = "gaussian", priors = NULL,
                                     iter = 1000, chains = 2, model = NULL) {
  if (!is.null(model)) {
    data <- data %||% model$data
    base_model_formula <- stats::formula(model)$formula
    family <- model$family$family
    priors <- priors %||% model$prior[nzchar(model$prior$prior), ]
  }
  if (is.null(data) || is.null(base_model_formula)) {
    stop("run_sensitivity_analysis() needs `data` and `base_model_formula`, or a fitted `model`.",
         call. = FALSE)
  }
  formula_str <- if (inherits(base_model_formula, "formula"))
    paste(deparse(base_model_formula), collapse = " ") else base_model_formula
  refit <- function(p, fam) {
    if (is.null(model)) {
      return(build_model(data, formula_str, priors = p, iter = iter, chains = chains, family = fam))
    }
    # update() keeps distributional submodels such as sigma ~ phase
    args <- c(list(model, prior = p, iter = iter, chains = chains, refresh = 0, silent = 2),
              if (!identical(fam, family)) list(family = brms::brmsfamily(fam)))
    tryCatch(do.call(stats::update, args), error = function(e) {
      warning("Sensitivity refit failed: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  }
  fit <- function(p, fam) {
    m <- refit(p, fam)
    if (is.null(m)) NULL else brms::fixef(m)
  }
  if (is.list(priors) && !inherits(priors, "brmsprior")) priors <- priors_to_brms(priors)
  base_priors <- priors %||% .default_priors(data, stats::as.formula(formula_str), family)
  coef <- .treatment_coef(data, formula_str)
  response <- all.vars(stats::as.formula(formula_str))[1]
  default_sd <- generate_adaptive_priors(data, list(), model_type = family,
                                         outcome_var = response)$treatment$sd

  prior_arm <- function(mult) {
    arm <- brms::prior_string(sprintf("normal(0, %.4f)", mult * default_sd),
                              class = "b", coef = coef %||% "")
    keep <- !(base_priors$class == "b" & base_priors$coef == (coef %||% "") &
              base_priors$dpar == "")
    c(base_priors[keep, ], arm)
  }
  sd_arm <- function() {
    sd_y <- stats::sd(data[[response]], na.rm = TRUE)
    scale <- if (.identity_family(family) && is.finite(sd_y) && sd_y > 0) sd_y else 2.5
    c(base_priors[base_priors$class != "sd", ],
      brms::prior_string(sprintf("student_t(3, 0, %.4f)", scale / sqrt(10)), class = "sd"))
  }
  results <- list()
  if (vary_priors) {
    arms <- c(default = 1, narrow = 0.5, diffuse = 2)
    results[paste0("prior_", names(arms))] <- lapply(arms, function(k) fit(prior_arm(k), family))
    if (.has_group_term(formula_str)) results$prior_sd_small <- fit(sd_arm(), family)
  }
  # a Student-t base fit already has the heavy-tailed likelihood
  if (vary_outliers && identical(family, "gaussian")) {
    results$student_t <- fit(base_priors, "student")
  }
  out <- Filter(Negate(is.null), results)
  attr(out, "treatment_sd") <- default_sd
  attr(out, "treatment_coef") <- coef
  out
}
