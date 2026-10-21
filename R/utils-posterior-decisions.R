# Posterior decision quantities on link-scale brms draws

#' @noRd
.decision_summary <- function(draws, threshold, rope,
                              intervention_goal, ci_level) {
  draws <- as.numeric(draws)
  draws <- draws[is.finite(draws)]
  if (length(draws) == 0) {
    out <- list(mean = NA_real_, sd = NA_real_,
                lower = NA_real_, upper = NA_real_, pd = NA_real_,
                mean_native = NA_real_, median_native = NA_real_,
                lower_native = NA_real_, upper_native = NA_real_)
    if (!is.null(threshold)) out$prob_meaningful_change <- NA_real_
    if (!is.null(rope)) out$rope_prob <- NA_real_
    return(out)
  }
  alpha <- (1 - ci_level) / 2
  # native (unreflected) bounds feed the conventional IRR/OR ratio columns
  q_native <- stats::quantile(draws, c(alpha, 1 - alpha), names = FALSE, na.rm = TRUE)
  mean_native <- mean(draws, na.rm = TRUE)
  median_native <- stats::median(draws, na.rm = TRUE)
  if (intervention_goal == "decrease") {
    draws <- -draws
    if (!is.null(rope) && length(rope) == 2) rope <- c(-rope[2], -rope[1])
  }
  q <- stats::quantile(draws, c(alpha, 1 - alpha), names = FALSE, na.rm = TRUE)
  out <- list(
    mean  = mean(draws, na.rm = TRUE),
    sd    = stats::sd(draws, na.rm = TRUE),
    lower = q[1],
    upper = q[2],
    pd    = max(mean(draws > 0, na.rm = TRUE), mean(draws < 0, na.rm = TRUE)),
    mean_native   = mean_native,
    median_native = median_native,
    lower_native  = q_native[1],
    upper_native  = q_native[2]
  )
  if (!is.null(threshold) && length(threshold) == 1) {
    out$prob_meaningful_change <- mean(draws >= threshold, na.rm = TRUE)
  }
  if (!is.null(rope) && length(rope) == 2) {
    out$rope_prob <- mean(draws >= rope[1] & draws <= rope[2], na.rm = TRUE)
  }
  out
}

#' @noRd
.is_withdrawal_term <- function(term, nm = term) {
  if (grepl("^(b_)?(level|trend)_ret[0-9]+$", term)) return(TRUE)
  roles <- .step_roles(term, nm)
  isTRUE(roles[1] >= 1 && roles[2] == 0)
}

#' @noRd
.term_goal <- function(term, intervention_goal, nm = term, data = NULL) {
  direction <- attr(data[[sub("^b_", "", term)]], "expected_direction")
  if (length(direction) > 1) stop("Criterion direction differs across tiers; summarize separate tier models.")
  if (length(direction)) return(direction)
  if (.is_withdrawal_term(term, nm)) return(setdiff(c("increase", "decrease"), intervention_goal))
  intervention_goal
}

.coefficient_goal <- function(model, term, intervention_goal, nm = term) {
  goal <- .term_goal(term, intervention_goal, nm, model$data)
  link <- model$family$link %||% "identity"
  if (link %in% c("inverse", "1/mu^2", "inverse_square")) {
    return(setdiff(c("increase", "decrease"), goal))
  }
  goal
}

#' @noRd
.is_slope_term <- function(term) grepl("^(b_)?(trend|time)_", term)

#' Default region of practical equivalence for treatment coefficients
#'
#' On identity links (gaussian, student, skew_normal) the region is +/- 0.1
#' SD of the outcome, and on logit links +/- 0.1 * pi / sqrt(3) (about
#' 0.18), a tenth of the SD of the standard logistic distribution. On log
#' links the region is +/- log(1.1), a 10\% change in the rate or mean.
#' All defaults are conventions, not substantive thresholds; supply a
#' context-specific ROPE when one is known.
#' Other links require explicit bounds on their own scale.
#'
#' @param data Data frame with the response column.
#' @param family brms family name or family object.
#' @param response Response column name.
#' @return Numeric length-2 vector \code{c(lower, upper)} on the link scale.
#' @examples
#' default_rope(simulate_scd_data(n_cases = 3), "gaussian")
#' default_rope(simulate_scd_data(n_cases = 3), "poisson")
#' @export
default_rope <- function(data, family = "gaussian", response = "outcome") {
  fam <- if (is.character(family)) brms::brmsfamily(family) else family
  link <- fam$link %||% "identity"
  if (link == "log") return(c(-1, 1) * log(1.1))
  if (link == "logit") return(c(-0.1, 0.1) * pi / sqrt(3))
  if (link != "identity") stop("Supply explicit ROPE bounds for this link scale.")
  c(-0.1, 0.1) * stats::sd(data[[response]], na.rm = TRUE)
}

#' @noRd
.tier_phase_data <- function(data) {
  phase_var <- find_phase_var(data)
  if (!is.null(phase_var) && phase_var != "phase") data$phase <- data[[phase_var]]
  steps <- grep("^(level_|atd_|treatment$)", names(data), value = TRUE)
  steps <- steps[vapply(steps, function(v) is.numeric(data[[v]]), logical(1))]
  phase <- if ("phase" %in% names(data)) as.character(data$phase)
           else if (length(steps)) do.call(paste, unname(data[steps]))
  if (is.null(phase)) return(data)
  key <- .std_key(data, tier_vars(data))
  # a reversal repeats labels (A, B, A, B); each run within a tier is its own phase, except in an ATD
  if ("phase" %in% names(data) && !identical(detect_design_type(data), "alternating")) {
    time_var <- find_time_var(data)
    t <- if (!is.null(time_var)) as.numeric(data[[time_var]]) else seq_len(nrow(data))
    ord <- order(key, t)
    lab <- ifelse(is.na(phase[ord]), "\r", phase[ord])
    k <- key[ord]
    run <- cumsum(c(TRUE, lab[-1] != lab[-length(lab)] | k[-1] != k[-length(k)]))
    phase[ord] <- paste(phase[ord], run)
  }
  data$phase <- paste(key, phase, sep = "\r")
  data
}

#' @noRd
.decision_row <- function(summ, leading = NULL) {
  row <- data.frame(
    mean  = summ$mean,  sd    = summ$sd,
    lower = summ$lower, upper = summ$upper,
    pd    = summ$pd,
    prob_meaningful_change = summ$prob_meaningful_change %||% NA_real_,
    rope_prob = summ$rope_prob %||% NA_real_,
    mean_native   = summ$mean_native   %||% NA_real_,
    median_native = summ$median_native %||% NA_real_,
    lower_native  = summ$lower_native  %||% NA_real_,
    upper_native  = summ$upper_native  %||% NA_real_,
    stringsAsFactors = FALSE
  )
  if (!is.null(leading)) row <- cbind(leading, row, stringsAsFactors = FALSE)
  row
}

#' @noRd
.case_random_effects <- function(model, case_var) {
  re <- tryCatch(brms::ranef(model, summary = FALSE),
                 error = function(e) NULL)
  if (is.null(re) || length(re) == 0) return(NULL)
  fixed <- tryCatch(rownames(brms::fixef(model)), error = function(e) character(0))
  comps <- strsplit(names(re), ":", fixed = TRUE)
  has_case <- vapply(comps, function(x) case_var %in% x, logical(1)) | length(re) == 1
  if (!any(has_case)) return(NULL)
  # the tier grouping that carries the treatment slopes, innermost first (case:behavior over case)
  slopes <- vapply(re, function(a) any(setdiff(dimnames(a)[[3]], "Intercept") %in% fixed),
                   logical(1))
  cand <- which(has_case)
  group <- names(re)[cand[order(!slopes[cand], -lengths(comps)[cand])][1]]
  list(group = group, draws = re[[group]], offsets = .outer_offsets(model, re, group))
}

#' @noRd
.outer_offsets <- function(model, re, group) {
  d <- model$data
  inner <- strsplit(group, ":", fixed = TRUE)[[1]]
  if (!all(inner %in% names(d))) return(list())
  # brms labels the levels of an a:b grouping "<a>_<b>"
  lab <- function(v) do.call(paste, c(unname(lapply(d[v], as.character)), sep = "_"))
  parts <- stats::setNames(strsplit(names(re), ":", fixed = TRUE), names(re))
  outer <- names(Filter(function(p) all(p %in% inner), parts))
  outer <- setdiff(outer, group)
  lapply(stats::setNames(outer, outer), function(g) {
    map <- unique(data.frame(inner = lab(inner), outer = lab(parts[[g]]),
                             stringsAsFactors = FALSE))
    list(draws = re[[g]], map = stats::setNames(map$outer, map$inner))
  })
}

#' @noRd
.unit_draws <- function(fixed_draws, re, j, term) {
  id <- dimnames(re$draws)[[2]][j]
  Reduce(function(acc, o) {
    lvl <- unname(o$map[id])
    hit <- !is.na(lvl) && lvl %in% dimnames(o$draws)[[2]] && term %in% dimnames(o$draws)[[3]]
    if (hit) acc + o$draws[, lvl, term] else acc
  }, re$offsets, fixed_draws[, term] + re$draws[, j, term])
}

#' @noRd
.response_scale_ratio <- function(model) {
  link <- tryCatch(model$family$link, error = function(e) NULL) %||% "identity"
  fam  <- tryCatch(model$family$family, error = function(e) NULL) %||% ""
  # lognormal mu is the log-scale location, so exp(b) is a ratio of medians
  if (identical(fam, "lognormal")) return("median_ratio")
  # in hurdle and zero-inflated families "irr" is the ratio on the conditional
  # count component, not on the marginal expectation
  log_ratios <- c(gaussian = "mean_ratio", student = "mean_ratio", skew_normal = "mean_ratio",
                  gamma = "mean_ratio", Gamma = "mean_ratio", weibull = "mean_ratio", exponential = "mean_ratio",
                  poisson = "irr", negbinomial = "irr", geometric = "irr",
                  zero_inflated_poisson = "irr", zero_inflated_negbinomial = "irr",
                  hurdle_poisson = "irr", hurdle_negbinomial = "irr")
  switch(link,
    log   = if (fam %in% names(log_ratios)) unname(log_ratios[fam]) else NULL,
    logit = "or",
    NULL)
}

#' @noRd
.attach_response_ratio <- function(out, model) {
  ratio <- .response_scale_ratio(model)
  if (!is.null(ratio)) {
    # exp(median) and exp(quantiles) are transformation-invariant on the native coefficient
    out[[ratio]]                   <- exp(out$median_native)
    out[[paste0(ratio, "_lower")]] <- exp(out$lower_native)
    out[[paste0(ratio, "_upper")]] <- exp(out$upper_native)
    # exp(b) holds random effects at zero: a typical-case ratio, not a population average
    out$ratio_type <- "conditional"
  }
  out[c("mean_native", "median_native", "lower_native", "upper_native")] <- NULL
  out
}

#' Posterior decision summary for fixed-effect coefficients
#'
#' For each fixed-effect coefficient (`b_*`) in the model, returns
#' posterior mean, SD, credible interval, probability of direction (`pd`),
#' and optional probability of meaningful change and ROPE-probability.
#' Posteriors are on the link scale (Stan parameterization).
#'
#' @param model A fitted `brmsfit`.
#' @param threshold Numeric. A positive magnitude expressed on the
#'   improvement-oriented scale (i.e., after any `decrease` reflection). If
#'   supplied, the posterior probability that the effect meets or exceeds
#'   this magnitude in the goal direction is reported as
#'   `prob_meaningful_change`. It applies to level terms only: slope terms
#'   (`trend_*`, `time_*`) get `NA`, since a per-session slope is meaningful
#'   or not only relative to the number of sessions it is projected over.
#' @param rope Numeric length-2. ROPE bounds on the link scale, expressed on
#'   the native coefficient scale. For `intervention_goal = "decrease"` the
#'   bounds are reflected together with the draws, so an asymmetric ROPE
#'   stays attached to the same parameter region. If supplied, posterior
#'   probability inside the ROPE is reported as `rope_prob` for level terms;
#'   slope terms get `NA`. [default_rope()] gives the default region.
#' @param intervention_goal `"increase"` (default) or `"decrease"`.
#'   For `"decrease"`, every reported coefficient is sign-flipped so that a
#'   positive value denotes improvement (full reflection onto an
#'   improvement-oriented scale); `pd` is unchanged and the ROPE is reflected
#'   with the draws. Reversal withdrawal steps (an intervention-role phase
#'   followed by a baseline-role phase, e.g. `level_B1A2`,
#'   `trend_Intervention1Baseline2`) remove the treatment, so they take the opposite
#'   reflection: under `"increase"` they are sign-flipped and under
#'   `"decrease"` they are not. A positive value on every row therefore reads
#'   as change in the direction the intervention predicts (improvement at
#'   onset, return toward baseline at withdrawal). Intercept and ordinal
#'   threshold terms (`b_Intercept`) and coefficients of distributional
#'   parameters (`b_sigma_*`, `b_nu_*`, `b_phi_*`, `b_shape_*`, `b_zi_*`,
#'   `b_hu_*`, `b_disc_*`, ...) are excluded from the table, so only
#'   treatment, trend, and moderator slopes on the mean are reported.
#' @param ci_level Credible interval width (default 0.95).
#' @return A data frame with one row per fixed-effect term and columns
#'   `term`, `mean`, `sd`, `lower`, `upper`, `pd`,
#'   `prob_meaningful_change`, `rope_prob`. For a log link on a count
#'   family, the response-scale incidence-rate ratio columns `irr`,
#'   `irr_lower`, `irr_upper` are attached; for a log-link Gamma (or
#'   Weibull/exponential) family, the ratio of means `mean_ratio`; for a
#'   lognormal family, the ratio of medians `median_ratio`; for a logit link,
#'   the odds-ratio columns `or`, `or_lower`, `or_upper`. Each with its
#'   `_lower`/`_upper` bounds. These ratios are `exp()` of the native
#'   (unreflected) coefficient, so they carry the conventional definition
#'   regardless of `intervention_goal`; only the `mean`/`lower`/`upper`
#'   columns are reflected for direction. An identity link attaches nothing,
#'   since link scale equals response scale. When a ratio is attached, a
#'   `ratio_type` column reads `"conditional"`: on a nonlinear link,
#'   `exp(b)` is the ratio for a typical case (all random effects at zero),
#'   not the population-averaged (marginal) ratio, and the two differ when
#'   random effects are present. Label these columns as typical-case ratios;
#'   [marginal_contrast()] gives the population-averaged ratio. In a hurdle or
#'   zero-inflated family the `irr` columns are a ratio on the conditional
#'   count component only, not on the marginal expectation, which also moves
#'   with the hurdle or zero-inflation part.
#' @examples
#' if (interactive()) {
#'   dat <- simulate_scd_data()
#'   fit <- brms::brm(outcome ~ phase + (1 | case), data = dat, refresh = 0)
#'   summarize_fixed_effect_decisions(fit, threshold = 0.5,
#'                                    rope = c(-0.1, 0.1))
#' }
#' @export
summarize_fixed_effect_decisions <- function(model,
                                             threshold = NULL,
                                             rope = NULL,
                                             intervention_goal = c("increase", "decrease"),
                                             ci_level = 0.95) {
  intervention_goal <- match.arg(intervention_goal)
  draws <- brms::as_draws_df(model)
  b_cols <- grep("^b_", names(draws), value = TRUE)
  # mu coefficients only: distributional-parameter and threshold terms are not effects
  b_cols <- b_cols[!grepl("^b_(Intercept|(sigma|nu|phi|shape|zi|hu|disc|zoi|coi|ndt|bs|alpha)_)", b_cols)]
  if (length(b_cols) == 0) return(data.frame())
  terms <- sub("^b_", "", b_cols)
  rows <- lapply(terms, function(term) {
    slope <- .is_slope_term(term)
    summ <- .decision_summary(draws[[paste0("b_", term)]],
                              if (slope) NULL else threshold, if (slope) NULL else rope,
                              .coefficient_goal(model, term, intervention_goal, terms), ci_level)
    .decision_row(summ, leading = data.frame(term = term, stringsAsFactors = FALSE))
  })
  out <- do.call(rbind, rows)
  .attach_response_ratio(out, model)
}

#' Posterior summary of variance components
#'
#' Reports posterior mean, SD, and credible interval for each
#' random-effect standard deviation (`sd_*`), each random-effect
#' correlation (`cor_*`), and each auxiliary distributional parameter
#' (`sigma`, `nu`, `phi`, `shape`, `alpha`).
#'
#' @param model A fitted `brmsfit`.
#' @param ci_level Credible interval width (default 0.95).
#' @return A data frame with columns `parameter`, `type` (one of
#'   `"random_effect_sd"`, `"random_effect_cor"`, `"auxiliary"`),
#'   `mean`, `sd`, `lower`, `upper`.
#' @examples
#' if (interactive()) {
#'   dat <- simulate_scd_data()
#'   fit <- brms::brm(outcome ~ phase + (1 + phase | case),
#'                    data = dat, refresh = 0)
#'   summarize_variance_components(fit)
#' }
#' @export
summarize_variance_components <- function(model, ci_level = 0.95) {
  draws <- brms::as_draws_df(model)
  alpha <- (1 - ci_level) / 2
  cols <- c(
    grep("^sd_",  names(draws), value = TRUE),
    grep("^cor_", names(draws), value = TRUE),
    intersect(c("sigma", "nu", "phi", "shape", "alpha"), names(draws))
  )
  if (length(cols) == 0) return(data.frame())
  rows <- lapply(cols, function(col) {
    v <- as.numeric(draws[[col]])
    v <- v[is.finite(v)]
    if (length(v) == 0) return(NULL)
    q <- stats::quantile(v, c(alpha, 1 - alpha), names = FALSE, na.rm = TRUE)
    type <- if (grepl("^sd_",  col)) "random_effect_sd"
            else if (grepl("^cor_", col)) "random_effect_cor"
            else "auxiliary"
    data.frame(parameter = col, type = type,
               mean = mean(v, na.rm = TRUE), sd = stats::sd(v, na.rm = TRUE),
               lower = q[1], upper = q[2],
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

#' Per-case posterior decision summary
#'
#' For each unit and each fixed-effect term that has a corresponding
#' unit-level random effect, returns the unit-specific posterior
#' (`b + u`) summarized with decision quantities, on the link scale.
#'
#' @details The unit is the grouping factor that contains \code{case_var}
#'   and carries random slopes on fixed-effect terms, the innermost one when
#'   several do: with the default structure \code{(1 | case) + (1 + level_AB
#'   | case:behavior)} the units are case-behavior tiers, labeled
#'   \code{"<case>_<behavior>"} as brms labels them. Random effects of every
#'   enclosing grouping (\code{study}, \code{study:case}, \code{case}) on the
#'   same term are added, so the unit posterior is the full
#'   \code{b + u_study + u_case + u_tier}.
#'
#' @inheritParams summarize_fixed_effect_decisions
#' @param case_var Name of the case-level grouping column in the model
#'   (default `"case"`).
#' @return A data frame with columns `case` (the unit label), `term`, `mean`, `sd`,
#'   `lower`, `upper`, `pd`, `prob_meaningful_change`, `rope_prob`, plus
#'   the response-scale ratio columns of
#'   [summarize_fixed_effect_decisions()] on a log or logit link. Those are
#'   `exp(b + u)`, the ratio conditional on that case's random effects, and
#'   carry `ratio_type = "conditional"`; they are not population-averaged
#'   ratios (see [marginal_contrast()]).
#' @examples
#' if (interactive()) {
#'   dat <- simulate_scd_data()
#'   fit <- brms::brm(outcome ~ phase + (1 + phase | case),
#'                    data = dat, refresh = 0)
#'   summarize_per_case_effects(fit, threshold = 0.5)
#' }
#' @export
summarize_per_case_effects <- function(model,
                                       case_var = "case",
                                       threshold = NULL,
                                       rope = NULL,
                                       intervention_goal = c("increase", "decrease"),
                                       ci_level = 0.95) {
  intervention_goal <- match.arg(intervention_goal)
  fixed_draws <- tryCatch(brms::fixef(model, summary = FALSE),
                          error = function(e) NULL)
  re <- .case_random_effects(model, case_var)
  if (is.null(fixed_draws) || is.null(re)) return(data.frame())
  case_ids <- dimnames(re$draws)[[2]]
  # a case's baseline level is not a treatment effect, so it gets no decision quantities
  terms <- setdiff(intersect(dimnames(re$draws)[[3]], colnames(fixed_draws)), "Intercept")
  terms <- terms[!grepl("^Intercept\\[", terms)]
  grid <- expand.grid(j = seq_along(case_ids), term = terms, stringsAsFactors = FALSE)
  rows <- Map(function(j, term) {
    case_draws <- .unit_draws(fixed_draws, re, j, term)
    slope <- .is_slope_term(term)
    summ <- .decision_summary(case_draws, if (slope) NULL else threshold,
                              if (slope) NULL else rope,
                              .coefficient_goal(model, term, intervention_goal, colnames(fixed_draws)),
                              ci_level)
    .decision_row(summ, leading = data.frame(case = case_ids[j], term = term,
                                             stringsAsFactors = FALSE))
  }, grid$j, grid$term)
  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  .attach_response_ratio(out, model)
}

#' Posterior probability of consistent intervention direction across cases
#'
#' For each fixed-effect term with a corresponding unit-level random
#' effect, returns the posterior probability that the unit-specific
#' effect (`b` plus the random effects of the unit and every enclosing
#' grouping, as in [summarize_per_case_effects()]) points in the expected
#' direction for every unit, from the joint posterior of the unit effects.
#'
#' @inheritParams summarize_per_case_effects
#' @param intervention_goal `"increase"` (default) or `"decrease"`. The
#'   expected direction of a term is the goal direction, reversed for
#'   reversal withdrawal steps (see [summarize_fixed_effect_decisions()]).
#' @return A data frame with columns `term`, `prob_all_positive`,
#'   `prob_all_negative` (both on the native coefficient scale), and
#'   `prob_consistent`, the probability that every case changes in the
#'   expected direction (`prob_all_positive` when that direction is an
#'   increase, `prob_all_negative` when it is a decrease).
#' @examples
#' if (interactive()) {
#'   dat <- simulate_scd_data()
#'   fit <- brms::brm(outcome ~ phase + (1 + phase | case),
#'                    data = dat, refresh = 0)
#'   summarize_cross_case_consistency(fit)
#' }
#' @export
summarize_cross_case_consistency <- function(model,
                                             case_var = "case",
                                             ci_level = 0.95,
                                             intervention_goal = c("increase", "decrease")) {
  intervention_goal <- match.arg(intervention_goal)
  fixed_draws <- tryCatch(brms::fixef(model, summary = FALSE),
                          error = function(e) NULL)
  re <- .case_random_effects(model, case_var)
  if (is.null(fixed_draws) || is.null(re)) return(data.frame())
  case_ids <- dimnames(re$draws)[[2]]
  terms    <- dimnames(re$draws)[[3]]
  terms    <- terms[!grepl("^Intercept(\\[|$)", terms)]
  rows <- lapply(terms, function(term) {
    if (!term %in% colnames(fixed_draws)) return(NULL)
    n_draws <- nrow(fixed_draws)
    case_matrix <- vapply(seq_along(case_ids), function(j) .unit_draws(fixed_draws, re, j, term),
                          numeric(n_draws))
    p_pos <- mean(rowMeans(case_matrix > 0) == 1, na.rm = TRUE)
    p_neg <- mean(rowMeans(case_matrix < 0) == 1, na.rm = TRUE)
    goal <- .coefficient_goal(model, term, intervention_goal, colnames(fixed_draws))
    data.frame(term = term,
               prob_all_positive = p_pos,
               prob_all_negative = p_neg,
               prob_consistent   = if (goal == "increase") p_pos else p_neg,
               stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

# Response-scale phase contrasts: secondary to the link-scale summaries above.

#' Pre-model descriptive phase summary
#'
#' Raw-scale descriptive summary of a two-phase contrast (typically
#' phase A vs B). Reports means, SDs, sample sizes per phase and the
#' raw mean difference on the response scale. No standardized effect
#' sizes; use [compute_phase_contrast()] on a fitted brmsfit for
#' posterior decision quantities.
#'
#' @param data A data frame.
#' @param phase_var Phase column name.
#' @param outcome_var Outcome column name.
#' @return A list with `raw_difference`, `phase_a_mean`, `phase_b_mean`,
#'   `phase_a_sd`, `phase_b_sd`, `phase_a_n`, `phase_b_n`. NA fields
#'   when phases or outcome are not usable.
#' @examples
#' summarize_phase_descriptives(simulate_scd_data(n_cases = 2))
#' @export
summarize_phase_descriptives <- function(data, phase_var = "phase",
                                         outcome_var = "outcome") {
  na_out <- list(raw_difference = NA_real_,
                 phase_a_mean = NA_real_, phase_b_mean = NA_real_,
                 phase_a_sd = NA_real_, phase_b_sd = NA_real_,
                 phase_a_n = 0L, phase_b_n = 0L)
  if (!phase_var %in% names(data) || !outcome_var %in% names(data)) return(na_out)
  phases <- unique(data[[phase_var]])
  if (length(phases) < 2) return(na_out)
  if ("time" %in% names(data)) {
    phase_start <- tapply(data[["time"]], data[[phase_var]], min, na.rm = TRUE)
    phases <- names(sort(phase_start))
  }
  a <- as.numeric(data[[outcome_var]][data[[phase_var]] == phases[1]])
  b <- as.numeric(data[[outcome_var]][data[[phase_var]] == phases[2]])
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (length(a) == 0 || length(b) == 0) return(na_out)
  list(
    raw_difference = mean(b) - mean(a),
    phase_a_mean   = mean(a),
    phase_b_mean   = mean(b),
    phase_a_sd     = if (length(a) > 1) stats::sd(a) else NA_real_,
    phase_b_sd     = if (length(b) > 1) stats::sd(b) else NA_real_,
    phase_a_n      = length(a),
    phase_b_n      = length(b)
  )
}

#' @noRd
.select_reference_rows <- function(rows, at_session, case_var, time_var) {
  if (at_session == "mean") return(rows)
  vars <- unique(c(tier_vars(rows, time_var), intersect(case_var, names(rows))))
  key <- .std_key(rows, vars)
  time <- if (time_var %in% names(rows)) rows[[time_var]] else seq_len(nrow(rows))
  ord <- order(key, time, na.last = FALSE)
  rows[ord[!duplicated(key[ord], fromLast = TRUE)], , drop = FALSE]
}

#' @noRd
.summarize_contrast_draws <- function(draws, threshold, rope, ci_level) {
  draws <- as.numeric(draws)
  draws <- draws[is.finite(draws)]
  if (length(draws) == 0) {
    return(data.frame(mean = NA_real_, sd = NA_real_,
                      lower = NA_real_, upper = NA_real_,
                      pd = NA_real_, stringsAsFactors = FALSE))
  }
  alpha <- (1 - ci_level) / 2
  q <- stats::quantile(draws, c(alpha, 1 - alpha), names = FALSE, na.rm = TRUE)
  out <- data.frame(
    mean  = mean(draws, na.rm = TRUE),
    sd    = stats::sd(draws, na.rm = TRUE),
    lower = q[1],
    upper = q[2],
    pd    = max(mean(draws > 0, na.rm = TRUE), mean(draws < 0, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
  if (!is.null(threshold) && length(threshold) == 1) {
    out$prob_meaningful_change <- mean(draws >= threshold, na.rm = TRUE)
  }
  if (!is.null(rope) && length(rope) == 2) {
    out$rope_prob <- mean(draws >= rope[1] & draws <= rope[2], na.rm = TRUE)
  }
  out
}

#' Posterior phase contrast on the response scale
#'
#' Computes the posterior distribution of an end-of-phase contrast
#' (e.g., phase B vs phase A) on the outcome's response scale using
#' `brms::posterior_epred`. Supported for families with a scalar mean on
#' the response scale (gaussian, student, skew_normal, poisson,
#' negbinomial, zero_inflated_poisson, bernoulli, beta, Gamma). Ordinal
#' and categorical families (cumulative, sratio, cratio, acat,
#' categorical) are **not** supported, since a response-scale mean
#' contrast is not well defined for them; read those effects on the link
#' scale via [summarize_fixed_effect_decisions()]. Designs with more than
#' two phases (alternating-treatment, changing-criterion) require explicit
#' `from`/`to` labels.
#'
#' Reference rows are drawn from the fitted data. With
#' `at_session = "last"`, the last session per case in each phase is
#' used; with `at_session = "mean"`, all sessions in each phase
#' contribute and are averaged per draw.
#'
#' The population contrast is a typical-case contrast of observed rows: it
#' predicts with `re_formula = NA` (every random effect at zero) and
#' compares rows from the two phases, which sit at different sessions. On a
#' nonlinear link, the expected outcome at zero random effects is not the
#' population average, and the two sides differ
#' in time as well as condition. [marginal_contrast()] gives the
#' population-averaged contrast at one common session, averaging over new
#' cases drawn from the random-effect distribution with and without
#' treatment. The per-case contrasts include each case's random effects.
#'
#' @param model A fitted `brmsfit`.
#' @param data Data frame; defaults to `model$data`.
#' @param from Character. Reference phase for the baseline side of the
#'   contrast (default `"A"`).
#' @param to Character. Reference phase for the treatment side of the
#'   contrast (default `"B"`).
#' @param phase_var Name of the phase column (default `"phase"`).
#' @param case_var Name of the case column (default `"case"`).
#' @param time_var Name of the time column (default `"time"`).
#' @param at_session `"last"` (last session of each phase, default) or
#'   `"mean"` (average across all sessions of each phase).
#' @param per_case Logical; include per-case contrasts (default `TRUE`).
#' @param threshold Numeric. A positive magnitude on the
#'   improvement-oriented scale (i.e., after any `decrease` reflection). If
#'   supplied, posterior probability that the contrast meets or exceeds this
#'   clinically meaningful change is reported as `prob_meaningful_change`.
#' @param rope Numeric length-2. ROPE bounds on the response scale. For
#'   `intervention_goal = "decrease"` the bounds are reflected together with
#'   the contrast, so an asymmetric ROPE stays attached to the same region.
#'   If supplied, posterior probability inside the ROPE is reported as
#'   `rope_prob`.
#' @param intervention_goal `"increase"` (default) or `"decrease"`.
#'   When `"decrease"`, the contrast sign is flipped so positive values
#'   denote improvement, and the ROPE is reflected with it.
#' @param ci_level Credible interval width (default 0.95).
#' @return A list with:
#'   \describe{
#'     \item{`population_contrast`}{One-row data frame: posterior mean,
#'       SD, lower / upper credible bounds, probability of direction,
#'       optional `prob_meaningful_change` and `rope_prob`.}
#'     \item{`per_case_contrast`}{Data frame of per-case contrasts with
#'       the same columns plus `case`. `NULL` if `per_case = FALSE`.}
#'     \item{`draws`}{Numeric vector of posterior draws for the
#'       population contrast.}
#'     \item{`metadata`}{Arguments recorded for downstream reporting.}
#'   }
#' @examples
#' if (interactive()) {
#'   dat <- simulate_scd_data()
#'   fit <- brms::brm(outcome ~ phase + (1 | case), data = dat, refresh = 0)
#'   compute_phase_contrast(fit, threshold = 5, rope = c(-1, 1))
#' }
#' @export
compute_phase_contrast <- function(model,
                                   data = NULL,
                                   from = "A",
                                   to = "B",
                                   phase_var = "phase",
                                   case_var = "case",
                                   time_var = "time",
                                   at_session = c("last", "mean"),
                                   per_case = TRUE,
                                   threshold = NULL,
                                   rope = NULL,
                                   intervention_goal = c("increase", "decrease"),
                                   ci_level = 0.95) {
  at_session <- match.arg(at_session)
  intervention_goal <- match.arg(intervention_goal)
  if (is.null(data)) data <- model$data
  if (!phase_var %in% names(data)) {
    stop(sprintf("Phase column '%s' not found in data.", phase_var))
  }
  fam <- tryCatch(model$family$family, error = function(e) NULL) %||% "unknown"
  if (fam %in% c("cumulative", "categorical", "sratio", "cratio", "acat")) {
    stop(sprintf(paste0("compute_phase_contrast() does not support the '%s' family; ",
                        "a response-scale mean contrast is not well defined for ordinal ",
                        "or categorical outcomes. Read these effects on the link scale ",
                        "with summarize_fixed_effect_decisions()."), fam))
  }
  phase_levels <- unique(stats::na.omit(data[[phase_var]]))
  if (length(phase_levels) > 2 && missing(from) && missing(to)) {
    stop(sprintf(paste0("Phase column '%s' has more than two levels (%s). For ",
                        "alternating-treatment or changing-criterion designs, pass ",
                        "explicit 'from' and 'to' phase labels."),
                 phase_var, paste(phase_levels, collapse = ", ")))
  }
  sign_flip <- if (intervention_goal == "decrease") -1 else 1
  rope_use <- if (sign_flip < 0 && !is.null(rope) && length(rope) == 2) {
    c(-rope[2], -rope[1])
  } else {
    rope
  }

  rows_from <- data[data[[phase_var]] == from, , drop = FALSE]
  rows_to   <- data[data[[phase_var]] == to,   , drop = FALSE]
  if (nrow(rows_from) == 0 || nrow(rows_to) == 0) {
    stop(sprintf("Phase '%s' or '%s' not present in data.", from, to))
  }

  ref_from <- .select_reference_rows(rows_from, at_session, case_var, time_var)
  ref_to   <- .select_reference_rows(rows_to,   at_session, case_var, time_var)

  epred_from <- brms::posterior_epred(model, newdata = ref_from,
                                      re_formula = NA, allow_new_levels = TRUE)
  epred_to   <- brms::posterior_epred(model, newdata = ref_to,
                                      re_formula = NA, allow_new_levels = TRUE)

  pop_draws <- sign_flip * (rowMeans(epred_to) - rowMeans(epred_from))
  population_contrast <- .summarize_contrast_draws(pop_draws, threshold, rope_use, ci_level)

  per_case_contrast <- NULL
  if (per_case && case_var %in% names(data)) {
    vars <- unique(c(tier_vars(data, time_var), case_var))
    from_key <- .std_key(rows_from, vars)
    to_key <- .std_key(rows_to, vars)
    cases <- intersect(unique(from_key), unique(to_key))
    case_summaries <- lapply(cases, function(cid) {
      rf <- rows_from[from_key == cid, , drop = FALSE]
      rt <- rows_to[to_key == cid, , drop = FALSE]
      if (nrow(rf) == 0 || nrow(rt) == 0) return(NULL)
      pf <- .select_reference_rows(rf, at_session, case_var, time_var)
      pt <- .select_reference_rows(rt, at_session, case_var, time_var)
      ef <- tryCatch(brms::posterior_epred(model, newdata = pf, re_formula = NULL),
                     error = function(e) NULL)
      et <- tryCatch(brms::posterior_epred(model, newdata = pt, re_formula = NULL),
                     error = function(e) NULL)
      if (is.null(ef) || is.null(et)) return(NULL)
      diff_draws <- sign_flip * (rowMeans(et) - rowMeans(ef))
      summ <- .summarize_contrast_draws(diff_draws, threshold, rope_use, ci_level)
      identity <- rf[1, vars, drop = FALSE]
      identity$case <- if (length(vars) == 1) as.character(rf[[case_var]][1]) else cid
      cbind(identity, summ, stringsAsFactors = FALSE)
    })
    case_summaries <- Filter(Negate(is.null), case_summaries)
    if (length(case_summaries) > 0) {
      per_case_contrast <- do.call(rbind, case_summaries)
    }
  }

  list(
    population_contrast = population_contrast,
    per_case_contrast   = per_case_contrast,
    draws               = pop_draws,
    metadata = list(
      from = from, to = to, phase_var = phase_var,
      case_var = case_var, time_var = time_var,
      at_session = at_session,
      intervention_goal = intervention_goal,
      threshold = threshold, rope = rope, ci_level = ci_level,
      family = model$family$family %||% "unknown"
    )
  )
}

