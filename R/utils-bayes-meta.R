# Bayesian design-comparable BC-SMD per study and Bayesian random-effects pooling

#' @noRd
.bc_smd_levels <- function(data, case_var, time_var) {
  key <- function(vars) {
    vars <- intersect(vars, names(data))
    if (!length(vars)) return(NULL)
    .std_key(data, vars, sep = ".")
  }
  study <- find_study_var(data)
  cluster <- find_cluster_var(data)
  outer_vars <- intersect(unique(c(study, cluster, case_var)), names(data))
  nest <- setdiff(tier_vars(data, time_var), outer_vars)
  participant <- key(outer_vars)
  has_cluster <- !is.null(cluster) && length(unique(stats::na.omit(data[[cluster]]))) >= 2
  if (length(nest) && has_cluster) {
    stop(paste0("bc_smd_bayes() does not support data with both a cluster column and ",
                "several series per participant (a four-level design)."), call. = FALSE)
  }
  list(tier = key(c(outer_vars, nest)), participant = participant,
       outer = if (length(nest)) participant else if (has_cluster) key(c(study, cluster)),
       kind = if (length(nest)) "replicated" else if (has_cluster) "clustered" else "basic")
}

#' @noRd
.bc_smd_code <- function(data, design, outcome_var, case_var, time_var) {
  t <- if (!is.null(time_var) && time_var %in% names(data)) as.numeric(data[[time_var]])
       else stats::ave(seq_len(nrow(data)), data[[case_var]], FUN = seq_along)
  lv <- .bc_smd_levels(data, case_var, time_var)
  role <- .phase_numeric(data[[find_phase_var(data) %||% "phase"]], t)
  keep <- role %in% c(0, 1) & is.finite(t) & is.finite(as.numeric(data[[outcome_var]]))
  cd <- data.frame(case = lv$tier[keep], participant = lv$participant[keep],
                   time = t[keep], role = role[keep],
                   outcome = as.numeric(data[[outcome_var]][keep]), stringsAsFactors = FALSE)
  if (!is.null(lv$outer)) cd$outer <- lv$outer[keep]
  cd <- cd[order(cd$case, cd$time), , drop = FALSE]
  rownames(cd) <- NULL
  # one time origin across series, so r'theta and the time_A terms share one centering
  first <- min(cd$time)
  onset_of <- function(time, role) {
    on <- time[role == 1]
    rep(if (length(on)) min(on) else NA_real_, length(time))
  }
  # last session of the first intervention-role run, before any withdrawal
  first_end_of <- function(time, role) {
    run <- cumsum(c(TRUE, role[-1] != role[-length(role)]))
    first_run <- run[role == 1][1]
    rep(if (is.na(first_run)) NA_real_ else max(time[run == first_run]), length(time))
  }
  by_case <- function(f) unsplit(Map(f, split(cd$time, cd$case), split(cd$role, cd$case)),
                                 cd$case)
  onset <- by_case(onset_of)
  cd$time_A <- cd$time - first
  cd$level_AB <- .step(cd$time, onset)
  cd$trend_AB <- .zero_na((cd$time - onset) * cd$level_AB)
  cd$first_trt_end <- by_case(first_end_of)
  cd$treatment <- as.integer(cd$role == 1)
  if (!identical(design, "reversal")) cd$treatment <- cd$level_AB
  attr(cd, "kind") <- lv$kind
  cd
}

#' @noRd
.bc_smd_formula <- function(design, trend, ar, random_effect = FALSE, outer = FALSE) {
  effect <- if (identical(design, "reversal")) "treatment" else "level_AB"
  fixed <- c(effect,
             if (trend != "none") "time_A",
             if (trend != "none" && !identical(design, "reversal")) "trend_AB")
  re_terms <- function(extra) paste(c("1", extra, if (random_effect) effect), collapse = " + ")
  random <- c(if (outer) sprintf("(%s | outer)", re_terms(NULL)),
              sprintf("(%s | case)", re_terms(if (trend == "random") "time_A")))
  paste(c(paste("outcome ~", paste(fixed, collapse = " + ")), random,
          if (ar) "ar(time = time, gr = case, p = 1)"), collapse = " + ")
}

#' @noRd
.bc_smd_priors <- function(cd, formula_str, ar) {
  out <- .default_priors(cd, stats::as.formula(formula_str), "gaussian")
  if (!ar) return(out)
  # bounds keep the AR(1) stationary so sigma^2 / (1 - phi^2) stays finite
  c(out[out$class != "ar", ], brms::prior_string("uniform(-1, 1)", class = "ar", lb = -1, ub = 1))
}

#' @noRd
.bc_smd_delta <- function(draws, effect, trend_term = NULL, trend_lag = 0, time_lag = 0) {
  col <- function(nm) if (!is.null(nm) && nm %in% names(draws)) as.numeric(draws[[nm]]) else 0
  # p'beta: treatment-vs-control difference at B for treatment starting just after A
  numerator <- col(paste0("b_", effect)) + col(if (!is.null(trend_term)) paste0("b_", trend_term)) *
    trend_lag
  sd0 <- col("sd_case__Intercept")
  sd1 <- col("sd_case__time_A")
  rho <- col("cor_case__Intercept__time_A")
  ar1 <- col("ar[1]")
  # r'theta: untreated variance at B across outer units and series plus the stationary AR(1) variance
  between <- col("sd_outer__Intercept")^2 + sd0^2 + 2 * rho * sd0 * sd1 * time_lag +
    sd1^2 * time_lag^2
  # a non-stationary draw has no finite level-1 variance
  within <- col("sigma")^2 / pmax(1 - ar1^2, 0)
  within[abs(ar1) >= 1] <- NaN
  numerator / sqrt(between + within)
}

#' Bayesian design-comparable between-case standardized mean difference
#'
#' Fits one study's multiple-baseline (multiple-probe) or treatment-reversal
#' data with a Gaussian piecewise multilevel model in \pkg{brms} and returns
#' the posterior of the design-comparable BC-SMD, computed draw by draw as
#' \eqn{\delta_{AB} = p'\beta / \sqrt{r'\theta}}.
#'
#' @details \strong{Effect size.} \eqn{\delta_{AB}} is the model-implied
#'   difference between the average outcome at follow-up session \eqn{B}
#'   when treatment is introduced just after session \eqn{A} and the average
#'   outcome at \eqn{B} without treatment, divided by the standard deviation
#'   of the outcome at \eqn{B} without treatment. With \code{intervention_goal =
#'   "decrease"} the draws are multiplied by \eqn{-1}, so a positive
#'   \eqn{\delta_{AB}} always means improvement, the orientation
#'   [meta_bayes()] expects.
#'
#'   \strong{Model.} Rows with a baseline-role or intervention-role phase
#'   are kept (maintenance and generalization rows are dropped); the tiers
#'   of [tier_vars()] are the series. For multiple-baseline data the fixed
#'   part uses the package's AB coding (see [process_scd_phases()]):
#'   \code{level_AB} for \code{trend = "none"}, plus \code{time_A} and
#'   \code{trend_AB} for \code{"fixed"} and a series-varying \code{time_A}
#'   for \code{"random"}; \code{time_A} counts sessions from the earliest
#'   session across all series, one origin as in \pkg{scdhlm}. Series carry a
#'   random intercept, and by default
#'   (\code{random_effect = TRUE}) the treatment effect varies too,
#'   \code{(1 + level_AB | case)}, matching the one-stage default; this
#'   leaves the form of \eqn{\delta_{AB}} unchanged. For reversal data a single
#'   \code{treatment} indicator (intervention role vs baseline role) is the
#'   effect; \code{"fixed"} and \code{"random"} add only the baseline time
#'   trend \code{time_A}. With \code{ar = TRUE} the residuals follow a
#'   stationary AR(1) process within series with a \code{uniform(-1, 1)}
#'   prior bounded to \eqn{[-1, 1]}.
#'
#'   \strong{Participants and series.} When a participant carries several
#'   series (a replicated multiple baseline across behaviors or settings) the
#'   model adds a participant level, \code{outer}, above the series; in a
#'   clustered multiple baseline (a cluster column with two or more clusters)
#'   \code{outer} is the cluster. The outer level carries an intercept and,
#'   with \code{random_effect = TRUE}, a varying treatment effect,
#'   \code{(1 + level_AB | outer)} (\code{(1 + treatment | outer)} for
#'   reversal data); \code{random_effect = FALSE} keeps \code{(1 | outer)}.
#'   Both intercept variances enter \eqn{r'\theta}. Data with both a cluster
#'   column and several series per participant stop. At least three participants are required; series
#'   of one participant do not count separately.
#'
#'   \strong{Numerator \eqn{p'\beta}.} The \code{level_AB} (or
#'   \code{treatment}) coefficient plus, for multiple-baseline models with a
#'   trend, the \code{trend_AB} coefficient times \eqn{B - T_B}, where
#'   \eqn{T_B} is the first observed session after \eqn{A}, matching the
#'   coding of \code{trend_AB}.
#'
#'   \strong{Denominator \eqn{r'\theta}.} Per draw, the untreated
#'   between-series variance at \eqn{B}, \eqn{\tau_0^2 + 2\tau_{02}(B - T_A) +
#'   \tau_2^2 (B - T_A)^2} (the last two terms only with random trends;
#'   \eqn{T_A} is the earliest first baseline session), the participant or
#'   cluster intercept variance when present, and the within-series
#'   variance. \pkg{brms} parameterizes AR(1) residuals by the innovation SD
#'   \code{sigma}, so the stationary residual variance is
#'   \eqn{\sigma^2 / (1 - \phi^2)}, the level-1 variance.
#'
#'   \strong{Times.} \eqn{A} is the last session of the shortest baseline
#'   across series. \code{ab_rule = "shortest_treatment"} sets \eqn{B} from
#'   the shortest treatment duration across cases;
#'   \code{scdhlm::default_times()} instead adds to \eqn{A} the treatment
#'   duration of the case(s) with that shortest baseline. With
#'   \code{ab_rule = "scdhlm"} (default) \eqn{B} follows \pkg{scdhlm}, so a
#'   comparison with its REML estimate is like for like. \code{duration}
#'   sets \eqn{B - A} directly, for a common benchmark duration across the
#'   studies of a meta-analysis; \code{time_B} overrides both.
#'   For reversal data both times refer to the first baseline phase and the
#'   first treatment phase: \eqn{B} never passes the end of the first
#'   treatment phase. With \code{trend = "none"} and no random trend,
#'   \eqn{\delta_{AB}} does not depend on \eqn{A} or \eqn{B}.
#'
#'   \strong{Uncertainty.} The REML estimator applies a small-sample
#'   correction \eqn{J}; here the posterior of \eqn{\delta_{AB}} carries the
#'   uncertainty in every parameter, so no \eqn{J} correction is applied.
#'   Draws that are not finite (for example from \eqn{|\phi| = 1}) are
#'   dropped from the summary and counted in \code{n_nonfinite}.
#'
#'   The BC-SMD rests on Gaussian errors, so only \code{family = "gaussian"}
#'   is accepted; count or proportion outcomes go through the one-stage GLMM
#'   path ([run_scd_analysis()], [marginal_contrast()]). scdbayes does not
#'   compute the within-case SMD, which is biased under autocorrelation.
#'
#' @param data One study's data with case, phase, time and outcome columns.
#' @param design \code{"multiple_baseline"}, \code{"multiple_probe"}, or
#'   \code{"reversal"}; defaults to [detect_design_type()].
#' @param time_A,time_B Sessions \eqn{A} and \eqn{B}; \code{NULL} uses the
#'   defaults in Details.
#' @param trend \code{"none"}, \code{"fixed"}, or \code{"random"} time trend.
#' @param ar Logical; AR(1) residuals within series.
#' @param family Must be \code{"gaussian"}.
#' @param priors brms priors; \code{NULL} uses the package defaults scaled to
#'   the outcome SD plus the bounded AR(1) prior.
#' @param iter,chains Sampler settings.
#' @param outcome_var Outcome column.
#' @param intervention_goal \code{"increase"} or \code{"decrease"}; with
#'   \code{"decrease"} the sign of \eqn{\delta_{AB}} is reversed.
#' @param ab_rule \code{"scdhlm"} or \code{"shortest_treatment"}; the default
#'   rule for \eqn{B} (see Details).
#' @param duration Optional \eqn{B - A} in sessions; overrides
#'   \code{ab_rule}.
#' @param random_effect Logical; let the treatment effect vary across cases
#'   (default \code{TRUE}). \code{FALSE} fits a common-effect model, useful
#'   as a sensitivity check.
#' @param ... Passed to \code{brms::brm()}.
#'
#' @return A list with \code{summary} (one-row data frame: \code{estimate}
#'   (posterior median), \code{sd}, \code{lower}, \code{upper} (95\%
#'   credible bounds)), \code{draws} (numeric vector of \eqn{\delta_{AB}}
#'   draws), \code{n_nonfinite}, \code{time_A}, \code{time_B},
#'   \code{n_cases} (participants), \code{n_series}, \code{design}, and
#'   \code{model} (the \code{brmsfit}).
#'
#' @examples
#' if (interactive()) {
#'   d <- simulate_scd_data(n_cases = 4)
#'   bc_smd_bayes(d, design = "multiple_baseline", trend = "fixed")$summary
#' }
#'
#' @export
bc_smd_bayes <- function(data, design = detect_design_type(data), time_A = NULL,
                         time_B = NULL, trend = c("none", "fixed", "random"), ar = TRUE,
                         family = "gaussian", priors = NULL, iter = 2000, chains = 4,
                         outcome_var = "outcome",
                         intervention_goal = c("increase", "decrease"),
                         ab_rule = c("scdhlm", "shortest_treatment"), duration = NULL,
                         random_effect = TRUE, ...) {
  trend <- match.arg(trend)
  intervention_goal <- match.arg(intervention_goal)
  ab_rule <- match.arg(ab_rule)
  fam <- if (is.character(family)) family else family$family %||% ""
  if (!identical(fam, "gaussian")) {
    stop(paste0("bc_smd_bayes() needs family = 'gaussian': the BC-SMD assumes Gaussian ",
                "errors. Analyze count or proportion outcomes with the one-stage GLMM path ",
                "(run_scd_analysis(), marginal_contrast())."))
  }
  if (!design %in% c("multiple_baseline", "multiple_probe", "reversal")) {
    stop(sprintf(paste0("bc_smd_bayes() supports multiple-baseline, multiple-probe, and ",
                        "reversal designs; design '%s' is not supported."), design))
  }
  case_var <- find_case_var(data)
  if (is.null(case_var) || !outcome_var %in% names(data)) {
    stop("Data need case, phase, and outcome columns.")
  }
  cd <- .bc_smd_code(data, design, outcome_var, case_var, find_time_var(data))
  n_cases <- length(unique(cd$participant))
  if (n_cases < 3) {
    stop(sprintf("bc_smd_bayes() needs at least 3 cases (participants); the data have %d.",
                 n_cases))
  }
  in_first_ab <- !is.na(cd$first_trt_end) & cd$time <= cd$first_trt_end
  timed <- if (identical(design, "reversal")) cd[in_first_ab, , drop = FALSE] else cd
  times <- .marginal_times(timed, "time", "case", time_A, time_B, ab_rule, duration)
  formula_str <- .bc_smd_formula(design, trend, ar, random_effect, "outer" %in% names(cd))
  model <- build_model(cd, formula_str, priors = priors %||% .bc_smd_priors(cd, formula_str, ar),
                       iter = iter, chains = chains, family = "gaussian", ...)
  if (is.null(model)) stop("Model fitting failed.")
  effect <- if (identical(design, "reversal")) "treatment" else "level_AB"
  draws <- .bc_smd_delta(brms::as_draws_df(model), effect,
                         trend_term = if (trend != "none" && effect == "level_AB") "trend_AB",
                         trend_lag = times$time_B - times$onset,
                         time_lag = times$time_B - times$start)
  if (intervention_goal == "decrease") draws <- -draws
  finite <- draws[is.finite(draws)]
  list(summary = .summarize_draws(finite), draws = draws,
       n_nonfinite = sum(!is.finite(draws)), time_A = times$time_A, time_B = times$time_B,
       n_cases = n_cases, n_series = length(unique(cd$case)), design = design, model = model)
}

#' @noRd
.meta_prior_scale <- function(metric = c("smd", "lrr")) {
  switch(match.arg(metric), smd = 2, lrr = 1)
}

#' @noRd
.meta_priors <- function(has_moderators, metric = "smd") {
  scale <- .meta_prior_scale(metric)
  out <- brms::prior_string(sprintf("normal(0, %.2f)", scale), class = "Intercept") +
    brms::prior_string("student_t(3, 0, 0.5)", class = "sd")
  if (has_moderators) {
    out <- out + brms::prior_string(sprintf("normal(0, %.2f)", scale), class = "b")
  }
  out
}

#' @noRd
.meta_formula <- function(yi, sei, study, case, moderators) {
  paste0(yi, " | se(", sei, ") ~ ", paste(c("1", moderators), collapse = " + "),
         " + (1 | ", study, ")", if (!is.null(case)) paste0(" + (1 | ", study, ":", case, ")"))
}

#' @noRd
.summarize_draws <- function(x) {
  q <- stats::quantile(x, c(0.5, 0.025, 0.975), names = FALSE, na.rm = TRUE)
  data.frame(estimate = q[1], sd = stats::sd(x, na.rm = TRUE), lower = q[2], upper = q[3])
}

#' @noRd
.es_from_fits <- function(fits, yi_stat) {
  if (is.null(names(fits)) || any(!nzchar(names(fits)))) {
    stop("A list of bc_smd_bayes() results must be named by study.", call. = FALSE)
  }
  stat <- switch(yi_stat, median = stats::median, mean = mean)
  finite <- lapply(fits, function(f) f$draws[is.finite(f$draws)])
  out <- data.frame(study = names(fits), yi = vapply(finite, stat, numeric(1)),
                    sei = vapply(finite, stats::sd, numeric(1)), stringsAsFactors = FALSE)
  bad <- out$study[!is.finite(out$yi) | !is.finite(out$sei)]
  if (length(bad)) {
    warning(sprintf("No usable yi or sei (fewer than 2 finite draws) for: %s.",
                    paste(bad, collapse = ", ")), call. = FALSE)
  }
  out
}

#' Bayesian random-effects meta-analysis of study effect sizes
#'
#' Pools study-level effect sizes, such as the \eqn{\delta_{AB}} posteriors of
#' [bc_smd_bayes()], in the \pkg{brms} model \code{yi | se(sei) ~ 1 + (1 |
#' study)}, adding \code{(1
#' | study:case)} when several estimates come from one study and moderators
#' when given.
#'
#' @details \strong{Orientation.} Every \code{yi} must point the same way,
#'   with positive values meaning improvement. Effects on outcomes the
#'   intervention aims to decrease need their sign reversed first
#'   ([bc_smd_bayes()] does this with \code{intervention_goal = "decrease"}).
#'
#'   \strong{Inputs.} Each study enters through a normal likelihood with
#'   known standard error, the normal-theory approximation of Bayesian
#'   meta-analysis. \code{es_data} is a data
#'   frame, or a list of [bc_smd_bayes()] results named by study; for a list,
#'   \code{yi} is the posterior median (\code{yi_stat = "median"}) or mean
#'   (\code{"mean"}) of the finite \eqn{\delta_{AB}} draws and \code{sei}
#'   their SD. This two-stage approximation has three consequences. A skewed
#'   per-study posterior is summarized by two numbers only. The per-study
#'   summaries already carry the first-stage priors of [bc_smd_bayes()], so
#'   the pooled posterior is not the posterior of one joint model. And the
#'   precision weights \eqn{1/(\tau^2 + \mathrm{sei}^2)} can correlate with
#'   the estimates, because \code{sei} grows with \eqn{|\delta_{AB}|}; such an
#'   association biases average effects and confidence interval coverage in
#'   the meta-analysis of within-case effect sizes. Pooling the full
#'   per-study draws (a mixture approach) is not implemented.
#'
#'   \strong{Priors.} The intercept and moderator slopes get a fixed,
#'   weakly informative \code{normal(0, s)} on the effect size metric, set
#'   before seeing the data: \eqn{s = 2} for standardized mean differences
#'   (\code{metric = "smd"}, including BC-SMD), which keeps 95\% of the prior
#'   mass within \eqn{\pm 4} SD units, and \eqn{s = 1} for log response
#'   ratios (\code{metric = "lrr"}), which keeps 95\% within a ratio of about
#'   0.14 to 7. Moderator slopes are per unit of the moderator, so center and
#'   scale continuous moderators. Effects on any other scale need explicit
#'   \code{priors}. Each between-study (and within-study) SD gets a
#'   half-\eqn{t_3}(0, 0.5) prior; the half-\eqn{t} family lets the data
#'   dominate when the likelihood is informative. Supply \code{priors} to
#'   override, and refit with a wider and a narrower \eqn{s} as a prior
#'   sensitivity check.
#'
#'   \strong{Convergence.} The fit is checked with
#'   [compute_convergence_diagnostics()]: R-hat < 1.01, bulk and tail ESS above
#'   100 per chain, and no divergent transitions. A
#'   warning names the failed checks.
#'
#'   \strong{Sampler.} The sampler runs with \code{adapt_delta = 0.99} and
#'   \code{max_treedepth} from the \code{scdbayes.max_treedepth} option
#'   (default 12): with few studies the posterior of \eqn{\tau} has a funnel
#'   near 0 that causes divergent transitions at the \pkg{brms} default.
#'   Entries of \code{control} replace these defaults one at a time, so
#'   \code{list(max_treedepth = 15)} keeps \code{adapt_delta = 0.99}.
#'
#'   \strong{Outputs.} The mean effect is \code{b_Intercept} (the effect at
#'   moderator values of 0). The prediction interval is the posterior
#'   predictive distribution of the true effect in a new study, \eqn{\mu +
#'   N(0, \tau^2)} per draw. When \code{case} is given the within-study SD is
#'   added, so it is the prediction for a new case (or measure) in a new study. Study effects
#'   are the shrunken study means \eqn{\mu + u_{study}} (plus moderator
#'   terms), averaged over a study's rows.
#'
#' @param es_data Data frame with one row per effect size, or a named list
#'   of [bc_smd_bayes()] results (names are the studies).
#' @param yi,sei Effect size and standard error columns.
#' @param study Study column.
#' @param case Optional column identifying several effects within a study.
#' @param moderators Optional character vector of moderator columns.
#' @param priors brms priors; \code{NULL} uses the defaults in Details.
#' @param iter,chains Sampler settings.
#' @param metric \code{"smd"} (standardized mean difference, including
#'   BC-SMD) or \code{"lrr"} (log response ratio); sets the default prior
#'   scale on the mean effect and moderator slopes.
#' @param mcid Optional minimal clinically important difference on the
#'   effect size scale; reports \eqn{\Pr(\mu > \mathrm{MCID})}.
#' @param yi_stat \code{"median"} or \code{"mean"}; the per-study posterior
#'   summary used as \code{yi} when \code{es_data} is a list of
#'   [bc_smd_bayes()] results.
#' @param control Sampler control list passed to \code{brms::brm()}, merged
#'   over the defaults \code{adapt_delta = 0.99} and \code{max_treedepth}.
#' @param ... Passed to \code{brms::brm()}.
#'
#' @return A list with \code{summary} (data frame with columns
#'   \code{quantity} (\code{"mu"}, \code{"tau"}, \code{"tau_within"} when
#'   \code{case} is given, \code{"prediction"}), \code{estimate} (posterior
#'   median), \code{sd}, \code{lower}, \code{upper}), \code{study_effects}
#'   (data frame: \code{study}, \code{estimate}, \code{sd}, \code{lower},
#'   \code{upper}), \code{pd} (probability of direction of \eqn{\mu}),
#'   \code{prob_mcid} (\eqn{\Pr(\mu > \mathrm{MCID})}, \code{NA} without
#'   \code{mcid}), \code{priors} (the priors used), \code{convergence}
#'   (list: \code{converged}, \code{max_rhat}, \code{min_ess_bulk},
#'   \code{min_ess_tail}, \code{n_divergent}), \code{diagnostics} (the
#'   [compute_convergence_diagnostics()] result), and \code{model} (the
#'   \code{brmsfit}).
#'
#' @examples
#' if (interactive()) {
#'   es <- data.frame(study = paste0("S", 1:6), yi = c(0.8, 1.2, 0.4, 1.5, 0.9, 0.6),
#'                    sei = c(0.3, 0.4, 0.35, 0.5, 0.3, 0.45))
#'   meta_bayes(es)$summary
#' }
#'
#' @export
meta_bayes <- function(es_data, yi = "yi", sei = "sei", study = "study", case = NULL,
                       moderators = NULL, priors = NULL, metric = c("smd", "lrr"),
                       iter = 4000, chains = 4, mcid = NULL, yi_stat = c("median", "mean"),
                       control = list(), ...) {
  yi_stat <- match.arg(yi_stat)
  metric <- match.arg(metric)
  if (!is.data.frame(es_data)) {
    es_data <- .es_from_fits(es_data, yi_stat)
    yi <- "yi"
    sei <- "sei"
    study <- "study"
  }
  missing_cols <- setdiff(c(yi, sei, study, case, moderators), names(es_data))
  if (length(missing_cols)) {
    stop(sprintf("Columns not found: %s.", paste(missing_cols, collapse = ", ")))
  }
  .check_syntactic(c(yi, sei, study, case, moderators))
  formula_str <- .meta_formula(yi, sei, study, case, moderators)
  backend <- getOption("scdbayes.backend", .default_backend())
  prior_used <- priors %||% .meta_priors(length(moderators) > 0, metric)
  control <- utils::modifyList(list(adapt_delta = 0.99, max_treedepth = .max_treedepth()),
                               control)
  model <- brms::brm(stats::as.formula(formula_str), data = es_data, prior = prior_used,
                     iter = iter, warmup = floor(iter / 2), chains = chains,
                     cores = min(chains, parallel::detectCores(), na.rm = TRUE), backend = backend,
                     control = control, refresh = 0, silent = 2, ...)
  diagnostics <- compute_convergence_diagnostics(model)
  convergence <- .meta_convergence(diagnostics)
  draws <- brms::as_draws_df(model)
  mu <- as.numeric(draws$b_Intercept)
  tau <- as.numeric(draws[[paste0("sd_", study, "__Intercept")]])
  within_col <- paste0("sd_", study, ":", case, "__Intercept")
  tau_within <- if (!is.null(case)) as.numeric(draws[[within_col]]) else 0
  prediction <- stats::rnorm(length(mu), mu, sqrt(tau^2 + tau_within^2))
  quantities <- list(mu = mu, tau = tau,
                     tau_within = if (!is.null(case)) tau_within,
                     prediction = prediction)
  quantities <- Filter(Negate(is.null), quantities)
  summary <- cbind(quantity = names(quantities),
                   do.call(rbind, lapply(quantities, .summarize_draws)))
  rownames(summary) <- NULL
  study_rows <- brms::posterior_epred(model, newdata = es_data,
                                      re_formula = stats::as.formula(paste0("~ (1 | ", study, ")")))
  ids <- as.character(es_data[[study]])
  study_ids <- unique(ids)
  study_effects <- do.call(rbind, lapply(study_ids, function(s) {
    cbind(study = s, .summarize_draws(rowMeans(study_rows[, ids == s, drop = FALSE])))
  }))
  list(summary = summary, study_effects = study_effects,
       pd = max(mean(mu > 0), mean(mu < 0)),
       prob_mcid = if (is.null(mcid)) NA_real_ else mean(mu > mcid),
       priors = prior_used, convergence = convergence, diagnostics = diagnostics,
       model = model)
}

#' @noRd
.meta_convergence <- function(diagnostics) {
  out <- list(converged = diagnostics$converged$converged %||% NA,
              max_rhat = .max_present(diagnostics$basic$rhat),
              min_ess_bulk = .min_present(diagnostics$basic$ess_bulk),
              min_ess_tail = .min_present(diagnostics$basic$ess_tail),
              n_divergent = diagnostics$divergence$n_divergent %||% NA_integer_,
              max_treedepth_hit = diagnostics$divergence$max_treedepth_hit %||% NA)
  if (isTRUE(out$converged)) return(out)
  lead <- if (is.na(out$converged)) {
    "Meta-analysis convergence checks could not run"
  } else {
    "Meta-analysis did not pass the convergence checks"
  }
  treedepth <- if (isTRUE(out$max_treedepth_hit)) ", transitions at max_treedepth (efficiency only)" else ""
  warning(sprintf(paste0("%s (R-hat < 1.01, bulk and tail ESS > 100 per chain, no divergences): ",
                         "max R-hat %.4f, min bulk ESS %.0f, ",
                         "min tail ESS %.0f, %s divergent transitions%s. Raise iter, adapt_delta, ",
                         "or max_treedepth, or supply tighter priors on tau."),
                  lead, out$max_rhat, out$min_ess_bulk, out$min_ess_tail,
                  format(out$n_divergent), treedepth),
          call. = FALSE)
  return(out)
}
