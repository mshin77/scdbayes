# Population-averaged (marginal) counterfactual contrast for AB and multiple-baseline fits

#' @noRd
.marginal_tiers <- function(data, time_var, case_var) {
  vars <- unique(c(case_var, intersect(tier_vars(data, time_var), names(data))))
  vars <- intersect(vars, names(data))
  if (!length(vars)) return(factor(rep(1, nrow(data))))
  .std_factor(data, vars)
}

#' @noRd
.marginal_times <- function(data, time_var = NULL, case_var = NULL,
                            time_A = NULL, time_B = NULL,
                            ab_rule = c("scdhlm", "shortest_treatment"), duration = NULL) {
  ab_rule <- match.arg(ab_rule)
  t <- if (!is.null(time_var) && time_var %in% names(data)) as.numeric(data[[time_var]])
       else as.numeric(data$time_A)
  treated <- as.numeric(data$level_AB) >= 1
  ok <- is.finite(t) & !is.na(treated)
  tier <- .marginal_tiers(data, time_var, case_var)[ok]
  t <- t[ok]
  treated <- treated[ok]
  last_base <- tapply(t[!treated], tier[!treated], max)
  last_obs <- tapply(t, tier, max)
  span <- tapply(t[treated], tier[treated], function(x) diff(range(x)) + 1)
  both <- intersect(intersect(names(last_base)[is.finite(last_base)], names(last_obs)),
                    names(span)[is.finite(span)])
  if (!length(both)) stop("No tier has both baseline and treatment sessions.")
  A <- time_A %||% min(last_base[both])
  shortest <- both[last_base[both] == min(last_base[both])]
  rule_B <- switch(ab_rule,
    shortest_treatment = A + min(last_obs[both] - last_base[both]),
    scdhlm = A + min(span[shortest]))
  B <- time_B %||% if (!is.null(duration)) A + duration else rule_B
  if (B <= A) stop("time_B must be later than time_A.")
  later <- sort(unique(t[t > A]))
  onset <- if (length(later)) min(later[1], B) else A + 1
  start <- min(t - as.numeric(data$time_A)[ok])
  list(time_A = A, time_B = B, onset = onset, start = start)
}

#' @noRd
.counterfactual_rows <- function(template, group_vars, n_new, times, time_var = NULL) {
  # each new case borrows one observed row, so other covariates average over the data
  pick <- if (nrow(template) == 1) rep(1L, n_new)
          else sample.int(nrow(template), n_new, replace = TRUE)
  rows <- template[rep(pick, 2), , drop = FALSE]
  rownames(rows) <- NULL
  arm <- rep(0:1, each = n_new)
  phase_var <- find_phase_var(template)
  if (!is.null(phase_var)) rows[[phase_var]] <- .arm_phase(template[[phase_var]], arm)
  B <- times$time_B
  set_col <- function(df, col, value) {
    if (col %in% names(df)) df[[col]] <- value
    df
  }
  rows <- set_col(rows, "time_A", B - times$start)
  rows <- set_col(rows, "level_AB", arm)
  rows <- set_col(rows, "trend_AB", arm * (B - times$onset))
  rows <- set_col(rows, "time_B", B - times$onset)
  rows <- set_col(rows, "intervention_start", times$onset)
  if (!is.null(time_var)) rows <- set_col(rows, time_var, B)
  later <- intersect(c("level_BM", "trend_BM", "time_M", "level_BG", "trend_BG", "time_G"),
                     names(rows))
  rows[later] <- 0
  # each new case carries the same new level in both arms, so the contrast is within case
  new_id <- paste0("new_case_", rep(seq_len(n_new), 2))
  rows[intersect(group_vars, names(rows))] <- new_id
  rows$.arm <- arm
  rows
}

#' @noRd
.arm_phase <- function(phase, arm) {
  role <- phase_role(phase)
  label <- function(r) phase[which(role == r)[1]]
  if (anyNA(c(label(0), label(1)))) return(phase[rep(1, length(arm))])
  out <- ifelse(arm == 1, as.character(label(1)), as.character(label(0)))
  if (is.factor(phase)) factor(out, levels = levels(phase)) else out
}

#' @noRd
.model_ar_spec <- function(model) {
  f <- model$formula
  if (inherits(f, "brmsformula")) f <- f$formula
  if (!inherits(f, "formula")) stop("AR mode cannot be established from the fitted formula.", call. = FALSE)
  collect <- function(x) {
    if (!is.call(x)) return(list())
    head <- x[[1]]
    name <- if (is.symbol(head)) as.character(head) else
      if (is.call(head) && identical(head[[1]], as.name("::"))) as.character(head[[3]]) else ""
    if (name %in% c("ar", "arma")) return(list(x))
    unlist(lapply(as.list(x)[-1], collect), recursive = FALSE)
  }
  terms <- c(collect(f), collect(attr(f, "autocor")))
  if (!length(terms)) {
    if (!is.null(attr(f, "autocor"))) stop("AR mode cannot be established from the fitted formula.", call. = FALSE)
    return(NULL)
  }
  if (length(terms) != 1L) stop("AR mode requires a single autocorrelation term.", call. = FALSE)
  term <- terms[[1]]
  head <- term[[1]]
  name <- if (is.symbol(head)) as.character(head) else as.character(head[[3]])
  fun <- getExportedValue("brms", name)
  args <- as.list(match.call(fun, term))[-1]
  defaults <- formals(fun)
  spec <- list(p = args$p %||% defaults$p, q = args$q %||% defaults$q %||% 0L,
               cov = args$cov %||% defaults$cov)
  valid <- is.numeric(spec$p) && length(spec$p) == 1L && is.finite(spec$p) &&
    is.numeric(spec$q) && length(spec$q) == 1L && is.finite(spec$q) &&
    is.logical(spec$cov) && length(spec$cov) == 1L && !is.na(spec$cov)
  if (!valid) stop("AR mode requires explicit numeric orders and a logical covariance flag.", call. = FALSE)
  if (spec$p != 1L || spec$q != 0L)
    stop("Residual integration supports AR(1) without MA terms only.", call. = FALSE)
  spec
}

#' @noRd
.latent_ar_factor <- function(model, ndraws_idx) {
  dr <- brms::as_draws_df(model)
  if (!"sderr" %in% names(dr)) return(NULL)
  spec <- .model_ar_spec(model)
  if (is.null(spec)) stop("AR mode cannot be established from the fitted formula.", call. = FALSE)
  if (!"ar[1]" %in% names(dr) || length(setdiff(grep("^(ar|ma)\\[", names(dr), value = TRUE), "ar[1]")) > 0L)
    stop("Latent residual integration supports AR(1) without MA terms only.", call. = FALSE)
  sderr <- as.numeric(dr$sderr)[ndraws_idx]
  phi <- as.numeric(dr[["ar[1]"]])[ndraws_idx]
  variance <- if (spec$cov) sderr^2 / (1 - phi^2) else sderr^2 * (1 + phi^2)
  ok <- is.finite(sderr) & is.finite(phi) & abs(phi) < 1
  ifelse(ok, exp(variance / 2), NA_real_)
}

#' @noRd
.model_group_vars <- function(model) {
  groups <- tryCatch(names(brms::ngrps(model)), error = function(e) NULL)
  unique(unlist(strsplit(groups %||% character(0), ":", fixed = TRUE)))
}

#' @noRd
.marginal_summary <- function(draws, cols) {
  do.call(rbind, lapply(cols, function(col) {
    q <- stats::quantile(draws[[col]], c(0.5, 0.025, 0.975), names = FALSE, na.rm = TRUE)
    data.frame(quantity = col, median = q[1], lower = q[2], upper = q[3],
               stringsAsFactors = FALSE)
  }))
}

#' Population-averaged counterfactual contrast at a common session
#'
#' For each posterior draw, predicts the expected outcome of \code{n_new}
#' new cases drawn from the fitted random-effect distribution at follow-up
#' session \eqn{B}, once with treatment introduced just after session
#' \eqn{A} and once without treatment, and averages over the new cases. The
#' result is the population-averaged (marginal) treatment contrast at a
#' single session, in contrast to the typical-case (conditional) contrasts
#' of [summarize_fixed_effect_decisions()] and [compute_phase_contrast()],
#' which hold random effects at zero. On a log or logit link the two differ
#' whenever random effects are present, because the mean of
#' \eqn{\exp(\cdot)} over cases is not \eqn{\exp(\cdot)} at the mean case.
#'
#' @details The treated row uses the package's AB and multiple-baseline
#'   coding (see [process_scd_phases()]): \code{time_A = B - T_A}, with
#'   \eqn{T_A} the earliest first baseline session, \code{level_AB = 1}, and
#'   \code{trend_AB = B - T_B}, with \eqn{T_B} the first observed session
#'   after \eqn{A} (the treatment onset). The untreated row has
#'   \code{time_A = B - T_A}, \code{level_AB = 0}, and \code{trend_AB = 0}.
#'   A \code{time_B} column is \code{B - T_B} in both arms, as
#'   [process_scd_phases()] codes it on every row.
#'   Maintenance and generalization terms are 0. Each new case borrows one
#'   fitted row drawn at random, shared by its two arms, so predictors outside
#'   the design coding average over their observed distribution instead of
#'   being fixed at one row. A phase column (for example in
#'   \code{sigma ~ phase}) takes a baseline-role label in the untreated arm
#'   and an intervention-role label in the treated arm. Every grouping factor
#'   of the model, including an observation-level random effect, is
#'   relabeled to a new level per new case, so
#'   \code{brms::posterior_epred()} draws it with \code{allow_new_levels =
#'   TRUE}, \code{sample_new_levels = "gaussian"}, and \code{re_formula =
#'   NULL}; both arms of a new case share its draw.
#'
#'   \strong{Autocorrelation.} Predictions use \code{incl_autocor = FALSE}:
#'   the AR(1) residual has mean 0, so the expected outcome of a new case does
#'   not depend on lagged residuals, and the outcome values of the template
#'   rows never enter. On a log link with latent AR(1) residuals (non-Gaussian
#'   families in \pkg{brms}), covariance mode (\code{cov = TRUE}) uses
#'   stationary latent variance \eqn{\sigma_e^2 / (1 - \phi^2)}, with
#'   \code{sderr} = \eqn{\sigma_e}. Conditional mode
#'   (\code{cov = FALSE}) instead adds an independent latent residual and
#'   its lagged contribution: \eqn{z_t + \phi z_{t-1}}, with independent
#'   \eqn{z_t} of variance \eqn{\sigma_e^2}. Its interior-session variance
#'   is \eqn{\sigma_e^2 (1 + \phi^2)}; at the initial session, the absent
#'   lag contributes zero and variance is \eqn{\sigma_e^2}. The follow-up
#'   contrast uses the interior-session variance. Each draw is multiplied by the
#'   exponential of half the mode-specific variance. Unknown modes, higher
#'   AR orders, and MA terms are rejected rather than assigned this AR(1)
#'   correction. Other non-identity links
#'   hold the latent residual at 0 and warn. Draws with a non-finite
#'   contrast are \code{NA} in \code{draws}, excluded from \code{summary},
#'   counted by \code{n_draws}, and reported in a warning.
#'
#'   \strong{Times.} \eqn{A} is the last session of the shortest baseline
#'   across tiers. With \code{ab_rule = "scdhlm"} (default) \eqn{B - A} is
#'   the treatment duration of the tier(s) with that shortest baseline, as in
#'   \code{scdhlm::default_times()}; with \code{"shortest_treatment"} it is
#'   the shortest treatment duration across tiers. Tiers without both
#'   baseline and treatment sessions are left out. \code{duration} sets
#'   \eqn{B - A} directly, for a common benchmark duration across studies;
#'   \code{time_B} overrides both. Only AB and multiple-baseline
#'   (multiple-probe) fits carrying \code{level_AB}, \code{trend_AB}, and
#'   \code{time_A} are supported.
#'
#'   No established population-averaged estimand for single-case designs
#'   exists yet; this function follows the general counterfactual
#'   marginalization over new cases.
#'
#' @param model A fitted \code{brmsfit} from an AB or multiple-baseline
#'   design.
#' @param data The processed data the model was fitted to (with
#'   \code{level_AB} and \code{time_A}); defaults to \code{model$data}.
#' @param time_var Time column; defaults to [find_time_var()], and
#'   \code{time_A} is used when neither is present.
#' @param case_var Case column; defaults to [find_case_var()].
#' @param time_A,time_B Last pre-treatment session \eqn{A} and follow-up
#'   session \eqn{B} on the time scale of \code{time_var}; \code{NULL} uses
#'   the defaults in Details.
#' @param n_new Number of new cases averaged per draw.
#' @param ndraws Number of posterior draws.
#' @param intervention_goal \code{"increase"} or \code{"decrease"}; sets the
#'   direction counted by \code{prob_improvement}.
#' @param ab_rule \code{"scdhlm"} or \code{"shortest_treatment"}; the default rule for
#'   \eqn{B} (see Details).
#' @param duration Optional \eqn{B - A} in sessions; overrides
#'   \code{ab_rule}.
#'
#' @return A list with
#'   \describe{
#'     \item{\code{summary}}{Data frame with columns \code{quantity},
#'       \code{median}, \code{lower}, \code{upper} (2.5\% and 97.5\%
#'       quantiles) for \code{mean_untreated}, \code{mean_treated},
#'       \code{difference} (treated minus untreated), \code{ratio}
#'       (treated over untreated; log links only), and
#'       \code{prob_improvement}, the posterior probability that the
#'       difference lies in the \code{intervention_goal} direction (given in
#'       \code{median}, with \code{NA} bounds).}
#'     \item{\code{draws}}{Data frame with one row per draw: \code{draw},
#'       \code{mean_untreated}, \code{mean_treated}, \code{difference}, and
#'       \code{ratio} (log links only).}
#'     \item{\code{n_draws}}{Number of finite draws the summary uses.}
#'     \item{\code{time_A}, \code{time_B}}{The sessions used.}
#'   }
#'
#' @examples
#' if (interactive()) {
#'   d <- process_scd_phases(simulate_scd_data(n_cases = 4), list(), "AB")
#'   fit <- build_model(d, "outcome ~ time_A + level_AB + trend_AB + (1 + level_AB | case)",
#'                      family = "poisson")
#'   marginal_contrast(fit, d)$summary
#' }
#'
#' @export
marginal_contrast <- function(model, data = NULL, time_var = NULL, case_var = NULL,
                              time_A = NULL, time_B = NULL, n_new = 200, ndraws = 500,
                              intervention_goal = c("increase", "decrease"),
                              ab_rule = c("scdhlm", "shortest_treatment"), duration = NULL) {
  intervention_goal <- match.arg(intervention_goal)
  ab_rule <- match.arg(ab_rule)
  data <- data %||% model$data
  fam <- tryCatch(model$family$family, error = function(e) NULL) %||% "unknown"
  if (fam %in% c("cumulative", "categorical", "sratio", "cratio", "acat")) {
    stop(sprintf("marginal_contrast() does not support the '%s' family.", fam))
  }
  f <- paste(deparse(tryCatch(stats::formula(model)$formula, error = function(e) NULL)),
             collapse = " ")
  if (grepl("offset(", f, fixed = TRUE)) {
    stop("marginal_contrast() does not support models with an exposure offset: the ",
         "counterfactual rows carry no exposure value.", call. = FALSE)
  }
  needed <- c("level_AB", "trend_AB", "time_A")
  coded <- names(model$data)
  if (!all(needed %in% names(data)) || !"level_AB" %in% coded ||
      length(reversal_terms(coded)) || any(grepl("^level_crit", coded))) {
    stop(paste0("marginal_contrast() supports AB and multiple-baseline fits coded with ",
                "level_AB, trend_AB, and time_A only; reversal, changing-criterion, and ",
                "alternating-treatments designs are not supported."))
  }
  time_var <- time_var %||% find_time_var(data)
  case_var <- case_var %||% find_case_var(data)
  times <- .marginal_times(data, time_var, case_var, time_A, time_B, ab_rule, duration)
  model_time <- if (!is.null(time_var) && time_var %in% coded) time_var
  rows <- .counterfactual_rows(model$data, .model_group_vars(model), n_new, times, model_time)
  n_total <- brms::ndraws(model)
  draw_ids <- sort(sample.int(n_total, min(ndraws, n_total)))
  epred <- brms::posterior_epred(model, newdata = rows, re_formula = NULL,
                                 allow_new_levels = TRUE, sample_new_levels = "gaussian",
                                 draw_ids = draw_ids, incl_autocor = FALSE)
  latent <- .latent_ar_factor(model, draw_ids)
  link <- tryCatch(model$family$link, error = function(e) NULL) %||% "identity"
  if (!is.null(latent) && identical(link, "log")) epred <- epred * latent
  if (!is.null(latent) && !identical(link, "log")) {
    warning(paste0("The latent AR(1) residual variance is integrated for log links only; ",
                   "on the '", link, "' link the contrast holds the latent residual at 0."),
            call. = FALSE)
  }
  draws <- data.frame(draw = seq_len(nrow(epred)),
                      mean_untreated = rowMeans(epred[, rows$.arm == 0, drop = FALSE]),
                      mean_treated = rowMeans(epred[, rows$.arm == 1, drop = FALSE]))
  draws$difference <- draws$mean_treated - draws$mean_untreated
  log_link <- identical(link, "log")
  if (log_link) draws$ratio <- draws$mean_treated / draws$mean_untreated
  finite <- is.finite(draws$difference)
  if (!all(finite)) {
    warning(sprintf("%d of %d draws give a non-finite contrast and are excluded from the summary.",
                    sum(!finite), length(finite)), call. = FALSE)
  }
  goal_sign <- if (intervention_goal == "decrease") -1 else 1
  summary <- rbind(
    .marginal_summary(draws[finite, , drop = FALSE], setdiff(names(draws), "draw")),
    data.frame(quantity = "prob_improvement",
               median = mean(goal_sign * draws$difference[finite] > 0),
               lower = NA_real_, upper = NA_real_, stringsAsFactors = FALSE))
  rownames(summary) <- NULL
  list(summary = summary, draws = draws, n_draws = sum(finite),
       time_A = times$time_A, time_B = times$time_B)
}
