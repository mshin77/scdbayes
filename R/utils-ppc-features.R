# Posterior predictive checks on visual-analysis features

.ppc_statistics_all <- c("initial", "level", "trend", "variability",
                         "absence", "immediacy", "nonoverlap", "autocorrelation")

#' @noRd
.ppc_meta <- function(data, outcome_var, case_var, phase_var, time_var, design = NULL) {
  vars <- intersect(tier_vars(data, time_var), names(data))
  if (!length(vars)) vars <- case_var
  case <- as.character(.std_factor(data, vars))
  time <- if (!is.null(time_var)) as.numeric(data[[time_var]]) else
    as.numeric(stats::ave(seq_along(case), case, FUN = seq_along))
  phase_raw <- as.character(data[[phase_var]])
  role <- .phase_numeric(phase_raw, time)
  lab <- .role_letter(phase_raw)
  lab[is.na(phase_raw)] <- NA_character_
  atd <- identical(design %||% detect_design_type(data), "alternating")

  runs <- lapply(split(which(!is.na(lab)), case[!is.na(lab)]), function(i) {
    i <- i[order(time[i])]
    l <- lab[i]
    run <- cumsum(c(TRUE, l[-1] != l[-length(l)]))
    first <- !duplicated(run)
    k <- stats::ave(seq_len(sum(first)), l[first], FUN = seq_along)
    list(i = i, phase = if (atd) l else paste0(l[first], k)[run])
  })
  phase <- rep(NA_character_, length(lab))
  phase[unlist(lapply(runs, `[[`, "i"))] <- unlist(lapply(runs, `[[`, "phase"))

  case_levels <- sort(unique(case))
  keyed <- which(!is.na(phase))
  group_idx <- split(keyed, paste(case[keyed], phase[keyed], sep = "\r"))
  first_row <- vapply(group_idx, function(i) i[which.min(time[i])], integer(1))
  ord <- order(match(case[first_row], case_levels), time[first_row])
  group_idx <- unname(lapply(group_idx[ord], function(i) i[order(time[i])]))
  first_row <- unname(first_row[ord])
  groups <- data.frame(case = case[first_row], phase = phase[first_row],
                       role = role[first_row], stringsAsFactors = FALSE)
  # adjacent onset (baseline -> intervention) and withdrawal (intervention -> baseline) phases
  nxt <- c(groups$case[-1] == groups$case[-nrow(groups)], FALSE)
  next_role <- c(groups$role[-1], NA)
  onset <- nxt & groups$role %in% 0 & next_role >= 1
  withdrawal <- nxt & (groups$role >= 1) %in% TRUE & next_role %in% 0
  pair_from <- which(onset | withdrawal)
  pairs <- lapply(pair_from, function(g) c(g, g + 1L))

  list(case = case, case_levels = case_levels, phase = phase,
       phase_levels = unique(groups$phase),
       condition = ifelse(role == 0, "baseline", "intervention"),
       time = time, outcome = as.numeric(data[[outcome_var]]),
       groups = groups, group_idx = group_idx, pairs = pairs,
       withdrawal = withdrawal[pair_from])
}

#' @noRd
.ols_slope <- function(y, t) {
  ok <- is.finite(y) & is.finite(t)
  if (sum(ok) < 2) return(NA_real_)
  tc <- t[ok] - mean(t[ok])
  den <- sum(tc^2)
  if (den == 0) NA_real_ else sum(tc * y[ok]) / den
}

#' @noRd
.ppc_tste <- function(y, t, base, int) {
  out <- rep(NA_real_, 5)
  slope <- .ols_slope(y[base], t[base])
  if (is.na(slope)) return(out)
  ok <- is.finite(y[base]) & is.finite(t[base])
  intercept <- mean(y[base][ok]) - slope * mean(t[base][ok])
  first5 <- int[seq_len(min(5, length(int)))]
  out[seq_along(first5)] <- y[first5] - (intercept + slope * t[first5])
  out
}

#' @noRd
.ppc_tau <- function(a, b) {
  a <- a[is.finite(a)]
  b <- b[is.finite(b)]
  if (!length(a) || !length(b)) return(NA_real_)
  mean(sign(outer(b, a, "-")))
}

#' @noRd
.ppc_ar1_level1 <- function(y, t, case, group_idx) {
  resid <- rep(NA_real_, length(y))
  resid[unlist(group_idx)] <- unlist(lapply(group_idx, function(i) {
    ok <- is.finite(y[i]) & is.finite(t[i])
    r <- rep(NA_real_, length(i))
    if (!any(ok)) return(r)
    slope <- if (sum(ok) >= 3) .ols_slope(y[i], t[i]) else NA_real_
    r[ok] <- y[i][ok] - mean(y[i][ok]) - (if (is.na(slope)) 0 else slope * (t[i][ok] - mean(t[i][ok])))
    r
  }))
  rows <- sort(unlist(group_idx))
  lag <- vapply(split(rows, case[rows]), function(i) {
    r <- resid[i][order(t[i])]
    ok <- is.finite(r[-1]) & is.finite(r[-length(r)])
    c(sum(r[-1][ok] * r[-length(r)][ok]), sum(r^2, na.rm = TRUE))
  }, numeric(2))
  tot <- rowSums(matrix(lag, nrow = 2))
  if (tot[2] == 0) NA_real_ else tot[1] / tot[2]
}

#' @noRd
.ppc_ar1_gls <- function(y, t, case, condition, group_idx) {
  rows <- sort(unlist(group_idx))
  rows <- rows[is.finite(y[rows]) & is.finite(t[rows])]
  if (length(rows) < 4) return(NA_real_)
  d <- data.frame(y = y[rows], t = t[rows], H = as.integer(condition[rows] == "intervention"),
                  case = factor(case[rows]))
  # corAR1 needs an integer covariate unique within case; otherwise use session order
  integer_time <- all(d$t == round(d$t)) && !anyDuplicated(paste(d$case, d$t))
  tt <- if (integer_time) as.integer(d$t) else
    as.integer(stats::ave(d$t, d$case, FUN = function(x) rank(x, ties.method = "first")))
  X <- if (nlevels(d$case) > 1) {
    stats::model.matrix(~ 0 + case + case:t + case:H + case:t:H, d)
  } else {
    stats::model.matrix(~ t * H, d)
  }
  qx <- qr(X)
  X <- X[, qx$pivot[seq_len(qx$rank)], drop = FALSE]
  fit <- tryCatch(
    nlme::gls(y ~ 0 + X, data = list(y = d$y, X = X, tt = tt, case = d$case),
              correlation = nlme::corAR1(form = ~ tt | case), method = "REML"),
    error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  as.numeric(stats::coef(fit$modelStruct$corStruct, unconstrained = FALSE))
}

#' @noRd
.ppc_rows <- function(statistic, case, phase, value) {
  data.frame(statistic = statistic, case = case, phase = phase, value = value,
             stringsAsFactors = FALSE)
}

#' @noRd
.ppc_by_phase <- function(statistic, phases, row_phase, row_case, values, pooled) {
  do.call(rbind, lapply(phases, function(p) {
    sel <- row_phase == p
    .ppc_rows(statistic, c("pooled", row_case[sel]), p, c(pooled(sel), values[sel]))
  }))
}

# rows in a fixed order so the observed and every replicate frame align for p-values
#' @noRd
.ppc_frame <- function(y, meta, statistics, goal, pool = "case_means", ar_method = "moment") {
  gi <- meta$group_idx
  gr <- meta$groups
  time <- meta$time
  dir <- if (goal == "decrease") -1 else 1
  out <- list()

  if ("initial" %in% statistics) {
    v <- vapply(meta$case_levels, function(k) {
      idx <- which(meta$case == k & meta$condition %in% "baseline" & is.finite(y))
      if (length(idx) == 0) NA_real_ else y[idx][which.min(time[idx])]
    }, numeric(1))
    out$initial <- .ppc_rows("initial", c("pooled", meta$case_levels), NA_character_,
                             c(mean(v, na.rm = TRUE), v))
  }

  n_finite <- vapply(gi, function(i) sum(is.finite(y[i])), numeric(1))
  per_group <- list(
    level       = function(i) mean(y[i], na.rm = TRUE),
    trend       = function(i) .ols_slope(y[i], time[i]),
    variability = function(i) stats::sd(y[i], na.rm = TRUE),
    absence     = function(i) mean(y[i] <= 0, na.rm = TRUE))
  pool_obs <- function(sel, v, f) {
    if (identical(pool, "all_obs")) mean(f(y[unlist(gi[sel])]), na.rm = TRUE)
    else mean(v[sel], na.rm = TRUE)
  }
  pooled <- list(
    level       = function(sel, v) pool_obs(sel, v, identity),
    trend       = function(sel, v) mean(v[sel], na.rm = TRUE),
    variability = function(sel, v) {
      keep <- sel & is.finite(v) & n_finite >= 2
      den <- sum(n_finite[keep] - 1)
      if (den <= 0) NA_real_ else sqrt(sum((n_finite[keep] - 1) * v[keep]^2) / den)
    },
    absence     = function(sel, v) pool_obs(sel, v, function(x) x <= 0))
  for_stats <- intersect(c("level", "trend", "variability", "absence"), statistics)
  out[for_stats] <- lapply(for_stats, function(s) {
    v <- vapply(gi, per_group[[s]], numeric(1))
    .ppc_by_phase(s, meta$phase_levels, gr$phase, gr$case, v, function(sel) pooled[[s]](sel, v))
  })

  pairs <- meta$pairs
  withdrawal <- meta$withdrawal %||% logical(length(pairs))
  pair_phase <- gr$phase[vapply(pairs, `[`, integer(1), 2)]
  pair_case <- gr$case[vapply(pairs, `[`, integer(1), 2)]
  onset <- which(!withdrawal)
  if ("immediacy" %in% statistics && length(onset)) {
    tste <- matrix(vapply(pairs[onset], function(p) dir * .ppc_tste(y, time, gi[[p[1]]], gi[[p[2]]]),
                          numeric(5)), nrow = 5)
    out$immediacy <- do.call(rbind, lapply(1:5, function(k) {
      .ppc_by_phase(paste0("immediacy_t", k), unique(pair_phase[onset]), pair_phase[onset],
                    pair_case[onset], tste[k, ], function(sel) mean(tste[k, sel], na.rm = TRUE))
    }))
  }
  if ("nonoverlap" %in% statistics && length(pairs)) {
    # a withdrawal is expected to move opposite to the goal, as in .term_goal
    pair_dir <- ifelse(withdrawal, -dir, dir)
    tau <- pair_dir * vapply(pairs, function(p) .ppc_tau(y[gi[[p[1]]]], y[gi[[p[2]]]]), numeric(1))
    out$nonoverlap <- .ppc_by_phase("nonoverlap", unique(pair_phase), pair_phase, pair_case, tau,
                                    function(sel) mean(tau[sel], na.rm = TRUE))
  }
  if ("autocorrelation" %in% statistics) {
    rho <- if (identical(ar_method, "gls")) {
      .ppc_ar1_gls(y, time, meta$case, meta$condition, gi)
    } else {
      .ppc_ar1_level1(y, time, meta$case, gi)
    }
    out$autocorrelation <- .ppc_rows("autocorrelation", "pooled", NA_character_, rho)
  }
  out <- do.call(rbind, unname(out)) %||% .ppc_rows(character(0), character(0),
                                                    character(0), numeric(0))
  rownames(out) <- NULL
  out
}

#' @noRd
.latent_ar <- function(model) {
  fam <- tryCatch(model$family$family, error = function(e) NULL) %||% "gaussian"
  f <- paste(deparse(tryCatch(stats::formula(model)$formula, error = function(e) NULL)),
             collapse = " ")
  grepl("\\b(ar|ma|arma)\\(", f) && !fam %in% c("gaussian", "student")
}

#' @noRd
.ppc_yrep_fitted <- function(model, data, n_draws, mode, observed) {
  if (identical(mode, "new")) {
    stop("The model has a latent ar() term (", model$family$family, " family): brms cannot ",
         "predict new latent residuals, so mode = \"new\" is unavailable. Use mode = \"same\", ",
         "which predicts the fitted rows conditional on their latent residuals.", call. = FALSE)
  }
  yrep <- brms::posterior_predict(model, ndraws = n_draws)
  # brms drops rows with a missing outcome, so fitted rows are the finite observed rows
  rows <- if (ncol(yrep) == nrow(data)) seq_len(nrow(data)) else which(is.finite(observed))
  if (length(rows) != ncol(yrep)) {
    stop("The model has a latent ar() term, so replicates come from the fitted rows, but ",
         "`data` does not match them (", nrow(data), " rows, ", ncol(yrep), " fitted). ",
         "Pass the data the model was fitted on.", call. = FALSE)
  }
  out <- matrix(NA_real_, nrow(yrep), nrow(data))
  out[, rows] <- yrep
  out
}

#' @noRd
.ppc_yrep <- function(model, data, n_draws, mode, observed = NULL) {
  if (.latent_ar(model)) return(.ppc_yrep_fitted(model, data, n_draws, mode, observed))
  # observation-level REs (one level per fitted row) are redrawn, else replicates hug the data
  n_fit <- nrow(model$data)
  olre <- names(Filter(function(n) n == n_fit,
                       tryCatch(brms::ngrps(model), error = function(e) NULL)))
  redraw <- intersect(c(olre, if (identical(mode, "new")) find_case_var(data)), names(data))
  nd <- data
  nd[redraw] <- lapply(nd[redraw], function(v) paste0(as.character(v), "__ppcnew"))
  yrep <- tryCatch(
    if (length(redraw) > 0) {
      brms::posterior_predict(model, newdata = nd, ndraws = n_draws,
                              allow_new_levels = TRUE,
                              sample_new_levels = "gaussian")
    } else {
      brms::posterior_predict(model, newdata = data, ndraws = n_draws)
    },
    error = function(e) stop("Posterior predictive draws failed: ", conditionMessage(e),
                             call. = FALSE))
  if (is.null(yrep) || is.null(observed) || ncol(yrep) != length(observed)) return(yrep)
  yrep[, !is.finite(observed)] <- NA
  yrep
}

#' Posterior predictive replicate datasets
#'
#' Draws replicate outcomes the way [posterior_predictive_features()] does,
#' for use in any graphical or numerical posterior predictive check: an
#' observation-level random effect (one level per fitted row) is redrawn
#' rather than conditioned on, \code{mode = "new"} also draws new case
#' effects, and replicate values are \code{NA} wherever the observed outcome
#' is missing. A latent \code{ar()} model is predicted on its fitted rows.
#'
#' @inheritParams posterior_predictive_features
#' @param n_draws Number of replicate datasets.
#' @return A numeric matrix with \code{n_draws} rows and one column per row
#'   of \code{data}.
#' @examples
#' if (interactive()) {
#'   fit <- run_scd_analysis(simulate_scd_data())$model
#'   yrep <- ppc_replicates(fit, n_draws = 50)
#'   bayesplot::ppc_dens_overlay(fit$data$outcome, yrep)
#' }
#' @export
ppc_replicates <- function(model, data = NULL, mode = c("same", "new"), n_draws = 100L) {
  if (!inherits(model, "brmsfit")) stop("`model` must be a brmsfit.", call. = FALSE)
  mode <- match.arg(mode)
  data <- data %||% model$data
  outcome_var <- .response_column(model, data)
  observed <- if (!is.null(outcome_var)) as.numeric(data[[outcome_var]])
  .ppc_yrep(model, data, n_draws, mode, observed = observed)
}

#' @noRd
.ppc_p_value <- function(reps, observed) mean(vapply(observed, function(o) mean(reps >= o), numeric(1)))

#' @noRd
.ppc_p_tie <- function(reps, observed) mean(vapply(observed, function(o) mean(reps == o), numeric(1)))

#' @noRd
.has_time_trend <- function(model, time_var = NULL) {
  f <- tryCatch(stats::formula(model)$formula, error = function(e) NULL)
  terms <- .formula_terms(paste(deparse(f), collapse = " "))
  # a time variable in a fixed or random-effect term is a trend; an ar() time index is not
  terms <- terms[!grepl("^(ar|ma|arma|cosy|unstr|car|sar|fcor)\\(", terms)]
  vars <- unlist(lapply(terms, function(l) all.vars(str2lang(l))))
  # derived columns such as session_c or wk_time carry a time-like prefix or the whole time name
  token <- if (!is.null(time_var)) sprintf("(^|_)\\Q%s\\E(_|$)", time_var)
  any(grepl("^(time|session|trend)", vars)) ||
    !is.null(token) && any(grepl(token, vars, perl = TRUE))
}

#' @noRd
.ppc_initial_rows <- function(meta) {
  gr <- meta$groups
  lapply(stats::setNames(meta$case_levels, meta$case_levels), function(k) {
    g <- which(gr$case == k & gr$role %in% 0)[1]
    i <- if (is.na(g)) integer(0) else meta$group_idx[[g]]
    i[is.finite(meta$outcome[i])]
  })
}

#' Posterior predictive checks on single-case visual-analysis features
#'
#' Computes visual-analysis summary statistics on the observed data and on
#' posterior-predictive replicate datasets, then returns the posterior
#' predictive p-value for each statistic.
#'
#' @details Statistics are computed per tier (each combination of the
#'   grouping columns of [tier_vars()], so a case identifier repeated across
#'   studies or a case measured on several behaviors gives separate series)
#'   and per phase occurrence within the tier: A1, B1, A2, B2, M1 (phase
#'   labels with a known role, see [phase_role()], take the role letters;
#'   other labels keep their own). Each statistic is computed in each phase
#'   for each participant and pooled across participants. In an alternating-treatments design every
#'   session of a condition forms one phase.
#'
#'   \itemize{
#'     \item \code{initial}: first baseline observation, compared with the
#'       replicate value at that session. When the model formula has no time
#'       term (a stable baseline level), every session of the initial
#'       baseline phase has the same predictive distribution, so each
#'       observation of that phase is compared with the replicate first
#'       session: the p-value averages the per-observation p-values and
#'       \code{observed} is the phase mean. The pooled row compares the mean
#'       of these tier phase means with the same mean computed on each
#'       replicate; under \code{pool = "case_means"} with \code{level}
#'       requested this equals the pooled level row of that baseline phase
#'       and is dropped. A variable in a fixed, random-effect, or smooth term
#'       counts as a time term when its name contains the time column name
#'       or starts with \code{time}, \code{session}, or \code{trend} (e.g.
#'       \code{session_c}); the time index of an \code{ar()} term does not.
#'     \item \code{level}, \code{trend}, \code{variability}, \code{absence}:
#'       within-phase mean, OLS slope, SD, and proportion of outcomes
#'       \eqn{\le 0}. Pooled rows average the tier means (level) and tier
#'       proportions (absence) with equal weight per tier under the default
#'       \code{pool = "case_means"}, or use all observations of the phase
#'       under \code{pool = "all_obs"}, as the published reference code does;
#'       the two
#'       differ only when tiers have unequal phase lengths. Trend pools the
#'       mean of the tier slopes and variability the within-tier pooled SD.
#'       Absence is computed for every family: for a continuous family it
#'       exposes a model that predicts values \eqn{\le 0} the data never
#'       show (for example a Normal model of a rate).
#'     \item \code{immediacy_t1} to \code{immediacy_t5}: time-specific
#'       treatment effects (TSTE) at each of the first five intervention time
#'       points, the observation minus the prediction of the regression line
#'       fitted to the preceding baseline phase, one row per adjacent
#'       baseline-to-intervention transition and a pooled mean per time
#'       point. Withdrawal transitions are not used.
#'     \item \code{nonoverlap}: Tau between each pair of adjacent phases, the
#'       mean of the signs of all later-minus-earlier differences. Pairs are
#'       onsets (baseline to intervention, e.g. A1-B1, A2-B2) and, in
#'       reversal designs, withdrawals (intervention to baseline, e.g.
#'       B1-A2), reported as separate phase contrasts. The row's \code{phase} is the later phase of the pair, so \code{"A2"}
#'       is the B1-A2 withdrawal contrast.
#'     \item \code{autocorrelation}: one Level-1 lag-1 estimate pooled across
#'       tiers. \code{ar_method = "moment"} (default) is a moment estimate
#'       from time-ordered residuals after each phase's level and trend are
#'       removed. \code{ar_method = "gls"} fits a generalized least squares
#'       model with \code{nlme::gls()} by REML: tier-specific intercept,
#'       slope, phase (intervention role vs baseline role), and slope-by-phase
#'       terms with a common AR(1) correlation over sessions within tier. The
#'       same estimator is applied to observed and replicate data. The phase
#'       term is a binary intervention indicator (\code{case:condition});
#'       interacting case with the phase occurrence (\code{Case:Phase}, A1,
#'       B1, A2, B2) instead gives each reversal phase its own level. The two
#'       coincide for AB tiers and differ in
#'       reversal designs, where the binary indicator leaves level shifts
#'       between same-condition phases in the residuals.
#'   }
#'
#'   The role of each phase (baseline vs intervention) only pairs phases for
#'   immediacy and non-overlap and colors the overlay plot. Immediacy and Tau
#'   are reversed for \code{intervention_goal = "decrease"}; a withdrawal Tau
#'   takes the opposite reversal, since removing treatment is expected to
#'   move the outcome against the goal, so a positive Tau reads as change in
#'   the direction the intervention predicts on every row. The posterior
#'   predictive p-value is \eqn{p = \Pr[T(y^{rep}) \ge T(y)]}, the share of
#'   replicates whose statistic is at least the observed one. Ties count
#'   fully, so a statistic equal to the observed value in every replicate
#'   (e.g. absence = 0 throughout) gives \eqn{p = 1}. The share of ties,
#'   \eqn{\Pr[T(y^{rep}) = T(y)]}, is reported as \code{p_tie}. The flag
#'   uses the mid-p value \eqn{\Pr[T(y^{rep}) > T(y)] + 0.5 \Pr[T(y^{rep}) =
#'   T(y)]}, which counts ties half and equals \code{p_value} when there are
#'   none, and is set below 0.025 or above 0.975. Under a correct
#'   model p-values concentrate near 0.5 rather than spreading uniformly, so
#'   a flag is a strong signal and an unflagged value
#'   is weak evidence of fit.
#'
#'   For a model with an \code{ar()} term fitted without \code{cov = TRUE},
#'   brms predicts each occasion conditional on the observed lagged
#'   residuals, so replicates track the observed series; the autocorrelation
#'   check is then conservative and is best read alongside the posterior of
#'   the \code{ar} coefficient. For non-Gaussian families the \code{ar()}
#'   term is a latent residual that brms cannot predict for new data, so
#'   replicates are drawn on the fitted rows conditional on the fitted
#'   latent residuals (and on any observation-level effect), \code{mode =
#'   "new"} stops with an error, and the returned \code{note} says so. Level
#'   and immediacy checks are then conservative, while variability and
#'   autocorrelation replicates add sampling noise on top of the fitted
#'   residuals and can flag without misfit. With
#'   a Gaussian \code{ar()} term, \code{mode = "new"} still conditions each
#'   occasion on lagged residuals computed from the observed outcomes and the
#'   new-case prediction, so replicates are pulled toward the observed
#'   series; the \code{note} says so and the generalization check is then
#'   conservative.
#'
#'   An observation-level random effect is redrawn, as in the published
#'   reference code, which relabels the \code{obs} levels and predicts with
#'   \code{allow_new_levels = TRUE}; conditioning on the fitted effects would
#'   make the replicates reproduce the observed values almost exactly, so
#'   every check would pass.
#'
#' @param model A fitted \code{brmsfit}.
#' @param data Data holding the outcome, case, phase and (optional) time
#'   columns; defaults to the model's data. Rows with a missing outcome are
#'   allowed: replicate values are set to \code{NA} there.
#' @param statistics Character vector selecting which statistics to compute
#'   (\code{"immediacy"} selects all five time points).
#' @param n_draws Number of posterior-predictive replicate datasets (default
#'   500). The p-value is a proportion over these draws, so its Monte Carlo
#'   error is \code{sqrt(p * (1 - p) / n_draws)}: about 0.007 at the 0.025 flag
#'   boundary with 500 draws and about 0.022 with 50, large enough at small
#'   \code{n_draws} that a value near the boundary can flag by chance.
#' @param mode \code{"same"} replicates for the observed cases;
#'   \code{"new"} draws new case effects from the population posterior
#'   (\code{allow_new_levels = TRUE}) to assess generalization. In both modes
#'   an observation-level random effect (one level per fitted row) is redrawn
#'   rather than conditioned on, and replicate values are set to \code{NA}
#'   wherever the observed outcome is missing so both sides use the same rows.
#' @param intervention_goal \code{"increase"} or \code{"decrease"}; sets the
#'   improvement direction for immediacy and Tau.
#' @param ar_method \code{"moment"} (default) or \code{"gls"} estimator for
#'   the autocorrelation statistic (see Details). \code{"gls"} needs the
#'   \pkg{nlme} package and falls back to \code{"moment"} with a message
#'   when it is not installed; it fits one model per replicate, so it is
#'   slower.
#' @param pool \code{"case_means"} (default) or \code{"all_obs"}: how the
#'   pooled level and absence rows combine tiers (see Details).
#' @param design Design the model was fit with, as returned by
#'   \code{\link{detect_design_type}}. Only \code{"alternating"} changes the
#'   phase structure: conditions are pooled instead of being split into
#'   numbered runs. \code{NULL} (default) re-derives it from \code{data}, which
#'   can disagree with the design the model was fit with.
#'
#' @return A list with \code{summary} (one row per statistic/tier/phase, with
#'   columns \code{statistic}, \code{case} (the tier label, or
#'   \code{"pooled"}), \code{phase} (phase occurrence such as \code{"B1"}),
#'   \code{observed}, \code{p_value}, \code{p_tie} (share of replicates equal
#'   to the observed statistic), and \code{flag}; the pooled \code{initial}
#'   row is left out when it duplicates the pooled \code{level} row, see
#'   Details) and \code{draws} (long
#'   data frame of replicate values for density plots), and \code{note}
#'   (\code{NULL}, or how replicates were conditioned for an \code{ar()}
#'   model and which requested statistics were not computed and why).
#'   Empty data frames with a \code{note} if the case, phase, or outcome
#'   column is missing; an error if \code{model} is not a \code{brmsfit} or
#'   brms cannot produce posterior draws.
#'
#' @examples
#' if (interactive()) {
#'   fit <- run_scd_analysis(simulate_scd_data())$model
#'   posterior_predictive_features(fit, n_draws = 200)$summary
#' }
#'
#' @export
posterior_predictive_features <- function(model, data = NULL,
                                          statistics = .ppc_statistics_all,
                                          n_draws = 500L,
                                          mode = c("same", "new"),
                                          intervention_goal = c("increase", "decrease"),
                                          ar_method = c("moment", "gls"),
                                          pool = c("case_means", "all_obs"),
                                          design = NULL) {
  if (!inherits(model, "brmsfit")) stop("`model` must be a brmsfit.", call. = FALSE)
  mode <- match.arg(mode)
  intervention_goal <- match.arg(intervention_goal)
  ar_method <- match.arg(ar_method)
  pool <- match.arg(pool)
  if (ar_method == "gls" && !requireNamespace("nlme", quietly = TRUE)) {
    message("Package 'nlme' is not available; autocorrelation uses the moment estimator.")
    ar_method <- "moment"
  }
  unknown <- setdiff(statistics, .ppc_statistics_all)
  statistics <- intersect(statistics, .ppc_statistics_all)
  data <- data %||% model$data
  outcome_var <- .response_column(model, data)
  case_var  <- find_case_var(data)
  phase_var <- find_phase_var(data)
  time_var  <- find_time_var(data)
  empty <- function(reason) {
    list(summary = data.frame(), draws = data.frame(),
         note = sprintf("Not computed: %s (%s).",
                        paste(c(unknown, statistics), collapse = ", "), reason))
  }
  if (is.null(outcome_var) || is.null(case_var) || is.null(phase_var)) {
    return(empty("no outcome, case, or phase column"))
  }
  if (length(statistics) == 0) return(empty("unknown name"))

  meta <- .ppc_meta(data, outcome_var, case_var, phase_var, time_var, design)
  yrep <- .ppc_yrep(model, data, n_draws, mode, observed = meta$outcome)
  if (is.null(yrep) || nrow(yrep) == 0) {
    stop("brms returned no posterior predictive draws.", call. = FALSE)
  }

  obs <- .ppc_frame(meta$outcome, meta, statistics, intervention_goal, pool, ar_method)
  rep_mat <- vapply(seq_len(nrow(yrep)), function(d) {
    .ppc_frame(yrep[d, ], meta, statistics, intervention_goal, pool, ar_method)$value
  }, numeric(nrow(obs)))
  rep_mat <- matrix(rep_mat, nrow = nrow(obs))

  obs_sets <- as.list(obs$value)
  if ("initial" %in% statistics && !.has_time_trend(model, time_var)) {
    init <- which(obs$statistic == "initial" & obs$case != "pooled")
    rows <- unname(.ppc_initial_rows(meta)[obs$case[init]])
    obs_sets[init] <- lapply(rows, function(i) meta$outcome[i])
    obs$value[init] <- vapply(obs_sets[init], function(y) if (length(y)) mean(y) else NA_real_,
                              numeric(1))
    obs_sets[init][!is.finite(obs$value[init])] <- list(NA_real_)
    pooled_init <- which(obs$statistic == "initial" & obs$case == "pooled")
    obs$value[pooled_init] <- mean(obs$value[init], na.rm = TRUE)
    obs_sets[pooled_init] <- list(obs$value[pooled_init])
    # the pooled replicate averages the tier phase means, as the observed side does
    rows <- Filter(length, rows)
    if (length(pooled_init) && length(rows)) {
      rep_mat[pooled_init, ] <- vapply(seq_len(nrow(yrep)), function(d) {
        mean(vapply(rows, function(i) mean(yrep[d, i], na.rm = TRUE), numeric(1)), na.rm = TRUE)
      }, numeric(1))
    }
    # under case_means the pooled initial row repeats the pooled level row of that baseline phase
    if (pool == "case_means" && "level" %in% statistics && length(unique(meta$phase[unlist(rows)])) == 1) {
      obs <- obs[-pooled_init, ]
      obs_sets <- obs_sets[-pooled_init]
      rep_mat <- rep_mat[-pooled_init, , drop = FALSE]
    }
  }

  p_stats <- vapply(seq_len(nrow(obs)), function(i) {
    reps <- rep_mat[i, ]
    reps <- reps[is.finite(reps)]
    if (length(reps) == 0 || !all(is.finite(obs_sets[[i]]))) return(c(NA_real_, NA_real_))
    c(.ppc_p_value(reps, obs_sets[[i]]), .ppc_p_tie(reps, obs_sets[[i]]))
  }, numeric(2))
  p_value <- p_stats[1, ]
  p_tie <- p_stats[2, ]

  # the flag counts ties half, so ties at the observed value (e.g. absence 0 throughout) do not flag
  mid_p <- p_value - 0.5 * p_tie
  summary_df <- data.frame(
    obs[, c("statistic", "case", "phase")],
    observed = obs$value,
    p_value  = p_value,
    p_tie    = p_tie,
    flag     = !is.na(mid_p) & (mid_p < 0.025 | mid_p > 0.975),
    stringsAsFactors = FALSE)

  draws_df <- data.frame(
    statistic = rep(obs$statistic, times = ncol(rep_mat)),
    case      = rep(obs$case, times = ncol(rep_mat)),
    phase     = rep(obs$phase, times = ncol(rep_mat)),
    value     = as.numeric(rep_mat),
    stringsAsFactors = FALSE)
  draws_df <- draws_df[is.finite(draws_df$value), ]

  returned <- unique(sub("_t[1-5]$", "", obs$statistic))
  dropped <- c(unknown, setdiff(statistics, returned))
  notes <- c(
    if (.latent_ar(model))
      paste("Latent ar() term: replicates are predicted on the fitted rows conditional on",
            "the fitted latent residuals (and any observation-level effect), so they track",
            "the observed series. Level and immediacy checks are conservative; variability",
            "and autocorrelation replicates add sampling noise on top of the fitted residuals,",
            "so a flag there can reflect this construction rather than misfit."),
    if (identical(mode, "new") && .gaussian_ar(model))
      paste("New participants with a Gaussian ar() term: brms conditions each occasion on",
            "lagged residuals computed from the observed outcomes and the new-case prediction,",
            "so replicates are pulled toward the observed series and the generalization",
            "check is conservative."),
    if (length(dropped))
      sprintf("Not computed: %s (unknown name, or no phase or phase pair to compute it on).",
              paste(dropped, collapse = ", ")))
  note <- if (length(notes)) paste(notes, collapse = " ")
  return(list(summary = summary_df, draws = draws_df, note = note))
}

#' @noRd
.gaussian_ar <- function(model) {
  f <- paste(deparse(tryCatch(stats::formula(model)$formula, error = function(e) NULL)),
             collapse = " ")
  grepl("\\b(ar|ma|arma)\\(", f) && !.latent_ar(model)
}

#' Posterior-predictive line draws for a spaghetti overlay
#'
#' Returns the observed single-case series alongside a small number of
#' posterior-predictive replicate series, shaped for a line-graph overlay
#' colored by baseline vs intervention condition.
#'
#' @inheritParams posterior_predictive_features
#' @param n_lines Number of replicate series to return.
#'
#' @return A list with \code{observed} and \code{replicates} data frames
#'   (columns \code{case} (tier label), \code{time}, \code{condition},
#'   \code{phase} (phase occurrence such as \code{"A1"}), \code{outcome}, and
#'   \code{draw} for replicates); \code{NULL} if the case, phase, or outcome
#'   column is missing, and an error if brms cannot produce posterior draws.
#'
#' @examples
#' if (interactive()) {
#'   fit <- run_scd_analysis(simulate_scd_data())$model
#'   str(ppc_overlay_draws(fit, n_lines = 5))
#' }
#'
#' @export
ppc_overlay_draws <- function(model, data = NULL, n_lines = 20L,
                              mode = c("same", "new"), design = NULL) {
  if (!inherits(model, "brmsfit")) stop("`model` must be a brmsfit.", call. = FALSE)
  mode <- match.arg(mode)
  data <- data %||% model$data
  outcome_var <- .response_column(model, data)
  case_var  <- find_case_var(data)
  phase_var <- find_phase_var(data)
  time_var  <- find_time_var(data)
  if (is.null(outcome_var) || is.null(case_var) || is.null(phase_var)) return(NULL)
  meta <- .ppc_meta(data, outcome_var, case_var, phase_var, time_var, design)
  yrep <- .ppc_yrep(model, data, n_lines, mode, observed = meta$outcome)

  base_df <- data.frame(case = meta$case, time = meta$time,
                        condition = meta$condition, phase = meta$phase,
                        stringsAsFactors = FALSE)
  observed <- cbind(base_df, outcome = meta$outcome)
  replicates <- do.call(rbind, lapply(seq_len(nrow(yrep)), function(d) {
    cbind(base_df, outcome = yrep[d, ], draw = d)
  }))
  return(list(observed = observed, replicates = replicates))
}
