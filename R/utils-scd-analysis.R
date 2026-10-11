#' Lag-1 autocorrelation per phase per case
#'
#' Computes lag-1 autocorrelation per phase per case as derived features.
#'
#' @param data Data frame containing SCD data.
#' @param outcome_var Name of the outcome variable.
#' @param phase_var Name of the phase variable.
#' @param case_var Name of the case variable.
#' @param time_var Name of the time variable.
#'
#' @return The input data frame with added \code{autocor_A} and \code{autocor_B}
#'   columns.
#'
#' @examples
#' d <- simulate_scd_data(n_cases = 2, n_phase_a = 5, n_phase_b = 5)
#' compute_autocorrelation_features(d)
#'
#' @export
compute_autocorrelation_features <- function(data, outcome_var = "outcome",
                                             phase_var = "phase", case_var = "case",
                                             time_var = "time") {
  if (!all(c(outcome_var, phase_var, case_var) %in% names(data))) return(data)

  phases <- sort(unique(data[[phase_var]]))
  data$autocor_A <- 0
  data$autocor_B <- 0

  for (cid in unique(data[[case_var]])) {
    idx <- data[[case_var]] == cid
    for (i in seq_along(phases)) {
      phase_idx <- idx & data[[phase_var]] == phases[i]
      vals <- data[[outcome_var]][phase_idx]
      if (time_var %in% names(data)) vals <- vals[order(data[[time_var]][phase_idx])]
      ac <- if (length(vals) > 3) {
        tryCatch(acf(vals, lag.max = 1, plot = FALSE)$acf[2], error = function(e) 0)
      } else 0
      if (is.na(ac)) ac <- 0
      col <- if (i == 1) "autocor_A" else "autocor_B"
      data[[col]][phase_idx] <- ac
    }
  }
  data
}

#' Build candidate model formulas
#'
#' A coarse factor-level screen (intercept-only, level change, level + trend)
#' for LOO comparison. It uses the raw `phase`/`time` columns and is
#' deliberately simpler than the piecewise `time_A + level_AB + trend_AB`
#' parameterization used for the reported models; it is not a substitute for
#' that family.
#'
#' @param outcome_var Name of the outcome variable.
#' @param phase_var Name of the phase variable.
#' @param case_var Name of the case variable.
#' @param time_var Name of the time variable (optional).
#' @param has_autocor Logical; whether autocorrelation features are present.
#'
#' @return A named list of model formula strings.
#'
#' @examples
#' build_comparison_models("outcome", "phase", "case", time_var = "time")
#'
#' @export
build_comparison_models <- function(outcome_var, phase_var, case_var,
                                    time_var = NULL, has_autocor = FALSE) {
  .check_syntactic(c(outcome_var, phase_var, case_var, if (nzchar(time_var %||% "")) time_var),
                   "Comparison model variables")
  base <-paste0(outcome_var, " ~ ", phase_var)
  models <- list(
    "Intercept only"   = paste0(outcome_var, " ~ 1 + (1|", case_var, ")"),
    "Level change only" = paste0(base, " + (1|", case_var, ")")
  )
  if (!is.null(time_var) && time_var != "") {
    models[["Level + trend"]] <- paste0(base, " + ", time_var, " + (1|", case_var, ")")
  }
  models
}

#' Model comparison via LOO-CV
#'
#' @param data Data frame for model fitting.
#' @param model_formulas Named list of formula strings to compare.
#' @param family Model family (default \code{"gaussian"}).
#' @param iter Number of sampling iterations.
#' @param chains Number of MCMC chains.
#' @param existing_model An existing \code{brmsfit} object to include in comparison.
#'
#' @return A list with LOO comparison table, fitted models, and LOO objects,
#'   or \code{NULL} if fewer than two models succeed.
#'
#' @examples
#' if (interactive()) {
#' dat <- simulate_scd_data()
#' run_model_comparison(dat, build_comparison_models("outcome", "phase", "case"))
#' }
#'
#' @export
run_model_comparison <- function(data, model_formulas, family = "gaussian",
                                 iter = 1000, chains = 2, existing_model = NULL) {
  unsafe <- names(model_formulas)[!vapply(model_formulas, is_safe_formula, logical(1),
                                          allowed_vars = names(data))]
  if (length(unsafe)) {
    stop(sprintf("Comparison formulas may use only data columns and brms term syntax: %s.",
                 paste(unsafe, collapse = ", ")), call. = FALSE)
  }
  fitted <- list()

  if (!is.null(existing_model)) {
    fitted[["Current model"]] <- existing_model
  }

  for (nm in names(model_formulas)) {
    has_ar <- grepl("\\bar\\(", model_formulas[[nm]])
    use_threads <- if (has_ar) NULL else getOption("scdbayes.threads", brms::threading(2))
    fitted[[nm]] <- tryCatch(
      suppressWarnings(brms::brm(
        stats::as.formula(model_formulas[[nm]]), data = data, family = family,
        iter = iter, chains = chains,
        cores = min(chains, parallel::detectCores()),
        threads = use_threads,
        backend = getOption("scdbayes.backend", .default_backend()),
        stan_model_args = .stan_model_args(getOption("scdbayes.backend", .default_backend())),
        control = list(adapt_delta = 0.95),
        file = file.path(getOption("scdbayes.cache_dir", tempdir()), paste0("comparison-", gsub("\\W+", "-", tolower(nm)))),
        file_refit = "on_change",
        refresh = 0, silent = 2)),
      error = function(e) NULL
    )
  }
  fitted <- Filter(Negate(is.null), fitted)
  if (length(fitted) < 2) return(NULL)

  loos <- lapply(fitted, function(m) tryCatch(brms::loo(m), error = function(e) NULL))
  loos <- Filter(Negate(is.null), loos)
  if (length(loos) < 2) return(NULL)

  comp <- brms::loo_compare(loos)
  list(comparison = comp, models = fitted, loos = loos)
}

#' Per-case descriptive features
#'
#' @param data Data frame containing SCD data.
#' @param outcome_var Name of the outcome variable.
#' @param phase_var Name of the phase variable.
#' @param case_var Name of the case variable.
#' @param time_var Name of the time variable.
#'
#' @param intervention_goal Direction of a desirable change: \code{"increase"}
#'   (default) or \code{"decrease"}. For \code{"decrease"}, \code{NAP} is
#'   reported as \code{1 - NAP} and \code{PND} counts phase-B points below the
#'   phase-A minimum.
#'
#' @return A data frame of summary features with one row per tier (each
#'   combination of the grouping columns of [tier_vars()], so a case
#'   identifier repeated across studies or a case measured on several
#'   behaviors gives separate rows), or \code{NULL} if requirements are not
#'   met. \code{case} holds the case identifier and \code{tier} the full
#'   tier label (e.g. \code{"S1 / P1 / reading"}). Within each tier, the
#'   \code{_A} features use the first run of a baseline-role phase (see
#'   [phase_role()]) and the \code{_B} features the next intervention-role
#'   run, so labels such as \code{"Baseline 1"} and later reversal phases do
#'   not change which phases are compared. The nonoverlap
#'   descriptives \code{NAP} and \code{PND} are computed in the direction
#'   given by \code{intervention_goal}.
#'
#' @examples
#' d <- simulate_scd_data(n_cases = 2, n_phase_a = 6, n_phase_b = 6)
#' compute_case_features(d)
#'
#' @export
compute_case_features <- function(data, outcome_var = "outcome",
                                      phase_var = "phase", case_var = "case",
                                      time_var = "time",
                                      intervention_goal = c("increase", "decrease")) {
  intervention_goal <- match.arg(intervention_goal)
  if (!all(c(outcome_var, phase_var, case_var) %in% names(data))) return(NULL)

  has_time <- time_var %in% names(data)
  if (length(unique(stats::na.omit(data[[phase_var]]))) < 2) return(NULL)

  tiers <- unique(c(intersect(tier_vars(data, if (has_time) time_var), names(data)), case_var))
  key <- .std_key(data, tiers)
  feat_list <- lapply(unique(key), function(k) {
    cd <- data[key == k & !is.na(data[[outcome_var]]), ]
    cid <- cd[[case_var]][1]
    if (has_time) cd <- cd[order(cd[[time_var]]), ]
    # A = first baseline-role run of the tier, B = the next intervention-role run
    ph <- as.character(cd[[phase_var]])
    run <- cumsum(c(TRUE, dplyr::coalesce(ph[-1] != ph[-length(ph)], TRUE)))
    role <- .phase_numeric(ph, seq_along(ph))
    base_run <- run[role %in% 0][1]
    tx_run <- run[run > base_run & role %in% 1][1]
    in_a <- run %in% base_run
    in_b <- run %in% tx_run
    a <- cd[[outcome_var]][in_a]
    b <- cd[[outcome_var]][in_b]
    if (length(a) < 2 || length(b) < 2) return(NULL)

    slope_fn <- function(vals, times) {
      if (length(vals) < 2 || is.null(times)) return(NA)
      unname(stats::coef(stats::lm(vals ~ times))[2])
    }

    t_a <- if (has_time) cd[[time_var]][in_a] else seq_along(a)
    t_b <- if (has_time) cd[[time_var]][in_b] else seq_along(b)

    nap_count <- sum(sapply(b, function(bi) sum(bi > a))) + 0.5 * sum(sapply(b, function(bi) sum(bi == a)))
    nap <- nap_count / (length(a) * length(b))
    pnd <- sum(b > max(a)) / length(b)
    if (intervention_goal == "decrease") {
      nap <- 1 - nap
      pnd <- sum(b < min(a)) / length(b)
    }

    half_a <- floor(length(a) / 2)
    half_b <- floor(length(b) / 2)
    accel_a <- if (half_a >= 2) slope_fn(a[(half_a+1):length(a)], t_a[(half_a+1):length(a)]) -
                                 slope_fn(a[1:half_a], t_a[1:half_a]) else NA
    accel_b <- if (half_b >= 2) slope_fn(b[(half_b+1):length(b)], t_b[(half_b+1):length(b)]) -
                                 slope_fn(b[1:half_b], t_b[1:half_b]) else NA

    ac_a <- if (length(a) > 3) tryCatch(acf(a, lag.max = 1, plot = FALSE)$acf[2], error = function(e) NA) else NA
    ac_b <- if (length(b) > 3) tryCatch(acf(b, lag.max = 1, plot = FALSE)$acf[2], error = function(e) NA) else NA

    data.frame(
      case = cid, tier = k,
      mean_A = mean(a), mean_B = mean(b),
      sd_A = sd(a), sd_B = sd(b),
      slope_A = slope_fn(a, t_a), slope_B = slope_fn(b, t_b),
      cv_A = sd(a) / abs(mean(a)), cv_B = sd(b) / abs(mean(b)),
      autocor_A = ac_a, autocor_B = ac_b,
      mean_diff = mean(b) - mean(a),
      pooled_SD = sqrt(((length(a)-1)*sd(a)^2 + (length(b)-1)*sd(b)^2) / (length(a)+length(b)-2)),
      NAP = nap, PND = pnd,
      phase_length_ratio = length(a) / length(b),
      trend_accel_A = accel_a, trend_accel_B = accel_b,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), feat_list))
}

#' Simulate SCD data
#'
#' Generates simulated single-case design data using a piecewise multilevel
#' model.
#'
#' @param n_cases Number of cases to simulate.
#' @param n_phase_a Number of time points in phase A.
#' @param n_phase_b Number of time points in phase B.
#' @param effect_size Standardized effect size for the phase change.
#' @param trend_effect Standardized trend effect after phase change.
#' @param autocorr Lag-1 autocorrelation coefficient.
#' @param baseline_mean Mean of the baseline phase.
#' @param baseline_sd Standard deviation of the baseline phase.
#' @param baseline_trend Linear trend in the baseline phase.
#'
#' @return A data frame with columns \code{case}, \code{time}, \code{phase},
#'   \code{phase_num}, \code{time_center}, \code{time_since_change}, and
#'   \code{outcome}.
#'
#' @examples
#' simulate_scd_data(n_cases = 3, n_phase_a = 5, n_phase_b = 5,
#'                   effect_size = 1.0)
#'
#' @export
simulate_scd_data <- function(n_cases = 5, n_phase_a = 5, n_phase_b = 5,
                              effect_size = 0, trend_effect = 0, autocorr = 0,
                              baseline_mean = 50, baseline_sd = 10,
                              baseline_trend = 0) {
  rows <- list()
  for (i in seq_len(n_cases)) {
    n_total <- n_phase_a + n_phase_b
    errors <- numeric(n_total)
    errors[1] <- rnorm(1, 0, baseline_sd)
    for (t in 2:n_total) {
      errors[t] <- autocorr * errors[t - 1] + rnorm(1, 0, baseline_sd * sqrt(1 - autocorr^2))
    }

    time_seq <- seq_len(n_total)
    phase_b <- rep(c(0, 1), c(n_phase_a, n_phase_b))
    time_since_change <- (time_seq - n_phase_a - 1) * phase_b

    outcome <- baseline_mean +
      baseline_trend * (time_seq - 1) +
      effect_size * baseline_sd * phase_b +
      trend_effect * baseline_sd * time_since_change +
      errors

    rows[[i]] <- data.frame(
      case = paste0("C", i),
      time = time_seq,
      phase = rep(c("A", "B"), c(n_phase_a, n_phase_b)),
      phase_num = phase_b,
      time_center = time_seq - 1,
      time_since_change = time_since_change,
      outcome = outcome,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' Bayesian power and design analysis by simulation
#'
#' Simulation-based design analysis for multilevel SCD models. Data are
#' generated under an assumed treatment effect, the model is refit by
#' \code{update()} each replication (reusing one compiled Stan model), and the
#' proportion of fits meeting the chosen posterior decision rule is the power.
#' The same multilevel structure covers a single study (cases) and a
#' meta-analysis of single-case data (cases nested in studies, set
#' \code{n_studies > 1} with between-study effect heterogeneity \code{study_sd}).
#'
#' @param n_sims Replications per effect magnitude. The reported Monte Carlo
#'   SE of a power estimate is the Agresti-Coull form sqrt(p'(1 - p') /
#'   (n_valid + 4)) with p' = (k + 2) / (n_valid + 4), where k counts the
#'   fired replications among the \code{n_valid} converged ones; at power 0.5
#'   that is about 0.049 for 100, 0.035 for 200, and 0.025 for 400 valid
#'   replications.
#' @param n_cases Cases per study; a positive integer or one integer per study.
#' @param n_studies Studies. 1 for a single-study design; \code{> 1} adds a
#'   study-level random effect (meta-analysis of single-case data).
#' @param n_phase_a,n_phase_b Time points per phase; a positive integer or one integer per study.
#' @param effect_sizes Treatment-effect magnitudes on the model link scale
#'   (log-rate ratio for count families). For a Gaussian family with
#'   \code{standardized = TRUE} the value is multiplied by the baseline
#'   (within-case) SD (a Cohen's-d-style planning grid). The element 0 yields
#'   the Type I rate.
#' @param autocorr Lag-1 autocorrelation of within-case residuals. Count power
#'   requires zero; serially dependent count power is unsupported.
#' @param icc Intraclass correlation; sets the between-unit SD via
#'   tau = sigma * sqrt(icc / (1 - icc)). With \code{n_studies > 1}, tau^2 is
#'   split evenly between the study and case intercepts so the total ICC stays
#'   at \code{icc}. Count families have no residual SD, so sigma is fixed at
#'   0.5 on the log scale (tau = 0.5 * sqrt(icc / (1 - icc))); the ICC is then
#'   a planning value on the latent log-rate scale, not the observed-count
#'   ICC, and the Poisson or negative binomial noise adds to it. Counts are
#'   generated from a baseline log-rate of log(50) with no serial
#'   dependence.
#' @param study_sd Between-study SD of the treatment effect, on the same
#'   scale as \code{effect_sizes} (SD units for a standardized Gaussian
#'   family, link scale for count families); used when \code{n_studies > 1}.
#'   The fitted model then carries \code{(1 + <tested terms> || study)} when
#'   \code{study_sd > 0} and \code{(1 | study)} otherwise.
#' @param slope_sd Between-case SD of the treatment effect, adding case-level
#'   random-slope heterogeneity to the data-generating process. On the link
#'   scale for count families; in SD units for a standardized Gaussian
#'   family. Default 0 (homogeneous effect). With two or more level-2 units
#'   the fitted model always carries a correlated case random slope on the
#'   level change, \code{(1 + phase_num | case)}, as the analysis formula
#'   does; when \code{slope_sd > 0} and the tested coefficient is a trend
#'   term, an uncorrelated random slope on that term is added.
#' @param fit_autocor Logical; add a level-1 AR(1) term over each series,
#'   `ar(time, gr = case)` or `ar(time, gr = study:case)` with studies, to
#'   the fitted model so it matches the autocorrelated errors the simulator
#'   generates. Gaussian family only (default TRUE). Its coefficient gets
#'   \code{normal(0, 0.5)}, the default of the app.
#' @param iter,chains MCMC iterations and chains.
#' @param design One of \code{"ab"}, \code{"reversal"}, \code{"multiple_baseline"}.
#'   A reversal design is simulated as ABAB with a common effect in every B
#'   phase (steps of +d, -d, +d at the three transitions) and coded with
#'   [process_scd_phases()] as the analysis codes it: \code{time_A1} plus a
#'   cumulative level and trend step per transition (\code{level_A1B1},
#'   \code{level_B1A2}, \code{level_A2B2}, and the matching \code{trend_*}).
#'   The fitted model carries all seven terms and, from two level-2 units on,
#'   the random effects of [default_random_effects()]: the onset step
#'   correlated with the case intercept, the later steps uncorrelated.
#' @param family One of \code{"gaussian"}, \code{"poisson"}, \code{"negbinomial"}.
#' @param planning_target \code{"pooled"} tests the overall effect;
#'   \code{"category"} tests the direct category B minus A coefficient.
#' @param studies_per_category Two study counts, each at least two, for
#'   categories A and B. Category planning sets \code{n_studies} to their sum.
#' @param common_effect Category A effect magnitude in the intervention goal
#'   direction. Category B adds each \code{effect_sizes} magnitude. Both use
#'   the same scale as the effect grid. Category labels are study-level and
#'   allocation is fixed. Ordinal outcomes and assurance are unsupported.
#' @param coefficient Coefficient carrying the effect and being tested:
#'   \code{"level"} (b_phase_num), \code{"trend"} (b_time_center), or
#'   \code{"slope_change"} (b_time_since_change). For a reversal design,
#'   \code{"level"} tests the level steps \code{level_A1B1},
#'   \code{level_B1A2}, \code{level_A2B2}, \code{"slope_change"} the trend
#'   steps \code{trend_A1B1}, \code{trend_B1A2}, \code{trend_A2B2}, and
#'   \code{"trend"} the single baseline slope \code{time_A1}.
#' @param reversal_decision Reversal designs only. \code{"all_transitions"}
#'   (default) fires only when every transition meets the decision rule in its
#'   expected direction: the A1-B1 and A2-B2 steps in the intervention goal
#'   direction and the B1-A2 withdrawal step opposite to it, so the effect is
#'   demonstrated three times. Each transition is then judged one-sided in its
#'   expected direction (\code{prob_direction}: P(oriented step > 0);
#'   \code{prob_meaningful}: P(oriented step >= rope); \code{rope}: posterior
#'   mass inside the ROPE <= 1 - prob_threshold with the posterior mean in the
#'   expected direction). \code{"first_transition"} tests only the onset step
#'   (\code{level_A1B1} or \code{trend_A1B1}) with the single-coefficient rule.
#' @param intervention_goal \code{"increase"} (default) or \code{"decrease"}:
#'   the direction of improvement. \code{effect_sizes}, \code{rope}, and the
#'   \code{prob_meaningful} rule stay in magnitudes, positive for improvement;
#'   with \code{"decrease"} the data are generated with the negated effect and
#'   \code{prob_meaningful} uses P(effect <= -rope). \code{prob_direction} on a
#'   single coefficient stays two-sided.
#' @param decision_rule One of \code{"prob_direction"} (probability of direction
#'   pd >= prob_threshold), \code{"prob_meaningful"} (posterior probability of an
#'   effect at or beyond the ROPE bound in the improvement direction,
#'   P(effect >= rope), or P(effect <= -rope) for
#'   \code{intervention_goal = "decrease"}, >= prob_threshold; one-sided like
#'   \code{prob_meaningful_change}), or \code{"rope"} (posterior probability
#'   inside the ROPE <= 1 - prob_threshold). These mirror the \code{pd},
#'   \code{prob_meaningful_change}, and \code{rope_prob} quantities reported by
#'   the analysis decision summaries.
#' @param prob_threshold Posterior-probability threshold the chosen rule must
#'   meet to flag an effect. The default 0.975 corresponds, for the
#'   \code{prob_direction} rule, to a 5\% two-sided Type I analogue
#'   (pd >= 0.975 mirrors a two-sided alpha of 0.05); 0.95 mirrors a 10\%
#'   two-sided rate.
#' @param rope Half-width of the region of practical equivalence, on the same
#'   scale as \code{effect_sizes}.
#' @param standardized For a Gaussian family, multiply the magnitude by the
#'   baseline (within-case) SD (planning in SD units). Ignored for non-Gaussian
#'   families.
#' @param prior A brms prior. Defaults to the priors the analysis uses:
#'   [generate_adaptive_priors()] on the template data (simulated with no
#'   effect, so the priors do not depend on \code{effect_sizes}) passed through
#'   [assemble_brms_priors()] (\code{normal(0, 2.5 * outcome SD)} on
#'   coefficients for Gaussian, \code{normal(0, 2.5)} on the log scale for
#'   count families, half-Student-t SD priors, \code{lkj(2)}), plus the
#'   \code{normal(0, 0.5)} AR prior truncated to the stationary region
#'   \eqn{(-1, 1)}.
#' @param seed Optional integer. When supplied, sets the R random-number seed
#'   for the template and reseeds every replication from \code{seed}, the
#'   effect size, and the replication index, so each effect-size row is
#'   reproducible and independent of the rest of \code{effect_sizes}; the global RNG state
#'   (\code{.Random.seed}) in place before the call is restored on exit.
#'   \code{NULL} (default) draws from and advances the current RNG state.
#'
#' @return A data frame with one row per magnitude: \code{effect_size},
#'   \code{true_beta} (signed effect on the coefficient scale, negative for
#'   \code{intervention_goal = "decrease"}), \code{power} (the
#'   Type I rate when the magnitude is 0), \code{mc_se} (Agresti-Coull Monte
#'   Carlo SE of power, which stays positive when power is 0 or 1),
#'   \code{type_s} (sign-error rate against the signed true effect,
#'   conditional on the posterior-mean sign; \code{NA} under a one-sided rule,
#'   \code{prob_meaningful} or a reversal \code{all_transitions} decision
#'   that tests more than one term (\code{coefficient = "level"} or
#'   \code{"slope_change"}), since a fired replication then has the expected
#'   sign by construction; a reversal \code{coefficient = "trend"} tests one
#'   term and reports \code{type_s})
#'   and \code{type_m} (posterior-mean
#'   exaggeration ratio among fired
#'   replications; the posterior mean is shrunk by the prior, so this is not
#'   the MLE-based Type M quantity; for a reversal design both use the onset
#'   step), \code{power_all} (the firing rate over all \code{n_sims},
#'   counting non-converged and failed fits as non-detections: a lower bound
#'   on power, and at magnitude 0 an optimistic (low) Type I rate, since
#'   non-convergence clusters near the null), \code{mc_se_all}
#'   (Agresti-Coull Monte Carlo SE of \code{power_all} over \code{n_sims}),
#'   \code{n_valid},
#'   \code{n_nonconverged} (replications that fail the simulation screen:
#'   rank-normalized Rhat >= 1.05, bulk ESS < 100, or a non-computable value
#'   on a tested coefficient, or any divergent transition; every fit uses
#'   \code{adapt_delta = 0.99}). The screen is looser
#'   than the Rhat < 1.01 boundary of the analysis diagnostics because, at the
#'   default 2 chains of 500 post-warmup draws, Rhat crosses 1.01 by noise
#'   alone in a share of well-mixed fits; the bulk ESS floor guards the looser
#'   Rhat. Non-converged replications are excluded from \code{power} and
#'   counted against \code{power_all}, with a warning naming the total, since
#'   non-convergence is more frequent at small between-case variance and near
#'   null effects, so \code{power} (and the Type I rate at magnitude 0) is
#'   conditional on convergence; neither column bounds the Type I rate from
#'   above, and a large gap between them flags that dependence,
#'   \code{n_divergent} (how many of those had a
#'   divergent transition), \code{n_diagnostic_failed} (replications excluded
#'   because sampler diagnostics are unavailable or invalid), \code{n_fit_failed} (replications whose
#'   \code{update()} errored or returned no draws for the tested coefficient),
#'   and \code{n_sims}. An error is raised when the template model (compiled
#'   once and reused by \code{update()}) fails to fit.
#'
#' @examples
#' if (interactive()) {
#' run_power_simulation(n_sims = 50, n_cases = 4,
#'                      effect_sizes = c(0, 0.5, 1.0),
#'                      decision_rule = "prob_direction")
#' }
#'
#' @export
run_power_simulation <- function(n_sims = 200, n_cases = 5, n_studies = 1,
                                 n_phase_a = 5, n_phase_b = 5,
                                 effect_sizes = c(0, 0.5, 1.0, 1.5),
                                 autocorr = 0.2, icc = 0.2, study_sd = 0,
                                 slope_sd = 0,
                                 fit_autocor = TRUE,
                                 iter = 1000, chains = 2,
                                 design = "ab",
                                 family = "gaussian",
                                 coefficient = "level",
                                 decision_rule = "prob_direction",
                                 reversal_decision = c("all_transitions", "first_transition"),
                                 intervention_goal = c("increase", "decrease"),
                                 prob_threshold = 0.975,
                                 rope = 0.1, standardized = TRUE,
                                 prior = NULL, seed = NULL,
                                 planning_target = c("pooled", "category"),
                                 studies_per_category = c(4, 4), common_effect = 0.5) {
  planning_target <- match.arg(planning_target)
  if (planning_target == "category") {
    if (!is.numeric(studies_per_category) || length(studies_per_category) != 2L ||
        any(!is.finite(studies_per_category)) || any(studies_per_category < 2) ||
        any(studies_per_category != floor(studies_per_category)))
      stop("studies_per_category requires two integers of at least 2.", call. = FALSE)
    if (!is.numeric(common_effect) || length(common_effect) != 1L ||
        !is.finite(common_effect) || common_effect < 0)
      stop("common_effect must be a finite nonnegative magnitude.", call. = FALSE)
    n_studies <- sum(studies_per_category)
  }
  integers <- list(n_sims = n_sims, n_studies = n_studies, iter = iter,
                   chains = chains)
  valid_integer <- vapply(integers, function(x)
    is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 1 && x == floor(x), logical(1))
  if (!all(valid_integer)) stop("Positive integer required: ",
    paste(names(integers)[!valid_integer], collapse = ", "), call. = FALSE)
  n_cases <- .power_study_sizes(n_cases, n_studies)
  n_phase_a <- .power_study_sizes(n_phase_a, n_studies)
  n_phase_b <- .power_study_sizes(n_phase_b, n_studies)
  if (chains < 2) stop("chains must be at least 2 for convergence diagnostics.", call. = FALSE)
  if (iter < 4) stop("iter must be at least 4.", call. = FALSE)
  scalars <- list(autocorr = autocorr, icc = icc, study_sd = study_sd,
                  slope_sd = slope_sd, prob_threshold = prob_threshold, rope = rope)
  finite_scalar <- vapply(scalars, function(x)
    is.numeric(x) && length(x) == 1L && is.finite(x), logical(1))
  if (!all(finite_scalar)) stop("Finite numeric scalar required: ",
    paste(names(scalars)[!finite_scalar], collapse = ", "), call. = FALSE)
  if (abs(autocorr) >= 1) stop("autocorr must be strictly between -1 and 1.", call. = FALSE)
  if (icc < 0 || icc > 0.95) stop("icc must be between 0 and 0.95.", call. = FALSE)
  if (study_sd < 0 || slope_sd < 0) stop("study_sd and slope_sd must be nonnegative.", call. = FALSE)
  if (prob_threshold <= 0.5 || prob_threshold >= 1)
    stop("prob_threshold must be strictly between 0.5 and 1.", call. = FALSE)
  if (rope < 0) stop("rope must be nonnegative.", call. = FALSE)
  if (!is.numeric(effect_sizes) || !length(effect_sizes) || any(!is.finite(effect_sizes)))
    stop("effect_sizes must contain finite numeric values.", call. = FALSE)
  if (!is.logical(fit_autocor) || length(fit_autocor) != 1L || is.na(fit_autocor) ||
      !is.logical(standardized) || length(standardized) != 1L || is.na(standardized))
    stop("fit_autocor and standardized must be single logical values.", call. = FALSE)
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != floor(seed)))
    stop("seed must be an integer between 0 and the integer maximum.", call. = FALSE)
  if (!is.null(seed)) {
    restore_seed <- .seed_restorer()
    on.exit(restore_seed(), add = TRUE)
    set.seed(seed)
  }
  design        <- match.arg(design,      c("ab", "reversal", "multiple_baseline"))
  family        <- match.arg(family,      c("gaussian", "poisson", "negbinomial"))
  coefficient   <- match.arg(coefficient, c("level", "trend", "slope_change"))
  decision_rule <- match.arg(decision_rule,
                             c("prob_direction", "prob_meaningful", "rope"))
  reversal_decision <- match.arg(reversal_decision)
  intervention_goal <- match.arg(intervention_goal)
  goal_sign <- if (intervention_goal == "decrease") -1 else 1

  tested <- .power_terms(design, coefficient, reversal_decision)
  params <- paste0("b_", tested$terms,
                   if (planning_target == "category") ":category" else "")
  brms_family <- switch(family,
                        gaussian    = stats::gaussian(),
                        poisson     = stats::poisson(),
                        negbinomial = brms::negbinomial())

  baseline_sd <- 10
  n_units <- sum(n_cases)
  if (n_units <= 5) {
    warning(sprintf(paste0(
      "Only %d level-2 unit(s): between-unit variance is weakly identified; read the ",
      "effect_size = 0 row as the realized Type I rate."), n_units))
  }
  if (autocorr != 0 && family %in% c("poisson", "negbinomial")) {
    stop("Count power does not support serial dependence; set autocorr = 0.", call. = FALSE)
  }
  rope_half <- if (family == "gaussian" && isTRUE(standardized)) rope * baseline_sd else rope
  rope_param <- c(-rope_half, rope_half)
  f <- .build_power_formula(n_cases = sum(n_cases) / n_studies, n_studies = n_studies,
                            fit_autocor = fit_autocor, family = family,
                            coefficient = coefficient, slope_sd = slope_sd,
                            study_sd = study_sd, design = design,
                            planning_target = planning_target)

  # no effect in the template, so default priors do not depend on the grid order
  compile_data <- .simulate_power_data(design = design, family = family,
                                       n_cases = n_cases, n_studies = n_studies,
                                       n_phase_a = n_phase_a, n_phase_b = n_phase_b,
                                       effect = 0, coefficient = coefficient,
                                       autocorr = autocorr, icc = icc,
                                       study_sd = study_sd, slope_sd = slope_sd,
                                       standardized = standardized,
                                       baseline_sd = baseline_sd,
                                       studies_per_category = if (planning_target == "category") studies_per_category else NULL,
                                       common_effect = goal_sign * common_effect)
  has_ar <- fit_autocor && family == "gaussian"
  if (is.null(prior)) {
    prior <- .power_priors(compile_data, f, family, n_units, has_ar)
  }
  # threading is incompatible with the latent ar() term
  backend <- getOption("scdbayes.backend", .default_backend())
  stan_seed <- function() if (is.null(seed)) NA else sample.int(.Machine$integer.max, 1L)
  control <- list(adapt_delta = 0.99, max_treedepth = .max_treedepth())
  template <- tryCatch(brms::brm(
    formula = f, data = compile_data, family = brms_family, prior = prior,
    iter = iter, chains = chains, warmup = floor(iter / 2), control = control,
    cores = min(chains, parallel::detectCores()),
    threads = if (has_ar) NULL else getOption("scdbayes.threads", brms::threading(2)),
    backend = backend,
    stan_model_args = .stan_model_args(backend),
    seed = stan_seed(),
    refresh = 0, silent = 2), error = function(e) e)
  if (inherits(template, "error")) {
    stop("Power simulation template model failed to fit: ", conditionMessage(template),
         call. = FALSE)
  }

  rows <- lapply(effect_sizes, function(d) {
    fired <- logical(0)
    post_means <- numeric(0)
    n_nonconverged <- 0L
    n_divergent <- 0L
    n_diagnostic_failed <- 0L
    n_fit_failed <- 0L
    for (s in seq_len(n_sims)) {
      if (!is.null(seed)) set.seed(.replication_seed(seed, d, s))
      sim_data <- .simulate_power_data(design = design, family = family,
                                       n_cases = n_cases, n_studies = n_studies,
                                       n_phase_a = n_phase_a, n_phase_b = n_phase_b,
                                       effect = goal_sign * d, coefficient = coefficient,
                                       autocorr = autocorr, icc = icc,
                                       study_sd = study_sd, slope_sd = slope_sd,
                                       standardized = standardized,
                                       baseline_sd = baseline_sd,
                                       studies_per_category = if (planning_target == "category") studies_per_category else NULL,
                                       common_effect = goal_sign * common_effect)
      fit <- tryCatch(
        stats::update(template, newdata = sim_data, recompile = FALSE, control = control,
                      seed = stan_seed(), refresh = 0, silent = 2),
        error = function(e) NULL)
      if (is.null(fit)) { n_fit_failed <- n_fit_failed + 1L; next }
      draws <- tryCatch(posterior::as_draws_df(fit), error = function(e) NULL)
      if (is.null(draws) || !all(params %in% colnames(draws))) {
        n_fit_failed <- n_fit_failed + 1L; next
      }
      div_rep <- .power_divergences(fit)
      if (is.na(div_rep)) {
        n_diagnostic_failed <- n_diagnostic_failed + 1L
        n_nonconverged <- n_nonconverged + 1L
        next
      }
      if (div_rep > 0) n_divergent <- n_divergent + 1L
      if (!.sim_converged(draws, params) || div_rep > 0) { n_nonconverged <- n_nonconverged + 1L; next }
      effs <- lapply(params, function(p) draws[[p]])
      fired <- c(fired, if (length(effs) == 1) {
        .power_decision(effs[[1]], decision_rule, prob_threshold, rope_param, intervention_goal)
      } else {
        .transition_decision(effs, goal_sign * tested$signs, decision_rule, prob_threshold,
                             rope_param)
      })
      post_means <- c(post_means, mean(effs[[1]], na.rm = TRUE))
    }
    n_valid <- length(fired)
    power <- if (n_valid > 0) mean(fired) else NA_real_
    power_ci <- if (n_valid > 0) stats::binom.test(sum(fired), n_valid)$conf.int else c(NA_real_, NA_real_)
    attempted_ci <- stats::binom.test(sum(fired), n_sims)$conf.int
    mc_se <- .agresti_coull_se(sum(fired), n_valid)
    true_beta <- goal_sign * (if (family == "gaussian" && isTRUE(standardized)) d * baseline_sd else d)
    type_s <- NA_real_
    type_m <- NA_real_
    if (abs(true_beta) > 0 && any(fired)) {
      hit <- post_means[fired]
      # a one-sided rule only fires in the expected direction, so its sign-error rate is ~0
      one_sided <- decision_rule == "prob_meaningful" || length(params) > 1
      type_s <- if (one_sided) NA_real_ else mean(sign(hit) != sign(true_beta))
      type_m <- mean(abs(hit) / abs(true_beta))
    }
    data.frame(effect_size = d, true_beta = true_beta, power = power, mc_se = mc_se,
               power_all = sum(fired) / n_sims, mc_se_all = .agresti_coull_se(sum(fired), n_sims),
               type_s = type_s, type_m = type_m, n_valid = n_valid,
               n_nonconverged = n_nonconverged, n_divergent = n_divergent,
               n_diagnostic_failed = n_diagnostic_failed,
               n_fit_failed = n_fit_failed, n_sims = n_sims,
               power_lo = power_ci[1], power_hi = power_ci[2],
               power_all_lo = attempted_ci[1], power_all_hi = attempted_ci[2],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (sum(out$n_nonconverged + out$n_fit_failed) > 0) {
    warning(sprintf(paste0(
      "%d of %d replications excluded (%d divergent, %d missing diagnostics, %d fit failures); ",
      "power is conditional on convergence, power_all counts them as non-detections."),
      sum(out$n_nonconverged + out$n_fit_failed), sum(out$n_sims), sum(out$n_divergent),
      sum(out$n_diagnostic_failed), sum(out$n_fit_failed)), call. = FALSE)
  }
  attr(out, "specification") <- list(planning_target = planning_target,
    n_cases = n_cases, n_phase_a = n_phase_a, n_phase_b = n_phase_b,
    studies_per_category = if (planning_target == "category") studies_per_category else NULL,
    common_effect = if (planning_target == "category") common_effect else NULL,
    category_contrast = if (planning_target == "category") "B minus A" else NULL)
  out
}

#' @noRd
.power_divergences <- function(fit) {
  values <- tryCatch(brms::nuts_params(fit, pars = "divergent__")$Value,
                     error = function(e) NULL)
  if (!is.numeric(values) || !length(values) || anyNA(values) ||
      any(!values %in% c(0, 1))) return(NA_real_)
  sum(values)
}

#' @noRd
.sim_converged <- function(draws, params, rhat_max = 1.05, ess_min = 100) {
  # judged on the tested coefficients, not nuisance parameters
  stat <- function(fn) tryCatch(vapply(params, function(p)
    fn(posterior::extract_variable_matrix(draws, p)), numeric(1)),
    error = function(e) NA_real_)
  r <- stat(posterior::rhat)
  ess <- stat(posterior::ess_bulk)
  length(r) > 0 && all(!is.na(r) & r < rhat_max) && all(!is.na(ess) & ess >= ess_min)
}

#' @noRd
.replication_seed <- function(seed, effect, s) {
  # effect resolved to 0.001; the primes keep (effect, s) pairs apart
  as.integer((seed + 104729 * s + 15485863 * round(effect * 1000)) %% .Machine$integer.max)
}

#' @noRd
.seed_restorer <- function() {
  g <- globalenv()
  if (!exists(".Random.seed", envir = g, inherits = FALSE)) {
    return(function() if (exists(".Random.seed", envir = g, inherits = FALSE))
      rm(".Random.seed", envir = g))
  }
  old <- get(".Random.seed", envir = g, inherits = FALSE)
  function() assign(".Random.seed", old, envir = g)
}

#' @noRd
.agresti_coull_se <- function(k, n) {
  # stays positive when power is exactly 0 or 1
  if (n <= 0) return(NA_real_)
  p_adj <- (k + 2) / (n + 4)
  sqrt(p_adj * (1 - p_adj) / (n + 4))
}

#' @noRd
.power_decision <- function(eff, rule, prob_threshold, rope, goal = "increase") {
  eff <- eff[is.finite(eff)]
  if (length(eff) == 0) return(FALSE)
  gain <- if (identical(goal, "decrease")) -eff else eff
  switch(rule,
    prob_direction  = max(mean(eff > 0), mean(eff < 0)) >= prob_threshold,
    prob_meaningful = mean(gain >= rope[2]) >= prob_threshold,
    rope            = mean(eff >= rope[1] & eff <= rope[2]) <= 1 - prob_threshold,
    FALSE)
}

#' @noRd
.transition_decision <- function(effs, signs, rule, prob_threshold, rope) {
  meets <- function(eff, s) {
    gain <- s * eff[is.finite(eff)]
    length(gain) > 0 && switch(rule,
      prob_direction  = mean(gain > 0) >= prob_threshold,
      prob_meaningful = mean(gain >= rope[2]) >= prob_threshold,
      rope            = mean(gain >= rope[1] & gain <= rope[2]) <= 1 - prob_threshold &&
                          mean(gain) > 0,
      FALSE)
  }
  all(mapply(meets, effs, signs))
}

#' @noRd
.power_terms <- function(design, coefficient, reversal_decision = "all_transitions") {
  if (design != "reversal") {
    term <- switch(coefficient, level = "phase_num", trend = "time_center",
                   slope_change = "time_since_change")
    return(list(terms = term, signs = 1))
  }
  # the B1-A2 withdrawal reverses the effect
  steps <- switch(coefficient, level = "level", trend = NULL, slope_change = "trend")
  if (is.null(steps)) return(list(terms = "time_A1", signs = 1))
  keep <- if (reversal_decision == "first_transition") 1 else 1:3
  list(terms = paste0(steps, "_", c("A1B1", "B1A2", "A2B2"))[keep], signs = c(1, -1, 1)[keep])
}

#' @noRd
.power_priors <- function(data, formula, family, n_units, has_ar) {
  formula_str <- paste(deparse(formula), collapse = " ")
  adaptive <- generate_adaptive_priors(data, list(n_levels = if (n_units >= 2) 2 else 1),
                                       model_type = family)
  prior <- priors_to_brms(assemble_brms_priors(
    "default", NULL, NULL, FALSE, family, adaptive,
    treatment_coef = "phase_num", has_random_slope = has_random_slope(formula_str),
    formula_str = formula_str))
  # brms leaves ar unbounded, so stationarity is imposed through the bounds
  if (has_ar) prior <- c(prior, brms::prior_string("normal(0, 0.5)", class = "ar", lb = -1, ub = 1))
  prior
}

#' @noRd
.phase_structure <- function(design, i, n_phase_a, n_phase_b) {
  if (design == "reversal") {
    list(labels = rep(c("A", "B", "A", "B"), c(n_phase_a, n_phase_b, n_phase_a, n_phase_b)),
         num    = rep(c(0, 1, 0, 1),         c(n_phase_a, n_phase_b, n_phase_a, n_phase_b)))
  } else if (design == "multiple_baseline") {
    a_len <- n_phase_a + (i - 1)
    list(labels = rep(c("A", "B"), c(a_len, n_phase_b)),
         num    = rep(c(0, 1),     c(a_len, n_phase_b)))
  } else {
    list(labels = rep(c("A", "B"), c(n_phase_a, n_phase_b)),
         num    = rep(c(0, 1),     c(n_phase_a, n_phase_b)))
  }
}

.power_study_sizes <- function(x, n_studies) {
  if (!is.numeric(x) || !length(x) || any(!is.finite(x)) ||
      any(x < 1) || any(x != floor(x)))
    stop(paste(deparse(substitute(x)), "must contain positive integers."), call. = FALSE)
  if (!length(x) %in% c(1L, n_studies))
    stop("Study sizes require one value or one per study.", call. = FALSE)
  rep(x, length.out = n_studies)
}

#' @noRd
.simulate_power_data <- function(design, family, n_cases, n_studies, n_phase_a, n_phase_b,
                                 effect, coefficient, autocorr, icc, study_sd,
                                 slope_sd = 0, standardized,
                                 baseline_mean = 50, baseline_sd = 10,
                                 studies_per_category = NULL, common_effect = 0) {
  n_cases <- .power_study_sizes(n_cases, n_studies)
  n_phase_a <- .power_study_sizes(n_phase_a, n_studies)
  n_phase_b <- .power_study_sizes(n_phase_b, n_studies)
  icc <- pmax(0, pmin(0.95, icc))
  tau <- if (family %in% c("poisson", "negbinomial")) {
    # latent log-scale sigma of 0.5 stands in for the residual SD counts lack
    sqrt(icc / pmax(1 - icc, 0.01)) * 0.5
  } else {
    baseline_sd * sqrt(icc / pmax(1 - icc, 0.01))
  }
  # link scale for counts, raw outcome units for a standardized Gaussian effect
  slope_tau <- if (family == "gaussian" && isTRUE(standardized)) slope_sd * baseline_sd else slope_sd
  studies <- seq_len(max(1, n_studies))
  categories <- if (is.null(studies_per_category)) rep(0, length(studies)) else
    rep(c(0, 1), studies_per_category)
  if (length(categories) != length(studies))
    stop("Category allocation must equal n_studies.", call. = FALSE)
  # with studies, the between-unit variance tau^2 splits evenly across study and case
  multi <- length(studies) > 1
  study_tau <- if (multi) tau / sqrt(2) else 0
  case_tau  <- if (multi) tau / sqrt(2) else tau
  study_intercepts <- stats::rnorm(length(studies), 0, study_tau)
  study_effects    <- stats::rnorm(length(studies), 0, study_sd)
  # one independent deviation per transition, matching the uncorrelated reversal steps fitted
  n_steps <- if (design == "reversal" && coefficient != "trend") 3 else 1
  rows <- list()
  for (sidx in studies) {
    case_intercepts <- stats::rnorm(n_cases[sidx], 0, case_tau)
    case_slopes     <- matrix(stats::rnorm(n_cases[sidx] * n_steps, 0, slope_tau), n_cases[sidx])
    eff_s <- if (is.null(studies_per_category)) effect + study_effects[sidx] else
      common_effect + categories[sidx] * effect + study_effects[sidx]
    for (i in seq_len(n_cases[sidx])) {
      ph <- .phase_structure(design, i, n_phase_a[sidx], n_phase_b[sidx])
      rows[[length(rows) + 1]] <- .gen_case(
        study_id = paste0("S", sidx), case_id = paste0("S", sidx, "C", i),
        phase_labels = ph$labels, phase_num = ph$num, family = family,
        autocorr = autocorr, magnitude = eff_s, coefficient = coefficient,
        baseline_mean = baseline_mean, baseline_sd = baseline_sd,
        case_intercept = case_intercepts[i] + study_intercepts[sidx],
        case_slope = case_slopes[i, ], standardized = standardized)
    }
  }
  out <- do.call(rbind, rows)
  if (!is.null(studies_per_category))
    out$category <- categories[match(out$study, paste0("S", studies))]
  if (design == "reversal") process_scd_phases(out, list(), "reversal") else out
}

#' @noRd
.gen_case <- function(study_id, case_id, phase_labels, phase_num, family, autocorr,
                      magnitude, coefficient, baseline_mean, baseline_sd,
                      case_intercept, case_slope = 0, standardized) {
  n_total <- length(phase_num)
  time_seq <- seq_len(n_total)
  time_center <- time_seq - 1
  in_tx <- phase_num >= 1
  run_id <- cumsum(c(1L, as.integer(diff(in_tx) != 0)))
  # 0 at the first intervention session, so the level effect is the change at onset
  time_since_change <- ifelse(in_tx, sequence(tabulate(run_id)) - 1, 0)
  design_col <- switch(coefficient,
                       level        = phase_num,
                       trend        = time_center,
                       slope_change = time_since_change)
  # case deviations load on the cumulative step of each transition, as the fitted terms do
  onsets <- which(diff(in_tx) != 0) + 1
  dev_cols <- if (coefficient == "trend" || length(onsets) == 0) {
    matrix(design_col)
  } else if (coefficient == "level") {
    vapply(onsets, function(k) as.numeric(time_seq >= k), numeric(n_total))
  } else {
    vapply(onsets, function(k) pmax(0, time_seq - k), numeric(n_total))
  }
  case_dev <- drop(dev_cols %*% rep_len(case_slope, ncol(dev_cols)))
  if (family == "gaussian") {
    case_mean <- baseline_mean + case_intercept
    errors <- numeric(n_total)
    errors[1] <- stats::rnorm(1, 0, baseline_sd)
    if (n_total >= 2) for (t in 2:n_total) {
      errors[t] <- autocorr * errors[t - 1] +
                   stats::rnorm(1, 0, baseline_sd * sqrt(1 - autocorr^2))
    }
    beta <- if (isTRUE(standardized)) magnitude * baseline_sd else magnitude
    outcome <- case_mean + beta * design_col + case_dev + errors
  } else {
    log_mu <- log(pmax(baseline_mean, 1)) + case_intercept +
              magnitude * design_col + case_dev
    outcome <- if (family == "poisson") stats::rpois(n_total, exp(log_mu))
               else stats::rnbinom(n_total, mu = exp(log_mu), size = 3)
  }
  data.frame(study = study_id, case = case_id, time = time_seq,
             phase = phase_labels, phase_num = phase_num,
             time_center = time_center, time_since_change = time_since_change,
             outcome = outcome, stringsAsFactors = FALSE)
}

#' Power-simulation model formula
#'
#' @details With two or more level-2 units the case term is
#'   \code{(1 + phase_num | case)} regardless of \code{slope_sd}, matching the
#'   analysis formula: omitting a random slope that exists inflates the Type
#'   I error rate with few cases. A tested trend coefficient with
#'   \code{slope_sd > 0} adds an uncorrelated slope on that term. A reversal
#'   design uses the fixed terms of the analysis coding and its case terms,
#'   \code{(1 + level_A1B1 | case) + (0 + level_B1A2 + level_A2B2 || case)}.
#' @noRd
.build_power_formula <- function(n_cases, n_studies = 1, fit_autocor = TRUE,
                                 family = "gaussian", coefficient = "level",
                                 slope_sd = 0, study_sd = 0, design = "ab",
                                 planning_target = "pooled") {
  reversal <- design == "reversal"
  base <- if (reversal) {
    paste("outcome ~ time_A1 + level_A1B1 + trend_A1B1 + level_B1A2 + trend_B1A2",
          "+ level_A2B2 + trend_A2B2")
  } else {
    "outcome ~ time_center + phase_num + time_since_change"
  }
  slope_term <- paste(.power_terms(design, coefficient)$terms, collapse = " + ")
  if (planning_target == "category")
    base <- paste0(base, " + category + ",
      paste(paste0(.power_terms(design, coefficient)$terms, ":category"), collapse = " + "))
  level_re <- if (reversal) {
    "(1 + level_A1B1 | case) + (0 + level_B1A2 + level_A2B2 || case)"
  } else {
    "(1 + phase_num | case)"
  }
  level_tested <- coefficient == "level"
  case_re <- if (n_cases * max(1, n_studies) <= 1) {
    ""
  } else {
    paste0(" + ", level_re,
           if (slope_sd > 0 && !level_tested) sprintf(" + (0 + %s || case)", slope_term))
  }
  # a study slope matches the between-study effect heterogeneity drawn when study_sd > 0
  study_re <- if (n_studies <= 1) {
    ""
  } else if (study_sd > 0) {
    sprintf(" + (1 + %s || study)", slope_term)
  } else {
    " + (1 | study)"
  }
  # case IDs are unique across studies here, but the tier key keeps gr aligned with tier_vars()
  ar_gr <- if (n_studies > 1) "study:case" else "case"
  ar_term <- if (fit_autocor && family == "gaussian") sprintf(" + ar(time = time, gr = %s, p = 1)", ar_gr) else ""
  stats::as.formula(paste0(base, case_re, study_re, ar_term))
}

#' Sequential Bayesian updating
#'
#' Refits the model on cumulative case subsets (cases 1-2, then 1-3, ...) with
#' the same prior each refit, tracking how the treatment-effect estimate and
#' its interval change as cases accrue. This is a cumulative-refit variant of
#' sequential Bayesian updating; the posterior is not literally carried
#' forward as the next prior.
#'
#' @param data Data frame containing SCD data.
#' @param case_var Name of the case variable.
#' @param formula_str Model formula string; defaults to
#'   \code{"outcome ~ phase + (1|case)"}.
#' @param family Model family (default \code{"gaussian"}).
#' @param iter Number of sampling iterations.
#' @param chains Number of MCMC chains.
#' @param priors A \code{brmsprior} object applied to every refit. A
#'   supplied prior such as the analysis model's \code{model$prior} was
#'   scaled on all cases, so early refits then draw on outcome scale from
#'   cases not yet in the cumulative subset. \code{NULL} (default) fixes
#'   every prior class from the first two cases only, computed once so every
#'   refit shares the same prior: the package normal coefficient (\code{b})
#'   and Student-t \code{sd} priors, plus the \pkg{brms} default Student-t
#'   \code{Intercept} and \code{sigma} priors evaluated on those two cases
#'   rather than recomputed on each refit's data; never brms flat priors.
#'
#' @return A list with \code{tracking} (data frame of cumulative estimates)
#'   and \code{final_model}.
#'
#' @examples
#' if (interactive()) {
#' run_sequential_update(simulate_scd_data(n_cases = 5))
#' }
#'
#' @export
run_sequential_update <- function(data, case_var = "case", formula_str = NULL,
                                  family = "gaussian", iter = 2000, chains = 2,
                                  priors = NULL) {
  cases <- unique(data[[case_var]])
  if (length(cases) < 3) stop("Need at least 3 cases for sequential update")

  if (is.null(formula_str)) formula_str <- "outcome ~ phase + (1|case)"

  seq_formula <- gsub("\\s*\\+\\s*ar\\([^)]+\\)", "", formula_str)
  if (!identical(seq_formula, formula_str)) {
    warning("Autocorrelation term ar(...) dropped for the sequential update; ",
            "the cumulative refits are fitted without it.")
  }

  seq_formula_obj <- stats::as.formula(seq_formula)
  cumulative_data <- data[data[[case_var]] %in% cases[1:2], ]
  # scaled on the first two cases so no refit uses cases it has not seen
  if (is.null(priors)) {
    defaults <- brms::get_prior(seq_formula_obj, data = cumulative_data, family = family)
    keep <- defaults$class %in% c("Intercept", "sigma") & nzchar(defaults$prior) &
      !nzchar(defaults$coef) & !nzchar(defaults$group) & !nzchar(defaults$dpar) &
      !nzchar(defaults$nlpar)
    priors <- .default_priors(cumulative_data, seq_formula_obj, family)
    if (any(keep)) {
      priors <- c(priors, brms::prior_string(defaults$prior[keep], class = defaults$class[keep]))
    }
  }
  # an ar prior has no parameter once ar() is dropped
  if (!is.null(priors$class)) priors <- priors[priors$class != "ar", ]
  use_threads <- getOption("scdbayes.threads", brms::threading(2))
  current_model <- suppressWarnings(brms::brm(
    seq_formula_obj, data = cumulative_data, family = family, prior = priors,
    iter = iter, chains = chains,
    cores = min(chains, parallel::detectCores()),
    threads = use_threads,
    backend = getOption("scdbayes.backend", .default_backend()),
    stan_model_args = .stan_model_args(getOption("scdbayes.backend", .default_backend())),
    refresh = 0, silent = 2
  ))
  if (is.null(current_model)) stop("Initial model fitting failed")

  fe <- brms::fixef(current_model)
  phase_row <- grep("^level_", rownames(fe))
  if (length(phase_row) == 0) {
    phase_row <- grep("phase|treatment|^trend_", rownames(fe))
  }
  if (length(phase_row) == 0) return(NULL)

  tracking <- data.frame(
    n_cases = 2,
    estimate = fe[phase_row[1], "Estimate"],
    lower = fe[phase_row[1], "Q2.5"],
    upper = fe[phase_row[1], "Q97.5"],
    ci_width = fe[phase_row[1], "Q97.5"] - fe[phase_row[1], "Q2.5"],
    stringsAsFactors = FALSE
  )

  for (i in 3:length(cases)) {
    cumulative_data <- data[data[[case_var]] %in% cases[1:i], ]
    current_model <- tryCatch(
      update(current_model, newdata = cumulative_data, recompile = FALSE,
                   iter = iter, chains = chains, refresh = 0, silent = 2),
      error = function(e) NULL
    )
    if (is.null(current_model)) next

    fe <- brms::fixef(current_model)
    tracking <- rbind(tracking, data.frame(
      n_cases = i,
      estimate = fe[phase_row[1], "Estimate"],
      lower = fe[phase_row[1], "Q2.5"],
      upper = fe[phase_row[1], "Q97.5"],
      ci_width = fe[phase_row[1], "Q97.5"] - fe[phase_row[1], "Q2.5"]
    ))
  }
  list(tracking = tracking, final_model = current_model)
}

#' Posterior variance components and ICCs
#'
#' Summarizes the residual variance, the random-intercept variance of each
#' grouping factor, and the intraclass correlations from the posterior
#' draws of a fitted model. For each draw,
#' \eqn{ICC_g = \tau_g^2 / (\sum_h \tau_h^2 + \sigma^2)}, where
#' \eqn{\tau_g} is the intercept SD (\code{sd_<g>__Intercept}) of grouping
#' factor \eqn{g} and \eqn{\sigma^2} the residual response variance; the residual enters the
#' denominator once. The intervals are posterior quantiles, so they carry
#' the full posterior uncertainty of every variance component.
#'
#' @details The name is kept for compatibility. It previously refit the
#'   model to posterior-predictive replicates, which double counted the
#'   residual and held the case effects at their estimates, so its intervals
#'   were too narrow. A parametric bootstrap of a Bayesian multilevel SCD
#'   model is unnecessary: the posterior draws already give the distribution of each component. For
#'   families without a residual SD (\code{sigma}), such as Poisson or
#'   negative binomial, or for a model with \code{sigma} predictors, the
#'   ICCs are \code{NA} with a message, since a level-1 variance on the
#'   response scale is not a single parameter there.
#'   The reported \code{sigma2} includes the Student-t factor
#'   \code{nu/(nu - 2)}. Natural-residual stationary AR(1) models include
#'   the factor \code{1/(1 - ar^2)}. A covariance-form latent Gaussian
#'   AR(1) adds \code{sderr^2/(1 - ar^2)} to the independent response
#'   variance rather than rescaling its \code{sigma^2}. Conditional latent
#'   serial models have occasion-dependent boundary variances and return
#'   \code{NA}; no interior-occasion ICC is substituted.
#'   Constant response variances are supported for identity-link Gaussian,
#'   Student-t, and skew-normal families only. ICCs are undefined if any
#'   retained response SD is nonfinite or negative, if any retained Student-t
#'   draw has \code{nu <= 2}, if the fitted serial
#'   specification cannot establish stationary AR(1), or if the family or
#'   link prevents a constant residual response variance.
#'   Random-slope models report intercept-reference ratios, not correlations
#'   at arbitrary sessions or covariate values.
#'
#' @param model A \code{brmsfit} object.
#' @param n_boot Positive finite integer number of posterior draws summarized (evenly spaced over the
#'   chains; all draws when fewer are available).
#'
#' @return A list with \code{summary} (a matrix with rows \code{mean},
#'   \code{sd}, \code{ci_lower.2.5\%}, \code{ci_upper.97.5\%} and one column
#'   per quantity: \code{sigma2}, each grouping factor's intercept variance,
#'   and \code{iccs.<group>}), \code{n_successful} (draws used), and
#'   \code{n_attempted} (\code{n_boot}); \code{NULL} for a non-\code{brmsfit}
#'   or when fewer than 10 draws are available.
#'
#' @examples
#' if (interactive()) {
#' fit <- brms::brm(outcome ~ phase + (1 | case), data = simulate_scd_data())
#' bootstrap_variance_components(fit, n_boot = 1000)
#' }
#'
#' @export
bootstrap_variance_components <- function(model, n_boot = 200) {
  if (!is.numeric(n_boot) || length(n_boot) != 1L || !is.finite(n_boot) ||
      n_boot < 1 || n_boot != floor(n_boot))
    stop("n_boot must be a positive finite integer.", call. = FALSE)
  if (!inherits(model, "brmsfit")) return(NULL)
  draws <- tryCatch(as.data.frame(brms::as_draws_df(model)), error = function(e) NULL)
  if (is.null(draws) || nrow(draws) < 10) return(NULL)
  idx <- unique(round(seq(1, nrow(draws), length.out = min(n_boot, nrow(draws)))))
  draws <- draws[idx, , drop = FALSE]

  sd_cols <- grep("^sd_.+__Intercept$", names(draws), value = TRUE)
  groups <- sub("^sd_(.+)__Intercept$", "\\1", sd_cols)
  tau2 <- as.matrix(draws[sd_cols])^2
  sigma2 <- if ("sigma" %in% names(draws)) draws$sigma^2 else rep(NA_real_, nrow(draws))
  family <- model$family$family %||% "gaussian"
  student <- identical(family, "student")
  undefined <- ("sigma" %in% names(draws) && any(!is.finite(draws$sigma) | draws$sigma < 0)) ||
    (student && (!"nu" %in% names(draws) || any(draws$nu <= 2 | !is.finite(draws$nu))))
  if (student && !undefined) sigma2 <- sigma2 * draws$nu / (draws$nu - 2)
  ar_cols <- grep("^ar\\[[0-9]+\\]$", names(draws), value = TRUE)
  serial <- length(ar_cols) > 0 || any(grepl("^ma\\[", names(draws)))
  spec <- if (serial) tryCatch(.model_ar_spec(model), error = function(e) NULL) else NULL
  stationary <- serial && !is.null(spec) && spec$p == 1L && spec$q == 0L &&
    length(ar_cols) == 1L && identical(ar_cols, "ar[1]") &&
    !any(grepl("^ma\\[", names(draws))) && all(is.finite(draws[[ar_cols]])) &&
    all(abs(draws[[ar_cols]]) < 1)
  latent <- "sderr" %in% names(draws)
  latent_valid <- latent && stationary && isTRUE(spec$cov) &&
    all(is.finite(draws$sderr) & draws$sderr >= 0)
  if (stationary && !latent) sigma2 <- sigma2 / (1 - draws[[ar_cols]]^2)
  if (latent_valid) sigma2 <- sigma2 + draws$sderr^2 / (1 - draws[[ar_cols]]^2)
  unavailable <- undefined || (serial && !stationary) || (latent && !latent_valid) ||
    (serial && family == "skew_normal" && !latent) ||
    !family %in% c("gaussian", "student", "skew_normal") ||
    !identical(model$family$link %||% "identity", "identity")
  if (unavailable) {
    sigma2[] <- NA_real_
    message("bootstrap_variance_components: constant response variance is undefined for this specification; ICCs are NA.")
  }
  if (!unavailable && all(is.na(sigma2)) && length(sd_cols)) {
    message("bootstrap_variance_components: no single residual SD (sigma) in this model, ",
            "so the ICCs are NA; the intercept variances are on the link scale.")
  }
  iccs <- tau2 / (rowSums(tau2) + sigma2)
  mat <- cbind(sigma2, tau2, iccs)
  colnames(mat) <- c("sigma2", groups, paste0("iccs.", groups))
  summary_fn <- function(x) c(mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE),
                              ci_lower = stats::quantile(x, 0.025, na.rm = TRUE),
                              ci_upper = stats::quantile(x, 0.975, na.rm = TRUE))
  list(summary = apply(mat, 2, summary_fn), n_successful = length(idx), n_attempted = n_boot)
}
