#' Convergence diagnostics for Bayesian models
#'
#' @param model A brmsfit model object.
#' @param data Optional data frame for posterior predictive checks.
#'
#' @return An scd_diagnostics list with convergence metrics.
#'
#' @examples
#' if (interactive()) {
#' fit <- brms::brm(outcome ~ phase + (1 | case), data = simulate_scd_data())
#' compute_convergence_diagnostics(fit)
#' }
#'
#' @export
compute_convergence_diagnostics <- function(model, data = NULL) {
  diagnostics <- list()

  diagnostics <- tryCatch({
    draws <- posterior::as_draws_df(model)
    
    diagnostics$basic <- tryCatch({
      # summarise_draws applies the per-variable diagnostics to each parameter
      smry <- posterior::summarise_draws(draws, "rhat", "ess_bulk", "ess_tail", "mcse_mean")
      list(
        rhat = stats::setNames(smry$rhat, smry$variable),
        ess_bulk = stats::setNames(smry$ess_bulk, smry$variable),
        ess_tail = stats::setNames(smry$ess_tail, smry$variable),
        mcse = stats::setNames(smry$mcse_mean, smry$variable)
      )
    }, error = function(e) {
      tryCatch({
        if (inherits(model, "brmsfit")) {
          summary_obj <- summary(model)

          if (!is.null(summary_obj$fixed)) {
            list(
              rhat = summary_obj$fixed[, "Rhat"],
              ess_bulk = summary_obj$fixed[, "Bulk_ESS"],
              ess_tail = summary_obj$fixed[, "Tail_ESS"],
              mcse = rep(NA, nrow(summary_obj$fixed))
            )
          } else {
            list(
              rhat = numeric(0),
              ess_bulk = numeric(0),
              ess_tail = numeric(0),
              mcse = numeric(0)
            )
          }
        } else {
          list(
            rhat = numeric(0),
            ess_bulk = numeric(0),
            ess_tail = numeric(0),
            mcse = numeric(0)
          )
        }
      }, error = function(e2) {
        list(
          rhat = numeric(0),
          ess_bulk = numeric(0),
          ess_tail = numeric(0),
          mcse = numeric(0)
        )
      })
    })
    
    # auxiliary diagnostics degrade to NULL so one failure never blocks convergence
    diagnostics$rhat_variants <- tryCatch(compute_rhat_variants(draws), error = function(e) NULL)
    diagnostics$chains <- tryCatch(diagnose_chains(draws), error = function(e) NULL)
    diagnostics$efficiency <- tryCatch(calculate_sampling_efficiency(model, draws), error = function(e) NULL)

    if (!is.null(data)) {
      diagnostics$ppc <- tryCatch(perform_ppc_checks(model, data), error = function(e) NULL)
    }

    diagnostics$divergence <- check_divergences(model)
    diagnostics$recommendations <- generate_diagnostic_recommendations(diagnostics)
    diagnostics$converged <- assess_overall_convergence(diagnostics)
    diagnostics
  }, error = function(e) {
    diagnostics$error <- conditionMessage(e)
    diagnostics$converged <- list(converged = FALSE, criteria = list(), confidence = NA_real_)
    diagnostics
  })
  
  class(diagnostics) <- c("scd_diagnostics", "list")
  return(diagnostics)
}

#' @noRd
.max_present <- function(v) if (any(!is.na(v))) max(v, na.rm = TRUE) else NA_real_

#' @noRd
.min_present <- function(v) if (any(!is.na(v))) min(v, na.rm = TRUE) else NA_real_

#' @noRd
compute_rhat_variants <- function(draws) {
  # rank_rhat is posterior::rhat(), the max of bulk_rhat and the rank-normalized folded (tail) R-hat
  smry <- posterior::summarise_draws(draws,
    split_rhat  = function(x) posterior::rhat_basic(x, split = TRUE),
    bulk_rhat   = function(x) posterior::rhat_basic(posterior::z_scale(x), split = TRUE),
    folded_rhat = function(x) posterior::rhat_basic(posterior::z_scale(abs(x - stats::median(x))),
                                                    split = TRUE),
    rank_rhat   = posterior::rhat,
    posterior::ess_quantile)
  list(
    split_rhat   = .max_present(smry$split_rhat),
    bulk_rhat    = .max_present(smry$bulk_rhat),
    folded_rhat  = .max_present(smry$folded_rhat),
    rank_rhat    = .max_present(smry$rank_rhat),
    quantile_ess = lapply(smry[grep("^ess_q", names(smry))], .min_present)
  )
}

#' @noRd
diagnose_chains <- function(draws) {
  chain_diags <- list()
  nchains <- posterior::nchains(draws)
  
  for (chain in 1:nchains) {
    chain_draws <- posterior::subset_draws(draws, chain = chain)
    # summarise_draws applies rhat/ess per variable
    smry <- posterior::summarise_draws(chain_draws, "rhat", "ess_bulk")

    chain_diags[[paste0("chain", chain)]] <- list(
      mean_rhat = mean(smry$rhat, na.rm = TRUE),
      min_ess = .min_present(smry$ess_bulk),
      autocorr = calculate_autocorrelation(chain_draws),
      stationarity = test_stationarity(chain_draws)
    )
  }
  
  return(chain_diags)
}

#' @noRd
calculate_autocorrelation <- function(chain_draws, max_lag = 50) {
  vars <- posterior::variables(chain_draws)
  if (length(vars) == 0) return(NA)

  values <- posterior::extract_variable(chain_draws, vars[1])
  if (length(values) < max_lag) return(NA)
  
  acf_result <- acf(values, lag.max = max_lag, plot = FALSE)
  
  effective_lag <- which(abs(acf_result$acf) < 0.1)[1]
  
  return(list(
    effective_lag = effective_lag,
    max_autocorr = .max_present(abs(acf_result$acf[-1]))
  ))
}

#' @noRd
test_stationarity <- function(chain_draws) {
  vars <- posterior::variables(chain_draws)
  if (length(vars) == 0) return(NA)

  values <- posterior::extract_variable(chain_draws, vars[1])
  n <- length(values)
  
  if (n < 20) return(NA)
  
  first_portion <- values[1:floor(n*0.1)]
  last_portion <- values[floor(n*0.5):n]

  # ESS-based segment SE keeps autocorrelated chains from spurious flags
  segment_se2 <- function(x) {
    ess <- tryCatch(posterior::ess_mean(x), error = function(e) NA_real_)
    if (!is.finite(ess) || ess <= 0) ess <- length(x)
    stats::var(x) / ess
  }

  z_score <- (mean(first_portion) - mean(last_portion)) /
             sqrt(segment_se2(first_portion) + segment_se2(last_portion))
  
  return(list(
    geweke_z = z_score,
    stationary = abs(z_score) < 2
  ))
}

#' @noRd
calculate_sampling_efficiency <- function(model, draws) {
  # post-warmup draw count works across rstan and cmdstanr, unlike fit@sim
  total_iterations <- tryCatch(brms::ndraws(model), error = function(e) NA_real_)
  if (is.na(total_iterations)) {
    total_iterations <- tryCatch(posterior::ndraws(draws), error = function(e) NA_real_)
  }

  ess_bulk_vals <- posterior::summarise_draws(draws, "ess_bulk")$ess_bulk
  # efficiency is ESS relative to total draws; NA when the draw count is unknown
  ess_per_iteration <- if (is.na(total_iterations) || total_iterations <= 0) {
    NA_real_
  } else {
    mean(ess_bulk_vals, na.rm = TRUE) / total_iterations
  }

  return(list(
    total_iterations = total_iterations,
    ess_per_iteration = ess_per_iteration
  ))
}

#' @noRd
.response_column <- function(model, data) {
  # resp | trials(n) / cens() forms: the first variable is the response
  nm <- tryCatch(all.vars(brms::brmsterms(stats::formula(model))$respform)[1],
                 error = function(e) NULL) %||% "outcome"
  if (nm %in% names(data) && is.numeric(data[[nm]])) nm else NULL
}

#' Posterior predictive discrepancy statistics
#'
#' @details \code{fit_heuristic} is a rough flag, not a calibrated
#'   accept/reject rule: posterior predictive p-values concentrate near 0.5
#'   under a correct model rather than being uniform, so read the statistics
#'   and the graphical check. Replicates come from [ppc_replicates()], so
#'   observation-level effects are redrawn.
#' @noRd
perform_ppc_checks <- function(model, data) {
  outcome_var <- .response_column(model, data)
  if (is.null(outcome_var)) return(list())
  yrep <- tryCatch(ppc_replicates(model, data, n_draws = 100), error = function(e) NULL)
  if (is.null(yrep) || ncol(yrep) != nrow(data)) return(list())

  y <- as.numeric(data[[outcome_var]])
  keep <- is.finite(y)
  y <- y[keep]
  yrep <- yrep[, keep, drop = FALSE]
  if (length(y) == 0) return(list())

  stats <- tryCatch({
    sd_y <- stats::sd(y)
    list(
      mean_check = abs(mean(y) - mean(yrep)) / sd_y,
      sd_check   = abs(sd_y - mean(apply(yrep, 1, stats::sd))) / sd_y,
      min_check  = abs(min(y) - mean(apply(yrep, 1, min))) / sd_y,
      max_check  = abs(max(y) - mean(apply(yrep, 1, max))) / sd_y,
      p_mean     = mean(apply(yrep, 1, mean) >= mean(y)),
      p_sd       = mean(apply(yrep, 1, stats::sd) >= sd_y)
    )
  }, error = function(e) NULL)
  if (is.null(stats)) return(list())

  stats$fit_heuristic <- isTRUE(
    stats$mean_check < 0.2 && stats$sd_check < 0.3 &&
    stats$p_mean > 0.025 && stats$p_mean < 0.975 &&
    stats$p_sd   > 0.025 && stats$p_sd   < 0.975)
  stats
}

#' @noRd
.predictive_coverage <- function(model, data, ndraws = 100) {
  outcome_var <- .response_column(model, data)
  if (is.null(outcome_var)) return(NULL)
  # replicates are drawn on the same rows the observed range is taken from
  yrep <- tryCatch(
    if (identical(data, model$data)) brms::posterior_predict(model, ndraws = ndraws)
    else brms::posterior_predict(model, newdata = data, ndraws = ndraws),
    error = function(e) NULL)
  if (is.null(yrep)) return(NULL)
  y <- data[[outcome_var]]
  y <- y[!is.na(y)]
  q <- stats::quantile(as.numeric(yrep), c(0.005, 0.5, 0.995), names = FALSE)
  list(
    predictive_median = q[2],
    predictive_lower  = q[1],
    predictive_upper  = q[3],
    observed_min = min(y),
    observed_max = max(y),
    covers_observed = min(y) >= q[1] && max(y) <= q[3]
  )
}

#' Prior predictive check for a fitted SCD model
#'
#' Resamples the model from the prior alone and compares the implied outcome
#' distribution to the observed data range, the prior-predictive step of the
#' Bayesian workflow. A prior-predictive interval that fails to span the
#' observed data signals a mis-scaled prior.
#'
#' @param model A fitted \code{brmsfit}.
#' @param data Data holding the outcome column; defaults to the model's data.
#'   A different frame must carry every model term, since the replicates are
#'   drawn on it so that both sides use the same rows.
#' @param ndraws Prior-predictive draws summarized.
#'
#' @details With a Gaussian \code{ar()} term, brms predicts each occasion
#'   conditional on lagged residuals computed from the observed outcomes, so
#'   prior predictions are pulled toward the observed series and the
#'   coverage check is optimistic; the returned \code{note} says so.
#'
#' @return A list with the prior-predictive median and central 99\% interval,
#'   the observed outcome range, \code{covers_observed}, and \code{note}
#'   (\code{NULL}, or how an \code{ar()} term conditions the predictions);
#'   \code{NULL} if the model cannot be sampled from the prior (for example
#'   under improper priors).
#'
#' @examples
#' if (interactive()) {
#' fit <- run_scd_analysis(simulate_scd_data())$model
#' prior_predictive_check(fit)
#' }
#'
#' @export
prior_predictive_check <- function(model, data = NULL, ndraws = 100) {
  data <- data %||% model$data
  prior_model <- tryCatch(
    stats::update(model, sample_prior = "only", recompile = FALSE,
                  refresh = 0, silent = 2),
    error = function(e) NULL)
  if (is.null(prior_model)) return(NULL)
  out <- .predictive_coverage(prior_model, data, ndraws)
  if (is.null(out)) return(NULL)
  out$note <- if (.gaussian_ar(model))
    paste("Gaussian ar() term: prior predictions condition on lagged residuals computed from",
          "the observed outcomes, so they are pulled toward the observed series and the",
          "coverage check is optimistic.")
  out
}

#' @noRd
.fit_max_treedepth <- function(model) {
  args <- tryCatch(model$fit@stan_args[[1]], error = function(e) NULL)
  depth <- args$control$max_treedepth %||% args$max_treedepth %||% args$max_depth
  if (is.numeric(depth) && length(depth) == 1 && is.finite(depth)) depth else .max_treedepth()
}

#' @noRd
check_divergences <- function(model) {
  div_checks <- list()
  
  sampler_params <- tryCatch({
    if (inherits(model, "brmsfit")) {
      brms::nuts_params(model)
    } else {
      nuts_params(model)
    }
  }, error = function(e) NULL)
  
  if (!is.null(sampler_params) && all(c("Parameter", "Value") %in% names(sampler_params))) {
    val <- sampler_params$Value
    par <- sampler_params$Parameter
    divergent <- val[par == "divergent__"]
    energy_vals <- val[par == "energy__"]

    div_checks$n_divergent <- sum(divergent, na.rm = TRUE)
    div_checks$divergence_rate <- mean(divergent, na.rm = TRUE)
    div_checks$max_treedepth_hit <- any(val[par == "treedepth__"] >= .fit_max_treedepth(model),
                                        na.rm = TRUE)
    div_checks$stepsize <- mean(val[par == "stepsize__"], na.rm = TRUE)
    div_checks$energy <- tryCatch({
      # E-BFMI is per chain; diff() across concatenated chains would straddle boundaries
      bfmi_by_chain <- vapply(split(energy_vals, sampler_params$Chain[par == "energy__"]),
        function(e) var(diff(e)) / var(e), numeric(1))
      list(
        mean = mean(energy_vals, na.rm = TRUE),
        sd = sd(energy_vals, na.rm = TRUE),
        bfmi = .min_present(bfmi_by_chain)
      )
    }, error = function(e) NULL)
  }
  
  return(div_checks)
}

#' Convergence recommendations
#'
#' @details Reads the pass/fail criteria of \code{assess_overall_convergence()}:
#'   R-hat of 1.01 or above, bulk or tail ESS of 400 or below, divergent transitions, and
#'   transitions at max_treedepth fail. Low E-BFMI (< 0.2) and split R-hat
#'   above 1.05 are advisory and do not set \code{converged}.
#' @noRd
generate_diagnostic_recommendations <- function(diagnostics) {
  recs <- list()
  conv <- assess_overall_convergence(diagnostics)
  crit <- conv$criteria

  if (isFALSE(crit$rhat_ok)) {
    recs$rhat <- if (isTRUE(.max_present(diagnostics$basic$rhat) > 1.1))
      "CRITICAL: R-hat > 1.1. Increase iterations or check model specification."
      else "WARNING: R-hat >= 1.01. Consider increasing iterations."
  }

  if (isFALSE(crit$ess_ok)) {
    recs$ess <- if (isTRUE(.min_present(c(diagnostics$basic$ess_bulk, diagnostics$basic$ess_tail)) < 100))
      "CRITICAL: bulk or tail ESS < 100. Increase iterations significantly."
      else "WARNING: bulk or tail ESS <= 400. Consider increasing iterations."
  }

  if (isFALSE(crit$no_divergences)) {
    recs$divergence <- sprintf("WARNING: %d divergent transitions. Increase adapt_delta.",
                               diagnostics$divergence$n_divergent)
  }

  if (isFALSE(conv$advisory$treedepth_ok)) {
    recs$treedepth <- "ADVISORY: transitions hit max_treedepth (an efficiency limit, not a validity failure). Raise max_treedepth or simplify the model."
  }

  if (isFALSE(conv$advisory$bfmi_ok)) {
    recs$energy <- "ADVISORY: low E-BFMI (< 0.2). Model may be poorly specified."
  }

  if (isFALSE(conv$advisory$split_rhat_ok)) {
    recs$split_rhat <- "ADVISORY: split R-hat > 1.05 indicates within-chain convergence issues."
  }

  recs$overall <- if (isFALSE(conv$converged)) {
    "Model has convergence warnings. Review recommendations above."
  } else if (isTRUE(conv$converged) && length(recs)) {
    "Convergence criteria pass. Review the advisory notes above."
  } else if (isTRUE(conv$converged)) {
    "All diagnostics pass. Model appears well-converged."
  } else {
    "Diagnostics could not be computed, so convergence is not established."
  }

  return(recs)
}

#' @details Pass/fail criteria: R-hat < 1.01, bulk and tail ESS > 400, no
#'   divergent transitions. No transition at max_treedepth, E-BFMI >= 0.2, and
#'   split R-hat <= 1.05 are returned under \code{advisory} and do not set
#'   \code{converged}.
#' @noRd
assess_overall_convergence <- function(diagnostics) {
  criteria <- list()
  # an all-NA check could not run, so it is NA rather than passed; Inf fails
  .all_ok <- function(x, ok) if (any(!is.na(x))) all(ok(x[!is.na(x)])) else NA

  if (!is.null(diagnostics$basic$rhat)) {
    criteria$rhat_ok <- .all_ok(diagnostics$basic$rhat, function(v) v < 1.01)
  }

  if (!is.null(diagnostics$basic$ess_bulk)) {
    bulk_ok <- .all_ok(diagnostics$basic$ess_bulk, function(v) v > 400)
    tail_ok <- if (!is.null(diagnostics$basic$ess_tail))
      .all_ok(diagnostics$basic$ess_tail, function(v) v > 400) else TRUE
    criteria$ess_ok <- bulk_ok & tail_ok
  }
  
  if (!is.null(diagnostics$divergence$n_divergent)) {
    criteria$no_divergences <- diagnostics$divergence$n_divergent == 0
  }

  # hitting max treedepth is an efficiency warning, not a validity failure
  # (Stan warnings guide), so it is advisory and does not set `converged`
  treedepth_ok <- if (!is.null(diagnostics$divergence$max_treedepth_hit))
    !isTRUE(diagnostics$divergence$max_treedepth_hit) else NA

  # converged reflects MCMC sampling only; posterior predictive fit is reported separately
  crit <- unlist(criteria)
  converged <- if (length(crit)) all(crit) else NA

  return(list(
    converged = converged,
    criteria = criteria,
    advisory = list(treedepth_ok = treedepth_ok,
                    bfmi_ok = .all_ok(diagnostics$divergence$energy$bfmi, function(v) v >= 0.2),
                    split_rhat_ok = .all_ok(diagnostics$rhat_variants$split_rhat, function(v) v <= 1.05)),
    ppc_fit_heuristic = diagnostics$ppc$fit_heuristic,
    confidence = if (length(crit)) mean(crit %in% TRUE) else NA_real_
  ))
}
