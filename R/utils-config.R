#' @noRd
get_scd_config <- function(n_obs, n_cases = 1) {
  large <- n_obs >= 2000 && n_cases >= 10
  list(iter = if (large) 5000 else findInterval(n_obs, c(100, 500)) * 1000 + 2000,
       chains = 4,
       adapt_delta = if (n_cases < 10) 0.95 else 0.90,
       max_treedepth = if (large) 15 else 12)
}

#' Evaluate dataset size against per-task server limits
#'
#' Row and case thresholds sized for the shared 2 GB / 1 vCPU web instance;
#' typical SCD datasets (3-15 cases, under 1,000 rows) pass every gate.
#'
#' @param data Data frame to evaluate.
#' @param task One of "upload", "fit", "comparison", "sequential", "bootstrap".
#' @return NULL when within limits; otherwise a list with `level`
#'   ("block" or "warn") and `message`.
#' @keywords internal
#' @export
check_data_size <- function(data, task = c("upload", "fit", "comparison",
                                           "sequential", "bootstrap")) {
  task <- match.arg(task)
  n_rows <- nrow(data)
  case_var <- find_case_var(data)
  n_cases <- if (!is.null(case_var)) length(unique(data[[case_var]])) else NA_integer_

  limits <- list(
    upload     = list(block = 20000, warn = 5000, label = "data upload"),
    fit        = list(block = 5000,  warn = 1500, label = "model fitting"),
    comparison = list(block = 2000,  warn = 1000, label = "model comparison"),
    # sequential updating refits once per cumulative case
    sequential = list(block = 2000,  warn = 1000, label = "sequential updating",
                      block_cases = 25),
    # summaries of the fitted draws, so the fit gate applies
    bootstrap  = list(block = 5000,  warn = 1500, label = "variance-component summaries")
  )[[task]]

  over_cases <- !is.null(limits$block_cases) && !is.na(n_cases) &&
    n_cases > limits$block_cases

  if (n_rows > limits$block || over_cases) {
    detail <- if (over_cases) {
      sprintf("%d cases (limit %d)", n_cases, limits$block_cases)
    } else {
      sprintf("%s rows (limit %s)", format(n_rows, big.mark = ","),
              format(limits$block, big.mark = ","))
    }
    return(list(level = "block", message = sprintf(
      paste0("Dataset too large for %s on this server: %s. ",
             "Reduce the number of rows or cases, or copy the R Code from ",
             "the Export tab and run the analysis in a local R session."),
      limits$label, detail)))
  }
  if (n_rows > limits$warn) {
    return(list(level = "warn", message = sprintf(
      paste0("Large dataset for %s on this server (%s rows). ",
             "Analyses may be slow; a local R session handles large data better."),
      limits$label, format(n_rows, big.mark = ","))))
  }
  NULL
}

#' @noRd
validate_scd_config <- function(data) {
  required_cols <- c("case", "phase", "outcome")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  n_cases <- length(unique(data$case))
  if (n_cases < 3) {
    warning("Very few cases (", n_cases, "). Results may be unstable.")
  }

  phases_per_case <- data %>%
    group_by(case, phase) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(case) %>%
    summarise(min_phase = min(n))

  if (any(phases_per_case$min_phase < 3)) {
    warning("Some phases have fewer than 3 observations")
  }

  invisible(TRUE)
}

#' Frequentist design-effect t approximation of power for a two-phase contrast
#'
#' A closed-form frequentist approximation reported alongside the Bayesian
#' workflow for orientation only; [run_power_simulation()] is the Bayesian
#' estimate the app reports.
#'
#' Each case splits its \eqn{n} sessions into a baseline phase of
#' \eqn{n_A = \lfloor n/2 \rfloor} and an intervention phase of
#' \eqn{n_B = n - n_A} sessions. Under AR(1) errors with correlation matrix
#' \eqn{R_{ij} = \rho^{|i-j|}}, the variance of the phase-mean difference
#' is \eqn{\sigma^2 c^\top R c} with \eqn{c} holding \eqn{-1/n_A} on baseline
#' and \eqn{1/n_B} on intervention sessions. This carries each phase's own
#' design effect and the covariance between the two phase means, which
#' lowers the variance of their difference when \eqn{\rho > 0}. The
#' noncentrality over independent cases is
#' \eqn{d / \sqrt{c^\top R c / n_{cases}}}, with degrees of freedom from the
#' effective sample size. The contrast is within case, so the between-case
#' intercept variance cancels; between-case variation in the effect is not
#' modeled, so the approximation overstates power when effects vary across
#' cases. [run_power_simulation()] covers both. Power is bounded below by
#' \code{alpha}, the rejection rate at a zero effect.
#'
#' @noRd
quick_power_estimate <- function(n_cases, n_timepoints, effect_size = 0.5,
                                 autocorrelation = 0.2, alpha = 0.05) {

  if (n_cases < 3 || n_timepoints < 6) {
    return(list(
      power = 0,
      label = "Approximate power (frequentist design-effect t approximation)",
      adequate = FALSE,
      recommendation = "Insufficient data for reliable analysis. Run simulation in Power tab for precise estimates."
    ))
  }

  n_a <- floor(n_timepoints / 2)
  n_b <- n_timepoints - n_a
  contrast <- rep(c(-1 / n_a, 1 / n_b), c(n_a, n_b))
  ar_cor <- autocorrelation^abs(outer(seq_len(n_timepoints), seq_len(n_timepoints), "-"))
  var_diff <- drop(contrast %*% ar_cor %*% contrast)
  design_effect <- var_diff / (1 / n_a + 1 / n_b)
  effective_n <- n_cases * n_timepoints / design_effect

  if (effect_size == 0) {
    power_est <- alpha
  } else {
    df <- effective_n - 2
    ncp <- effect_size / sqrt(var_diff / n_cases)
    crit_t <- stats::qt(1 - alpha / 2, df)
    power_est <- 1 - stats::pt(crit_t, df, ncp) + stats::pt(-crit_t, df, ncp)
  }

  power_est <- pmin(0.99, pmax(alpha, power_est))
  adequate <- power_est >= 0.80

  recommendation <- sprintf(
    "Approximate power: %.1f%% (lag-1 autocorrelation = %.2f). Run Bayesian simulation in Power tab for precise estimates.",
    power_est * 100, autocorrelation
  )

  list(
    power = power_est,
    label = "Approximate power (frequentist design-effect t approximation)",
    adequate = adequate,
    recommendation = recommendation,
    n_cases = n_cases,
    n_timepoints = n_timepoints,
    effect_size = effect_size
  )
}
