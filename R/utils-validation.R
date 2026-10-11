#' Validate SCD data
#'
#' @param data A data frame to validate.
#'
#' @return A list with valid, issues, warnings, recommendations, and diagnostics.
#'
#' @examples
#' d <- data.frame(
#'   case = rep(c("C1", "C2"), each = 10),
#'   phase = rep(c("A", "B"), 10),
#'   time = rep(1:10, 2),
#'   outcome = rnorm(20)
#' )
#' validate_scd_data(d)
#'
#' @export
validate_scd_data <- function(data) {
  issues <- list()
  warnings <- list()
  recommendations <- list()

  if (is.null(data) || nrow(data) == 0) {
    issues <- c(issues, "No data provided")
    return(list(valid = FALSE, issues = issues, warnings = warnings, recommendations = recommendations))
  }

  required_cols <- c("case", "phase", "outcome")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    issues <- c(issues, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  if ("outcome" %in% names(data)) {
    if (!is.numeric(data$outcome)) {
      issues <- c(issues, "Outcome variable must be numeric")
    } else {
      if (any(!is.finite(data$outcome) & !is.na(data$outcome))) {
        issues <- c(issues, "Outcome values must be finite")
      }
      if (all(is.na(data$outcome))) {
        issues <- c(issues, "All outcome values are missing")
      } else if (sum(is.na(data$outcome)) > nrow(data) * 0.5) {
        warnings <- c(warnings, paste0("High proportion of missing outcomes: ",
                                       round(sum(is.na(data$outcome)) / nrow(data) * 100, 1), "%"))
      }

      # one observed value or none has an NA SD
      if (!isTRUE(stats::sd(data$outcome, na.rm = TRUE) > 0)) {
        issues <- c(issues, "Outcome has zero variance")
      }
    }
  }

  if ("phase" %in% names(data)) {
    miss <- .na_phase_tiers(data)
    if (miss$n > 0) {
      warnings <- c(warnings, sprintf(paste0("%d row(s) have a missing phase label (tier: %s); ",
                                             "these rows are dropped before coding"),
                                      miss$n, paste(miss$tiers, collapse = ", ")))
      data <- data[!miss$rows, , drop = FALSE]
    }
    phases <- unique(data$phase)
    n_phases <- length(phases)

    if (n_phases < 2) {
      issues <- c(issues, "Need at least 2 phases (e.g., A and B)")
    }
  }

  # tiers (study x cluster x case x behavior), counted after rows without a phase are dropped
  n_cases <- 0L
  if ("case" %in% names(data) && nrow(data) > 0) {
    key <- .std_key(data, tier_vars(data))
    if ("phase" %in% names(data)) {
      for (tier in unique(key)) {
        tier_data <- data[key == tier, ]
        if ("time" %in% names(tier_data)) tier_data <- tier_data[order(tier_data$time), ]

        if (length(unique(tier_data$phase)) < 2) {
          warnings <- c(warnings, paste("Case", tier, "has only one phase"))
        }

        # each phase run counts on its own, so A-B-A-B has four phases
        if (any(rle(as.character(tier_data$phase))$lengths < 3)) {
          warnings <- c(warnings, paste("Case", tier, "has phases with fewer than 3 observations"))
        }
      }
    }

    n_cases <- length(unique(key))

    if (n_cases < 3) {
      warnings <- c(warnings, paste("Only", n_cases, "case(s). SCD meta-analysis typically requires at least 3 cases"))
    }

    case_sizes <- table(key)
    min_obs <- min(case_sizes)
    max_obs <- max(case_sizes)

    if (min_obs < 5) {
      warnings <- c(warnings, paste("Some cases have very few observations (min:", min_obs, ")"))
    }

    if (max_obs / min_obs > 3) {
      recommendations <- c(recommendations, "Large imbalance in observations per case detected")
    }
  }

  if ("time" %in% names(data)) {
    if (!is.numeric(data$time)) {
      warnings <- c(warnings, "Time variable should be numeric")
    }
  } else {
    recommendations <- c(recommendations, "No time variable found. Consider adding sequential time points")
  }

  diagnostic_data <- data
  valid_outcome <- is.numeric(data$outcome) && all(is.finite(data$outcome) | is.na(data$outcome))
  if (!valid_outcome) diagnostic_data <- data[, setdiff(names(data), "outcome"), drop = FALSE]
  autocorr_check <- check_autocorrelation(diagnostic_data)
  trend_check <- check_baseline_trend(diagnostic_data)

  valid <- length(issues) == 0

  list(
    valid = valid,
    issues = issues,
    warnings = warnings,
    recommendations = recommendations,
    autocorr_cases = autocorr_check$cases,
    autocorr_warning = autocorr_check$warning,
    trend_cases = trend_check$cases,
    trend_warning = trend_check$warning,
    n_cases = if ("case" %in% names(data)) n_cases else NA,
    n_obs = nrow(data),
    n_phases = if ("phase" %in% names(data)) length(unique(data$phase)) else NA
  )
}

#' @noRd
check_autocorrelation <- function(data) {
  if (!all(c("case", "outcome", "time", "phase") %in% names(data))) {
    return(list(cases = character(), warning = NULL))
  }
  if (!is.numeric(data$outcome) || !is.numeric(data$time) || nrow(data) == 0L) {
    return(list(cases = character(), warning = NULL))
  }

  data <- data[order(data$time), ]
  key <- .std_key(data, tier_vars(data))
  # lag-1 of residuals from phase-run means, so a level shift between phases is not autocorrelation
  lag1 <- vapply(split(seq_len(nrow(data)), key), function(i) {
    ph <- as.character(data$phase[i])
    y <- data$outcome[i]
    r <- y - stats::ave(y, cumsum(c(TRUE, ph[-1] != ph[-length(ph)])),
                        FUN = function(v) mean(v, na.rm = TRUE))
    # complete lag-1 pairs, so one missing outcome does not drop the tier
    if (sum(is.finite(r)) > 3 && isTRUE(stats::sd(r, na.rm = TRUE) > 0))
      tryCatch(stats::acf(r, lag.max = 1, plot = FALSE, na.action = stats::na.pass)$acf[2],
               error = function(e) NA_real_)
    else NA_real_
  }, numeric(1))
  high_autocorr_cases <- names(lag1)[which(abs(lag1) > 0.5)]

  if (length(high_autocorr_cases) > 0) {
    return(list(
      cases = high_autocorr_cases,
      warning = paste0("High autocorrelation (|r| > .5): ",
                       paste(high_autocorr_cases, collapse = ", "),
                       ". Consider adding an AR(1) term.")))
  }

  list(cases = character(), warning = NULL)
}

#' @noRd
check_baseline_trend <- function(data) {
  if (!all(c("case", "outcome", "time", "phase") %in% names(data))) {
    return(list(cases = character(), warning = NULL))
  }
  if (!is.numeric(data$outcome) || !is.numeric(data$time) || nrow(data) == 0L) {
    return(list(cases = character(), warning = NULL))
  }

  data <- data[order(data$time), ]
  key <- .std_key(data, tier_vars(data))
  # first baseline-role run of each tier
  p_value <- vapply(split(seq_len(nrow(data)), key), function(i) {
    ph <- as.character(data$phase[i])
    run <- cumsum(c(TRUE, ph[-1] != ph[-length(ph)]))
    base <- data[i[run %in% run[.phase_numeric(ph, seq_along(ph)) %in% 0][1]], ]
    base <- base[is.finite(base$outcome) & is.finite(base$time), , drop = FALSE]
    if (nrow(base) < 4 || length(unique(base$time)) < 2) return(NA_real_)
    fit <- tryCatch(stats::lm(outcome ~ time, data = base), error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    unname(suppressWarnings(summary(fit))$coefficients[, 4]["time"])
  }, numeric(1))
  trending_cases <- names(p_value)[which(p_value < 0.05)]

  if (length(trending_cases) > 0) {
    return(list(
      cases = trending_cases,
      warning = paste0("Baseline trend (p < .05): ",
                       paste(trending_cases, collapse = ", "),
                       ". Consider including a time predictor.")))
  }

  list(cases = character(), warning = NULL)
}
