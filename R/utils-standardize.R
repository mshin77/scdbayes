#' Re-express the outcome in within-case residual-SD units
#'
#' Scale harmonization for raw-data synthesis: fits an ordinary least squares
#' regression of the outcome on the fixed piecewise terms separately for every
#' tier (case, or case within study), then divides that tier's outcome by the
#' tier's residual standard deviation (root mean squared error). The returned
#' \code{<outcome>_std} column is in within-case residual-SD units, so studies
#' measured on different scales can enter one multilevel model. This is scale
#' harmonization, not an effect size: the standardized coefficients are not
#' standardized mean differences. scdbayes does not compute the within-case
#' SMD, which is biased under autocorrelation. The design-comparable BC-SMD
#' is estimated in a Bayesian model ([bc_smd_bayes()]), and studies are
#' pooled with a Bayesian meta-analysis ([meta_bayes()]); scdhlm/metafor
#' scripts are optional frequentist comparisons.
#'
#' @details Each participant's scores are divided by the root mean squared
#'   error of a participant-specific ordinary regression before the
#'   multilevel meta-analysis. Standardizing adds error from estimating the
#'   scale factor, so analyzing raw data is preferable when all cases share
#'   one scale.
#'
#'   Caveat: the per-case OLS residual SD ignores autocorrelation.
#'   Within-case standardized effect sizes are biased under autocorrelation,
#'   with neither OLS nor multilevel synthesis performing adequately, and
#'   raw-data synthesis of data standardized for each case may share the
#'   problem. Use the transformation as a sensitivity analysis, not as the
#'   primary synthesis metric.
#'
#'   The regression contains only fixed piecewise terms: no random effects,
#'   no moderators, no autocorrelation. With \code{formula_fixed = NULL} the
#'   terms are the package defaults for the design returned by
#'   [detect_design_type()] after coding with [process_scd_phases()]. A
#'   supplied formula keeps its fixed terms and drops grouping terms such as
#'   \code{(1 | case)} and \code{ar()} terms. Columns constant within a tier
#'   are aliased and do not count toward the number of coefficients.
#'
#'   A tier is left \code{NA} (with a warning) when it has fewer than
#'   \code{p + 2} non-missing observations, where \code{p} counts the
#'   estimated coefficients including the intercept (so at least two residual
#'   degrees of freedom remain), or when its residual SD is zero or not
#'   finite.
#'
#'   The transformation suits continuous or interval outcomes. For counts,
#'   rates, and short ordinal scales the residual SD depends on the mean and
#'   the result is approximate; there in particular, keep a count or ordinal
#'   likelihood on the natural scale as the primary analysis.
#'
#' @param data Data frame with the outcome and either a \code{phase} column
#'   or the piecewise columns named in \code{formula_fixed}.
#' @param formula_fixed Optional fixed-effects formula (formula object or
#'   string, with or without a left-hand side, e.g.
#'   \code{"~ time_A + level_AB + trend_AB"}). \code{NULL} uses the default
#'   fixed terms for the detected design. Only syntactic column names and
#'   [is_safe_formula()] syntax are accepted; function calls such as
#'   \code{I()} raise an error, and default terms skip non-syntactic names.
#' @param outcome_var Outcome column name.
#' @param tiers Columns whose combination defines one series; defaults to
#'   [tier_vars()].
#'
#' @return \code{data} with a numeric column \code{<outcome_var>_std}. The
#'   attribute \code{"case_sigma"} holds one row per tier: the tier columns,
#'   \code{tier} (label), \code{n} (non-missing observations), \code{p}
#'   (estimated coefficients), \code{sigma} (residual SD, \code{NA} when not
#'   estimable), and \code{status}.
#'
#' @examples
#' d <- simulate_scd_data(n_cases = 3, n_phase_a = 8, n_phase_b = 8)
#' out <- standardize_by_case(d)
#' attr(out, "case_sigma")
#'
#' @export
standardize_by_case <- function(data, formula_fixed = NULL, outcome_var = "outcome",
                                tiers = tier_vars(data)) {
  if (!outcome_var %in% names(data)) {
    stop(sprintf("Outcome column '%s' not found.", outcome_var), call. = FALSE)
  }
  terms <- .std_terms(formula_fixed, names(data))
  coded <- .std_coded(data, terms)
  terms <- terms %||% intersect(.scd_fixed_terms(names(coded)), names(coded))
  if (!length(terms) && is.null(formula_fixed)) {
    stop("No fixed piecewise terms found: supply a `phase` column or `formula_fixed`.",
         call. = FALSE)
  }
  absent <- setdiff(.term_vars(terms), names(coded))
  if (length(absent)) {
    stop(sprintf("Columns not found for formula_fixed: %s.", paste(absent, collapse = ", ")),
         call. = FALSE)
  }

  key <- .std_key(data, tiers)
  rows <- split(seq_len(nrow(data)), factor(key, levels = unique(key)))
  # phase coding can drop rows (e.g. a missing phase label), so tiers are matched
  # to coded rows through the original row id, never by position
  fits <- lapply(rows, function(i) .tier_sigma(coded[coded$.std_row %in% i, , drop = FALSE],
                                                outcome_var, terms))
  pick <- function(field, type) vapply(fits, `[[`, type, field)
  table <- data.frame(tier = names(rows), n = pick("n", numeric(1)), p = pick("p", numeric(1)),
                      sigma = pick("sigma", numeric(1)), row.names = NULL, stringsAsFactors = FALSE)
  failed <- pick("error", character(1))
  # residual SD at floating-point noise level marks an exact fit
  ok <- is.finite(table$sigma) & table$sigma > sqrt(.Machine$double.eps)
  short <- is.na(table$p) | table$n < table$p + 2
  table$status <- ifelse(ok, "ok",
                         ifelse(nzchar(failed), paste("regression failed:", failed),
                                ifelse(short, "too few observations for the fixed terms",
                                       "zero residual SD")))
  table$sigma[!ok] <- NA_real_
  first_rows <- vapply(rows, `[`, integer(1), 1)
  table <- cbind(data[first_rows, intersect(tiers, names(data)), drop = FALSE], table)
  rownames(table) <- NULL

  if (any(!ok)) {
    warning(sprintf("Outcome left NA for %d tier(s) without an estimable residual SD: %s.",
                    sum(!ok), paste(table$tier[!ok], collapse = ", ")), call. = FALSE)
  }
  data[[paste0(outcome_var, "_std")]] <- data[[outcome_var]] / table$sigma[match(key, table$tier)]
  attr(data, "case_sigma") <- table
  data
}

#' @noRd
.std_terms <- function(formula_fixed, allowed_vars) {
  if (is.null(formula_fixed)) return(NULL)
  txt <- if (inherits(formula_fixed, "formula")) paste(deparse(formula_fixed), collapse = " ")
         else paste(formula_fixed, collapse = " ")
  if (!grepl("~", txt, fixed = TRUE)) txt <- paste("~", txt)
  expr <- tryCatch(str2lang(txt), error = function(e) NULL)
  syms <- unique(c(allowed_vars, if (!is.null(expr)) all.names(expr)))
  if (is.null(expr) || !is_safe_formula(txt, .syntactic(syms))) {
    stop(paste("formula_fixed may use only syntactic data column names, 0, 1, and the",
               "operators + : * | ( ); function calls and quoted names are refused."),
         call. = FALSE)
  }
  parsed <- stats::terms(stats::as.formula(expr, env = baseenv()))
  labels <- attr(parsed, "term.labels")
  structure(labels[!grepl("\\||^(ar|arma|ma|cosy|unstr)\\(", labels)],
            intercept = attr(parsed, "intercept"))
}

#' @noRd
.term_vars <- function(terms) unique(unlist(strsplit(terms, ":", fixed = TRUE)))

#' @noRd
.term_call <- function(term) {
  Reduce(function(a, b) call(":", a, b), lapply(strsplit(term, ":", fixed = TRUE)[[1]], as.name))
}

#' @noRd
.std_formula <- function(terms, response) {
  intercept <- attr(terms, "intercept") %||% 1L
  rhs <- Reduce(function(a, b) call("+", a, b), lapply(terms, .term_call)) %||% intercept
  if (length(terms) && intercept == 0L) rhs <- call("+", 0, rhs)
  stats::as.formula(call("~", as.name(response), rhs), env = baseenv())
}

#' @noRd
.std_coded <- function(data, terms) {
  data$.std_row <- seq_len(nrow(data))
  have <- !is.null(terms) && all(.term_vars(terms) %in% names(data))
  if (have || !"phase" %in% names(data)) return(data)
  process_scd_phases(data, list(), scd_type = detect_design_type(data))
}

#' @noRd
.std_key <- function(data, tiers, sep = " / ") {
  tiers <- intersect(tiers, names(data))
  if (!length(tiers)) return(rep("all", nrow(data)))
  marks <- setdiff(unique(strsplit(sep, "", fixed = TRUE)[[1]]), c(" ", "%"))
  parts <- lapply(data[tiers], function(x) {
    value <- gsub("%", "%25", as.character(x), fixed = TRUE)
    value <- Reduce(function(v, mark) {
      code <- paste0("%", paste(as.character(charToRaw(mark)), collapse = ""))
      gsub(mark, code, v, fixed = TRUE)
    }, marks, init = value)
    value[is.na(x)] <- "%NA"
    value
  })
  do.call(paste, c(unname(parts), sep = sep))
}

#' @noRd
.std_factor <- function(data, vars, sep = ".") {
  vars <- intersect(vars, names(data))
  key <- .std_key(data, vars, sep)
  key[!stats::complete.cases(data[vars])] <- NA_character_
  factor(key)
}

#' @noRd
.tier_sigma <- function(df, outcome_var, terms) {
  n_obs <- sum(!is.na(df[[outcome_var]]))
  fit <- tryCatch(stats::lm(.std_formula(terms, outcome_var), data = df),
                  error = function(e) e)
  if (inherits(fit, "error")) {
    return(list(n = as.numeric(n_obs), p = NA_real_, sigma = NA_real_,
                error = substr(conditionMessage(fit), 1, 120)))
  }
  res <- stats::residuals(fit)
  p <- fit$rank
  sigma <- if (length(res) >= p + 2) sqrt(sum(res^2) / fit$df.residual) else NA_real_
  list(n = as.numeric(length(res)), p = as.numeric(p), sigma = sigma, error = "")
}
