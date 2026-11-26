#' Generate adaptive priors based on data characteristics
#'
#' @details Priors are weakly informative and expressed on the scale of the
#' linear predictor, with prior width tied to the measurement scale. Prior
#' width never depends on the number of observations
#' or cases: a small sample is not information about the effect, and the
#' data, not a narrower prior, decide how much the posterior moves.
#'
#' \itemize{
#'   \item Identity links: treatment and slope coefficients
#'     \code{normal(0, 2.5 * SD)}, intercept \code{student_t(3, mean, 2.5 * SD)},
#'     group-level SDs half-\code{student_t(3, 0, SD)}, and the residual SD
#'     half-\code{student_t(3, 0, SDw)}, where \code{SD} is the outcome
#'     standard deviation and \code{SDw} the pooled within-phase SD (the
#'     outcome SD when phases are missing). Coefficients and group-level SDs
#'     scale to the outcome SD, which spans between-case differences and
#'     floor-to-ceiling effects; the residual SD prior scales to the pooled
#'     within-phase SD. The half-t form scales the SD priors to the
#'     response; brms itself uses the scale \code{max(2.5, mad(y))}, so the
#'     two differ when the outcome SD is small or the outcome is skewed. The
#'     2.5 multiplier for coefficients carries the 2.5-unit logistic
#'     regression scale to the response scale, a package convention.
#'   \item Logit links: treatment coefficients \code{normal(0, 2.5)}, the
#'     weakly informative scale for logistic regression coefficients;
#'     intercept \code{student_t(3, 0, 2.5)} and group-level SDs half-
#'     \code{student_t(3, 0, 2.5)}, as the brms defaults on non-identity links.
#'   \item Log links: the same scale 2.5, intercept centered on the log of the
#'     outcome mean. The 2.5 scale was derived for logistic regression only;
#'     carrying it to log links is a package convention.
#' }
#'
#' Shape (negative binomial, Gamma), precision (beta), and degrees-of-freedom
#' (Student-t) parameters keep the brms defaults, for lack of an established
#' weakly informative prior. For these
#' families \code{recommendations$dispersion} suggests adding a second
#' shape or precision prior to the prior sensitivity analysis.
#'
#' The outcome column is \code{outcome_var}, which defaults to
#' \code{structure$outcome_var} and then to \code{"outcome"}; with no such
#' column the outcome-based scales fall back to 1 and 0.
#'
#' The 2.5 scale presumes standardized inputs: binary inputs differing by 1
#' and continuous inputs scaled to SD 0.5.
#' Level steps and condition dummies are 0/1 inputs and take the scale as
#' is. Session-count terms (\code{time_*}, \code{trend_*}) are not
#' standardized, so \code{slopes} holds one scale per such column,
#' \code{2.5 * SD / (2 * sd(x))} on identity links and \code{2.5 / (2 *
#' sd(x))} otherwise: the 2.5 scale for a coefficient on the input
#' standardized to SD 0.5, expressed per session. [assemble_brms_priors()]
#' applies them to the slope terms of a supplied formula.
#'
#' \code{log_sigma} centers the intercept of a log-scale residual SD
#' (\code{sigma ~ phase}) on the log of the pooled within-phase SD with
#' scale 1, tying the prior to the measurement scale; the scale 1 lets the
#' baseline SD range over a factor of about e either way and is a
#' convention. On non-identity links the location is 0.
#'
#' @param data A data frame containing the outcome and grouping variables.
#' @param structure A list describing the hierarchical structure (from
#'   \code{detect_hierarchical_structure}).
#' @param model_type Character string specifying the model family. Any brms
#'   family name is accepted and resolved by its default \pkg{brms} link:
#'   identity-link families use a response-scale intercept, log-link
#'   families (\code{"poisson"}, \code{"negbinomial"}, \code{"Gamma"},
#'   \code{"weibull"}, \code{"exponential"}, hurdle and zero-inflated count
#'   families, ...) and \code{"lognormal"} a log-scale intercept, and
#'   logit-type links (\code{"bernoulli"}, \code{"beta"},
#'   \code{"cumulative"}, ...) a link-scale intercept centered on 0.
#' @param outcome_var Outcome column; defaults to \code{structure$outcome_var},
#'   then \code{"outcome"}.
#' @param exposure Optional exposure column of a count model with an
#'   \code{offset(log(<exposure>))} term (see [add_exposure_offset()]); the
#'   log-link intercept is then centered on the log of total outcome over
#'   total exposure, the rate per unit of exposure.
#'
#' @return A list of prior specifications including intercept, treatment, sigma
#'   (for identity links), \code{log_sigma}, \code{slopes} (named numeric
#'   scales for the session-count columns), random effects (if multilevel),
#'   and recommendations.
#'
#' @examples
#' d <- simulate_scd_data(n_cases = 3)
#' structure <- list(n_levels = 2, levels = list(case = "case"))
#' generate_adaptive_priors(d, structure)
#'
#' @export
generate_adaptive_priors <- function(data, structure, model_type = "gaussian",
                                     outcome_var = structure$outcome_var %||% "outcome",
                                     exposure = NULL) {
  data_chars <- analyze_data_characteristics(data, outcome_var)
  link_group <- .prior_model_type(model_type)
  sd_y <- data_chars$outcome_sd
  if (is.null(sd_y) || !is.finite(sd_y) || sd_y <= 0) sd_y <- 1
  mean_y <- data_chars$outcome_mean %||% 0
  # residual scale only; the phase shift would widen it
  within_sd <- data_chars$within_sd
  if (is.null(within_sd) || !is.finite(within_sd) || within_sd <= 0) within_sd <- sd_y

  priors <- switch(link_group,
    gaussian = list(
      intercept = .t_prior(mean_y, 2.5 * sd_y),
      treatment = .normal_prior(2.5 * sd_y),
      sigma = .t_prior(0, within_sd)),
    binomial = list(
      intercept = .t_prior(0, 2.5),
      treatment = .normal_prior(2.5)),
    poisson = list(
      intercept = .t_prior(log(.rate_per_exposure(data, outcome_var, exposure, mean_y)), 2.5),
      treatment = .normal_prior(2.5)))
  priors$log_sigma <- list(location = if (link_group == "gaussian") log(within_sd) else 0,
                           scale = 1)
  priors$slopes <- .slope_scales(data, priors$treatment$sd)

  if (!is.null(structure$n_levels) && structure$n_levels > 1) {
    priors$random_effects <- list(
      sd_group = .t_prior(0, if (link_group == "gaussian") sd_y else 2.5),
      correlation = list(distribution = "lkj", eta = 2, description = "lkj(2)"))
  }

  priors$recommendations <- generate_prior_recommendations(data_chars, structure)
  dispersion <- .dispersion_class(model_type)
  if (!is.null(dispersion)) {
    priors$recommendations$dispersion <- sprintf(paste0(
      "The %s parameter keeps the brms default prior. Add a second %s prior to the ",
      "prior sensitivity analysis and compare the treatment posterior across the two."),
      dispersion, dispersion)
  }
  priors
}

#' @noRd
.rate_per_exposure <- function(data, outcome_var, exposure, mean_y) {
  if (is.null(exposure) || !exposure %in% names(data)) return(max(mean_y, 0.1))
  ok <- is.finite(data[[outcome_var]]) & is.finite(data[[exposure]])
  # the floor keeps log() finite for an all-zero outcome
  if (!any(ok)) return(max(mean_y, 0.1))
  max(sum(data[[outcome_var]][ok]) / sum(data[[exposure]][ok]), 1e-4)
}

#' @noRd
.dispersion_class <- function(model_type) {
  if (model_type %in% c("negbinomial", "zero_inflated_negbinomial", "hurdle_negbinomial",
                        "Gamma", "weibull", "hurdle_gamma")) return("shape")
  if (model_type %in% c("beta", "zero_inflated_beta", "zero_one_inflated_beta")) return("phi")
  if (identical(model_type, "student")) return("nu")
  NULL
}

#' @noRd
.t_prior <- function(location, scale) {
  list(distribution = "student_t", df = 3, location = location, scale = scale,
       mean = location, sd = scale,
       description = sprintf("student_t(3, %.2f, %.2f)", location, scale))
}

#' @noRd
.normal_prior <- function(scale) {
  list(distribution = "normal", mean = 0, sd = scale,
       description = sprintf("normal(0, %.2f)", scale))
}

#' Pooled within-phase standard deviation
#'
#' Square root of the residual mean square after removing each case-by-phase
#' mean (each phase mean when no case column is present), so the treatment
#' effect does not inflate the noise scale.
#'
#' @param data Data frame with the response, a \code{phase} column, and
#'   optionally a case column.
#' @param response Response column name.
#' @return Numeric scalar; \code{NA} when it cannot be computed.
#' @noRd
.pooled_within_sd <- function(data, response = "outcome") {
  data <- .tier_phase_data(data)
  phase_var <- find_phase_var(data)
  if (is.null(phase_var) || !response %in% names(data)) return(NA_real_)
  case_var <- find_case_var(data)
  grp <- .std_factor(data, c(case_var, phase_var))
  y <- as.numeric(data[[response]])
  ok <- !is.na(y) & !is.na(grp)
  dev <- y[ok] - stats::ave(y[ok], grp[ok])
  df <- sum(ok) - nlevels(droplevels(grp[ok]))
  if (df <= 0) return(NA_real_)
  sqrt(sum(dev^2) / df)
}

#' @noRd
.slope_scales <- function(data, scale) {
  cols <- grep("^(time|trend)_", names(data), value = TRUE)
  cols <- cols[vapply(cols, function(v) is.numeric(data[[v]]), logical(1))]
  sd_x <- vapply(cols, function(v) stats::sd(data[[v]], na.rm = TRUE), numeric(1))
  ok <- is.finite(sd_x) & sd_x > 0
  stats::setNames(scale / (2 * sd_x[ok]), cols[ok])
}

#' @noRd
analyze_data_characteristics <- function(data, outcome_var = "outcome") {
  chars <- list(n_obs = nrow(data),
                missing_rate = sum(is.na(data)) / (nrow(data) * ncol(data)))
  if (is.null(outcome_var) || !outcome_var %in% names(data) ||
      !is.numeric(data[[outcome_var]])) return(chars)
  outcome <- data[[outcome_var]][!is.na(data[[outcome_var]])]
  # moments::kurtosis() is raw (normal = 3); heavy tails call for a Student-t likelihood
  c(chars, list(
    outcome_mean = mean(outcome),
    outcome_sd = stats::sd(outcome),
    within_sd = .pooled_within_sd(data, outcome_var),
    heavy_tailed = isTRUE(moments::kurtosis(outcome) - 3 > 3)))
}

#' Generate prior recommendations
#'
#' @param data_chars A list of data characteristics (from
#'   \code{analyze_data_characteristics}).
#' @param structure A list describing the hierarchical structure.
#'
#' @return A list of recommendation strings covering sample size, distribution,
#'   hierarchy, missing data, and overall prior strength.
#'
#' @examples
#' data_chars <- list(n_obs = 30, heavy_tailed = FALSE, missing_rate = 0.05)
#' generate_prior_recommendations(data_chars, list(n_levels = 2))
#'
#' @export
generate_prior_recommendations <- function(data_chars, structure) {
  recs <- list()

  if (data_chars$n_obs < 50) {
    recs$sample_size <- "Small sample: run a prior predictive check and a prior sensitivity analysis before interpreting the posterior."
  }

  if (!is.null(data_chars$heavy_tailed) && data_chars$heavy_tailed) {
    recs$distribution <- "Heavy-tailed data detected. Consider switching to a Student-t likelihood (model_family = 'student')."
  }

  if (!is.null(structure$n_levels) && structure$n_levels > 2) {
    recs$hierarchy <- "Complex hierarchical structure: each grouping level gets a half-Student-t SD prior."
  }

  if (data_chars$missing_rate > 0.1) {
    recs$missing <- sprintf("%.1f%% missing data. Consider multiple imputation.",
                           data_chars$missing_rate * 100)
  }

  recs$overall <- "Prior strength: weakly informative, scaled to the outcome SD or the link scale."

  recs
}

#' Resolve a brms family to its intercept-link prior group
#'
#' Maps a brms family name, by the default link of [brms::brmsfamily()], to
#' \code{"gaussian"} (identity link), \code{"poisson"} (log link, plus
#' lognormal, whose location is on the log scale), or \code{"binomial"}
#' (logit, probit, cloglog, cauchit, and inverse links: a link-scale prior
#' centered on 0). Unknown names and other links fall back to
#' \code{"gaussian"}.
#'
#' @param model_type Character name of the brms family.
#' @return One of \code{"gaussian"}, \code{"poisson"}, \code{"binomial"}.
#' @keywords internal
#' @noRd
.prior_model_type <- function(model_type) {
  link <- tryCatch(brms::brmsfamily(model_type)$link, error = function(e) NULL)
  # lognormal has an identity link on log(y), so its intercept is a log-scale quantity
  if (identical(model_type, "lognormal")) return("poisson")
  if (is.null(link)) return("gaussian")
  switch(link, identity = "gaussian", log = "poisson", logit = , probit = , cloglog = ,
         cauchit = , inverse = "binomial", "gaussian")
}

#' @noRd
.treatment_coef <- function(data, formula_str = NULL) {
  candidates <- c("level_AB", .reversal_onset(names(data)),
                  grep("^level_crit[0-9]+$", names(data), value = TRUE),
                  .atd_terms(names(data)),
                  "treatment", "phase_num")
  hits <- intersect(candidates, names(data))
  hits <- hits[vapply(hits, function(v) is.numeric(data[[v]]), logical(1))]
  if (!is.null(formula_str)) {
    rhs <- tryCatch(
      attr(stats::terms(stats::as.formula(formula_str)), "term.labels"),
      error = function(e) character(0))
    hits <- intersect(hits, rhs)
  }
  if (length(hits) > 0) hits[1] else NULL
}

#' Identify the treatment coefficient of a piecewise SCD model
#'
#' Picks the first numeric treatment-onset column among \code{level_AB}, the
#' reversal onset step, changing-criterion steps, alternating-treatment
#' dummies, \code{treatment}, and \code{phase_num}. When \code{formula_str} is
#' supplied, only columns that appear as formula terms qualify.
#'
#' @param data Processed data frame.
#' @param formula_str Optional character formula string.
#' @return Column name, or \code{NULL} when none qualifies.
#' @examples
#' treatment_coef(data.frame(level_AB = c(0, 1), outcome = c(1, 2)))
#' @export
treatment_coef <- function(data, formula_str = NULL) {
  .treatment_coef(data, formula_str)
}

#' Assemble brms priors from app-level inputs
#'
#' Internal helper that builds a named list of brms prior strings based on
#' the chosen prior type, family, and adaptive prior recommendations.
#'
#' @param prior_type Character: "default", "informative", "meta_analytic", or "sensitivity".
#'   The informative prior is normal with SD equal to half the adaptive
#'   weakly-informative SD, centered on \code{prior_effect_mean} when that is a
#'   finite number and on 0 otherwise. The sensitivity (diffuse) prior doubles
#'   the adaptive SD (5 outcome SDs on identity links, 5 on log/logit links),
#'   so it is never narrower than the default, and doubles the session-slope
#'   priors of \code{formula_str} by the same factor; comparing the two is the
#'   prior sensitivity analysis.
#'   \code{"sensitivity"} returns only this diffuse arm: the default arm is
#'   the \code{"default"} type, and [run_sensitivity_analysis()] fits the
#'   arms and compares them.
#' @param prior_effect_mean Numeric mean of the elicited treatment prior, used
#'   by the meta-analytic and informative types.
#' @param prior_effect_sd Numeric SD for the meta-analytic prior; must be
#'   positive and finite (an error otherwise).
#' @param heterogeneous_variance Logical; if TRUE add separate sigma priors.
#' @param model_family_value Character name of the brms family.
#' @param adaptive_priors List from [generate_adaptive_priors()].
#' @param treatment_coef Optional name of the treatment coefficient. When
#'   supplied with an informative or meta-analytic prior type, the elicited
#'   treatment prior targets only this coefficient (\code{coef}) and a
#'   weakly-informative default covers the remaining \code{b} coefficients.
#'   When \code{NULL} with those types, a warning is issued: the elicited mean
#'   cannot be applied and the treatment SD falls on the whole \code{b} class
#'   with mean 0.
#' @param has_random_slope Logical; used only without \code{formula_str}: if
#'   TRUE an \code{lkj(2)} prior is emitted on the correlation (\code{cor})
#'   parameter. With \code{formula_str}, the \code{cor} prior is emitted only
#'   when a single-bar group term such as \code{(1 + level_AB | case)} has two
#'   or more coefficients; uncorrelated \code{||} terms and one-coefficient
#'   terms have no correlation, and brms errors on a prior that matches no
#'   parameter.
#' @param formula_str Optional model formula string. When supplied, each
#'   session-count term of the formula (\code{time_*}, \code{trend_*}) with a
#'   scale in \code{adaptive_priors$slopes} gets its own \code{normal(0,
#'   scale)} prior (see [generate_adaptive_priors()]).
#'
#' @details With \code{heterogeneous_variance = TRUE} the log-scale residual
#'   SD intercept is \code{normal(log pooled within-phase SD, 1)} and the
#'   phase contrasts on it \code{normal(0, 1)} (see
#'   [generate_adaptive_priors()]). Shape (negative binomial, Gamma),
#'   precision (beta), and degrees-of-freedom (Student-t) parameters keep the
#'   brms defaults. Default priors have limited influence in larger samples,
#'   not with a few cases; compare a second shape or precision prior in the
#'   sensitivity analysis (see
#'   \code{recommendations$dispersion} of [generate_adaptive_priors()]).
#'
#'   With an elicited (informative or meta-analytic) prior on
#'   \code{treatment_coef}, the remaining \code{b} coefficients get a
#'   class-level default prior. When \code{formula_str} is given and every
#'   fixed term already has its own prior, that class-level prior is dropped,
#'   since brms warns about a prior that matches no parameter.
#'
#' @return Named list of prior specifications with attributes.
#' @keywords internal
#' @export
assemble_brms_priors <- function(prior_type, prior_effect_mean, prior_effect_sd,
                                 heterogeneous_variance, model_family_value,
                                 adaptive_priors, treatment_coef = NULL,
                                 has_random_slope = FALSE, formula_str = NULL) {
  default_sd <- adaptive_priors$treatment$sd %||% 2.5
  if (identical(prior_type, "meta_analytic")) {
    meta_sd <- suppressWarnings(as.numeric(prior_effect_sd %||% 0.3))
    if (length(meta_sd) != 1 || !is.finite(meta_sd) || meta_sd <= 0) {
      stop("The meta-analytic prior SD must be a positive finite number.", call. = FALSE)
    }
  }
  treatment_sd <- switch(prior_type,
    informative   = 0.5 * default_sd,
    meta_analytic = as.numeric(prior_effect_sd %||% 0.3),
    sensitivity   = 2 * default_sd,
    default_sd)
  # elicited priors pin the treatment coef only, so trends and moderators stay unshifted
  elicited_type <- prior_type %in% c("informative", "meta_analytic")
  elicited <- elicited_type && !is.null(treatment_coef)
  if (elicited_type && is.null(treatment_coef)) {
    warning(sprintf(paste0("No treatment coefficient identified for the %s prior: ",
                           "the elicited mean is not applied and the treatment SD ",
                           "covers every fixed effect with mean 0."), prior_type))
  }
  effect_mean <- suppressWarnings(as.numeric(prior_effect_mean))
  effect_mean <- if (length(effect_mean) == 1 && is.finite(effect_mean)) effect_mean else 0
  treatment_loc <- if (elicited) effect_mean else 0
  # Gamma has shape, not sigma; lognormal sigma is log-scale, so it keeps student_t(3, 0, 2.5)
  has_sigma <- model_family_value %in% c("gaussian", "student", "skew_normal",
                                         "lognormal")
  # intercept generators return location/scale or mean/sd
  intercept_loc <- adaptive_priors$intercept$location %||%
    adaptive_priors$intercept$mean %||% 0
  intercept_scale <- adaptive_priors$intercept$scale %||%
    adaptive_priors$intercept$sd %||% 10
  priors <- list(
    intercept = list(class = "Intercept",
      string = sprintf("student_t(3, %.2f, %.2f)", intercept_loc, intercept_scale)),
    treatment = list(class = "b",
      coef = if (elicited) treatment_coef else NULL,
      string = sprintf("normal(%.2f, %.2f)", treatment_loc, treatment_sd))
  )
  if (elicited) {
    priors$b_default <- list(class = "b",
      string = sprintf("normal(0, %.2f)", adaptive_priors$treatment$sd %||% 1.0))
  }
  if (has_sigma && !isTRUE(heterogeneous_variance)) {
    priors$sigma <- list(class = "sigma",
      string = sprintf("student_t(3, 0, %.2f)",
        adaptive_priors$sigma$scale %||% 2.5))
  }
  if (has_sigma && isTRUE(heterogeneous_variance)) {
    log_sigma <- adaptive_priors$log_sigma %||% list(location = 0, scale = 1)
    priors$sigma_intercept <- list(class = "Intercept", dpar = "sigma",
      string = sprintf("normal(%.2f, %.2f)", log_sigma$location, log_sigma$scale))
    priors$sigma_b <- list(class = "b", dpar = "sigma",
      string = sprintf("normal(0, %.2f)", log_sigma$scale))
  }
  if (!is.null(formula_str) && length(adaptive_priors$slopes)) {
    slope_mult <- if (identical(prior_type, "sensitivity")) 2 else 1
    priors <- c(priors, .slope_prior_list(slope_mult * adaptive_priors$slopes, formula_str))
  }
  if (elicited && !is.null(formula_str)) {
    fixed <- grep("\\||^(ar|ma|arma|cosy|unstr|car|sar|fcor|gp|s|t2|mo|me|mi)\\(",
                  .formula_terms(formula_str), value = TRUE, invert = TRUE)
    own <- unlist(lapply(priors, function(p) if (identical(p$class, "b")) p$coef))
    if (length(fixed) && all(fixed %in% own)) priors$b_default <- NULL
  }
  re <- adaptive_priors$random_effects
  re_sd <- unlist(lapply(re[grepl("^sd_", names(re))], `[[`, "sd"))
  if (length(re_sd)) {
    # one class-level prior avoids per-group names such as "study:case"
    sd_scale <- if (model_family_value %in% c("gaussian", "student", "skew_normal"))
      max(re_sd) else 2.5
    priors$sd <- list(class = "sd",
      string = sprintf("student_t(3, 0, %.2f)", sd_scale))
  }
  # a cor parameter exists only for a single-bar term with 2+ coefficients; brms errors otherwise
  has_cor <- if (is.null(formula_str)) isTRUE(has_random_slope) else .has_correlated_term(formula_str)
  if (has_cor) {
    priors$correlation <- list(class = "cor", string = "lkj(2)")
  }
  attr(priors, "treatment_loc") <- treatment_loc
  attr(priors, "treatment_sd") <- treatment_sd
  priors
}

#' @noRd
.has_correlated_term <- function(formula_str) {
  expr <- tryCatch(str2lang(paste(formula_str, collapse = " ")), error = function(e) NULL)
  isTRUE(tryCatch(.bar_n_coef_max(expr) >= 2, error = function(e) FALSE))
}

#' @noRd
.bar_n_coef_max <- function(x) {
  if (!is.call(x)) return(0)
  inner <- if (identical(x[[1]], as.name("(")) && is.call(x[[2]]) &&
               identical(x[[2]][[1]], as.name("|"))) {
    lhs <- x[[2]][[2]]
    # brms (lhs | ID | g) parses as ((lhs | ID) | g)
    while (is.call(lhs) && identical(lhs[[1]], as.name("|"))) lhs <- lhs[[2]]
    tt <- stats::terms(stats::as.formula(call("~", lhs)))
    length(attr(tt, "term.labels")) + attr(tt, "intercept")
  } else 0
  max(c(inner, vapply(as.list(x)[-1], .bar_n_coef_max, numeric(1))))
}

.prior_dists <- function() {
  scale <- c("student_t", "normal", "exponential", "gamma", "inv_gamma", "cauchy")
  loc <- c("normal", "student_t", "cauchy", "skew_normal", "logistic",
           "double_exponential")
  list(b = loc, Intercept = loc, sd = scale, sigma = scale, shape = scale,
       phi = scale, nu = scale, sds = scale,
       cor = c("lkj", "lkj_corr_cholesky"), ar = c("normal", "uniform"),
       ma = c("normal", "uniform"))
}

.prior_arity <- list(
  normal = 2, student_t = 3, cauchy = 2, skew_normal = 3, logistic = 2,
  double_exponential = 2, exponential = 1, gamma = 2, inv_gamma = 2, lkj = 1,
  lkj_corr_cholesky = 1, uniform = 2)

.prior_positive <- list(
  normal = 2, student_t = c(1, 3), cauchy = 2, skew_normal = 2, logistic = 2,
  double_exponential = 2, exponential = 1, gamma = 1:2, inv_gamma = 1:2, lkj = 1,
  lkj_corr_cholesky = 1, uniform = integer(0))

.prior_pattern <- local({
  num <- "-?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?"
  sprintf("^[ \t]*[a-z_]+\\([ \t]*%s([ \t]*,[ \t]*%s)*[ \t]*\\)[ \t]*$", num, num)
})

.prior_args_ok <- function(fn, vals) {
  length(vals) == .prior_arity[[fn]] && all(vals[.prior_positive[[fn]]] > 0) &&
    (!identical(fn, "uniform") || vals[1] < vals[2])
}

.prior_num <- function(a) {
  if (is.call(a) && identical(a[[1]], as.name("-")) && length(a) == 2L) a <- a[[2]]
  is.numeric(a) && length(a) == 1L && is.finite(a)
}

#' Check that a prior string is a single distribution call
#'
#' Accepts one call to an allowlisted distribution whose arguments are all
#' finite numeric constants, such as `"normal(0, 1)"` or
#' `"student_t(3, 0, 2.5)"`. Statement separators, braces, `target`
#' increments, nested calls (`reject()`, `exp()`), `constant()`, and symbols
#' are rejected. With `class`, the distribution must suit that parameter
#' class: location families for `b` and `Intercept`; `student_t`, `normal`,
#' `exponential`, `gamma`, `inv_gamma`, `cauchy` for scale classes (brms adds
#' the lower bound); `lkj` for `cor`; and `normal` or `uniform` for `ar` and
#' `ma`, whose location must lie inside \[-1, 1\] and, for `uniform`, both
#' bounds as well.
#'
#' Each distribution takes its own number of arguments, and scale, degrees
#' of freedom, shape, rate, and `eta` arguments must be positive:
#' `normal(mu, sigma)`, `student_t(nu, mu, sigma)`, `cauchy(mu, sigma)`,
#' `skew_normal(xi, omega, alpha)`, `logistic(mu, sigma)`,
#' `double_exponential(mu, sigma)`, `exponential(rate)`, `gamma(a, b)`,
#' `inv_gamma(a, b)`, `lkj(eta)`, `lkj_corr_cholesky(eta)`, and
#' `uniform(a, b)` with `a < b`. `uniform` is accepted only with `class`
#' `"ar"` or `"ma"`; without `class` or for any other class it is rejected.
#' Numbers must be
#' plain decimals (optionally
#' with an exponent); hexadecimal or `L`-suffixed literals, comments, and
#' line breaks are rejected.
#'
#' @param x Prior string.
#' @param class Optional brms parameter class.
#' @return Logical scalar.
#' @examples
#' is_prior_string("normal(0, 1)")
#' is_prior_string("student_t(3, 0, 2.5)", class = "sd")
#' is_prior_string("normal(0, -1)")
#' is_prior_string("uniform(-1, 1)", class = "ar")
#' @export
is_prior_string <- function(x, class = NULL) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl(.prior_pattern, x)) return(FALSE)
  e <- tryCatch(str2lang(x), error = function(e) NULL)
  if (!is.call(e) || !is.symbol(e[[1]])) return(FALSE)
  dists <- .prior_dists()
  allowed <- if (is.null(class)) unique(unlist(dists)) else dists[[class]]
  fn <- as.character(e[[1]])
  args <- as.list(e)[-1]
  vals <- if (length(args) && all(vapply(args, .prior_num, logical(1))))
    vapply(args, function(a) eval(a, baseenv()), numeric(1)) else NULL
  ar_ma <- isTRUE(class %in% c("ar", "ma"))
  in_unit <- !is.null(vals) && abs(vals[1]) <= 1 &&
    (!identical(fn, "uniform") || all(abs(vals) <= 1))
  bounded <- if (ar_ma) in_unit else !identical(fn, "uniform")
  fn %in% allowed && length(vals) == length(args) && length(args) > 0 &&
    .unnamed(args) && bounded && .prior_args_ok(fn, vals)
}

#' Convert prior list to brms prior object
#'
#' Internal helper that converts the list returned by
#' `assemble_brms_priors` into a stacked `brmsprior`.
#'
#' @param priors Named list of prior specifications.
#' @return A `brmsprior` object.
#' @keywords internal
#' @export
priors_to_brms <- function(priors) {
  valid <- Filter(function(p) {
    is.character(p$string) && length(p$string) == 1L &&
      !is.na(p$string) && nzchar(p$string)
  }, priors)
  if (length(valid) == 0L) return(NULL)
  do.call(c, unname(lapply(valid, function(p) {
    args <- list(p$string, class = p$class)
    if (!is.null(p$coef)) args$coef <- p$coef
    if (!is.null(p$dpar)) args$dpar <- p$dpar
    do.call(brms::prior_string, args)
  })))
}
