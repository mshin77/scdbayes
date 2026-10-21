#' @noRd
.default_backend <- function() {
  if (requireNamespace("cmdstanr", quietly = TRUE)) "cmdstanr" else "rstan"
}

#' @noRd
.max_treedepth <- function() getOption("scdbayes.max_treedepth", 12L)

#' @noRd
.identity_family <- function(family) {
  fam <- if (is.character(family)) family else family$family %||% "gaussian"
  fam %in% c("gaussian", "student", "skew_normal")
}

#' @noRd
.has_group_term <- function(formula_str) {
  grepl("\\([^()]*\\|", paste(formula_str, collapse = " "))
}

#' @noRd
.formula_terms <- function(formula_str) {
  tryCatch(attr(stats::terms(stats::as.formula(paste(formula_str, collapse = " "))),
                "term.labels"),
           error = function(e) character(0))
}

#' @noRd
.slope_prior_list <- function(slope_scales, formula_str) {
  terms <- intersect(names(slope_scales), .formula_terms(formula_str))
  stats::setNames(lapply(terms, function(v) {
    list(class = "b", coef = v, string = sprintf("normal(0, %.4f)", slope_scales[[v]]))
  }), sprintf("slope_%s", terms))
}

#' @noRd
.prior_outcome_sd <- function(data, response) {
  s <- c(stats::sd(data[[response]], na.rm = TRUE), 1)
  return(s[is.finite(s) & s > 0][1])
}

#' @noRd
.default_priors <- function(data, formula_obj, family) {
  identity_link <- .identity_family(family)
  sd_y <- .prior_outcome_sd(data, all.vars(formula_obj)[1])
  b_scale  <- if (identity_link) 2.5 * sd_y else 2.5
  sd_scale <- if (identity_link) sd_y else 2.5
  formula_str <- paste(deparse(formula_obj), collapse = " ")
  out <- brms::prior_string(sprintf("normal(0, %.4f)", b_scale), class = "b")
  # an intercept-only formula has no class b parameter for the prior to match
  fixed <- grep("\\||^(ar|ma|arma|cosy)\\(", .formula_terms(formula_str), value = TRUE, invert = TRUE)
  if (!length(fixed)) out <- out[0, ]
  slopes <- priors_to_brms(.slope_prior_list(.slope_scales(data, b_scale), formula_str))
  if (!is.null(slopes)) out <- c(out, slopes)
  out <- .add_ar_prior(out, formula_str)
  # an sd prior without a group term errors in brms (no matching parameter)
  if (!.has_group_term(formula_str)) return(out)
  c(out, brms::prior_string(sprintf("student_t(3, 0, %.4f)", sd_scale), class = "sd"))
}

#' @noRd
.add_ar_prior <- function(priors, formula_str) {
  has_ar <- grepl("\\bar\\(", paste(formula_str, collapse = " "))
  if (!has_ar || (!is.null(priors) && "ar" %in% priors$class)) return(priors)
  ar <- brms::prior_string("normal(0, 0.5)", class = "ar", lb = -1, ub = 1)
  if (is.null(priors)) ar else c(priors, ar)
}

#' @noRd
.stan_model_args <- function(backend) {
  # stanc "O1" optimization is cmdstanr-only
  if (!identical(backend, "cmdstanr")) return(list())
  list(stanc_options = getOption("scdbayes.stanc_options", list("O1")))
}

#' Default piecewise formula for a coded SCD data set
#'
#' @details Fixed effects follow the design coding of [process_scd_phases()]:
#'   baseline trend, then a level and a slope change at each transition, so
#'   the immediate effect and the effect on slope are estimated separately.
#'   Changing-criterion models carry the baseline trend and one level step
#'   per criterion subphase and per return to baseline (\code{level_ret<k>}),
#'   without per-subphase slopes: steeper slopes
#'   across subphases do not indicate correspondence to the criterion.
#'   Alternating-treatment models carry one dummy per condition and the
#'   centered session time \code{time_session}, so a general trend is not
#'   absorbed by the condition dummies.
#'
#'   The random-effects part is [default_random_effects()]: level-2 units
#'   are the tiers of [tier_vars()], every level-change coefficient varies
#'   across them, and each outer study, cluster, or case grouping gets a
#'   random intercept. The half-Student-t SD prior of
#'   [generate_adaptive_priors()] regularizes the slope SDs when units are
#'   few. With \code{family = "binomial"} the response is
#'   \code{outcome | trials(<col>)} for a numeric trials column named
#'   \code{trials}, \code{n_trials}, \code{opportunities}, or
#'   \code{intervals}.
#'
#' @param data Coded data from [process_scd_phases()].
#' @param family brms family name.
#' @return Formula string.
#' @noRd
.build_scd_formula <- function(data, family = "gaussian") {
  fixed <- .scd_fixed_terms(names(data))
  re <- .scd_random_effects(data, fixed)
  sprintf("%s ~ %s%s", .response_lhs(data, family), paste(fixed, collapse = " + "),
          if (nzchar(re)) paste0(" + ", re) else "")
}

#' Default fixed piecewise terms for coded data
#'
#' @param nm Column names of data coded by [process_scd_phases()].
#' @return Character vector of fixed-effect term names.
#' @noRd
.scd_fixed_terms <- function(nm) {
  nm <- .syntactic(nm)
  crit_levels <- .criterion_steps(nm)
  atd <- .atd_terms(nm)
  rev_terms <- reversal_terms(nm)
  if (length(rev_terms)) return(rev_terms)
  if (length(crit_levels)) return(c(intersect("time_A", nm), crit_levels))
  if (all(c("treatment", "time_treatment") %in% nm)) {
    return(c(intersect("time_A", nm), "treatment", "time_treatment"))
  }
  if (length(atd)) return(c(intersect("time_session", nm), atd))
  c("time_A", "level_AB", "trend_AB", intersect(c("level_BM", "trend_BM",
                                                  "level_BG", "trend_BG"), nm))
}

#' @noRd
.trials_col <- function(data) {
  hit <- grep("^(trials|n_trials|opportunities|intervals)$", names(data),
              ignore.case = TRUE, value = TRUE)
  hit <- hit[vapply(hit, function(v) is.numeric(data[[v]]), logical(1))]
  if (length(hit)) hit[1] else NULL
}

#' @noRd
.response_lhs <- function(data, family) {
  if (!identical(family, "binomial")) return("outcome")
  col <- .trials_col(data)
  if (is.null(col)) {
    stop("The binomial family needs a numeric trials column named trials, n_trials, ",
         "opportunities, or intervals.", call. = FALSE)
  }
  sprintf("outcome | trials(%s)", col)
}

#' Add an exposure offset to a count-model formula
#'
#' Appends \code{offset(log(<exposure>))} to a Poisson or negative binomial
#' formula (zero-inflated or not), so the coefficients describe the rate per
#' unit of exposure (session length, number of intervals, opportunities)
#' rather than the raw count. The exposure column must be numeric, present on
#' every row, and strictly positive.
#'
#' @param formula_str Model formula as a string.
#' @param data Data holding the exposure column.
#' @param exposure Name of the exposure column, or \code{NULL} to return
#'   \code{formula_str} unchanged.
#' @param family brms family name; \code{"poisson"}, \code{"negbinomial"},
#'   \code{"zero_inflated_poisson"}, or \code{"zero_inflated_negbinomial"}.
#' @return The formula string with the offset term appended (unchanged when
#'   it already carries that offset); an error when the exposure column or
#'   family is not usable, or when the formula already holds a different
#'   offset.
#' @examples
#' d <- data.frame(outcome = c(3, 5, 2), level_AB = c(0, 1, 1), minutes = c(10, 20, 15))
#' add_exposure_offset("outcome ~ level_AB", d, "minutes", "poisson")
#' @export
add_exposure_offset <- function(formula_str, data, exposure, family) {
  if (is.null(exposure)) return(formula_str)
  if (!is.character(exposure) || length(exposure) != 1L || is.na(exposure)) {
    stop("The exposure must be a single column name.", call. = FALSE)
  }
  if (!nzchar(exposure)) return(formula_str)
  if (!exposure %in% names(data)) {
    stop("The exposure column must name one column of the data.", call. = FALSE)
  }
  if (!identical(.syntactic(exposure), exposure)) {
    stop("The exposure column name must be a syntactic R name.", call. = FALSE)
  }
  if (!is.character(family) || length(family) != 1L || !family %in% .exposure_families) {
    stop(sprintf("An exposure offset applies to the %s families only.",
                 paste(.exposure_families, collapse = ", ")), call. = FALSE)
  }
  e <- data[[exposure]]
  if (!is.numeric(e) || anyNA(e) || any(!is.finite(e)) || any(e <= 0)) {
    stop(sprintf("The exposure column '%s' must be numeric, present on every row, and > 0.",
                 exposure), call. = FALSE)
  }
  term <- sprintf("offset(log(%s))", exposure)
  if (grepl(term, formula_str, fixed = TRUE)) return(formula_str)
  if (grepl("offset(", formula_str, fixed = TRUE)) {
    stop("The formula already holds an offset(); a model takes one exposure offset only.",
         call. = FALSE)
  }
  paste(formula_str, "+", term)
}

.exposure_families <- c("poisson", "negbinomial", "zero_inflated_poisson",
                        "zero_inflated_negbinomial")

#' @noRd
.n_units <- function(data, vars = tier_vars(data)) {
  if (!length(vars)) return(1L)
  nlevels(.std_factor(data, vars))
}

#' @noRd
.scd_random_effects <- function(data, fixed) {
  .random_effects_string(data, tier_vars(data), fixed) %||% ""
}

#' @noRd
.rope_decisions <- function(model, data, family_value, intervention_goal) {
  summarize_fixed_effect_decisions(
    model, rope = default_rope(data, family_value, .response_column(model, data) %||% "outcome"),
    intervention_goal = intervention_goal)
}

#' @noRd
.model_cache_file <- function(formula_str, priors, data, family, sampler = list()) {
  # content hash: distinct fits and concurrent sessions never share one .rds
  if (!requireNamespace("digest", quietly = TRUE)) return(NULL)
  key <- digest::digest(list(formula_str, as.character(family), priors, data, sampler))
  file.path(getOption("scdbayes.cache_dir", tempdir()), paste0("scdbayes-model-", key))
}

#' @noRd
.prune_model_cache <- function(dir = getOption("scdbayes.cache_dir", tempdir()),
                               keep = getOption("scdbayes.cache_keep", 20L)) {
  files <- list.files(dir, pattern = "^scdbayes-model-.*\\.rds$", full.names = TRUE)
  if (length(files) <= keep) return(invisible(character(0)))
  old <- files[order(file.mtime(files), decreasing = TRUE)][-seq_len(keep)]
  unlink(old)
  invisible(old)
}

#' @noRd
.find_call <- function(x, name) {
  if (!is.call(x)) return(NULL)
  if (identical(x[[1]], as.name(name))) return(x)
  Find(Negate(is.null), lapply(as.list(x)[-1], .find_call, name = name))
}

#' @noRd
.ar_spacing_warning <- function(data, formula_str) {
  ar_call <- .find_call(tryCatch(str2lang(paste(formula_str, collapse = " ")),
                                 error = function(e) NULL), "ar")
  if (is.null(ar_call)) return(invisible(NULL))
  args <- as.list(match.call(function(time = NA, gr = NA, p = 1, cov = FALSE) NULL, ar_call))
  vars_of <- function(key) intersect(all.vars(args[[key]] %||% quote(NA)), names(data))
  time_var <- c(vars_of("time"), find_time_var(data))[1]
  # gr may be an interaction such as study:case; series are its level combinations
  gr <- vars_of("gr")
  response <- all.vars(stats::as.formula(formula_str))[1]
  if (!is.null(time_var)) {
    key <- if (length(gr)) .std_factor(data, gr) else rep(1, nrow(data))
    gaps <- unlist(lapply(split(as.numeric(data[[time_var]]), key),
                          function(t) unique(round(diff(sort(t)), 8))))
    if (any(gaps == 0)) {
      warning(sprintf(paste0("Time points repeat within an ar() series (gr = %s); set gr to ",
                             "the full tier, e.g. gr = %s."),
                      if (length(gr)) paste(gr, collapse = ":") else "none",
                      paste(tier_vars(data, time_var), collapse = ":")), call. = FALSE)
    } else if (length(unique(gaps)) > 1) {
      warning("ar() lags by row order within each series, but the time differences are ",
              "not constant; the AR(1) coefficient then mixes lags of different length.",
              call. = FALSE)
    }
  }
  if (response %in% names(data) && anyNA(data[[response]])) {
    warning("ar() is fitted with missing outcomes: rows with a missing outcome are ",
            "dropped, so the lag skips the missing sessions.", call. = FALSE)
  }
  invisible(NULL)
}

#' @noRd
build_model <- function(data, formula_str, priors = NULL, iter = 2000, chains = 4,
                        adapt_delta = 0.95, max_treedepth = .max_treedepth(), ...) {

  formula_obj <- stats::as.formula(formula_str)
  .ar_spacing_warning(data, formula_str)

  if (is.null(priors)) {
    priors <- .default_priors(data, formula_obj, list(...)$family %||% "gaussian")
  }

  backend <- getOption("scdbayes.backend", .default_backend())

  tryCatch({
    brm_args <- list(
      formula = formula_obj,
      data = data,
      prior = priors,
      iter = iter,
      warmup = floor(iter/2),
      chains = chains,
      cores = min(chains, parallel::detectCores()),
      backend = backend,
      file = .model_cache_file(formula_str, priors, data, list(...)$family,
                               sampler = list(iter = iter, chains = chains,
                                              warmup = floor(iter/2),
                                              adapt_delta = adapt_delta,
                                              max_treedepth = max_treedepth,
                                              seed = list(...)$seed, init = list(...)$init,
                                              save_pars = list(...)$save_pars,
                                              sample_prior = list(...)$sample_prior,
                                              backend = backend)),
      file_refit = "on_change",
      control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
      silent = 2,
      refresh = 0,
      ...
    )
    # within-chain threading is cmdstanr-only and incompatible with latent ar() terms
    has_ar <- grepl("\\bar\\(", formula_str)
    if (identical(backend, "cmdstanr")) {
      if (!has_ar) brm_args$threads <- getOption("scdbayes.threads", brms::threading(2))
      brm_args$stan_model_args <- .stan_model_args(backend)
    } else {
      thr <- getOption("scdbayes.threads", NULL)
      if (!is.null(thr) && !has_ar) brm_args$threads <- thr
    }
    model <- do.call(brm, brm_args)
    .prune_model_cache()

    if (max(brms::rhat(model), na.rm = TRUE) >= 1.01) {
      warning("Model may not have converged")
    }

    return(model)

  }, error = function(e) {
    warning("Model fitting failed: ", e$message)
    return(NULL)
  })
}

#' Main analysis function
#'
#' @param data Data frame containing SCD data with columns \code{case},
#'   \code{phase}, and \code{outcome}.
#' @param options Named list of analysis options. Recognized entries:
#'   \code{formula} (model formula or string; defaults to a design-based
#'   piecewise formula), \code{scd_type} (design passed to
#'   [process_scd_phases()]; defaults to [detect_design_type()]),
#'   \code{family} (brms family name, default \code{"gaussian"};
#'   \code{"binomial"} needs a numeric trials column, see Details),
#'   \code{prior_type} (\code{"default"}, \code{"informative"},
#'   \code{"meta_analytic"}, or \code{"sensitivity"}),
#'   \code{prior_effect_mean}/\code{prior_effect_sd} (for the meta-analytic
#'   prior), \code{intervention_goal} (\code{"increase"} or \code{"decrease"},
#'   passed to the posterior decision summary), \code{expected_effect_size},
#'   \code{quick_test}, \code{prior_predictive} (logical; when
#'   \code{TRUE} adds a prior predictive check to the diagnostics), and
#'   \code{exposure} (name of a positive exposure column for the
#'   \code{"poisson"}, \code{"negbinomial"}, \code{"zero_inflated_poisson"},
#'   or \code{"zero_inflated_negbinomial"} family; adds
#'   \code{offset(log(<exposure>))} through [add_exposure_offset()]).
#'
#' @return An object of class \code{scd_analysis} containing data summary,
#'   power estimate, fitted model, effect sizes, a posterior decision summary
#'   (\code{decisions}), and diagnostics.
#'
#' @details The decision summary reports a ROPE probability for level-change
#'   terms only, with the region of [default_rope()]. Slope terms
#'   (\code{trend_*}, \code{time_*}) get \code{NA}: a per-session slope is
#'   negligible or not only relative to the number of sessions it is
#'   projected over.
#'
#'   Proportions of a known number of trials or intervals can be modeled
#'   with \code{family = "binomial"} when the data carry a numeric
#'   \code{trials}, \code{n_trials}, \code{opportunities}, or
#'   \code{intervals} column; the response is then \code{outcome |
#'   trials(<col>)}. The beta family stops on outcomes at 0 or 1 and points
#'   to the (y * (n - 1) + 0.5) / n squeeze or the binomial family.
#'
#'   A formula with an \code{ar()} term warns when time differences within a
#'   series are not constant or when outcomes are missing: brms lags by row
#'   order, while an AR(1) model assumes equidistant occasions, and dropping
#'   missed sessions alters the series.
#'
#' @examples
#' if (interactive()) {
#' run_scd_analysis(simulate_scd_data())
#' }
#'
#' @export
run_scd_analysis <- function(data, options = list()) {

  results <- list(
    data_summary = NULL,
    power = NULL,
    model = NULL,
    effects = NULL,
    diagnostics = NULL
  )

  tryCatch({
    validate_scd_config(data)
  }, error = function(e) {
    stop("Data validation failed: ", e$message)
  })

  n_obs <- nrow(data)
  n_cases <- length(unique(data$case))

  results$data_summary <- list(
    n_observations = n_obs,
    n_cases = n_cases,
    phases = unique(data$phase)
  )

  timepoints_per_case <- n_obs / n_cases
  results$power <- quick_power_estimate(
    n_cases = n_cases,
    n_timepoints = round(timepoints_per_case),
    effect_size = options$expected_effect_size %||% 0.5
  )

  config <- get_scd_config(n_obs, n_cases)

  family_value <- options$family %||% "gaussian"
  if (identical(family_value, "beta")) {
    y <- data$outcome[!is.na(data$outcome)]
    if (any(y <= 0 | y >= 1)) {
      stop("The beta family needs outcomes strictly between 0 and 1; ",
           sum(y <= 0 | y >= 1), " value(s) fall at or outside 0/1. ",
           "Squeeze them with (y * (n - 1) + 0.5) / n, use a ",
           "zero-one-inflated beta family, or use family = \"binomial\" with a trials column.",
           call. = FALSE)
    }
  }

  formula_str <- options$formula
  if (is.null(formula_str)) {
    detected <- detect_design_type(data)
    scd_type <- options$scd_type %||% if (detected %in% c("unknown", "ab")) "AB" else detected
    data <- process_scd_phases(data, structure = list(), scd_type = scd_type)
    formula_str <- .build_scd_formula(data, family_value)
  }
  if (inherits(formula_str, "formula")) {
    formula_str <- paste(deparse(formula_str), collapse = " ")
  }
  formula_str <- add_exposure_offset(formula_str, data, options$exposure, family_value)

  if (options$quick_test %||% FALSE) {
    config$iter <- 1000
    config$chains <- 2
  }

  prior_type <- options$prior_type %||% "default"
  n_units <- .n_units(data)
  if (n_units <= 5) {
    warning(sprintf(paste0(
      "Only %d level-2 unit(s): between-unit variance is weakly identified. Omitting a ",
      "random treatment slope that exists inflates the false-positive rate, so keep the ",
      "random slopes in the formula and run a prior sensitivity analysis."), n_units))
  }
  # one builder for every prior type, so types differ only in the treatment prior
  priors <- tryCatch({
    structure <- if (.has_group_term(formula_str)) list(n_levels = 2) else list()
    adaptive <- generate_adaptive_priors(data, structure = structure, model_type = family_value,
                                         outcome_var = all.vars(stats::as.formula(formula_str))[1],
                                         exposure = options$exposure)
    prior_list <- assemble_brms_priors(
      prior_type = prior_type,
      prior_effect_mean = options$prior_effect_mean,
      prior_effect_sd = options$prior_effect_sd,
      heterogeneous_variance = FALSE,
      model_family_value = family_value,
      adaptive_priors = adaptive,
      treatment_coef = .treatment_coef(data, formula_str),
      has_random_slope = has_random_slope(formula_str),
      formula_str = formula_str
    )
    .add_ar_prior(priors_to_brms(prior_list), formula_str)
  }, error = function(e) {
    warning("Prior construction failed; falling back to default priors: ", e$message)
    NULL
  })

  model <- build_model(
    data = data,
    formula_str = formula_str,
    priors = priors,
    iter = config$iter,
    chains = config$chains,
    adapt_delta = config$adapt_delta,
    max_treedepth = config$max_treedepth,
    family = family_value
  )

  results$model <- model

  if (!is.null(model)) {
    results$effects <- summarize_phase_descriptives(data)

    fixed <- brms::fixef(model)
    effect_row <- c(match(.treatment_coef(data, formula_str), rownames(fixed)),
                    grep("level_crit|treatment|phase", rownames(fixed)))[1]
    if (!is.na(effect_row)) {
      results$effects$model_estimate <- fixed[effect_row, "Estimate"]
      results$effects$model_ci <- c(fixed[effect_row, "Q2.5"], fixed[effect_row, "Q97.5"])
    }

    results$decisions <- tryCatch(
      .rope_decisions(model, data, family_value,
                      options$intervention_goal %||% "increase"),
      error = function(e) NULL
    )
  }

  if (!is.null(model)) {
    conv <- compute_convergence_diagnostics(model, data = data)
    # MCSE over regression coefficients; all monitored quantities as fallback
    mcse_vals <- conv$basic$mcse
    mcse_b <- mcse_vals[grepl("^b_", names(mcse_vals))]
    if (!length(mcse_b)) mcse_b <- mcse_vals
    .agg <- function(x, f) if (any(!is.na(x))) f(x, na.rm = TRUE) else NA_real_
    results$diagnostics <- list(
      max_rhat     = .agg(conv$basic$rhat, max),
      min_ess_bulk = .agg(conv$basic$ess_bulk, min),
      min_ess_tail = .agg(conv$basic$ess_tail, min),
      max_mcse     = .agg(mcse_b, max),
      n_divergent  = conv$divergence$n_divergent %||% NA_integer_,
      converged    = isTRUE(conv$converged$converged),
      ppc          = conv$ppc
    )
    if (isTRUE(options$prior_predictive)) {
      results$diagnostics$prior_predictive <- prior_predictive_check(model, data)
    }
    if (!isTRUE(conv$converged$converged)) {
      warning("Model did not meet convergence criteria (Rhat < 1.01, bulk and tail ESS > 100 per chain, no divergent transitions). Interpret estimates with caution; consider more iterations, higher adapt_delta or max_treedepth, or a simpler model.")
    }
  }

  class(results) <- c("scd_analysis", "list")
  return(results)
}
