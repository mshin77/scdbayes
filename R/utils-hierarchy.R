#' Detect hierarchical structure in data
#'
#' @param data A data frame containing outcome and potential grouping variables.
#' @param max_levels Integer specifying the maximum number of hierarchical levels
#'   to detect. Default is 5.
#' @param method Character string specifying the detection method. One of
#'   \code{"auto"}, \code{"variance"}, or \code{"information"}.
#'
#' @return A list with components \code{levels}, \code{level_names},
#'   \code{level_variables}, \code{n_levels}, \code{units_per_level},
#'   \code{nesting_structure}, \code{cross_classification},
#'   \code{recommendations}, and \code{icc_values}.
#'
#' @examples
#' d <- data.frame(
#'   case = rep(c("C1", "C2", "C3"), each = 8),
#'   phase = rep(c("A", "B"), 12),
#'   time = rep(1:8, 3),
#'   outcome = rnorm(24)
#' )
#' detect_hierarchical_structure(d)
#'
#' @export
detect_hierarchical_structure <- function(data, max_levels = 5, method = "auto") {
  
  structure <- list(
    levels = list(),
    level_names = character(),
    level_variables = character(),
    n_levels = 0,
    units_per_level = numeric(),
    nesting_structure = list(),
    cross_classification = FALSE,
    recommendations = list()
  )
  
  grouping_candidates <- identify_grouping_variables(data)
  nesting_matrix <- detect_nesting_relationships(data, grouping_candidates)

  if (method %in% c("auto", "variance")) {
    variance_hierarchy <- detect_by_variance_decomposition(data, grouping_candidates)
    structure$variance_based <- variance_hierarchy
  }
  
  if (method %in% c("auto", "information")) {
    ic_hierarchy <- detect_by_information_criteria(data, grouping_candidates)
    structure$ic_based <- ic_hierarchy
  }
  
  cross_class <- detect_cross_classification(data, grouping_candidates, nesting_matrix)
  structure$cross_classification <- cross_class$is_crossed
  structure$crossed_factors <- cross_class$crossed_factors
  
  structure <- synthesize_detection_results(structure, nesting_matrix, data)
  structure$recommendations <- generate_model_recommendations(structure, data)
  structure$icc_values <- calculate_multilevel_icc(data, structure)
  
  return(structure)
}

#' @noRd
identify_grouping_variables <- function(data) {
  # only the tier columns are grouping variables; phase and between-case
  # covariates are low-cardinality too but name no level of the hierarchy
  vars <- intersect(tryCatch(tier_vars(data), error = function(e) character(0)), names(data))
  if (!length(vars)) vars <- intersect("case", names(data))
  if (!length(vars)) return(list())

  n_unique <- vapply(vars, function(v) length(unique(data[[v]])), numeric(1))
  level <- cut(n_unique, breaks = c(-Inf, 5, 20, 100, Inf),
               labels = c("level5", "level4", "level3", "level2"))

  Filter(length, split(vars, level))
}

#' @noRd
detect_nesting_relationships <- function(data, candidates) {
  all_vars <- unique(unlist(candidates))
  n_vars <- length(all_vars)
  
  if (n_vars < 2) return(matrix(0, n_vars, n_vars))
  
  nesting_matrix <- matrix(0, n_vars, n_vars,
                           dimnames = list(all_vars, all_vars))
  
  for (i in 1:(n_vars-1)) {
    for (j in (i+1):n_vars) {
      var1 <- all_vars[i]
      var2 <- all_vars[j]
      
      if (var1 %in% names(data) && var2 %in% names(data)) {
        if (is_nested(data[[var1]], data[[var2]])) {
          nesting_matrix[var1, var2] <- 1
        }
        if (is_nested(data[[var2]], data[[var1]])) {
          nesting_matrix[var2, var1] <- 1
        }
      }
    }
  }
  
  return(nesting_matrix)
}

#' @noRd
is_nested <- function(x, y) {
  df <- data.frame(x = x, y = y)
  df <- df[complete.cases(df), ]
  
  if (nrow(df) == 0) return(FALSE)
  
  x_to_y <- df %>%
    group_by(.data$x) %>%
    summarise(n_y = n_distinct(.data$y), .groups = "drop")

  all(x_to_y$n_y == 1)
}

#' @noRd
detect_by_variance_decomposition <- function(data, candidates) {
  outcome_vars <- names(data)[sapply(data, is.numeric)]

  if (length(outcome_vars) == 0) return(NULL)

  outcome <- NULL
  if ("outcome" %in% outcome_vars) {
    outcome <- "outcome"
  } else {
    non_time_vars <- outcome_vars[!outcome_vars %in% c("time", "session", "trial", "day", "week", "id", "case", "participant")]
    if (length(non_time_vars) > 0) {
      outcome <- non_time_vars[1]
    } else if (length(outcome_vars) > 0) {
      outcome <- outcome_vars[1]
    }
  }
  
  if (is.null(outcome) || !outcome %in% names(data)) return(NULL)
  if (!is.numeric(data[[outcome]])) return(NULL)
  
  variance_results <- list()
  
  for (level in names(candidates)) {
    for (var in candidates[[level]]) {
      if (var %in% names(data) && var != outcome) {
        var_decomp <- tryCatch({
          group_means <- aggregate(data[[outcome]],
                                  by = list(data[[var]]), 
                                  FUN = mean, na.rm = TRUE)
          between_var <- var(group_means$x, na.rm = TRUE)
          total_var <- var(data[[outcome]], na.rm = TRUE)
          
          list(
            variable = var,
            level = level,
            between_variance = between_var,
            prop_variance = between_var / total_var,
            n_groups = length(unique(data[[var]]))
          )
        }, error = function(e) NULL)
        
        if (!is.null(var_decomp)) {
          variance_results[[var]] <- var_decomp
        }
      }
    }
  }
  
  if (length(variance_results) > 0) {
    variance_results <- variance_results[order(sapply(variance_results, 
                                                      function(x) -x$prop_variance))]
  }
  
  return(variance_results)
}

#' @noRd
detect_by_information_criteria <- function(data, candidates) {
  ic_results <- list()

  outcome_vars <- names(data)[sapply(data, is.numeric)]
  if (length(outcome_vars) == 0) return(NULL)

  outcome <- NULL
  if ("outcome" %in% outcome_vars) {
    outcome <- "outcome"
  } else {
    non_time_vars <- outcome_vars[!outcome_vars %in% c("time", "session", "trial", "day", "week", "id", "case", "participant")]
    if (length(non_time_vars) > 0) {
      outcome <- non_time_vars[1]
    } else if (length(outcome_vars) > 0) {
      outcome <- outcome_vars[1]
    }
  }
  
  if (is.null(outcome) || !outcome %in% names(data)) return(NULL)
  if (!is.numeric(data[[outcome]])) return(NULL)
  
  all_vars <- unique(unlist(candidates))
  
  for (var in all_vars) {
    if (var %in% names(data) && var != outcome) {
      n <- nrow(data)
      k <- length(unique(data[[var]]))
      ic_score <- k * log(n) - 2 * k
      
      ic_results[[var]] <- list(
        variable = var,
        n_groups = k,
        ic_score = ic_score,
        effective_sample_per_group = n / k
      )
    }
  }
  
  return(ic_results)
}

#' @noRd
detect_cross_classification <- function(data, candidates, nesting_matrix) {
  result <- list(is_crossed = FALSE, crossed_factors = list())
  
  all_vars <- unique(unlist(candidates))
  
  for (i in seq_along(all_vars)) {
    for (j in seq_along(all_vars)) {
      if (i != j) {
        var1 <- all_vars[i]
        var2 <- all_vars[j]
        
        if (var1 %in% rownames(nesting_matrix) && 
            var2 %in% colnames(nesting_matrix)) {
          if (nesting_matrix[var1, var2] == 0 &&
              nesting_matrix[var2, var1] == 0) {
            if (var1 %in% names(data) && var2 %in% names(data)) {
              combinations <- unique(data[, c(var1, var2)])
              n_var1 <- length(unique(data[[var1]]))
              n_var2 <- length(unique(data[[var2]]))
              
              if (nrow(combinations) > max(n_var1, n_var2)) {
                result$is_crossed <- TRUE
                result$crossed_factors[[length(result$crossed_factors) + 1]] <- 
                  c(var1, var2)
              }
            }
          }
        }
      }
    }
  }
  
  return(result)
}

#' @noRd
synthesize_detection_results <- function(structure, nesting_matrix, data) {
  var_results <- structure$variance_based
  ic_results <- structure$ic_based
  
  if (!is.null(var_results) && length(var_results) > 0) {
    top_vars <- names(var_results)[1:min(4, length(var_results))]
    ordered_vars <- order_by_nesting(top_vars, nesting_matrix, data)
    
    structure$levels <- ordered_vars$levels
    structure$level_names <- ordered_vars$names
    structure$level_variables <- ordered_vars$variables
    structure$n_levels <- length(ordered_vars$levels)
    
    for (var in structure$level_variables) {
      if (var %in% names(data)) {
        structure$units_per_level <- c(structure$units_per_level, 
                                      length(unique(data[[var]])))
      }
    }
  }
  
  return(structure)
}

#' @noRd
order_by_nesting <- function(vars, nesting_matrix, data) {
  vars <- vars[vars %in% names(data)]
  n_unique <- vapply(vars, function(v) length(unique(data[[v]])), numeric(1))

  nests_in <- function(fine, coarse) {
    fine %in% rownames(nesting_matrix) && coarse %in% colnames(nesting_matrix) &&
      nesting_matrix[fine, coarse] == 1
  }
  # a variable nested inside others is the finer level whatever the counts say;
  # cardinality only breaks ties, and crossed candidates keep their place
  depth <- vapply(vars, function(v)
    sum(vapply(vars, function(w) !identical(v, w) && nests_in(v, w), logical(1))),
    numeric(1))
  ordered_vars <- vars[order(depth, n_unique, decreasing = TRUE)]

  level_names <- c("Observations", "Level 2", "Level 3", "Level 4", "Level 5")
  list(levels = as.list(ordered_vars),
       names = level_names[pmin(seq_along(ordered_vars) + 1L, length(level_names))],
       variables = ordered_vars)
}

#' @noRd
.anova_icc <- function(y, grp, phase = NULL) {
  ok <- !is.na(y) & !is.na(grp)
  y <- as.numeric(y[ok])
  grp <- factor(as.character(grp[ok]))
  if (!is.null(phase)) {
    phase <- as.character(phase[ok])
    # remove the average phase shift so the intervention effect does not inflate MSW
    y <- y - ave(y, phase, FUN = function(z) mean(z, na.rm = TRUE)) + mean(y, na.rm = TRUE)
  }
  k <- nlevels(grp)
  n <- length(y)
  if (k < 2 || n - k < 1) return(NA_real_)
  nj <- tabulate(grp)
  mj <- tapply(y, grp, mean)
  grand <- mean(y)
  msb <- sum(nj * (mj - grand)^2) / (k - 1)
  msw <- sum((y - mj[as.integer(grp)])^2) / (n - k)
  # adjusted mean cluster size handles unbalanced designs
  n0 <- (n - sum(nj^2) / n) / (k - 1)
  if (msb + (n0 - 1) * msw <= 0) return(0)
  icc <- (msb - msw) / (msb + (n0 - 1) * msw)
  max(0, min(1, icc))
}

#' @noRd
calculate_multilevel_icc <- function(data, structure) {
  icc_values <- list()

  outcome_vars <- names(data)[sapply(data, is.numeric)]
  if (length(outcome_vars) == 0) return(icc_values)

  outcome <- NULL
  if (!is.null(structure$outcome_var) && structure$outcome_var %in% outcome_vars) {
    outcome <- structure$outcome_var
  } else if ("outcome" %in% outcome_vars) {
    outcome <- "outcome"
  } else if (length(outcome_vars) > 0) {
    non_time_vars <- outcome_vars[!outcome_vars %in% c("time", "session", "trial", "day", "week")]
    if (length(non_time_vars) > 0) {
      outcome <- non_time_vars[1]
    } else {
      outcome <- outcome_vars[1]
    }
  }
  
  if (is.null(outcome) || !outcome %in% names(data)) return(icc_values)
  
  if (!is.numeric(data[[outcome]])) return(icc_values)

  for (i in seq_along(structure$level_variables)) {
    var <- structure$level_variables[i]
    if (var %in% names(data) && var != outcome) {
      icc <- tryCatch({
        if (sum(!is.na(data[[outcome]])) < 2 || length(unique(data[[var]])) < 2) {
          return(NA)
        }
        .anova_icc(data[[outcome]], data[[var]],
                   phase = if ("phase" %in% names(data)) data$phase else NULL)
      }, error = function(e) NA)
      
      icc_values[[var]] <- icc
    }
  }
  
  return(icc_values)
}

#' @noRd
generate_model_recommendations <- function(structure, data) {
  recommendations <- list()
  
  if (length(structure$icc_values) > 0) {
    high_icc <- names(structure$icc_values)[structure$icc_values > 0.05]
    if (length(high_icc) > 0) {
      recommendations$use_multilevel <- TRUE
      recommendations$random_effects <- high_icc
      recommendations$message <- paste("Multilevel modeling recommended. ",
                                      "Substantial clustering detected at:",
                                      paste(high_icc, collapse = ", "))
    } else {
      recommendations$use_multilevel <- FALSE
      recommendations$message <- "Low ICC values. Consider simpler models."
    }
  }
  
  if (structure$cross_classification) {
    recommendations$model_type <- "crossed_random_effects"
    recommendations$crossed_message <- "Cross-classified structure detected. Use crossed random effects."
  } else {
    recommendations$model_type <- "nested_random_effects"
  }
  
  if (structure$n_levels > 2) {
    recommendations$centering <- "group_mean_centering"
    recommendations$centering_message <- "Consider group-mean centering for Level 1 predictors"
  }
  
  if (length(structure$units_per_level) > 0) {
    min_units <- min(structure$units_per_level)
    if (min_units < 30) {
      recommendations$estimation <- "bayesian"
      recommendations$estimation_message <- paste("Small number of units at higher levels (",
                                                 min_units, 
                                                 "). Bayesian estimation recommended.")
    } else {
      recommendations$estimation <- "either"
      recommendations$estimation_message <- "Sufficient units for ML or Bayesian estimation."
    }
  }
  
  return(recommendations)
}

#' @noRd
export_structure_formula <- function(structure) {
  if (structure$n_levels < 2) {
    return("outcome ~ treatment")
  }
  
  random_parts <- character()
  
  for (i in 2:length(structure$level_variables)) {
    var <- structure$level_variables[i]
    if (i == 2) {
      random_parts <- c(random_parts, paste0("(1 + treatment | ", var, ")"))
    } else {
      random_parts <- c(random_parts, paste0("(1 | ", var, ")"))
    }
  }
  
  if (structure$cross_classification && length(structure$crossed_factors) > 0) {
    for (crossed in structure$crossed_factors) {
      random_parts <- c(random_parts, 
                       paste0("(1 | ", crossed[1], ":", crossed[2], ")"))
    }
  }
  
  formula_str <- paste("outcome ~ treatment",
                       paste(random_parts, collapse = " + "),
                       sep = " + ")
  
  return(formula_str)
}

