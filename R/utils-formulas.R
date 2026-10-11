#' Calculate meta-analytic summary statistics
#'
#' @param data A data frame containing effect size data.
#' @param structure A list describing the data structure, including grouping
#'   levels and variable names.
#'
#' @return A list with elements \code{n_studies}, \code{n_effects},
#'   \code{total_participants} (distinct cases resolved via
#'   \code{find_case_var()}; \code{NA} when no case column is found), and
#'   \code{effects_per_study}.
#'
#' @examples
#' d <- data.frame(
#'   study_id = rep(c("S1", "S2"), each = 10),
#'   participant = rep(1:4, 5),
#'   outcome = rnorm(20)
#' )
#' calculate_meta_summary(d, structure = list())
#'
#' @export
calculate_meta_summary <- function(data, structure) {
    study_var <- NULL

    if ("study_id" %in% names(data)) {
        study_var <- "study_id"
    } else if ("study" %in% names(data)) {
        study_var <- "study"
    } else if (!is.null(structure$levels) && length(structure$levels) > 0) {
        study_var <- names(structure$levels)[1]
    }

    case_var <- find_case_var(data)

    if (!is.null(study_var) && study_var %in% names(data)) {
        n_studies <- n_distinct(data[[study_var]])

        es_summary <- data %>%
            group_by(across(all_of(study_var))) %>%
            summarise(
                n_effects = n(),
                n_participants = if (!is.null(case_var)) n_distinct(.data[[case_var]]) else NA_integer_,
                .groups = 'drop'
            )
    } else {
        n_studies <- 1
        es_summary <- data.frame(
            n_effects = nrow(data),
            n_participants = if (!is.null(case_var)) n_distinct(data[[case_var]]) else NA_integer_
        )
    }

    summary_stats <- list(
        n_studies = n_studies,
        n_effects = nrow(data),
        total_participants = if (is.null(case_var)) NA_integer_
                             else sum(es_summary$n_participants, na.rm = TRUE),
        effects_per_study = list(
            min = min(es_summary$n_effects, na.rm = TRUE),
            mean = round(mean(es_summary$n_effects, na.rm = TRUE), 1),
            median = median(es_summary$n_effects, na.rm = TRUE),
            max = max(es_summary$n_effects, na.rm = TRUE)
        )
    )

    return(summary_stats)
}

#' Analyze continuous and categorical covariates
#'
#' @param data A data frame containing covariates to analyze.
#' @param structure A list describing the data structure, used to exclude
#'   grouping and phase variables from covariate detection.
#'
#' @return A list with elements \code{continuous} and \code{categorical}, each
#'   containing descriptive statistics for detected covariates.
#'
#' @examples
#' d <- data.frame(
#'   case = rep(c("C1", "C2"), each = 10),
#'   phase = rep(c("A", "B"), 10),
#'   age = rnorm(20, 8, 1),
#'   gender = sample(c("M", "F"), 20, replace = TRUE),
#'   outcome = rnorm(20)
#' )
#' analyze_covariates(d, structure = list(levels = list(case = "case")))
#'
#' @export
analyze_covariates <- function(data, structure) {
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

    numeric_vars <- setdiff(numeric_vars, c("effect_size", "variance", "se", "sample_size", "outcome"))
    categorical_vars <- setdiff(categorical_vars, c(unlist(structure$levels), "phase"))

    covariate_info <- list(
        continuous = list(),
        categorical = list()
    )

    if (length(numeric_vars) > 0) {
        for (var in numeric_vars) {
            covariate_info$continuous[[var]] <- data %>%
                summarise(
                    n = sum(!is.na(.data[[var]])),
                    missing_pct = round(mean(is.na(.data[[var]])) * 100, 1),
                    mean = round(mean(.data[[var]], na.rm = TRUE), 2),
                    sd = round(sd(.data[[var]], na.rm = TRUE), 2),
                    min = round(min(.data[[var]], na.rm = TRUE), 2),
                    q25 = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 2),
                    median = round(median(.data[[var]], na.rm = TRUE), 2),
                    q75 = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 2),
                    max = round(max(.data[[var]], na.rm = TRUE), 2)
                )
        }
    }

    if (length(categorical_vars) > 0) {
        for (var in categorical_vars) {
            freq_table <- table(data[[var]], useNA = "ifany")
            covariate_info$categorical[[var]] <- list(
                levels = names(freq_table),
                frequencies = as.vector(freq_table),
                proportions = round(prop.table(freq_table) * 100, 1)
            )
        }
    }

    return(covariate_info)
}

#' Match a column name (case-insensitive) against a pattern list
#'
#' Ranked by position in \code{patterns}, not by column order, so a data frame
#' holding both \code{session} and \code{time} resolves to the pattern listed
#' first rather than whichever column happens to come first.
#'
#' @param data Data frame.
#' @param patterns Character vector of lowercase patterns, most specific first.
#' @return Original column name (preserving case), or \code{NULL}.
#' @keywords internal
.match_var <- function(data, patterns) {
    nm <- names(data)
    rank <- match(tolower(nm), patterns)
    if (all(is.na(rank))) NULL else nm[which.min(rank)]
}

#' Locate the case-level grouping column by common SCED name patterns
#'
#' Matches \code{case}/\code{participant}/\code{subject}/\code{id}
#' (case-insensitive). Returns the original column name.
#'
#' @param data Data frame.
#' @return Column name, or \code{NULL} if no match.
#' @examples
#' find_case_var(data.frame(participant = 1:3, outcome = rnorm(3)))
#' @export
find_case_var <- function(data) {
    .match_var(data, c("case", "participant", "subject", "id"))
}

#' Locate the phase column by common SCED name patterns
#'
#' Matches \code{phase}/\code{condition}/\code{treatment_phase}
#' (case-insensitive).
#'
#' @param data Data frame.
#' @return Column name, or \code{NULL} if no match.
#' @examples
#' find_phase_var(data.frame(condition = c("A", "B"), outcome = c(1, 2)))
#' @export
find_phase_var <- function(data) {
    .match_var(data, c("phase", "condition", "treatment_phase"))
}

#' Locate the time column by common SCED name patterns
#'
#' Matches \code{time}/\code{session}/\code{occasion}/\code{wave}/
#' \code{timepoint}/\code{observation} (case-insensitive).
#'
#' @param data Data frame.
#' @return Column name, or \code{NULL} if no match.
#' @examples
#' find_time_var(data.frame(session = 1:5, outcome = rnorm(5)))
#' @export
find_time_var <- function(data) {
    .match_var(data, c("time", "session", "occasion", "wave",
                       "timepoint", "observation"))
}

#' Locate the study- / institution-level grouping column
#'
#' Matches \code{study}/\code{study_id}/\code{site}/\code{school}/
#' \code{district} (case-insensitive). Used for the highest grouping level
#' (meta-analytic study or institution).
#'
#' @param data Data frame.
#' @return Column name, or \code{NULL} if no match.
#' @examples
#' find_study_var(data.frame(study_id = c("S1", "S2"), outcome = c(1, 2)))
#' @export
find_study_var <- function(data) {
    .match_var(data, c("study", "study_id", "site", "school", "district"))
}

#' Locate a mid-level cluster grouping column
#'
#' Matches \code{cluster}/\code{therapist}/\code{teacher}/\code{classroom}/
#' \code{group} (case-insensitive). Used for a grouping level above case but
#' below study/institution. Observer, rater, and assessor columns record
#' interobserver agreement rather than a design level, so they never match.
#'
#' @param data Data frame.
#' @return Column name, or \code{NULL} if no match.
#' @examples
#' find_cluster_var(data.frame(teacher = c("T1", "T2"), outcome = c(1, 2)))
#' @export
find_cluster_var <- function(data) {
    .match_var(data, c("cluster", "therapist", "teacher", "classroom", "group"))
}

#' @noRd
.ioa_cols <- function(data) {
    grep("observer|rater|assessor|coder|ioa", names(data), ignore.case = TRUE, value = TRUE)
}

#' Grouping columns that define one data series (tier)
#'
#' Returns the grouping columns present, ordered from outermost to innermost:
#' study (\code{find_study_var()}), cluster (\code{find_cluster_var()}), case
#' (\code{find_case_var()}), and a within-case nest variable such as behavior
#' or setting (\code{find_nest_var()}). Each combination of these columns is
#' one tier: one AB series whose change points are coded at its own onset.
#'
#' @details A within-case nest variable is kept when time points repeat
#'   within the study-cluster-case key, that is, when a case carries several
#'   concurrently measured series, or, for a column named like a nest
#'   (behavior, setting, skill, ...), when phase onsets differ across its
#'   levels within a case (a multiple-baseline design across behaviors whose
#'   sessions do not overlap in time). Behaviors or settings within a participant
#'   form an additional level between measurements and participants; cases
#'   are nested within studies, so a case identifier repeated in two studies
#'   names two cases.
#'
#' @param data Data frame.
#' @param time_var Time column used to decide whether a nest variable splits
#'   concurrent series; defaults to \code{find_time_var(data)}.
#' @return Character vector of column names (possibly empty).
#' @examples
#' d <- data.frame(study = rep(c("S1", "S2"), each = 4), case = "C1",
#'                 time = rep(1:4, 2), outcome = rnorm(8))
#' tier_vars(d)
#' @export
tier_vars <- function(data, time_var = find_time_var(data)) {
    outer <- unique(c(find_study_var(data), find_cluster_var(data), find_case_var(data)))
    nest <- find_nest_var(data, exclude = outer, case_var = find_case_var(data),
                          time_var = time_var)
    no_time <- is.null(time_var) || !time_var %in% names(data)
    keep <- !is.null(nest) && (no_time || anyDuplicated(data[c(outer, time_var)]) > 0 ||
                               (nest %in% .nest_name_patterns &&
                                .onsets_differ(data, nest, outer, time_var)))
    c(outer, if (keep) nest)
}

# a column constant within every case is a between-case covariate, not a nest
.varies_within_case <- function(col, group) {
    if (is.null(group)) return(TRUE)
    any(tapply(col, group, function(x) length(unique(x[!is.na(x)])) > 1))
}

#' @noRd
.onsets_differ <- function(data, v, outer, time_var, phase_var = find_phase_var(data)) {
    if (is.null(phase_var) || is.null(time_var) || !time_var %in% names(data)) return(FALSE)
    groups <- split(seq_len(nrow(data)), .std_factor(data, c(outer, v)))
    onset <- vapply(groups, function(i) {
        i <- i[order(data[[time_var]][i])]
        ph <- as.character(data[[phase_var]][i])
        change <- which(ph[-1] != ph[-length(ph)])
        if (length(change)) as.numeric(data[[time_var]][i][change[1] + 1]) else NA_real_
    }, numeric(1))
    outer_key <- vapply(groups, function(i) .std_key(data[i[1], , drop = FALSE], outer, sep = "\r"),
                        character(1))
    any(tapply(onset, outer_key, function(o) length(unique(o[!is.na(o)])) > 1))
}

#' @noRd
.splits_series <- function(data, v, outer, time_var) {
    if (is.null(time_var) || !time_var %in% names(data)) return(FALSE)
    concurrent <- anyDuplicated(data[c(outer, time_var)]) > 0 &&
        anyDuplicated(data[c(outer, v, time_var)]) == 0
    concurrent || .onsets_differ(data, v, outer, time_var)
}

.coded_cols <- c("phase_numeric", "criterion_phase", "is_baseline", "alternation_block",
                 "intervention_start", "stagger_group")

.nest_name_patterns <- c("behavior", "setting", "target", "goal", "tier",
                         "stimulus", "context", "skill",
                         "task", "lesson", "topic", "domain")

#' Detect a within-case nesting variable by name pattern then structure
#'
#' @param data Data frame.
#' @param exclude Character vector of column names to skip (e.g., case, study,
#'   phase, time, outcome, piecewise terms).
#' @param case_var Optional case-grouping column. When supplied, a candidate must
#'   vary within at least one case; a column constant within every case is a
#'   between-case covariate and is rejected.
#' @param time_var Time column used by the structural fallback; defaults to
#'   \code{find_time_var(data)}.
#'
#' @details A column whose name matches a nest pattern (behavior, setting,
#'   target, skill, ...) qualifies when it varies within a case. Any other
#'   non-numeric column, or integer-coded column when a case column is known,
#'   with 2-20 levels (criterion,
#'   trials, IOA, and coded piecewise columns excluded) qualifies only when it splits
#'   concurrent series (time points repeat within a case but not within a
#'   case-by-column cell) or when phase onsets differ across its levels
#'   within a case, so a date or note column never becomes a nest. Without a
#'   time column the structural fallback is skipped.
#'
#' @return Column name (length-1 character), or \code{NULL} if no candidate is
#'   found.
#'
#' @examples
#' d <- data.frame(
#'   case = rep("Ian", 8),
#'   behavior = rep(c("greet", "share"), 4),
#'   outcome = rnorm(8)
#' )
#' find_nest_var(d, exclude = "case")
#'
#' @export
find_nest_var <- function(data, exclude = character(0), case_var = NULL,
                          time_var = find_time_var(data)) {
    group <- if (!is.null(case_var) && case_var %in% names(data)) data[[case_var]] else NULL
    exclude <- c(exclude, .ioa_cols(data))
    hit <- setdiff(intersect(.nest_name_patterns, names(data)), exclude)
    hit <- Filter(function(v) length(unique(data[[v]])) >= 2 && .varies_within_case(data[[v]], group), hit)
    if (length(hit) > 0) return(hit[1])

    excluded <- unique(c(exclude, find_cluster_var(data), find_study_var(data),
                         find_phase_var(data), time_var, "phase", "time", "outcome",
                         "session", "trial", "treatment", "condition", "intervention",
                         .criterion_col(data), .trials_col(data), .coded_cols,
                         grep("^(level|trend|time|treatment|atd)_", names(data), value = TRUE)))
    outer <- intersect(unique(c(find_study_var(data), find_cluster_var(data), case_var)),
                       names(data))
    Find(function(v) {
        col <- data[[v]]
        n_unique <- length(unique(col[!is.na(col)]))
        # a numeric nest must be an integer code inside a known case
        code_ok <- !is.numeric(col) || (length(outer) > 0 && all(col == round(col), na.rm = TRUE))
        code_ok && n_unique >= 2 && n_unique <= 20 &&
            .varies_within_case(col, group) && .splits_series(data, v, outer, time_var)
    }, setdiff(.syntactic(names(data)), excluded))
}

#' Detect a random slope in a model formula
#'
#' Parses the formula and inspects every grouping term written as
#' \code{(lhs | g)} or \code{(lhs || g)}. A term carries a random slope when its
#' left-hand side names a variable; constant-only sides such as \code{1},
#' \code{0 + 1}, or the brms ID form \code{(1 | ID | g)} do not.
#'
#' @param formula_str Character formula string.
#' @return Logical scalar; \code{FALSE} when the string does not parse.
#' @examples
#' has_random_slope("outcome ~ level_AB + (1 | case)")
#' has_random_slope("outcome ~ level_AB + (0 + level_AB || case)")
#' @export
has_random_slope <- function(formula_str) {
    expr <- tryCatch(str2lang(paste(formula_str, collapse = " ")),
                     error = function(e) NULL)
    if (is.null(expr)) return(FALSE)
    isTRUE(tryCatch(.bar_lhs_has_var(expr), error = function(e) FALSE))
}

#' @noRd
.is_bar_call <- function(x) {
    is.call(x) && (identical(x[[1]], as.name("|")) || identical(x[[1]], as.name("||")))
}

#' @noRd
.bar_lhs_has_var <- function(x) {
    if (!is.call(x)) return(FALSE)
    if (identical(x[[1]], as.name("(")) && .is_bar_call(x[[2]])) {
        lhs <- x[[2]][[2]]
        # brms (lhs | ID | g) parses as ((lhs | ID) | g)
        while (.is_bar_call(lhs)) lhs <- lhs[[2]]
        if (length(all.vars(lhs)) > 0) return(TRUE)
    }
    any(vapply(as.list(x)[-1], .bar_lhs_has_var, logical(1)))
}

#' Default random-effects structure of a single-case data set
#'
#' Returns the random-effects part of the default piecewise model, the same
#' string the package adds to the fixed terms when no formula is supplied.
#' Raw data (with a phase column but no coded columns) are first coded with
#' [process_scd_phases()] for \code{design}.
#'
#' @details Level-2 units are the tiers of [tier_vars()]: the combination of
#'   study, cluster, case, and within-case nest (behavior, setting) columns,
#'   so a case identifier repeated in two studies is two units. With two or
#'   more tiers every level-change coefficient of the coded design (reversal
#'   steps, criterion steps, alternating-treatment dummies, or
#'   \code{level_AB}/\code{level_BM}/\code{level_BG}) varies across tiers;
#'   omitting a random slope that exists inflates the Type I error rate with
#'   few cases. Each outer grouping (study, study:cluster,
#'   study:cluster:case) with more levels than the grouping above it and
#'   fewer than the tiers gets a random intercept, so behaviors or settings
#'   form a level between measurements and participants and cases sit within
#'   clusters and studies. Study, cluster, and nest columns constant in the
#'   data are dropped; the case column is kept, so the tier term always
#'   names cases.
#'
#'   In phase designs the onset step is correlated with the tier intercept
#'   and later steps enter as uncorrelated (\code{||}) terms, a reduction of
#'   the full random-effects covariance matrix that keeps the number of
#'   correlations small when units are few. Alternating-treatment dummies
#'   have no onset, so all of them enter as uncorrelated terms next to an
#'   intercept-only tier term; no condition is singled out.
#'
#' @param data Raw or coded single-case data.
#' @param design Design passed to [process_scd_phases()] when \code{data}
#'   are not coded yet; defaults to [detect_design_type()].
#' @return Character string such as
#'   \code{"(1 | study) + (1 + level_AB | study:case)"}, or \code{NULL} when
#'   the data hold a single series.
#' @examples
#' default_random_effects(simulate_scd_data(n_cases = 3))
#' @export
default_random_effects <- function(data, design = detect_design_type(data)) {
    if (!length(.step_columns(names(data)))) {
        scd_type <- if (design %in% c("unknown", "ab")) "AB" else design
        data <- process_scd_phases(data, list(), scd_type)
    }
    .random_effects_string(data, tier_vars(data), .scd_fixed_terms(names(data)))
}

#' Random effects with explicit grouping columns
#'
#' Builds the structure of [default_random_effects()] from coded data with
#' grouping columns that can be set explicitly; each \code{NULL} falls back
#' to the column [tier_vars()] detects.
#'
#' @param model_data Coded data from [process_scd_phases()].
#' @param case_var,nest_var,study_var Optional column names for the
#'   case-level grouping, the within-case grouping (e.g., behavior, skill,
#'   task), and the study-level grouping.
#'
#' @return A character string representing the random effects formula, or
#'   \code{NULL} when no case variable is found or the data hold a single
#'   series.
#'
#' @examples
#' d <- data.frame(
#'   case = rep(c("C1", "C2"), each = 10),
#'   phase = rep(c("A", "B"), 10),
#'   level_AB = rep(c(0, 1), 10),
#'   outcome = rnorm(20)
#' )
#' build_default_random_effects(d)
#'
#' @export
build_default_random_effects <- function(model_data, case_var = NULL,
                                          nest_var = NULL, study_var = NULL) {
    nm <- names(model_data)
    pick <- function(v, found) if (!is.null(v) && v %in% nm) v else found
    case_var <- pick(case_var, find_case_var(model_data))
    if (is.null(case_var)) return(NULL)
    outer <- c(find_study_var(model_data), find_cluster_var(model_data), find_case_var(model_data))
    nest <- pick(nest_var, setdiff(tier_vars(model_data), outer)[1])
    tv <- unique(c(pick(study_var, find_study_var(model_data)), find_cluster_var(model_data),
                   case_var, if (!is.na(nest)) nest))
    .random_effects_string(model_data, tv, nm, case_var)
}

#' @noRd
.random_effects_string <- function(data, tv, fixed, case_var = find_case_var(data)) {
    # a constant study or nest column adds nothing; the case column always names the units
    tv <- Filter(function(v) identical(v, case_var) || length(unique(data[[v]])) >= 2,
                 intersect(.syntactic(tv), names(data)))
    if (!length(tv) || .n_units(data, tv) < 2) return(NULL)
    steps <- .step_columns(fixed)
    atd <- length(steps) > 0 && all(steps %in% .atd_terms(steps))
    onset <- if (atd) NULL else .treatment_coef(data[intersect(steps, names(data))])
    others <- setdiff(steps, onset)
    units <- vapply(seq_along(tv), function(k) .n_units(data, tv[seq_len(k)]), numeric(1))
    outer <- which(utils::head(units, -1) >= 2 & utils::head(units, -1) < units[-1])
    tier <- paste(tv, collapse = ":")
    re <- c(vapply(outer, function(k) sprintf("(1 | %s)", paste(tv[seq_len(k)], collapse = ":")),
                   character(1)),
            sprintf("(%s | %s)", paste(c("1", onset), collapse = " + "), tier),
            if (length(others)) sprintf("(0 + %s || %s)", paste(others, collapse = " + "), tier))
    paste(re, collapse = " + ")
}

#' @noRd
.step_columns <- function(nm) {
    nm <- .syntactic(nm)
    candidates <- list(grep("^level_", reversal_terms(nm), value = TRUE),
                       .criterion_steps(nm),
                       .atd_terms(nm),
                       intersect(c("level_AB", "level_BM", "level_BG"), nm),
                       intersect("treatment", nm))
    Find(length, candidates) %||% character(0)
}
