#' WWC single-case design standards screening check
#'
#' Screens the phase structure of single-case data against the research
#' design requirements and the risk-of-bias rules of the What Works
#' Clearinghouse (WWC) Procedures and Standards Handbook, Version 5.0, and
#' reports the highest research rating the data structure is eligible for.
#' This is a screening aid, not an official WWC review: outcome validity,
#' confounding factors, researcher control of the independent variable, and
#' residual treatment effects are not assessed, and visual analysis by
#' trained reviewers is still required.
#'
#' @details
#'   \strong{Common rules.} Every design needs three attempts to demonstrate
#'   an effect at three different points in time. A phase with three or more
#'   data points and no within-phase variability counts toward the Meets
#'   Without Reservations data-point requirements. A design receives the
#'   highest rating any subset of it is eligible for, provided the subset
#'   still holds three demonstrations and compares only adjacent phases;
#'   extra phases or tiers with fewer data points do not lower the rating.
#'
#'   \strong{Reversal/withdrawal (each case is one experiment).} At least two
#'   phases per condition, that is four alternating phases. Without
#'   Reservations: the initial baseline of the case has six or more points,
#'   also when the qualifying window starts at a later phase, and the other
#'   phases five or more; With Reservations: three or more points in two
#'   phases per condition. The check searches every window of four
#'   consecutive alternating baseline/intervention phases. Reasons name the
#'   phases that pass only through zero variability.
#'
#'   \strong{Changing criterion (each case is one experiment).} The reversal
#'   rules apply with every baseline, intervention, or criterion change
#'   counted as a phase change, so three changes are required; maintenance
#'   and generalization phases are not criterion changes, and a series
#'   without a baseline phase does not meet standards. When
#'   the data carry a numeric \code{criterion}, \code{crit},
#'   \code{criterion_level}, or \code{criterion_value} column, phases are
#'   split on phase label and criterion value together, so each criterion
#'   change opens a phase; a missing criterion value carries the last one
#'   forward within the phase.
#'
#'   \strong{Multiple baseline and multiple probe (each study is one
#'   experiment).} At least six phases, i.e. three tiers with a baseline to
#'   intervention change, and at least three distinct intervention onsets.
#'   Without Reservations: first baseline of six or more points in each tier
#'   and later phases of five or more; With Reservations: three phases per
#'   condition with three or more points. Concurrence: every tier has
#'   baseline data before the first intervention onset, and every tier still
#'   in baseline has data at or after each earlier onset; vertical alignment
#'   is assumed from the shared time axis. Concurrence is judged across all
#'   tiers: a design that fails it is rated Does Not Meet WWC Standards, and
#'   no subset can restore the rating. Multiple probe designs
#'   add three rules: probes in the first three
#'   sessions (all three for Without, one common session for With), probes
#'   just before the onset (three consecutive for Without, at least one
#'   within the three prior sessions for With), and a probe for every
#'   untreated tier in the session another tier starts intervention.
#'   With \code{design} left at its default, a multiple baseline design with
#'   a baseline gap between sessions longer than the median session spacing
#'   of the same tier's intervention phase (baseline rows absent, as with
#'   planned probes) is reviewed as a multiple probe design. A row with an
#'   \code{NA} outcome is unplanned missing data and stays under the multiple
#'   baseline rules; pass \code{design} to override either reading.
#'   Concurrence is read strictly: a tier whose baseline
#'   data all end before an earlier tier's onset fails, even when it is
#'   measured again later in intervention.
#'
#'   \strong{Combination designs.} When a reversal or changing-criterion
#'   design has three or more tiers, the first baseline-intervention phases
#'   of the tiers are also screened as a multiple baseline design within each
#'   study; when it rates higher than every tier of that study, a row naming
#'   the multiple baseline reading is added.
#'
#'   A two-phase AB design
#'   is screened under the same rules, so AB tiers sharing one onset fail the
#'   distinct-onset requirement.
#'
#'   \strong{Alternating treatments (each case and pair of conditions is one
#'   contrast).} At least five points per condition for Without and four for
#'   With Reservations, and no more than two consecutive points of one
#'   condition within the rapidly alternating phase. A leading baseline of
#'   more than two points is set aside from the alternation, also when the
#'   baseline condition recurs within it. A final run of more than two points
#'   is a separate phase, exempt from the consecutive-point rule and left out
#'   of the point counts, only when it continues the most successful
#'   intervention: a condition with a recognised intervention, maintenance,
#'   or generalization role (see [phase_role()]; single letters C-H also count
#'   as interventions, other unrecognised labels such as \code{"Ctl"} are never
#'   exempt) whose alternating phase mean is strictly
#'   the highest of all conditions, ties excluded and baseline included (the
#'   lead baseline enters with its own mean when it does not recur in the
#'   alternation; lowest when \code{intervention_goal = "decrease"}); any
#'   other final run stays in the alternation. Trailing maintenance or
#'   generalization phases whose labels never alternate are left out of the
#'   alternating comparison but still count as phases for the IOA rule. When the
#'   baseline condition does not recur in the alternation, the single lead
#'   baseline is compared with each intervention-role condition of the
#'   alternating phase as its own contrast, under the same data-point rule
#'   (five points per condition for Without, four for With); the
#'   consecutive-point rule applies to the alternating condition only. Unrestricted random assignment of condition order is also exempt but
#'   cannot be detected from the data.
#'
#'   \strong{Risk of bias.} Designs other than alternating treatments that are
#'   eligible for Without Reservations are rated With Reservations at best when
#'   the initial baseline shows a therapeutic trend or reversibility is
#'   incomplete. Baseline trend compares the last three baseline points with
#'   all earlier baseline points (nonoverlap of all pairs, NAP); a baseline
#'   with zero variability and three or more points meets the rule.
#'   Reversibility compares the initial baseline with each return-to-baseline
#'   phase of a reversal or changing-criterion design. NAP of .85 or less meets each rule; NAP is oriented by
#'   \code{intervention_goal}. For multiple baseline designs every baseline in
#'   the qualifying subset must meet the trend rule.
#'
#'   \strong{Interassessor agreement (IOA).} At least .80 for percentage
#'   agreement or .60 for Cohen's kappa across phases and cases, collected in
#'   each phase and for at least 20\% of the data points. IOA is read
#'   from a numeric column whose name contains \code{ioa}, \code{agreement},
#'   \code{kappa}, or \code{reliab} (one value per session with agreement
#'   data, \code{NA} otherwise; values above 1 are read as percentages), or
#'   from \code{ioa_value} and \code{ioa_coverage}. IOA below the minimum
#'   fails the outcome reliability standard and caps the rating at Does Not
#'   Meet; without either source IOA is reported as not assessed. An
#'   \code{ioa_value} without \code{ioa_coverage} or an IOA column checks the
#'   value only: coverage is reported as not assessed (status
#'   \code{"partially assessed"}) and does not cap the rating. The rule that
#'   every phase carries IOA data applies only when the IOA column is the
#'   value source, not when \code{ioa_value} is supplied. The handbook asks for
#'   IOA in each phase without limiting it to the compared phases, so lead,
#'   final, maintenance, and generalization runs of an alternating design are
#'   each a phase for that rule.
#'
#'   Phase labels without a known role (see [phase_role()]) take their roles
#'   from first appearance in time, with a warning.
#'
#'   \strong{Study-level rating.} The overall rating is the highest rating
#'   among the experiments.
#'
#'   \strong{Next steps.} Findings that meet standards proceed to effect size
#'   estimation; the WWC computes a design-comparable effect size for
#'   reversal, multiple baseline, and multiple probe designs with three or
#'   more cases, implemented in the scdhlm package. Studies that meet
#'   standards can proceed to that design-comparable BC-SMD, which scdbayes
#'   estimates in a Bayesian model ([bc_smd_bayes()]) and pools with a
#'   Bayesian meta-analysis ([meta_bayes()]); scdhlm/metafor scripts are
#'   optional frequentist comparisons. scdbayes does not compute the
#'   within-case SMD, which is biased under autocorrelation.
#'
#' @param data Data frame with case, phase, time, and outcome columns.
#' @param design Design type: \code{"reversal"}, \code{"changing_criterion"},
#'   \code{"multiple_baseline"}, \code{"multiple_probe"}, \code{"ab"}, or
#'   \code{"alternating"}; defaults to [detect_design_type()].
#' @param time_var Time column; \code{NULL} uses row order.
#' @param outcome_var Outcome column.
#' @param intervention_goal \code{"increase"} or \code{"decrease"}; orients
#'   the baseline-trend and reversibility NAP.
#' @param ioa_value Optional overall IOA value (proportion or percentage).
#' @param ioa_coverage Optional proportion of data points with IOA (values
#'   above 1 are read as percentages); a value outside 0-1 after that
#'   scaling is an error.
#' @param ioa_type \code{"percentage"} (minimum .80) or \code{"kappa"}
#'   (minimum .60).
#'
#' @return A list with \code{rating} (overall), \code{reasons},
#'   \code{experiments} (one row per experiment or contrast with its rating
#'   and reasons), \code{tiers} (phase structure per tier), \code{baseline_trend},
#'   \code{reversibility}, \code{ioa} (status and detail), \code{design}, and
#'   \code{note}.
#'
#' @examples
#' d <- data.frame(case = "P1", time = 1:21,
#'                 phase = rep(c("A", "B", "A", "B"), c(6, 5, 5, 5)),
#'                 outcome = c(3, 4, 3, 4, 3, 4, 8, 9, 8, 9, 8,
#'                             4, 3, 4, 3, 4, 9, 8, 9, 8, 9))
#' wwc_design_check(d)$rating
#'
#' @export
wwc_design_check <- function(data, design = detect_design_type(data),
                             time_var = find_time_var(data),
                             outcome_var = "outcome", intervention_goal = "increase",
                             ioa_value = NULL, ioa_coverage = NULL,
                             ioa_type = c("percentage", "kappa")) {
  auto_design <- missing(design)
  ioa_type <- match.arg(ioa_type)
  phase_var <- find_phase_var(data)
  if (is.null(phase_var) || !outcome_var %in% names(data)) {
    stop("wwc_design_check() needs a phase column and the outcome column.", call. = FALSE)
  }
  data <- .drop_na_phase(data, time_var)
  design <- switch(design, alternating_treatment = "alternating", design)
  tiers <- tier_vars(data, time_var)
  study_var <- intersect(find_study_var(data), tiers)
  series <- .wwc_series(data, tiers, study_var, phase_var, time_var, outcome_var,
                        intervention_goal, .criterion_col(data))
  if (design != "alternating") .warn_inferred_roles(data[[phase_var]])
  if (auto_design && design == "multiple_baseline" && .wwc_probe_gaps(series)) {
    design <- "multiple_probe"
  }

  checked <- switch(design,
    reversal = .wwc_phase_design(series, reversal = TRUE),
    changing_criterion = .wwc_phase_design(series, reversal = FALSE),
    multiple_baseline = , multiple_probe = , ab = .wwc_mbd(series, probe = design == "multiple_probe"),
    alternating = .wwc_atd(series),
    NULL)
  if (is.null(checked)) {
    checked <- list(experiments = data.frame(experiment = "all", level = NA_integer_,
                                             reasons = "Design type not recognized.",
                                             stringsAsFactors = FALSE),
                    reversibility = .wwc_empty_reversibility())
  }
  if (design %in% c("reversal", "changing_criterion") && length(series) >= 3) {
    checked$experiments <- .wwc_combination(series, checked$experiments)
  }

  ioa <- .wwc_ioa(data, series, outcome_var, ioa_value, ioa_coverage, ioa_type, design)
  exps <- checked$experiments
  ioa_fail <- identical(ioa$status, "not met")
  if (ioa_fail) {
    exps$level <- pmin(exps$level, 0L)
    exps$reasons <- paste(exps$reasons, ioa$detail, sep = "; ")
  }
  exps$rating <- .wwc_label(exps$level)
  best <- if (all(is.na(exps$level))) NA_integer_ else max(exps$level, na.rm = TRUE)

  list(
    design = design,
    rating = .wwc_label(best),
    reasons = c(sprintf("%s: %s (%s)", exps$experiment, exps$rating, exps$reasons),
                if (!ioa_fail) sprintf("IOA %s: %s", ioa$status, ioa$detail)),
    experiments = exps[c("experiment", "rating", "reasons")],
    tiers = .wwc_tier_table(series),
    baseline_trend = .wwc_trend_table(series, design),
    reversibility = checked$reversibility,
    ioa = ioa,
    note = paste("Screening aid, not an official WWC review. Outcome measure standards,",
                 "confounding factors, researcher control of the independent variable, and",
                 "residual treatment effects are not assessed; visual analysis by trained",
                 "reviewers is still required. Studies that meet standards can proceed to",
                 "the design-comparable BC-SMD, estimated in a Bayesian model",
                 "(bc_smd_bayes) and pooled with a Bayesian meta-analysis (meta_bayes);",
                 "scdhlm/metafor scripts are optional frequentist comparisons. scdbayes",
                 "does not compute the within-case SMD, which is biased under",
                 "autocorrelation.")
  )
}

.wwc_ratings <- c("Does Not Meet WWC Standards", "Meets WWC Standards With Reservations",
                  "Meets WWC Standards Without Reservations")

#' @noRd
.wwc_label <- function(level) {
  ifelse(is.na(level), "Not rated", .wwc_ratings[pmax(0L, level) + 1L])
}

#' @noRd
.wwc_series <- function(data, tiers, study_var, phase_var, time_var, outcome_var, goal,
                        crit = NULL) {
  key <- .std_key(data, tiers)
  rows <- split(seq_len(nrow(data)), factor(key, levels = unique(key)))
  lapply(stats::setNames(names(rows), names(rows)), function(k) {
    i <- rows[[k]]
    t <- if (is.null(time_var) || !time_var %in% names(data)) seq_along(i)
         else as.numeric(data[[time_var]][i])
    o <- order(t)
    i <- i[o]
    t <- t[o]
    phase <- as.character(data[[phase_var]][i])
    # a criterion change is a phase change
    cv <- if (is.null(crit)) rep(NA, length(i)) else data[[crit]][i]
    # an unrecorded criterion carries the last one forward within the phase
    last <- stats::ave(ifelse(is.na(cv), 0L, seq_along(cv)),
                       cumsum(c(TRUE, phase[-1] != phase[-length(phase)])), FUN = cummax)
    cv <- cv[replace(last, last == 0L, NA)]
    seg <-ifelse(is.na(cv), phase, sprintf("%s [%s %s]", phase, crit, cv))
    r <- rle(seg)
    run <- rep(seq_along(r$values), r$lengths)
    y <- as.numeric(data[[outcome_var]][i])
    ys <- lapply(seq_along(r$values), function(j) y[run == j & !is.na(y)])
    list(tier = k, rows = i,
         experiment = if (length(study_var)) as.character(data[[study_var]][i[1]]) else "all",
         time = t, phase = phase, y = y, run = run, labels = r$values,
         role = .phase_numeric(phase[!duplicated(run)], seq_along(r$values)),
         n = lengths(ys), ys = ys,
         zero = vapply(ys, function(v) length(v) >= 3 && length(unique(v)) == 1, logical(1)),
         start = as.numeric(tapply(t, run, min)), goal = goal)
  })
}

#' @noRd
.wwc_probe_gaps <- function(series) {
  # planned probes leave baseline rows absent; an NA outcome row is unplanned missing data
  any(vapply(series, function(s) {
    base <- s$time[s$run == 1]
    tx <- s$time[s$run == 2]
    step <- if (length(tx) >= 2) stats::median(diff(tx), na.rm = TRUE) else NA_real_
    length(base) >= 2 && isTRUE(any(diff(base) > step))
  }, logical(1)))
}

#' @noRd
.wwc_nap <- function(a, b, goal) {
  if (!length(a) || !length(b)) return(NA_real_)
  better <- if (identical(goal, "decrease")) outer(b, a, `<`) else outer(b, a, `>`)
  mean(better + 0.5 * outer(b, a, `==`))
}

#' @noRd
.wwc_trend <- function(s) {
  base <- which(s$role == 0)[1]
  if (is.na(base)) return(list(n = 0L, nap = NA_real_, met = NA, status = "no baseline phase"))
  y <- s$ys[[base]]
  n <- length(y)
  if (s$zero[base]) return(list(n = n, nap = NA_real_, met = TRUE, status = "met (zero variability)"))
  if (n < 4) return(list(n = n, nap = NA_real_, met = NA, status = "not assessable (< 4 points)"))
  nap <- .wwc_nap(utils::head(y, n - 3), utils::tail(y, 3), s$goal)
  list(n = n, nap = nap, met = nap <= 0.85,
       status = if (nap <= 0.85) "met (NAP <= .85)" else "not met (NAP > .85)")
}

#' @noRd
.wwc_reversibility <- function(s, window) {
  base <- which(s$role == 0)[1]
  later <- setdiff(window[s$role[window] == 0], base)
  if (is.na(base) || !length(later)) return(.wwc_empty_reversibility())
  nap <- vapply(later, function(j) .wwc_nap(s$ys[[base]], s$ys[[j]], s$goal), numeric(1))
  data.frame(experiment = s$experiment, tier = s$tier,
             comparison = sprintf("initial baseline vs %s (phase %d)", s$labels[later], later),
             nap = nap, status = ifelse(nap <= 0.85, "met (NAP <= .85)", "not met (NAP > .85)"),
             stringsAsFactors = FALSE)
}

#' @noRd
.wwc_empty_reversibility <- function() {
  data.frame(experiment = character(0), tier = character(0), comparison = character(0),
             nap = numeric(0), status = character(0), stringsAsFactors = FALSE)
}

#' @noRd
.wwc_window_level <- function(s, w) {
  # the six-point minimum belongs to the initial baseline of the case, inside the window or not
  base <- which(s$role == 0)[1]
  need <- ifelse(!is.na(base) & w == base, 6, 5)
  init_ok <- s$n[base] >= 6 || s$zero[base]
  if (init_ok && all(s$n[w] >= need | s$zero[w])) return(2L)
  if (all(s$n[w] >= 3)) 1L else 0L
}

#' @noRd
.wwc_zero_note <- function(s, idx, need) {
  hit <- idx[s$zero[idx] & s$n[idx] < need]
  if (!length(hit)) return(NULL)
  sprintf("%s count through zero variability (>= 3 identical points)",
          paste0(s$labels[hit], "(", s$n[hit], ")", collapse = ", "))
}

#' @noRd
.wwc_phase_design <- function(series, reversal) {
  rows <- lapply(series, function(s) {
    m <- length(s$labels)
    alternates <- function(w) {
      on <- s$role[w] >= 1
      all(s$role[w] %in% c(0, 1)) && all(on[-1] != on[-length(on)])
    }
    # maintenance and generalization phases are no criterion change
    mg <- phase_role(s$phase[!duplicated(s$run)]) %in% c(2, 3)
    windows <- Filter(function(w) if (reversal) alternates(w) else !any(mg[w]),
                      lapply(seq_len(max(0, m - 3)), function(k) k:(k + 3)))
    changes <- if (reversal) sum(diff(s$role >= 1) != 0 & s$role[-1] %in% c(0, 1) &
                                   s$role[-m] %in% c(0, 1)) else sum(!mg[-1])
    base <- which(s$role == 0)[1]
    if (!length(windows) || is.na(base)) {
      msg <- if (is.na(base)) "no baseline phase; the design starts with a baseline"
             else if (reversal) sprintf("%d baseline/intervention phase change(s); four alternating phases (three changes) are required", changes)
             else sprintf("%d phase/criterion change(s); three are required", changes)
      return(list(level = 0L, reasons = msg, reversibility = .wwc_empty_reversibility()))
    }
    trend <- .wwc_trend(s)
    scored <- lapply(windows, function(w) {
      lvl <- .wwc_window_level(s, w)
      rev <- .wwc_reversibility(s, w)
      trend_fail <- lvl == 2L && isFALSE(trend$met)
      rev_fail <- lvl == 2L && any(rev$nap > 0.85, na.rm = TRUE)
      idx <- unique(c(w, stats::na.omit(base)))
      why <- c(
        sprintf("phases %d-%d: %s", w[1], w[4], paste0(s$labels[w], "(", s$n[w], ")", collapse = " ")),
        switch(lvl + 1L,
               "fewer than three data points in a phase",
               "initial baseline < 6 or another phase < 5 data points",
               "initial baseline >= 6 and other phases >= 5 data points"),
        if (lvl == 2L) .wwc_zero_note(s, idx, ifelse(idx %in% base, 6, 5)),
        if (trend_fail) "baseline trend NAP > .85 caps the rating at With Reservations",
        if (rev_fail) "incomplete reversibility (NAP > .85) caps the rating at With Reservations")
      list(level = lvl - as.integer(trend_fail || rev_fail), reasons = paste(why, collapse = "; "),
           reversibility = rev)
    })
    scored[[which.max(vapply(scored, `[[`, integer(1), "level"))]]
  })
  list(experiments = data.frame(experiment = names(series),
                                level = vapply(rows, `[[`, integer(1), "level"),
                                reasons = vapply(rows, `[[`, character(1), "reasons"),
                                stringsAsFactors = FALSE, row.names = NULL),
       reversibility = do.call(rbind, c(list(.wwc_empty_reversibility()),
                                        lapply(rows, `[[`, "reversibility"))))
}

#' @noRd
.wwc_ab_tier <- function(s) {
  ok <- length(s$labels) >= 2 && isTRUE(s$role[1] == 0)
  base_n <- s$n[1]
  tx_n <- if (ok) s$n[2] else 0L
  trend <- .wwc_trend(s)
  without <- ok && (base_n >= 6 || s$zero[1]) && (tx_n >= 5 || s$zero[2])
  list(ok = ok, onset = if (ok) s$start[2] else NA_real_,
       points = if (without) 2L else if (ok && base_n >= 3 && tx_n >= 3) 1L else 0L,
       trend = isTRUE(trend$met),
       zero = if (without) .wwc_zero_note(s, 1:2, c(6, 5)))
}

#' @noRd
.wwc_concurrence <- function(series, ids, info, probe) {
  onset <- vapply(info[ids], `[[`, numeric(1), "onset")
  observed <- function(id) series[[id]]$time[!is.na(series[[id]]$y)]
  base_times <- function(id) {
    s <- series[[id]]
    s$time[s$run == 1 & !is.na(s$y)]
  }
  grid <- expand.grid(k = seq_along(ids), j = seq_along(ids))
  grid <- grid[onset[grid$j] > onset[grid$k], , drop = FALSE]
  pairs <- Map(c, grid$k, grid$j)
  late <- ids[!vapply(ids, function(id) any(base_times(id) < min(onset)), logical(1))]
  c(
    if (length(late)) sprintf("%s ha%s no baseline data before the first intervention onset (t = %s)",
                              paste(late, collapse = ", "), if (length(late) > 1) "ve" else "s",
                              format(min(onset))),
    if (!probe && !all(vapply(pairs, function(p) any(base_times(ids[p[2]]) >= onset[p[1]]), logical(1))))
      "a tier still in baseline has no data at or after an earlier tier's onset",
    if (probe && !all(vapply(pairs, function(p) onset[p[1]] %in% observed(ids[p[2]]), logical(1))))
      "an untreated tier has no probe in the session another tier starts intervention")
}

#' @noRd
.wwc_mbd_checks <- function(series, ids, info, level, probe) {
  onset <- vapply(info[ids], `[[`, numeric(1), "onset")
  sessions <- sort(unique(unlist(lapply(series[ids], function(s) s$time[!is.na(s$y)]))))
  observed <- function(id) series[[id]]$time[!is.na(series[[id]]$y)]
  points <- vapply(info[ids], `[[`, integer(1), "points")
  trend_ok <- vapply(info[ids], `[[`, logical(1), "trend")
  prior_three <- function(j) utils::tail(sessions[sessions < onset[j]], 3)
  c(
    if (length(ids) < 3) sprintf("%d tier(s) with a baseline-to-intervention change; three (six phases) are required", length(ids)),
    if (length(unique(onset)) < 3) sprintf("intervention starts at %d distinct time(s); three are required", length(unique(onset))),
    if (any(points < level)) sprintf("data points below the %s threshold in %s",
                                     if (level == 2L) "Without Reservations (baseline >= 6, intervention >= 5)"
                                     else "With Reservations (>= 3 per phase)",
                                     paste(ids[points < level], collapse = ", ")),
    if (level == 2L && any(!trend_ok)) sprintf("baseline trend NAP > .85 or not assessable in %s",
                                               paste(ids[!trend_ok], collapse = ", ")),
    if (probe && level == 2L && !all(vapply(ids, function(id) all(utils::head(sessions, 3) %in% observed(id)), logical(1))))
      "not every tier has probes in all of the first three sessions",
    if (probe && level == 1L && !any(vapply(utils::head(sessions, 3), function(t)
      all(vapply(ids, function(id) t %in% observed(id), logical(1))), logical(1))))
      "no session among the first three has probes for all tiers",
    if (probe && level == 2L && !all(vapply(seq_along(ids), function(j)
      length(prior_three(j)) == 3 && all(prior_three(j) %in% observed(ids[j])), logical(1))))
      "not every tier has three consecutive probes just before its onset",
    if (probe && level == 1L && !all(vapply(seq_along(ids), function(j)
      any(prior_three(j) %in% observed(ids[j])), logical(1))))
      "a tier has no probe within the three sessions before its onset")
}

#' @noRd
.wwc_mbd <- function(series, probe) {
  info <- lapply(series, .wwc_ab_tier)
  exps <- split(names(series), vapply(series, `[[`, character(1), "experiment"))
  rows <- lapply(exps, function(ids) {
    ab <- ids[vapply(info[ids], `[[`, logical(1), "ok")]
    # concurrence is judged on the whole design: a failing design is Does Not Meet
    conc <- if (length(ab) >= 2) .wwc_concurrence(series, ab, info, probe) else character(0)
    subsets <- if (length(ab) < 3 || length(conc)) list() else if (length(ab) > 12) list(ab) else
      unlist(lapply(rev(seq(3, length(ab))), function(k) utils::combn(ab, k, simplify = FALSE)),
             recursive = FALSE)
    passes <- function(level) Find(function(ids) !length(.wwc_mbd_checks(series, ids, info, level, probe)),
                                   subsets)
    top <- passes(2L)
    reserved <- if (is.null(top)) passes(1L)
    level <- if (!is.null(top)) 2L else if (!is.null(reserved)) 1L else 0L
    target <- c(top, reserved)
    conc_msg <- if (length(conc)) paste("concurrence not met:", paste(conc, collapse = "; "))
    why <- if (!length(ab)) {
      "no tier has a baseline phase followed by an intervention phase"
    } else if (level == 2L) {
      c(sprintf("tiers %s: staggered onsets, first baselines >= 6 and intervention phases >= 5 data points, concurrence and baseline trend met",
                paste(target, collapse = ", ")),
        unlist(lapply(target, function(id) {
          z <- info[[id]]$zero
          if (length(z)) sprintf("%s: %s", id, z)
        })))
    } else if (level == 1L) {
      c(sprintf("tiers %s meet the With Reservations requirements, including concurrence",
                paste(target, collapse = ", ")),
        .wwc_mbd_checks(series, ab, info, 2L, probe))
    } else {
      c(conc_msg,
        .wwc_mbd_checks(series, ab, info, 1L, probe))
    }
    list(level = level, reasons = paste(why, collapse = "; "))
  })
  list(experiments = data.frame(experiment = names(exps),
                                level = vapply(rows, `[[`, integer(1), "level"),
                                reasons = vapply(rows, `[[`, character(1), "reasons"),
                                stringsAsFactors = FALSE, row.names = NULL),
       reversibility = .wwc_empty_reversibility())
}

#' @noRd
.wwc_keep_runs <- function(s, m) {
  keep <- s$run <= m
  runs <- seq_len(min(m, length(s$labels)))
  utils::modifyList(s, list(rows = s$rows[keep], time = s$time[keep], phase = s$phase[keep],
                            y = s$y[keep], run = s$run[keep], labels = s$labels[runs],
                            role = s$role[runs], n = s$n[runs], ys = s$ys[runs],
                            zero = s$zero[runs], start = s$start[runs]))
}

#' @noRd
.wwc_first_pair <- function(s) .wwc_keep_runs(s, 2L)

#' @noRd
.wwc_combination <- function(series, exps) {
  # tiers of a reversal or changing-criterion design also form a multiple baseline across their first A-B phases
  mbd <- .wwc_mbd(lapply(series, .wwc_first_pair), probe = FALSE)$experiments
  study <- vapply(series, `[[`, character(1), "experiment")
  own_best <- vapply(mbd$experiment, function(e)
    suppressWarnings(max(exps$level[exps$experiment %in% names(series)[study == e]], na.rm = TRUE)),
    numeric(1))
  better <- mbd[!is.na(mbd$level) & mbd$level > own_best, , drop = FALSE]
  if (!nrow(better)) return(exps)
  better$reasons <- paste("combination design rated as a multiple baseline across the first",
                          "baseline-intervention phases of each tier (the higher rating);", better$reasons)
  better$experiment <- paste(better$experiment, "(multiple baseline across tiers)")
  rbind(exps, better)
}

#' @noRd
.atd_lead <- function(s) {
  # a first baseline-role run of more than two sessions is the lead phase even when the
  # baseline label recurs as a condition of the alternation, as in detect_design_type()
  isTRUE(phase_role(s$phase[1]) == 0) && length(s$labels) > 1 && sum(s$run == 1) > 2
}

#' @noRd
.atd_post <- function(s) {
  # trailing maintenance or generalization runs that never alternate sit outside the comparison
  tail_role <- rev(cumprod(rev(phase_role(s$labels) %in% c(2, 3)))) == 1
  post <- rev(cumprod(rev(tail_role & !s$labels %in% s$labels[!tail_role]))) == 1
  if (all(post)) post[] <- FALSE
  post
}

#' @noRd
.atd_trim <- function(s) .wwc_keep_runs(s, sum(!.atd_post(s)))

#' @noRd
.atd_final <- function(s) {
  # only the most successful condition, baseline included in the ranking, may continue as a final phase
  m <- length(s$labels)
  role <- phase_role(s$labels[m])
  # single-letter labels C-H name further interventions in A-B-C designs; free text such as "Ctl" stays unknown
  if (is.na(role) && grepl("^[C-H][0-9]*$", s$labels[m])) role <- 1
  if (m <= 2 || sum(s$run == m) <= 2 || is.na(role) || role == 0) return(FALSE)
  lead <- .atd_lead(s)
  alt <- setdiff(seq_len(m - 1), if (lead) 1L)
  # a lead baseline absent from the alternation enters the ranking with its own runs
  ranked <- if (lead && !s$labels[1] %in% s$labels[alt]) seq_len(m - 1) else alt
  means <- vapply(split(unlist(s$ys[ranked]), rep(s$labels[ranked], lengths(s$ys[ranked]))),
                  mean, numeric(1))
  score <- if (identical(s$goal, "decrease")) -means else means
  final <- s$labels[m]
  tol <- sqrt(.Machine$double.eps) * max(1, abs(means))
  length(score) >= 2 && final %in% names(score) &&
    all(score[[final]] - score[names(score) != final] > tol)
}

#' @noRd
.atd_phase <- function(s) {
  # an alternating phase is one phase however often its conditions switch; lead, final and post runs are their own
  t <- .atd_trim(s)
  m <- length(t$labels)
  ifelse(s$run > m, s$run + 2L,
         pmin(s$run, 1L + .atd_lead(t)) + (.atd_final(t) & s$run == m))
}

#' @noRd
.wwc_atd <- function(series) {
  rows <- lapply(series, function(s) {
    post <- s$labels[.atd_post(s)]
    post_note <- if (length(post)) paste0("; final ", paste(post, collapse = ", "),
                                          " phase outside the alternating comparison") else ""
    s <- .atd_trim(s)
    lead <- .atd_lead(s)
    alt <- if (lead) seq_along(s$labels)[-1] else seq_along(s$labels)
    # points and runs count only inside the alternating phase, not in a final phase
    alt <- if (.atd_final(s)) utils::head(alt, -1) else alt
    over <- vapply(split(s$n[alt], s$labels[alt]), function(v) any(v > 2), logical(1))
    conds <- unique(s$labels[alt])
    if (length(conds) < 2) {
      return(data.frame(experiment = s$tier, level = 0L,
                        reasons = paste0("fewer than two alternating conditions", post_note),
                        stringsAsFactors = FALSE))
    }
    counts <- vapply(conds, function(cn) sum(s$n[alt][s$labels[alt] == cn]), numeric(1))
    contrast <- function(a, b, n_a, n_b, contiguous, note = NULL) {
      n_min <- min(n_a, n_b)
      level <- if (contiguous) 0L else if (n_min >= 5) 2L else if (n_min >= 4) 1L else 0L
      data.frame(experiment = sprintf("%s: %s vs %s", s$tier, a, b), level = level,
                 reasons = paste(c(sprintf("%s(%d) %s(%d) data points", a, n_a, b, n_b), note,
                                   if (contiguous) "more than two consecutive points of one condition during alternation"
                                   else "at most two consecutive points per condition",
                                   "risk-of-bias rules do not apply to alternating treatments"),
                                 collapse = "; "),
                 stringsAsFactors = FALSE)
    }
    pairs <- lapply(utils::combn(conds, 2, simplify = FALSE), function(p)
      contrast(p[1], p[2], counts[[p[1]]], counts[[p[2]]], any(over[intersect(p, names(over))])))
    # a lead baseline absent from the alternation is compared with each intervention condition
    lead_vs <- if (lead && !s$labels[1] %in% conds) conds[!phase_role(conds) %in% 0] else character(0)
    lead_rows <- lapply(lead_vs, function(b)
      contrast(sprintf("%s (initial baseline)", s$labels[1]), b, s$n[1], counts[[b]],
               any(over[intersect(b, names(over))]),
               sprintf("initial baseline compared with %s during the alternating phase", b)))
    out <- do.call(rbind, c(pairs, lead_rows))
    out$reasons <- paste0(out$reasons, post_note)
    out
  })
  list(experiments = do.call(rbind, unname(rows)), reversibility = .wwc_empty_reversibility())
}

#' @noRd
.wwc_tier_table <- function(series) {
  do.call(rbind, lapply(unname(series), function(s) {
    data.frame(experiment = s$experiment, tier = s$tier,
               phases = paste0(s$labels, "(", s$n, ")", collapse = " "),
               n_phases = length(s$labels),
               first_baseline_n = if (any(s$role == 0)) s$n[which(s$role == 0)[1]] else NA_integer_,
               min_phase_n = min(s$n),
               zero_variability_phases = sum(s$zero),
               onset = if (length(s$start) >= 2) s$start[2] else NA_real_,
               phase_changes = length(s$labels) - 1L,
               stringsAsFactors = FALSE)
  }))
}

#' @noRd
.wwc_trend_table <- function(series, design) {
  do.call(rbind, lapply(unname(series), function(s) {
    tr <- if (design == "alternating") list(n = s$n[1], nap = NA_real_,
                                            status = "exempt (alternating treatments)")
          else .wwc_trend(s)
    data.frame(experiment = s$experiment, tier = s$tier, n = tr$n, nap = tr$nap,
               status = tr$status, stringsAsFactors = FALSE)
  }))
}

#' @noRd
.wwc_ioa <- function(data, series, outcome_var, ioa_value, ioa_coverage, ioa_type,
                     design = "") {
  cols <- grep("ioa|agreement|kappa|reliab", names(data), ignore.case = TRUE, value = TRUE)
  cols <- setdiff(cols[vapply(cols, function(v) is.numeric(data[[v]]), logical(1))], outcome_var)
  if (is.null(ioa_value) && !length(cols)) {
    return(list(status = "not assessed",
                detail = "no IOA column or ioa_value supplied; check IOA >= .80 (kappa >= .60) on >= 20% of data points in every phase"))
  }
  kappa <- ioa_type == "kappa" || (length(cols) && grepl("kappa", cols[1], ignore.case = TRUE))
  minimum <- if (kappa) 0.60 else 0.80
  scaled <- function(v) ifelse(v > 1, v / 100, v)
  observed <- !is.na(data[[outcome_var]])
  v <- if (length(cols)) data[[cols[1]]] else NULL
  value <- if (!is.null(ioa_value)) scaled(ioa_value) else mean(scaled(v[observed]), na.rm = TRUE)
  # an IOA value alone says nothing about coverage, which then stays unassessed
  coverage <- if (!is.null(ioa_coverage)) scaled(ioa_coverage)
              else if (is.null(v)) NA_real_ else mean(!is.na(v[observed]))
  if (isTRUE(coverage < 0 || coverage > 1)) {
    stop("ioa_coverage must be a proportion (0-1) or a percentage (0-100).", call. = FALSE)
  }
  coverage_known <- !is.na(coverage)
  # the every-phase rule needs the column as the value source; an ioa_value argument has no phases
  phase_gaps <- if (is.null(v) || !is.null(ioa_value)) 0L else sum(vapply(series, function(s) {
    phase <- if (design == "alternating") .atd_phase(s) else s$run
    has <- tapply(!is.na(v[s$rows]) & !is.na(s$y), phase, any)
    sum(!has)
  }, integer(1)))
  fails <- c(if (!is.finite(value) || value < minimum)
               sprintf("mean IOA %.2f below the %.2f minimum", value, minimum),
             if (coverage_known && coverage < 0.20)
               sprintf("IOA on %.0f%% of data points (20%% required)", 100 * coverage),
             if (phase_gaps > 0) sprintf("%d phase(s) without IOA data", phase_gaps))
  value_text <- sprintf("mean IOA %.2f (%s minimum %.2f)", value,
                        if (kappa) "kappa" else "percentage", minimum)
  list(status = if (length(fails)) "not met" else if (coverage_known) "met"
                else "partially assessed",
       detail = if (length(fails)) paste(fails, collapse = "; ")
                else if (coverage_known) sprintf("%s on %.0f%% of data points", value_text, 100 * coverage)
                else paste0(value_text, "; coverage not assessed: check IOA on >= 20% of ",
                            "data points in every phase"),
       value = value, coverage = coverage, minimum = minimum,
       source = if (!is.null(ioa_value)) "arguments" else cols[1])
}
