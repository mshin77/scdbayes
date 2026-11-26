#' Consolidate binary phase indicator columns into a single phase column
#'
#' @param data A data frame with binary phase indicator columns (A1, B1, etc.).
#'
#' @return The data frame with a single phase column.
#'
#' @examples
#' d <- data.frame(
#'   A1 = c(1, 1, 0, 0),
#'   B1 = c(0, 0, 1, 1),
#'   outcome = rnorm(4)
#' )
#' consolidate_phase_indicators(d)
#'
#' @export
consolidate_phase_indicators <- function(data) {
  phase_patterns <- c("A1", "B1", "A2", "B2", "A3", "B3", "M",
                      "generalization", "mixed_problem")
  indicator_cols <- intersect(phase_patterns, names(data))
  if (length(indicator_cols) < 2) return(data)

  is_binary <- vapply(indicator_cols, function(col) {
    all(data[[col]] %in% c(0, 1, NA))
  }, logical(1))
  indicator_cols <- indicator_cols[is_binary]
  if (length(indicator_cols) < 2) return(data)

  if ("phase_plot" %in% names(data) && !"phase" %in% names(data)) {
    data$phase <- data$phase_plot
    return(data)
  }

  data$phase <- apply(data[, indicator_cols, drop = FALSE], 1, function(row) {
    active <- names(row)[which(row == 1)]
    if (length(active) == 1) active else if (length(active) > 1) active[1] else NA_character_
  })
  data
}

#' Build piecewise regression coding for single-case design phases
#'
#' Adds level, trend, and slope-change columns suitable for a piecewise
#' multilevel model with time-series intervention coding inside a three-level
#' meta-analytic structure. Every change point is coded within each tier (the
#' combination of the columns returned by [tier_vars()]) at that tier's own
#' onset, using the actual time values, so unequally spaced sessions and
#' staggered onsets are honored.
#'
#' @details
#' \strong{AB, multiple baseline, multiple probe.} \code{time_A = t - T_A} on
#' every row, where \code{T_A} is the first baseline time of the tier, so the
#' intercept is the expected level at the start of baseline and
#' \code{time_A} carries the baseline trend through all phases.
#' \code{level_AB} is 1 from the first intervention time \code{T_B} on and
#' \code{trend_AB = (t - T_B) * level_AB}, so \code{level_AB} is the
#' immediate change at \code{T_B} relative to the projected baseline line and
#' \code{trend_AB} the change in slope. Maintenance and
#' generalization phases add \code{level_BM}/\code{trend_BM} and
#' \code{level_BG}/\code{trend_BG} coded the same way from their own onsets:
#' the first maintenance-role time and the first generalization-role time,
#' so an A-B-G tier carries no maintenance step and \code{level_BG} is the
#' change at the generalization onset.
#' \code{time_B}, \code{time_M}, \code{time_G} hold \code{t} minus the
#' corresponding onset on every row. Phases absent from a tier are coded 0.
#'
#' \strong{Reversal (ABAB, BAB, ABA, ABABAB).} Labels with a known role (see
#' [phase_role()], including numeric codes 0/1/2/3 and numbered labels such
#' as \code{"Baseline 1"} or \code{"A_2"}) are renamed to the role letters
#' A, B, M, G; other labels keep their letters. Runs are then numbered by
#' occurrence within the tier (A1, B1, A2, B2, M1). The first-run time
#' (\code{time_<first run>}) is \code{t - T_1} on every row of the tier. Each
#' transition into run \code{k} adds a cumulative step
#' \code{level_<prev><next> = 1[t >= T_k]} (step coding) and a continuing
#' slope change \code{trend_<prev><next> = (t - T_k) * 1[t >= T_k]}. This is
#' the centered ABAB model: each level coefficient
#' is the change between adjacent phases at the first observation of the
#' later phase, and each trend coefficient the change in slope between
#' adjacent phases. When tiers start in different phases (an ABAB tier next
#' to a BAB tier), every first-run time column is kept (0 on the other
#' tiers) and a warning notes that the onset steps differ across tiers and
#' are estimated separately.
#'
#' \strong{Changing criterion.} \code{time_A = t - T_A} on every row. Each
#' criterion subphase \code{j} (split on a numeric \code{criterion},
#' \code{crit}, \code{criterion_level}, or \code{criterion_value} column when
#' present, otherwise on phase-label runs) adds a cumulative step
#' \code{level_crit<j> = 1[t >= T_cj]} and a continuing
#' \code{trend_crit<j> = (t - T_cj) * level_crit<j>}, so each step compares
#' adjacent subphases. A return to an earlier criterion opens a new
#' subphase. A return to baseline after a criterion subphase adds the step
#' \code{level_ret<k> = 1[t >= T_rk]}. Without any baseline phase,
#' \code{time_A} runs from the first session of the tier and
#' \code{level_crit1}/\code{trend_crit1} are dropped with a warning, since
#' they would equal the intercept and \code{time_A}. The collapsed
#' \code{treatment} (intervention vs baseline) column
#' is an intervention indicator; \code{time_treatment = (t - T_c1) *
#' treatment} is a centered variant of an uncentered session-number time
#' variable.
#'
#' \strong{Alternating treatments.} The first baseline-role condition (else
#' the first condition) is the reference; every other condition, including a
#' second baseline-role label such as \code{"Control"}, gets a dummy
#' \code{atd_<condition>}. The \code{atd_} prefix keeps data columns such as
#' \code{treatment_fidelity} out of the condition terms. A general session
#' trend \code{time_session = t - T_1}, the session
#' time centered at the first session of the tier; centering at the first
#' session only shifts the intercept. Without it, a general trend loads on the
#' condition dummies whenever conditions are unevenly spread over time.
#' Alternation blocks are counted within each tier.
#'
#' The time column is \code{structure$time_vars[1]} when supplied, otherwise
#' [find_time_var()] (case-insensitive); with neither, sessions are numbered
#' in row order within each tier, with a warning that concurrence across
#' tiers cannot be assessed.
#'
#' @param data A data frame with a phase column.
#' @param structure A list describing the hierarchical structure.
#' @param scd_type Design type: "reversal", "multiple_baseline",
#'   "multiple_probe", "alternating" (alias "alternating_treatment"),
#'   "changing_criterion", or "AB".
#'
#' @return The data frame with piecewise regression columns added, rows in
#'   the input order.
#'
#' @examples
#' d <- simulate_scd_data(n_cases = 2, n_phase_a = 5, n_phase_b = 5)
#' process_scd_phases(d, structure = list(), scd_type = "AB")
#'
#' @export
process_scd_phases <- function(data, structure, scd_type = "reversal") {
  if (!"phase" %in% names(data)) return(data)

  time_var <- if (length(structure$time_vars)) structure$time_vars[1] else
    find_time_var(data) %||% "time"
  if (!time_var %in% names(data)) {
    data$time <- .within_tier_index(data)
    time_var <- "time"
  }
  data <- .drop_na_phase(data, time_var)

  data <- switch(scd_type,
    reversal = .process_reversal_design(data, structure, time_var),
    multiple_baseline = .process_multiple_baseline(data, structure, time_var),
    multiple_probe = .process_multiple_baseline(data, structure, time_var),
    alternating = ,
    alternating_treatment = .process_alternating_treatment(data, structure, time_var),
    changing_criterion = .process_changing_criterion(data, structure, time_var),
    .process_ab(data, structure, time_var)
  )

  return(data)
}

#' @noRd
.within_tier_index <- function(data) {
  warning("No time column: sessions are numbered in row order within each tier, so ",
          "concurrence across tiers cannot be assessed.", call. = FALSE)
  key <- .std_key(data, tier_vars(data, NULL))
  stats::ave(seq_len(nrow(data)), key, FUN = seq_along)
}

#' @noRd
.is_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

#' @noRd
.na_phase_tiers <- function(data, time_var = find_time_var(data)) {
  na_phase <- .is_blank(data[[find_phase_var(data) %||% "phase"]])
  tiers <- unique(.std_key(data, tier_vars(data, time_var))[na_phase])
  list(rows = na_phase, n = sum(na_phase), tiers = tiers)
}

#' @noRd
.drop_na_phase <- function(data, time_var) {
  miss <- .na_phase_tiers(data, time_var)
  if (!miss$n) return(data)
  warning(sprintf("Dropped %d row(s) with a missing phase label (tier: %s).",
                  miss$n, paste(miss$tiers, collapse = ", ")), call. = FALSE)
  data[!miss$rows, , drop = FALSE]
}

#' @noRd
.norm_label <- function(phase) {
  sub("([a-z])[[:space:]_.-]*[0-9]+$", "\\1", tolower(trimws(as.character(phase))))
}

#' Design role of phase labels
#'
#' Maps phase labels to their design role after lower-casing, trimming, and
#' stripping a trailing occurrence number together with an optional
#' separator, so \code{"Baseline 1"}, \code{"A_1"}, \code{"A-1"},
#' \code{"A.1"}, and \code{"A1"} all read as baseline.
#'
#' @param labels Character, factor, or numeric vector of phase labels.
#' @return Numeric vector of the same length: 0 for the baseline role
#'   (\code{baseline}, \code{a}, \code{bl}, \code{bsl}, \code{pre}, \code{0},
#'   \code{withdrawal}, \code{return}, \code{reversal}, \code{control},
#'   \code{wait}, \code{waitlist}, \code{wl}), 1 for intervention (\code{b},
#'   \code{treatment}, \code{treat}, \code{tx}, \code{trt},
#'   \code{intervention}, \code{reinstatement}, \code{post}, \code{1}), 2 for
#'   maintenance (\code{m}, \code{maintenance}, \code{2}), 3 for
#'   generalization (\code{g}, \code{generalization}, \code{gen}, \code{3}),
#'   and \code{NA} for any other label. \code{probe} has no fixed role: a
#'   probe can be a baseline, maintenance, or generalization session.
#' @examples
#' phase_role(c("Baseline 1", "Treatment 1", "A_2", "B-2", "M", 0, "Tx", "Probe"))
#' @export
phase_role <- function(labels) {
  p <- .norm_label(labels)
  dplyr::case_when(
    p %in% c("a", "baseline", "bl", "bsl", "pre", "0", "withdrawal", "return",
             "reversal", "control", "wait", "waitlist", "wl") ~ 0,
    p %in% c("b", "treatment", "treat", "tx", "trt", "intervention", "post", "1",
             "reinstatement") ~ 1,
    p %in% c("m", "maintenance", "2") ~ 2,
    p %in% c("g", "generalization", "gen", "3") ~ 3,
    TRUE ~ NA_real_
  )
}

#' @noRd
.phase_numeric <- function(phase, time, warn = FALSE) {
  p <- .norm_label(phase)
  # unrecognized labels follow first appearance in time, not alphabetical order
  first_seen <- stats::na.omit(unique(p[order(time)]))
  role <- phase_role(phase)
  if (warn) .warn_inferred_roles(phase, time)
  dplyr::coalesce(role, as.numeric(match(p, first_seen) - 1))
}

#' @noRd
.warn_inferred_roles <- function(phase, time = seq_along(phase)) {
  unknown <- unique(as.character(phase[is.na(phase_role(phase)) & !is.na(phase)]))
  if (!length(unknown)) return(invisible(NULL))
  p <- .norm_label(phase)
  first_seen <- stats::na.omit(unique(p[order(time)]))
  inferred <- match(.norm_label(unknown), first_seen) - 1
  as_m <- unknown[inferred %in% 2]
  as_g <- unknown[inferred %in% 3]
  note <- paste0("",
    if (length(as_m)) sprintf(" %s is treated as maintenance (M).",
                              paste(sQuote(as_m, FALSE), collapse = ", ")),
    if (length(as_g)) sprintf(" %s is treated as generalization (G).",
                              paste(sQuote(as_g, FALSE), collapse = ", ")))
  warning(sprintf(paste0("Phase label(s) %s have no known role; roles are inferred from ",
                         "first appearance in time (first = baseline, next = intervention, ",
                         "third = maintenance, fourth = generalization).%s ",
                         "Rename them (e.g. A/B, Baseline/Treatment) to set the roles."),
                  paste(sQuote(unknown, FALSE), collapse = ", "), note), call. = FALSE)
}

#' @noRd
.role_letter <- function(phase) {
  base <- gsub("[^A-Za-z]", "", sub("[[:space:]_.-]*[0-9]+$", "", trimws(as.character(phase))))
  base <- ifelse(nzchar(base), base, paste0("P", gsub("[^0-9]", "", as.character(phase))))
  role <- phase_role(phase)
  ifelse(is.na(role), base, c("A", "B", "M", "G")[role + 1])
}

#' @noRd
.tier_split <- function(data, structure, time_var) {
  vars <- unique(c(tier_vars(data, time_var),
                   intersect(names(structure$levels), names(data))))
  key <- if (length(vars)) .std_factor(data, vars)
         else factor(rep(1, nrow(data)))
  split(seq_len(nrow(data)), key)
}

#' @noRd
.code_tiers <- function(data, structure, time_var, code_fn) {
  pieces <- lapply(.tier_split(data, structure, time_var), function(idx) {
    idx <- idx[order(data[[time_var]][idx])]
    out <- code_fn(data[idx, , drop = FALSE], as.numeric(data[[time_var]][idx]))
    out$.row <- idx
    out
  })
  out <- dplyr::bind_rows(pieces)
  out <- out[order(out$.row), , drop = FALSE]
  out$.row <- NULL
  rownames(out) <- NULL
  out
}

#' @noRd
.first_time <- function(t, on) {
  on <- on & !is.na(t)
  if (any(on)) min(t[on]) else NA_real_
}

#' @noRd
.step <- function(t, onset) as.integer(!is.na(onset) & t >= onset)

#' @noRd
.drop_constant_pw_cols <- function(data, cols) {
  present <- intersect(cols, names(data))
  constant <- present[vapply(present,
    function(cc) length(unique(data[[cc]][!is.na(data[[cc]])])) <= 1, logical(1))]
  if (length(constant) > 0) data <- data[, setdiff(names(data), constant), drop = FALSE]
  data
}

#' @noRd
.zero_na <- function(x) replace(x, is.na(x), 0)

#' @noRd
.atd_terms <- function(nm) grep("^atd_", nm, value = TRUE)

#' @noRd
.criterion_steps <- function(nm) {
  by_index <- function(x) x[order(as.integer(sub("\\D+", "", x)))]
  c(by_index(grep("^level_crit[0-9]+$", nm, value = TRUE)),
    by_index(grep("^level_ret[0-9]+$", nm, value = TRUE)))
}

#' @noRd
.process_ab <- function(data, structure, time_var) {
  data$phase_numeric <- .phase_numeric(data$phase, data[[time_var]], warn = TRUE)
  code_tier <- function(df, t) {
    pn <- df$phase_numeric
    # probe sessions sit inside a condition, so they never open a phase; the
    # intervention starts at the first intervention-role session (Huitema &
    # McKean, 2000; Moeyaert et al., 2014), not at the first non-baseline label
    probe <- grepl("probe", .norm_label(df$phase))
    onset_B <- .first_time(t, pn == 1 & !probe)
    if (is.na(onset_B)) onset_B <- .first_time(t, pn >= 1 & !probe)
    # an A-B-G tier has no maintenance onset; G alone opens level_BG
    onset <- c(A = .first_time(t, pn == 0), B = onset_B,
               M = .first_time(t, pn == 2 & !probe), G = .first_time(t, pn == 3 & !probe))
    df$time_A <- .zero_na(t - onset[["A"]])
    df$time_B <- .zero_na(t - onset[["B"]])
    df$level_AB <- .step(t, onset[["B"]])
    df$trend_AB <- .zero_na((t - onset[["B"]]) * df$level_AB)
    # a maintenance or generalization condition that is not one final run
    # (e.g. generalization probes between intervention sessions) is coded as a
    # session indicator, so later sessions of another condition are not absorbed
    later_phase <- function(role, onset_t) {
      if (is.na(onset_t)) return(list(level = .step(t, NA), trend = rep(0, length(t))))
      is_role <- pn == role & !is.na(pn)
      after <- !is.na(t) & t >= onset_t & !is.na(pn) & !probe
      # maintenance may be followed by generalization (A-B-M-G), never by A or B
      contiguous <- all(pn[after] >= role)
      if (contiguous) {
        lv <- .step(t, onset_t)
        list(level = lv, trend = .zero_na((t - onset_t) * lv))
      } else {
        list(level = as.integer(is_role), trend = rep(0, length(t)))
      }
    }
    m <- later_phase(2, onset[["M"]])
    g <- later_phase(3, onset[["G"]])
    df$time_M <- .zero_na(t - onset[["M"]])
    df$level_BM <- m$level
    df$trend_BM <- m$trend
    df$time_G <- .zero_na(t - onset[["G"]])
    df$level_BG <- g$level
    df$trend_BG <- g$trend
    df$intervention_start <- onset[["B"]]
    df
  }
  data <- .code_tiers(data, structure, time_var, code_tier)
  # all-zero maintenance/generalization columns would be unidentified predictors
  .drop_constant_pw_cols(data, c("time_M", "level_BM", "trend_BM",
                                 "time_G", "level_BG", "trend_BG"))
}

#' Reversal-design piecewise terms
#'
#' Returns the first-run time terms followed by the per-transition step and
#' trend terms created by the reversal coder (e.g. `level_A1B1`, `level_B1A2`,
#' `level_A2B2`), in column order. Only first-run times enter the model; the
#' continuing `trend_*` terms carry every later slope. A combined design whose
#' tiers start in different phases carries one first-run time per starting
#' phase (e.g. `time_A1` and `time_B1`).
#'
#' @param nm Column names of processed data.
#' @return Character vector of term names; empty when the data carry no
#'   reversal coding.
#' @examples
#' d <- data.frame(case = "P1", time = 1:12, outcome = rnorm(12),
#'                 phase = rep(c("A", "B", "A", "B"), each = 3))
#' reversal_terms(names(process_scd_phases(d, list(), "reversal")))
#' @export
reversal_terms <- function(nm) {
  steps <- grep("^(level|trend)_[A-Za-z]+[0-9]+[A-Za-z]+[0-9]+$", nm, value = TRUE)
  if (!length(steps)) return(character(0))
  c(grep("^time_[A-Za-z]+1$", nm, value = TRUE), steps)
}

#' @noRd
.step_labels <- function(term) {
  term <- sub("^b_", "", term)
  parts <- regmatches(term, regexec("^(level|trend)_([A-Za-z]+[0-9]+)([A-Za-z]+[0-9]+)$",
                                    term))[[1]]
  if (length(parts) < 4) NULL else parts[3:4]
}

#' @noRd
.run_roles <- function(nm) {
  terms <- reversal_terms(sub("^b_", "", nm))
  runs <- unique(c(sub("^time_", "", grep("^time_", terms, value = TRUE)),
                   unlist(lapply(terms, .step_labels))))
  stats::setNames(.phase_numeric(sub("[0-9]+$", "", runs), seq_along(runs)), runs)
}

#' @noRd
.step_roles <- function(term, nm = term) {
  lab <- .step_labels(term)
  if (is.null(lab)) return(c(NA_real_, NA_real_))
  unname(.run_roles(unique(c(nm, sub("^b_", "", term))))[lab])
}

#' @noRd
.reversal_onset <- function(nm) {
  steps <- grep("^level_", reversal_terms(nm), value = TRUE)
  onset <- Filter(function(s) {
    r <- .step_roles(s, nm)
    isTRUE(r[1] == 0 && r[2] >= 1)
  }, steps)
  c(onset, steps)[1]
}

#' @noRd
.process_reversal_design <- function(data, structure, time_var) {
  if (!"phase" %in% names(data)) return(data)

  code_tier <- function(df, t) {
    ph <- .role_letter(df$phase)
    run <- cumsum(c(TRUE, ph[-1] != ph[-length(ph)]))
    first <- !duplicated(run)
    occ <- stats::ave(seq_len(sum(first)), ph[first], FUN = seq_along)
    run_lab <- paste0(ph[first], occ)
    onset <- tapply(t, run, min)
    df[[paste0("time_", run_lab[1])]] <- t - onset[[1]]
    if (length(run_lab) < 2) return(df)
    steps <- paste0(run_lab[-length(run_lab)], run_lab[-1])
    level <- lapply(seq_along(steps) + 1, function(i) as.integer(run >= i))
    trend <- lapply(seq_along(steps) + 1, function(i) (t - onset[[i]]) * (run >= i))
    cols <- stats::setNames(c(level, trend), c(paste0("level_", steps),
                                               paste0("trend_", steps)))
    ord <- as.vector(rbind(seq_along(steps), seq_along(steps) + length(steps)))
    df[names(cols)[ord]] <- cols[ord]
    df
  }
  data <- .code_tiers(data, structure, time_var, code_tier)
  pw_cols <- grep("^(time|level|trend)_[A-Za-z]+[0-9]+", names(data), value = TRUE)
  data[pw_cols] <- lapply(data[pw_cols], .zero_na)
  firsts <- grep("^time_[A-Za-z]+1$", pw_cols, value = TRUE)
  if (length(firsts) > 1) {
    warning(sprintf(paste0("Tiers start in different phases (%s): the onset steps differ ",
                           "across tiers and are estimated as separate coefficients."),
                    paste(sub("^time_", "", firsts), collapse = ", ")), call. = FALSE)
  }
  # all-zero later steps (ABA, incomplete reversals) would be unidentified predictors
  .drop_constant_pw_cols(data, grep("^(level|trend)_", pw_cols, value = TRUE))
}

#' @noRd
.process_multiple_baseline <- function(data, structure, time_var) {
  data <- .process_ab(data, structure, time_var)
  starts <- sort(unique(data$intervention_start[!is.na(data$intervention_start)]))
  data$stagger_group <- match(data$intervention_start, starts)
  data
}

#' @noRd
.process_alternating_treatment <- function(data, structure, time_var) {
  ord <- unlist(lapply(.tier_split(data, structure, time_var),
                       function(idx) idx[order(data[[time_var]][idx])]), use.names = FALSE)
  # conditions in order of first appearance within the earliest tier, then the rest
  unique_conditions <- unique(as.character(data$phase)[ord])
  # one reference condition: the first baseline-role label, else the first condition
  baseline_cond <- unique_conditions[phase_role(unique_conditions) %in% 0]
  reference <- c(baseline_cond, unique_conditions)[1]
  treatment_conds <- setdiff(unique_conditions, reference)

  dummies <- lapply(treatment_conds, function(cond) as.integer(data$phase == cond))
  data[paste0("atd_", gsub("[^a-zA-Z0-9]", "_", treatment_conds))] <- dummies

  .code_tiers(data, structure, time_var, function(df, t) {
    ph <- as.character(df$phase)
    df$alternation_block <- cumsum(c(TRUE, ph[-1] != ph[-length(ph)]))
    df$time_block <- t - stats::ave(t, df$alternation_block, FUN = min)
    df$time_session <- t - min(t)
    df
  })
}

#' @noRd
.criterion_col <- function(data) {
  hit <- grep("^(crit|criterion)(_(level|value))?$", names(data),
              ignore.case = TRUE, value = TRUE)
  hit <- hit[vapply(hit, function(v) is.numeric(data[[v]]), logical(1))]
  if (length(hit)) hit[1] else NULL
}

#' @noRd
.process_changing_criterion <- function(data, structure, time_var) {
  crit <- .criterion_col(data)
  code_tier <- function(df, t) {
    is_bl <- phase_role(df$phase) %in% 0
    ph <- as.character(df$phase)
    cv <- if (is.null(crit)) rep(NA_real_, nrow(df)) else df[[crit]]
    # an unrecorded criterion carries the last one forward within the phase
    last <- stats::ave(ifelse(is.na(cv), 0L, seq_along(cv)),
                       cumsum(c(TRUE, ph[-1] != ph[-length(ph)])), FUN = cummax)
    cv <- cv[replace(last, last == 0L, NA)]
    seg <- paste(is_bl, if (is.null(crit)) ph else cv)
    df$criterion_phase <- cumsum(c(TRUE, seg[-1] != seg[-length(seg)]))
    df$is_baseline <- as.integer(is_bl)
    df$time_in_criterion <- t - stats::ave(t, df$criterion_phase, FUN = min)
    # without a baseline the trend runs from the first session
    bl_start <- .first_time(t, is_bl)
    df$time_A <- .zero_na(t - if (is.na(bl_start)) min(t, na.rm = TRUE) else bl_start)
    starts <- tapply(t[!is_bl], df$criterion_phase[!is_bl], min)
    df$treatment <- as.integer(!is_bl)
    df$time_treatment <- if (length(starts)) (t - starts[[1]]) * df$treatment else 0
    level <- lapply(starts, function(s) .step(t, s))
    trend <- lapply(starts, function(s) (t - s) * .step(t, s))
    j <- seq_along(starts)
    df[c(paste0("level_crit", j), paste0("trend_crit", j))] <- c(level, trend)
    first <- !duplicated(df$criterion_phase)
    seg_bl <- is_bl[first]
    seg_cv <- cv[first]
    n <- length(seg_bl)
    direction <- c(NA_real_, sign(seg_cv[-1] - seg_cv[-n]))
    direction[c(TRUE, seg_bl[-1] | seg_bl[-n])] <- NA_real_
    df[paste0("direction_crit", j)] <- as.list(direction[!seg_bl])
    # a return to baseline after a criterion subphase is its own step
    returns <- tapply(t[is_bl], df$criterion_phase[is_bl], min)
    returns <- returns[returns > min(c(starts, Inf))]
    if (length(returns)) {
      df[paste0("level_ret", seq_along(returns))] <- lapply(returns, function(s) .step(t, s))
    }
    df
  }
  data <- .code_tiers(data, structure, time_var, code_tier)
  crit_cols <- grep("^(level|trend)_(crit|ret)[0-9]+$", names(data), value = TRUE)
  data[crit_cols] <- lapply(data[crit_cols], .zero_na)
  direction_cols <- grep("^direction_crit[0-9]+$", names(data), value = TRUE)
  directions <- lapply(data[direction_cols], function(x) unique(x[is.finite(x) & x != 0]))
  names(directions) <- sub("^direction", "", direction_cols)
  terms <- grep("^(level|trend)_crit[0-9]+$", names(data), value = TRUE)
  data[terms] <- lapply(stats::setNames(terms, terms), function(term) {
    direction <- directions[[sub("^(level|trend)", "", term)]]
    if (!length(direction)) return(data[[term]])
    structure(data[[term]], expected_direction = ifelse(direction > 0, "increase", "decrease"),
              decreasing = all(direction < 0))
  })
  data[direction_cols] <- NULL
  has_bl <- vapply(.tier_split(data, structure, time_var),
                   function(idx) any(phase_role(data$phase[idx]) %in% 0), logical(1))
  if (any(has_bl) && !all(has_bl)) {
    warning(sprintf(paste0("Tier(s) %s have no baseline phase: level_crit1 is constant within ",
                           "them and confounded with their random intercept."),
                    paste(names(has_bl)[!has_bl], collapse = ", ")), call. = FALSE)
  }
  if (!any(phase_role(data$phase) %in% 0) && "level_crit1" %in% names(data)) {
    warning("No baseline phase: the first criterion subphase is the reference, so ",
            "level_crit1 is dropped (it would equal the intercept).", call. = FALSE)
    data$level_crit1 <- NULL
    data$trend_crit1 <- NULL
  }
  data
}
