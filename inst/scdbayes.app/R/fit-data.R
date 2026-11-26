prepare_fit_data <- function(data, outcome, family, moderators = character(0),
                             center = FALSE, scale = FALSE, exposure = NULL,
                             subset_var = NULL, subset_values = character(0),
                             outcome_units = "count") {
  selected <- length(subset_var) == 1 && subset_var %in% names(data) && length(subset_values) > 0
  d <- if (selected) data[as.character(data[[subset_var]]) %in% subset_values, , drop = FALSE] else data
  mods <- intersect(moderators, names(d))
  mods <- mods[vapply(d[mods], is.numeric, logical(1))]
  constants <- lapply(d[mods], function(x) {
    shift <- if (center) mean(x, na.rm = TRUE) else 0
    spread <- if (scale) stats::sd(x, na.rm = TRUE) else 1
    list(center = shift, scale = if (is.finite(spread) && spread > 0) spread else 1)
  })
  d[mods] <- Map(function(x, p) (x - p$center) / p$scale, d[mods], constants)
  y <- d[[outcome]]
  counts <- family %in% c("poisson", "negbinomial", "zero_inflated_poisson", "zero_inflated_negbinomial")
  if (counts && !outcome_units %in% c("count", "rate")) stop("Select count or rate outcome units.")
  multiply <- counts && identical(outcome_units, "rate")
  valid_exposure <- length(exposure) == 1 && exposure %in% names(d)
  if (multiply && !valid_exposure) stop("Rate outcomes require an exposure column.")
  if (multiply && any(!is.finite(d[[exposure]]) | d[[exposure]] <= 0)) stop("Rate outcomes require finite positive exposure.")
  if (counts && !multiply && any(y != floor(y), na.rm = TRUE)) stop("Count outcomes must be integer values; select rate units for rates.")
  if (multiply) y <- round(y * d[[exposure]])
  rounded <- counts || identical(family, "cumulative")
  if (rounded) y <- round(y)
  beta_n <- NULL
  if (identical(family, "beta")) {
    if (any(y < 0 | y > 1, na.rm = TRUE)) stop("Beta outcomes must be between 0 and 1.")
    beta_n <- if (any(y == 0 | y == 1, na.rm = TRUE)) sum(!is.na(y)) else NULL
    if (!is.null(beta_n)) y <- (y * (beta_n - 1) + 0.5) / beta_n
  }
  d[[outcome]] <- y
  list(data = d, outcome = outcome, moderators = constants, beta_n = beta_n,
       exposure = if (multiply) exposure else NULL, rounded = rounded)
}

fit_transform_code <- function(prep, variable = "data") {
  if (is.null(prep)) return("")
  column <- function(nm) sprintf("%s[[%s]]", variable, deparse(nm))
  mods <- vapply(names(prep$moderators), function(nm) {
    p <- prep$moderators[[nm]]
    sprintf("%1$s <- (%1$s - %2$s) / %3$s\n", column(nm),
            sprintf("%.17g", p$center), sprintf("%.17g", p$scale))
  }, character(1))
  y <- column(prep$outcome)
  paste0(c(mods,
           if (!is.null(prep$exposure)) sprintf("%1$s <- round(%1$s * %2$s)\n", y, column(prep$exposure)),
           if (isTRUE(prep$rounded)) sprintf("%1$s <- round(%1$s)\n", y),
           if (!is.null(prep$beta_n)) sprintf("%1$s <- (%1$s * (%2$d - 1) + 0.5) / %2$d\n", y, prep$beta_n)),
         collapse = "")
}

forest_scale_text <- function(scale, link, goal) {
  if (identical(scale, "response")) return("Response-scale benefit difference; positive = benefit")
  ratio <- if (identical(goal, "decrease")) "exp(-estimate)" else "exp(estimate)"
  if (link %in% c("log", "logit"))
    return(sprintf("Link-scale benefit; %s gives the native treatment/control conditional ratio", ratio))
  "Link-scale benefit; positive = benefit"
}
