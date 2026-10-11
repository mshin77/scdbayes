# Allowlist checks for user-typed hypothesis and formula text before brms evaluates it

.is_text_scalar <- function(x, max_len) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x)) &&
    nchar(x) <= max_len
}

.parse_one <- function(x) {
  tryCatch(str2lang(x), error = function(e) NULL)
}

.unnamed <- function(args) {
  is.null(names(args)) || all(!nzchar(names(args)))
}

.is_safe_expr <- function(e, syms, ops, calls = list(), nums = NULL) {
  if (is.numeric(e)) {
    return(length(e) == 1L && is.finite(e) && (is.null(nums) || e %in% nums))
  }
  if (is.symbol(e)) return(as.character(e) %in% syms)
  if (!is.call(e) || !is.symbol(e[[1]])) return(FALSE)
  fn <- as.character(e[[1]])
  args <- as.list(e)[-1]
  if (fn %in% names(calls)) return(isTRUE(calls[[fn]](args, syms)))
  fn %in% ops && length(args) %in% 1:2 && .unnamed(args) &&
    all(vapply(args, .is_safe_expr, logical(1), syms = syms, ops = ops,
               calls = calls, nums = nums))
}

.is_term_arg <- function(a, syms) {
  is.symbol(a) && as.character(a) %in% syms ||
    is.call(a) && identical(a[[1]], as.name(":")) &&
      all(vapply(as.list(a)[-1], .is_term_arg, logical(1), syms = syms))
}

.is_flag_or_term <- function(a, syms) {
  is.logical(a) && length(a) == 1L && !is.na(a) || .is_term_arg(a, syms)
}

.named_args_ok <- function(args, syms, allowed) {
  nm <- names(args)
  !is.null(nm) && all(nm %in% allowed) && !anyDuplicated(nm) &&
    all(vapply(args, .is_flag_or_term, logical(1), syms = syms))
}

.formula_calls <- function() {
  list(
    ar = function(args, syms) {
      length(args) > 0 && .named_args_ok(args, syms, c("time", "gr", "cov"))
    },
    gr = function(args, syms) {
      nm <- names(args) %||% rep("", length(args))
      length(args) > 0 && !nzchar(nm[1]) && .is_term_arg(args[[1]], syms) &&
        (length(args) == 1 ||
           .named_args_ok(args[-1], syms, c("by", "cor", "id", "cov")))
    },
    offset = function(args, syms) {
      a <- if (length(args) == 1 && .unnamed(args)) args[[1]]
      is.call(a) && identical(a[[1]], as.name("log")) && length(a) == 2L &&
        is.symbol(a[[2]]) && as.character(a[[2]]) %in% syms
    },
    trials = function(args, syms) {
      length(args) == 1 && .unnamed(args) &&
        (.is_term_arg(args[[1]], syms) ||
           is.numeric(args[[1]]) && length(args[[1]]) == 1 && is.finite(args[[1]]))
    }
  )
}

#' @noRd
.syntactic <- function(nm) {
  nm <- as.character(nm)
  # ... and ..1, ..2 are reserved symbols that make.names() leaves unchanged
  nm[!is.na(nm) & make.names(nm) == nm & !grepl("^\\.\\.(\\.|[0-9]+)$", nm)]
}

#' @noRd
.check_syntactic <- function(nm, what = "Column names") {
  bad <- setdiff(as.character(nm), .syntactic(nm))
  if (length(bad)) {
    stop(sprintf("%s must be syntactic R names (letters, digits, '.', '_'): %s. Rename with safe_column_names().",
                 what, paste(sprintf("'%s'", substr(bad, 1, 60)), collapse = ", ")), call. = FALSE)
  }
  invisible(nm)
}

#' Rename data columns to syntactic names
#'
#' Column names reach model formulas, where text such as
#' `"x+I(file.create('f'))"` would run as code. Each name is trimmed, every
#' character outside `A-Z a-z 0-9 . _` becomes `.`, the reserved `...` and
#' `..1`, `..2`, ... get an `X` prefix, and [make.names()] with
#' `unique = TRUE` fixes the rest, so every result matches
#' `^[A-Za-z.][A-Za-z0-9._]*$` and parses as one symbol.
#'
#' @param data Data frame.
#' @return `data` with syntactic, unique names. The attribute `"renamed"`
#'   is a character vector of new names named by the original names, for
#'   the changed columns only.
#' @examples
#' d <- safe_column_names(data.frame(`a b` = 1, `x+I(q())` = 2, check.names = FALSE))
#' attr(d, "renamed")
#' @export
safe_column_names <- function(data) {
  old <- names(data) %||% character(0)
  new <- sub("^(\\.\\.(\\.|[0-9]+))$", "X\\1", gsub("[^A-Za-z0-9._]", ".", trimws(old)))
  new <- make.names(new, unique = TRUE)
  names(data) <- new
  changed <- old != new
  attr(data, "renamed") <- stats::setNames(new[changed], old[changed])
  data
}

#' Check that a hypothesis string uses only model parameters and arithmetic
#'
#' `brms::hypothesis()` evaluates its string as R code, so any function call
#' typed into a hypothesis box runs on the server. This check parses each
#' side of a single `=`, `==`, `<`, or `>` relation and accepts only
#' parameter names, numeric constants, and the operators `+ - * / ^ ( :`.
#'
#' @param x Hypothesis string such as `"level_AB > 0"`.
#' @param params Parameter names of the fitted model, for example
#'   `brms::variables(fit)`; names with and without the `b_` prefix are
#'   both accepted.
#' @return Logical scalar.
#' @examples
#' is_safe_hypothesis("level_AB > 0", c("b_Intercept", "b_level_AB"))
#' is_safe_hypothesis("level_AB*0 + q() = 0", "b_level_AB")
#' @export
is_safe_hypothesis <- function(x, params) {
  if (!.is_text_scalar(x, 500L) || !length(params)) return(FALSE)
  sides <- strsplit(x, "==|=|<|>", perl = TRUE)[[1]]
  if (length(sides) != 2L || !all(nzchar(trimws(sides)))) return(FALSE)
  base <- unique(c(params, sub("^b_", "", params)))
  syms <- unique(c(base, unlist(strsplit(base, ":", fixed = TRUE))))
  ops <- c("+", "-", "*", "/", "^", "(", ":")
  all(vapply(sides, function(s) {
    e <- .parse_one(s)
    !is.null(e) && .is_safe_expr(e, syms, ops)
  }, logical(1)))
}

#' Check that a model formula uses only data columns and brms term syntax
#'
#' Formula text typed into the app reaches `brms` and `model.frame()`, which
#' evaluate calls such as `I(q())`. This check accepts data columns, the
#' constants `0` and `1`, the operators `~ + : * | || / - (`, and the brms
#' helpers `ar()` (named `time`, `gr`, `cov` arguments), `gr()`,
#' `trials()`, and the exposure offset `offset(log(<column>))`; every other
#' function call, string, `$`, `@`, or `::` is rejected.
#'
#' @param x Formula string, either a full formula (`"y ~ x + (1 | case)"`)
#'   or a right-hand side.
#' @param allowed_vars Column names of the analysis data.
#' @return Logical scalar.
#' @examples
#' is_safe_formula("y ~ level_AB + (1 | case)", c("y", "level_AB", "case"))
#' is_safe_formula("y ~ I(q())", "y")
#' @export
is_safe_formula <- function(x, allowed_vars) {
  if (!.is_text_scalar(x, 2000L) || !length(allowed_vars)) return(FALSE)
  e <- .parse_one(x)
  ops <- c("~", "+", ":", "*", "|", "||", "/", "-", "(")
  !is.null(e) && .is_safe_expr(e, allowed_vars, ops, .formula_calls(), nums = c(0, 1))
}
