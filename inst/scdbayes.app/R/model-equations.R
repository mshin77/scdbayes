.equation_name <- function(x) {
  chars <- strsplit(as.character(x), "", fixed = TRUE)[[1]]
  escapes <- c("\\" = "\\backslash{}", "_" = "\\_", "%" = "\\%", "&" = "\\&", "#" = "\\#", "$" = "\\$", "{" = "\\{", "}" = "\\}", "^" = "\\textasciicircum{}", "~" = "\\textasciitilde{}", "<" = "\\lt{}", ">" = "\\gt{}")
  paste0("\\mathrm{", paste0(ifelse(chars %in% names(escapes), escapes[chars], chars), collapse = ""), "}")
}

.equation_text <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")

.equation_function <- function(x) {
  if (is.symbol(x)) return(as.character(x))
  if (is.call(x) && identical(x[[1]], as.name("::"))) return(as.character(x[[3]]))
  ""
}

.equation_groups <- function(x) {
  if (!is.call(x)) return(.equation_text(x))
  if (.equation_function(x[[1]]) == "gr") return(.equation_groups(x[[2]]))
  if (identical(x[[1]], as.name("/"))) {
    left <- .equation_groups(x[[2]])
    right <- .equation_groups(x[[3]])
    return(c(left, paste(tail(left, 1), right, sep = ":")))
  }
  if (identical(x[[1]], as.name("("))) return(.equation_groups(x[[2]]))
  .equation_text(x)
}

.equation_fixed <- function(x) {
  if (!is.call(x)) return(x)
  if (.equation_function(x[[1]]) == "ar") return(NULL)
  if (.equation_function(x[[1]]) %in% c("|", "||")) return(NULL)
  args <- lapply(as.list(x)[-1], .equation_fixed)
  if (identical(x[[1]], as.name("(")) && is.null(args[[1]])) return(NULL)
  if (identical(x[[1]], as.name("+")) && any(vapply(args, is.null, FALSE))) {
    retained <- Filter(Negate(is.null), args)
    if (!length(retained)) return(NULL)
    return(retained[[1]])
  }
  as.call(c(list(x[[1]]), args))
}

.equation_bars <- function(x) {
  if (!is.call(x)) return(list())
  if (.equation_function(x[[1]]) %in% c("|", "||")) return(list(x))
  unlist(lapply(as.list(x)[-1], .equation_bars), recursive = FALSE)
}

.equation_calls <- function(x, function_names) {
  if (!is.call(x)) return(list())
  if (.equation_function(x[[1]]) %in% function_names) return(list(x))
  unlist(lapply(as.list(x)[-1], .equation_calls, function_names = function_names), recursive = FALSE)
}

.equation_terms <- function(x, data = NULL) {
  rhs <- .equation_fixed(x)
  if (is.null(rhs)) rhs <- 1
  tt <- stats::terms(stats::as.formula(call("~", rhs)))
  labels <- attr(tt, "term.labels")
  variables <- as.list(attr(tt, "variables"))[-1]
  offsets <- vapply(Filter(function(v) is.call(v) && identical(v[[1]], as.name("offset")), variables), .equation_text, "")
  columns <- if (is.null(data)) labels else setdiff(colnames(stats::model.matrix(tt, data)), "(Intercept)")
  list(terms = labels[!grepl("^offset\\(", labels)], intercept = attr(tt, "intercept") == 1,
       columns = columns[!grepl("^offset\\(", columns)], offsets = offsets)
}

.equation_predictor <- function(parts, coefficient = "beta", intercept = parts$intercept) {
  factors <- vapply(parts$columns, .equation_name, "")
  terms <- c(if (intercept) paste0("\\", coefficient, "_0"),
             if (length(factors)) paste0("\\", coefficient, "_{", seq_along(factors), "}", factors, "_t"))
  if (!length(terms)) return("0")
  paste(terms, collapse = " + ")
}

.equation_offset <- function(x) {
  value <- str2lang(x)[[2]]
  if (is.call(value) && identical(value[[1]], as.name("log")))
    return(paste0("\\log(", .equation_name(.equation_text(value[[2]])), "_t)"))
  paste0(.equation_name(.equation_text(value)), "_t")
}

app_model_equations <- function(formula, family = "gaussian", ar = FALSE,
                                ar_covariance = FALSE, sigma_formula = NULL,
                                exposure = NULL, trials = NULL, data = NULL) {
  autocor <- NULL
  if (inherits(formula, "brmsformula")) {
    if (isTRUE(attr(formula$formula, "nl"))) stop("Unsupported nonlinear formula in equation preview", call. = FALSE)
    if (length(formula$pfix)) stop("Unsupported fixed distributional parameter in equation preview", call. = FALSE)
    if (length(setdiff(names(formula$pforms), "sigma")))
      stop("Unsupported distributional formula in equation preview", call. = FALSE)
    if (is.null(sigma_formula)) sigma_formula <- formula$pforms$sigma
    if (missing(family) && !is.null(formula$family)) family <- formula$family
    autocor <- attr(formula$formula, "autocor")
    formula <- formula$formula
  }
  if (is.character(formula)) formula <- stats::as.formula(formula)
  if (!is.null(autocor) && !length(.equation_calls(autocor, "ar")))
    stop("Unsupported autocorrelation formula in equation preview", call. = FALSE)
  specials <- c("mo", "s", "t2", "gp", "cs", "mi", "me", "mm", "mmc",
                "ma", "arma", "cosy", "unstr", "fcor", "sar", "car")
  if (length(.equation_calls(formula[[3]], specials)) || length(.equation_calls(autocor, specials)))
    stop("Unsupported special term in equation preview", call. = FALSE)
  serial_terms <- c(.equation_calls(formula[[3]], "ar"), .equation_calls(autocor, "ar"))
  if (length(serial_terms)) {
    ar <- TRUE
    serial_args <- as.list(serial_terms[[1]])[-1]
    if (length(serial_terms) != 1L || (!is.null(serial_args$p) && !identical(as.numeric(serial_args$p), 1)))
      stop("Unsupported AR order in equation preview; only AR(1) is represented", call. = FALSE)
    if (!is.null(serial_args$cov)) ar_covariance <- isTRUE(serial_args$cov)
  }
  family_name <- if (is.character(family)) family else family$family
  link <- if (is.character(family)) NULL else family$link
  defaults <- c(gaussian = "identity", student = "identity", skew_normal = "identity", Gamma = "log", poisson = "log", negbinomial = "log", zero_inflated_poisson = "log", zero_inflated_negbinomial = "log", beta = "logit", binomial = "logit", bernoulli = "logit", cumulative = "logit")
  if (!family_name %in% names(defaults)) stop("Unsupported family in equation preview", call. = FALSE)
  if (!is.null(sigma_formula) && !is.character(family) &&
      !is.null(family$link_sigma) && family$link_sigma != "log")
    stop("Unsupported sigma link in equation preview", call. = FALSE)
  if (is.null(link)) link <- unname(defaults[family_name])
  if (!length(link) || is.na(link)) stop("Unsupported family")
  parts <- .equation_terms(formula[[3]], data)
  bars <- .equation_bars(formula[[3]])
  groups <- unlist(lapply(bars, function(b) {
    if (length(.equation_bars(b[[2]]))) stop("Unsupported shared covariance ID in equation preview", call. = FALSE)
    slopes <- .equation_terms(b[[2]], data)
    grouping <- b[[3]]
    args <- if (is.call(grouping) && .equation_function(grouping[[1]]) == "gr") as.list(grouping)[-1] else list()
    if (length(setdiff(names(args), c("", "cor")))) stop("Unsupported grouping option in equation preview", call. = FALSE)
    correlated <- identical(b[[1]], as.name("|")) && !identical(args$cor, FALSE)
    lapply(.equation_groups(b[[3]]), function(g) list(name = g, terms = slopes$terms,
                                                  columns = slopes$columns,
                                                  intercept = slopes$intercept, correlated = correlated))
  }), recursive = FALSE)
  random_terms <- vapply(seq_along(groups), function(k) {
    g <- groups[[k]]
    terms <- c(if (g$intercept) paste0("b_{", k, ",0,g(t)}"),
               if (length(g$columns)) paste0("b_{", k, ",", seq_along(g$columns), ",g(t)}", vapply(g$columns, .equation_name, ""), "_t"))
    paste(terms, collapse = " + ")
  }, "")
  predictor <- .equation_predictor(parts, intercept = parts$intercept && family_name != "cumulative")
  offsets <- vapply(parts$offsets, .equation_offset, "")
  if (length(exposure) && nzchar(exposure) && !length(offsets)) offsets <- paste0("\\log(", .equation_name(exposure), "_t)")
  predictor <- paste(c(predictor, random_terms[nzchar(random_terms)], offsets), collapse = " + ")
  response <- formula[[2]]
  if (is.call(response) && identical(response[[1]], as.name("|"))) {
    extra <- .equation_text(response[[3]])
    if (!is.call(response[[3]]) || !identical(response[[3]][[1]], as.name("trials")))
      stop("Unsupported response modifier in equation preview", call. = FALSE)
    if (is.null(trials) && grepl("trials\\(", extra)) trials <- sub(".*trials\\(([^)]+)\\).*", "\\1", extra)
    response <- response[[2]]
  }
  y <- paste0(.equation_name(.equation_text(response)), "_t")
  mean <- if (family_name %in% c("binomial", "bernoulli")) "p_t" else "\\mu_t"
  link_tex <- switch(link, identity = mean, log = paste0("\\log(", mean, ")"), logit = paste0("\\operatorname{logit}(", mean, ")"), paste0("\\operatorname{", link, "}(", mean, ")"))
  likelihood <- switch(family_name,
    gaussian = paste0(y, "\\sim N(\\mu_t,\\sigma_t^2)"),
    student = paste0(y, "\\sim t_{\\nu}(\\mu_t,\\sigma_t),\\quad\\sigma_t\\text{ is scale}"),
    skew_normal = paste0(y, "\\sim\\operatorname{SkewNormal}_{meanSD}(\\mu_t,\\sigma_t,\\alpha)"),
    Gamma = paste0(y, "\\sim\\operatorname{Gamma}(\\text{shape}=a,\\text{rate}=a/\\mu_t)"),
    poisson = paste0(y, "\\sim\\operatorname{Poisson}(\\mu_t)"),
    negbinomial = paste0(y, "\\sim\\operatorname{NB2}(\\mu_t,a),\\quad\\operatorname{Var}(Y_t)=\\mu_t+\\mu_t^2/a"),
    zero_inflated_poisson = paste0(y, "\\sim\\pi\\delta_0+(1-\\pi)\\operatorname{Poisson}(\\mu_t),\\quad E(Y_t)=(1-\\pi)\\mu_t"),
    zero_inflated_negbinomial = paste0(y, "\\sim\\pi\\delta_0+(1-\\pi)\\operatorname{NB2}(\\mu_t,a),\\quad E(Y_t)=(1-\\pi)\\mu_t"),
    beta = paste0(y, "\\sim\\operatorname{Beta}(\\mu_t\\varphi,(1-\\mu_t)\\varphi)"),
    binomial = paste0(y, "\\sim\\operatorname{Binomial}(", if (is.null(trials)) "n_t" else if (grepl("^[0-9]+$", trials)) trials else paste0(.equation_name(trials), "_t"), ",p_t)"),
    bernoulli = paste0(y, "\\sim\\operatorname{Bernoulli}(p_t)"),
    cumulative = paste0("P(", y, "\\le c)=", if (link == "logit") "\\operatorname{logit}^{-1}" else paste0("\\operatorname{", link, "}^{-1}"), "(\\tau_c-\\eta_t),\\quad\\tau_1<\\cdots<\\tau_{C-1}"))
  sigma <- ""
  if (!is.null(sigma_formula)) {
    if (is.character(sigma_formula)) sigma_formula <- stats::as.formula(sigma_formula)
    sigma_rhs <- sigma_formula[[length(sigma_formula)]]
    if (length(.equation_bars(sigma_rhs)) || length(.equation_calls(sigma_rhs, c("ar", specials))))
      stop("Unsupported sigma random or serial terms in equation preview", call. = FALSE)
    sigma_parts <- .equation_terms(sigma_rhs, data)
    sigma <- paste0("\\log(\\sigma_t)=", paste(c(.equation_predictor(sigma_parts, "gamma"),
      vapply(sigma_parts$offsets, .equation_offset, "")), collapse = " + "))
  }
  random <- vapply(seq_along(groups), function(k) {
    g <- groups[[k]]
    covariance <- if (g$correlated) paste0("\\Sigma_", k) else paste0("\\operatorname{diag}(\\tau_", k, "^2)")
    paste0("\\mathbf b_{", k, ",g}\\sim N(\\mathbf0,", covariance, "),\\quad g=", .equation_name(g$name))
  }, "")
  serial <- ""
  natural <- family_name %in% c("gaussian", "student")
  if (ar && ar_covariance) serial <- paste0("K=D R D/(1-\\rho^2),\\quad D=\\operatorname{diag}(\\sigma_t),\\quad R_{ts}=\\rho^{|t-s|},\\quad |\\rho|<1\\text{ within each series}", if (!natural) ",\\quad\\mathbf e\\sim N(\\mathbf0,K)\\text{ (latent)}", if (family_name == "gaussian") ",\\quad\\mathbf Y\\sim N(\\boldsymbol\\mu,K)", if (family_name %in% c("student", "skew_normal")) ",\\quad K\\text{ is the residual scale matrix; joint likelihood follows the fitted family}")
  if (ar && !ar_covariance) serial <- paste0(if (!natural) "\\text{latent }" else "", "e_t=\\rho e_{t-1}+\\epsilon_t,\\quad e_0=0,\\quad\\epsilon_t\\sim ", switch(family_name, student = "t_{\\nu}(0,\\sigma_t)", skew_normal = "\\operatorname{SkewNormal}_{meanSD}(0,\\sigma_t,\\alpha)", "N(0,\\sigma_t^2)"), "\\text{ independently within each series}")
  if (ar && !ar_covariance && !natural)
    serial <- "\\text{latent }e_t\\sim N(0,s_e^2)\\text{ independently},\\quad e_0=0"
  if (ar && natural && !ar_covariance) {
    likelihood <- gsub("\\mu_t", "(\\mu_t+\\rho e_{t-1})", likelihood, fixed = TRUE)
    serial <- paste0(serial, ",\\quad e_t=", y, "-\\mu_t")
  }
  if (ar && ar_covariance && natural) likelihood <- paste0("\\text{Joint likelihood: }", .equation_name(family_name), ",\\quad\\text{residual scale matrix }K")
  if (ar && !natural) serial <- gsub("\\sigma_t", "s_e", serial, fixed = TRUE)
  mean_equation <- if (family_name == "cumulative") "" else paste0(link_tex, "=\\eta_t")
  tex <- c(likelihood, mean_equation, paste0("\\eta_t=", predictor, if (ar && !natural)
    if (ar_covariance) "+e_t" else "+e_t+\\rho e_{t-1}"), random, sigma, serial)
  list(fixed_terms = parts$terms, fixed_columns = parts$columns, groups = groups, predictor = predictor, likelihood = likelihood,
       random = random, sigma = sigma, serial = serial, tex = tex[nzchar(tex)])
}
