# Ask AI and prior-elicitation turns built on llm_chat()

.llm_conventions <- function() {
"Package conventions in scdbayes: piecewise coding with continuous time;
time_A counts sessions from the first baseline session, and the change-point
terms are centered at each tier's own intervention onset. level_* terms are
the change in level at onset and trend_* terms are the change in slope
(sessions since onset). Random slopes sit on
the step (level_*) terms. For log- and logit-link families (Poisson, negative
binomial, Bernoulli, binomial, beta, cumulative) coefficients are on the link
scale. Default priors are scaled to the outcome SD on identity links and are
normal(0, 2.5) on the link scale otherwise. A class b prior applies only to
the treatment level coefficient (the onset level_* step). brms places the
Intercept prior on the centered intercept at average predictor values,
not on the first baseline observation. Cumulative models use ordinal thresholds,
not a single expected-outcome intercept. Decisions use the probability of
direction (pd), a region of practical equivalence (ROPE), and a minimal
clinically important difference (MCID) chosen by the researcher.
Reported effects and MCID are oriented so positive means the goal direction
(withdrawal steps reversed); pd and ROPE probabilities are two-sided.
Posterior predictive immediacy and Tau are later minus earlier regardless of
the goal; misfit flags are two-sided. scdbayes does
not compute the within-case SMD, which is biased under autocorrelation. The
design-comparable BC-SMD is estimated in a Bayesian model (bc_smd_bayes), and studies are pooled with a Bayesian
meta-analysis (meta_bayes); scdhlm/metafor scripts are optional frequentist
comparisons. BC-SMD requires Gaussian outcomes; do not recommend this method
for ordinal scores, binary outcomes, counts, or incompatible rubric scales.
This Gaussian restriction applies to bc_smd_bayes, not every possible
two-stage meta-analysis method. That function supports multiple-baseline,
multiple-probe, and reversal designs with at least three cases.
Different ordinal scales require a justified measurement mapping or separate
analyses; standardizing numeric category labels does not establish comparability.
WWC Standards v5.0 checks are a screening aid only, not a rating.
Convergence diagnostics, credible intervals, pd, ROPE, and MCID do not establish causal effects.
Causal inference requires appropriate design, experimental control, replication,
and assessment of alternative explanations; a fitted model alone is insufficient.

Verified app controls and support limits:
These controls are on the Model tab in Single-Case Design and Meta-Analysis,
not the Data tab: Design Type, Outcome type, Specific distribution,
Trials column, Recorded outcome units, and Exposure (offset) column.
Specific distribution is visible only when Outcome type = Other. To manually
choose Poisson or Neg. Binomial, select Other before Specific distribution;
do not direct a family selection under that control while Outcome type = Count.
For binomial outcomes, select Outcome type = Other, then Specific distribution =
Binomial (successes / trials), and select the Trials column. The app has
no fixed-trials numeric control and no Variable trials option; a constant
denominator still requires a dataset column containing that value each session.
For counts, select Recorded outcome units and Exposure (offset) column.
Counts remain counts; Rate per exposure unit values are multiplied by exposure
and rounded to counts. Do not manually log the exposure column: the app
adds offset(log(column)). For binary observations select Outcome type = Yes/no.
When a question describes errors per minute or another recorded rate, explicitly
distinguish counts from rates and name Recorded outcome units = Rate per exposure unit.
Ordinal scores use Other, then Cumulative logit in Specific distribution.
Power Analysis supports only Gaussian, Poisson, and negative binomial outcomes;
Bernoulli, binomial, beta, and cumulative power are unavailable. Count power
does not support nonzero AR(1). Model support does not imply power support.
Phase coding occurs before measure selection in fitted data and exported scripts,
so the original full-series time and onset references are retained. Never advise
recoding phase or time after filtering a fitted subset. Export uses the metadata
of the last successful fit; changed settings or a failed refit do not replace it.
A log-link level ratio compares fitted intervention and baseline counterfactuals
at the same time and other predictors, not observations from consecutive sessions.
Unsupported contrasts must remain unavailable; changing model terms does not
guarantee that an unsupported analysis becomes supported.
Ask AI cannot operate R or Excel, fit models, click controls, run power simulations,
or export files. Explain the actual app steps without claiming those actions ran.
Use impersonal phrasing without first- or second-person pronouns. Never invent
control names, supported families, model results, or app error messages.
Never compute or state numeric effect sizes of any kind (including within-case
SMD or BC-SMD), effect estimates, or WWC ratings; estimates come only from the
fitted brms models in the app."
}

.llm_reply_limit <- function() {
"Keep every reply to at most 30 words of plain text, with no headings or lists:
give the direct answer first, then the one reason."
}

.llm_hosted_note <- function() {
  on <- function(v) tolower(Sys.getenv(v, "")) %in% c("1", "true", "yes", "on")
  if (!on("SCDBAYES_HOSTED") || on("SCDBAYES_HOSTED_FITS")) return("")
"This is the hosted demo: model fitting and power simulation are turned off
because the server has too little memory. When a question needs a fit or a
simulation, or a run stopped for lack of memory, direct the researcher to
install the scdbayes R package locally: devtools::install_github(\"mshin77/scdbayes\"),
source https://github.com/mshin77/scdbayes, then scdbayes::run_app(). Give only
that link and command, and never invent other URLs. The install command and
link do not count toward the word limit."
}

.llm_system_prompt <- function(instructions = NULL) {
  custom <- is.character(instructions) && length(instructions) == 1L &&
    !is.na(instructions) && nzchar(trimws(instructions))
  note <- .llm_hosted_note()
  paste0(if (custom) instructions else llm_default_instructions(), "\n\n",
         .llm_conventions(), "\n\n", .llm_reply_limit(),
         if (nzchar(note)) paste0("\n\n", note))
}

.llm_dialog_prompt <- function() {
paste0(
"Acts as a Bayesian-statistics methodology assistant helping a single-case
design researcher elicit priors. The researcher is the domain expert.

The CONTEXT block lists only what the app sends: design, outcome family,
intervention goal, number of units, and current prior type; only when the
researcher turns sharing on, it adds outcome quantiles (10/50/90%), the
outcome SD, and the app's default prior strings. The outcome is always
called 'outcome'.

Reply with one JSON object that matches the response schema.
- When the researcher has not yet answered any questions, set phase to
  \"elicit\" and give at most 3 questions, each one short line under 20
  words, chosen from: typical outcome level across all sessions and
  plausible range (the centered Intercept, not the baseline level alone),
  the smallest change worth calling real, and prior studies they trust.
  Leave rationale empty.
- After the researcher answers, set phase to \"propose\" and give candidate
  priors. Each candidate has label, class (Intercept, b, sd, sigma, cor,
  shape, phi, nu, ar; b is the treatment level coefficient), prior_string (one Stan distribution call with numeric
  arguments, such as normal(0, 2.5)), rationale (1 short sentence anchored in
  the researcher's answers), and tradeoffs (1 short sentence). Offer more than one
  candidate per class only when the researcher should choose among them.
  Add one clarifying_question and a next_action_hint.

When default prior strings are present, compare proposed scales with them
and say why a scale differs from the default by a large factor. Do not invent citations, author
names, years, or DOIs; name a source only when certain, otherwise call it
general practice. Do not claim an intervention is effective or causal. Never
use the words enhance, improve, leverage, utilize, robust. Never speak for
the researcher.

", .llm_conventions())
}

#' Default Ask AI instructions
#'
#' The editable part of the Ask AI system instruction. `llm_ask()` always
#' appends the package conventions, so a custom `instructions` text changes
#' tone and scope without dropping the modeling facts.
#'
#' @return A character string.
#' @examples
#' cat(llm_default_instructions())
#' @noRd
llm_default_instructions <- function() {
"Acts as a Bayesian single-case design (SCD) methodology assistant inside the
scdbayes analysis app. Answers questions about the researcher's current
analysis and about Bayesian SCD methodology generally.

The CONTEXT block is refreshed every turn and contains only what the app
sends. With sharing off it states that no analysis details were shared. With
sharing on it lists where the researcher is in the app (Location), design,
outcome family, intervention goal, counts of cases and observations, per-phase
n, mean, SD and slope, the WWC screening result, the model formula with
non-standard variable names replaced by generic tokens, and convergence
diagnostics (max R-hat, min bulk ESS, divergences), the fitted prior type,
decision settings, and flagged posterior predictive checks. No data rows and no
posterior estimates are sent.

Answer for the step named in Location and use the context numbers when they
bear on the question. Guides the analysis in the app's tab order: Data
(upload, plots, WWC Check), Model (design, terms, priors, then the Fit
button), Diagnostics, Decisions, Per Case, Export. Asked what to do next,
names the next step and the tab that holds it. Given an app message or error,
explains the cause and the fix in the app (which tab and control). Baseline stability is judged from the baseline phase's
level, variability (SD), and trend (slope); pd, ROPE, and MCID judge model
effects after a fit, not baseline stability. When diagnostics show a problem (R-hat of
1.01 or above, low ESS, divergences), say so and name the next step. Do not fabricate numbers that are
not in the context. Do not invent citations, author names, years, or DOIs;
name a source only when certain, otherwise call it general practice. Do not
assert that an intervention is effective or causal. Never use the words
enhance, improve, leverage, utilize, or robust. Never speak for the
researcher."
}

.llm_dialog_schema <- function() {
  str <- list(type = "string")
  list(
    type = "object",
    properties = list(
      phase = list(type = "string", enum = I(c("elicit", "propose"))),
      questions = list(type = "array", items = str),
      rationale = str,
      candidates = list(type = "array", items = list(
        type = "object",
        properties = list(label = str, class = str, prior_string = str,
                          rationale = str, tradeoffs = str),
        required = I(c("label", "class", "prior_string")),
        additionalProperties = FALSE)),
      clarifying_question = str,
      next_action_hint = str),
    required = I("phase"),
    additionalProperties = FALSE)
}

.llm_cap_history <- function(history, max_turns = 8L, max_chars = 12000L) {
  h <- Filter(function(t) is.character(t$q) && is.character(t$a) &&
                nzchar(t$q) && nzchar(t$a), history %||% list())
  h <- utils::tail(h, max_turns)
  sizes <- vapply(h, function(t) nchar(t$q) + nchar(t$a), numeric(1))
  h[rev(cumsum(rev(sizes))) <= max_chars]
}

.llm_pairs_to_messages <- function(pairs) {
  unlist(lapply(pairs, function(t) list(
    list(role = "user", content = t$q),
    list(role = "assistant", content = t$a))), recursive = FALSE) %||% list()
}

.llm_system <- function(prompt, context_text) {
  paste0(prompt, "\n\nCONTEXT (current analysis, refreshed this turn):\n",
         context_text %||% "none")
}

# a CPU-bound local model needs longer than a cloud API
.llm_timeout <- function(provider) {
  v <- suppressWarnings(as.numeric(getOption("scdbayes.llm_timeout",
                                             Sys.getenv("SCDBAYES_LLM_TIMEOUT", ""))))
  if (length(v) == 1L && isTRUE(v > 0)) v else if (identical(provider, "local")) 180 else 60
}

.llm_then <- function(x, f, async) {
  if (isTRUE(async) && .is_promise(x)) promises::then(x, onFulfilled = f) else f(x)
}

#' Context-aware methodology assistant turn
#'
#' Rebuilds every request from the current state: the analysis context goes
#' into the system instruction on each turn, and earlier turns are resent as
#' non-empty question/answer pairs, capped at `max_turns` pairs and
#' `max_chars` characters. Requests time out after 180 seconds for `"local"`
#' and 60 seconds otherwise; `SCDBAYES_LLM_TIMEOUT` or
#' `options(scdbayes.llm_timeout = )` (seconds) overrides both.
#'
#' @param history Earlier turns as a list of `list(q = , a = )`.
#' @param user_text The researcher's question; must be non-empty.
#' @param context_text Summary of the current analysis.
#' @param provider One of `"gemini"`, `"openai"`, `"anthropic"`, `"local"`.
#' @param model Model id; defaults to `llm_default_model()`.
#' @param api_key API key (not needed for `"local"`).
#' @param base_url Optional endpoint base URL.
#' @param max_turns,max_chars History caps.
#' @param async Return a promise instead of a value; falls back to a
#'   blocking request when `promises`, `curl`, or `later` is not installed.
#' @param instructions Optional replacement for `llm_default_instructions()`;
#'   blank or `NULL` uses the default. The package conventions are always
#'   appended.
#' @return A list with `text`, `model`, `served_model`, `provider`, `finish`, `usage`, and
#'   `next_history`.
#' @keywords internal
#' @noRd
llm_ask <- function(history, user_text, context_text, provider = "gemini",
                    model = NULL, api_key = NULL, base_url = NULL,
                    max_turns = 8L, max_chars = 12000L, async = FALSE,
                    instructions = NULL) {
  if (!is.character(user_text) || !nzchar(trimws(user_text)))
    stop("Type a question before sending.", call. = FALSE)
  model <- model %||% llm_default_model(provider)
  kept <- .llm_cap_history(history, max_turns, max_chars)
  out <- llm_chat(provider, model,
                  system = .llm_system(.llm_system_prompt(instructions), context_text),
                  messages = c(.llm_pairs_to_messages(kept),
                               list(list(role = "user", content = user_text))),
                  max_tokens = 2048, api_key = api_key, base_url = base_url,
                  timeout = .llm_timeout(provider), async = async)
  .llm_then(out, function(res) list(
    text = res$text, model = res$model, served_model = res$served_model,
    provider = provider, finish = res$finish,
    usage = res$usage,
    next_history = c(kept, list(list(q = user_text, a = res$text)))), async)
}

#' Prior elicitation dialog turn
#'
#' The first turn may carry an empty note; later turns need the researcher's
#' reply. The response follows a JSON schema with phase `"elicit"`
#' (questions) or `"propose"` (candidate priors).
#'
#' @inheritParams llm_ask
#' @param dialog_state Earlier turns as a list of `list(q = , a = )`.
#' @param user_text The researcher's note or reply.
#' @return A list with `raw_text`, `parsed` (`NULL` when the reply is not a
#'   valid elicit/propose object), `problem` (why `parsed` is `NULL`, else
#'   `NULL`), `model`, `served_model`, `provider`, `usage`, and `next_state`. A propose reply
#'   keeps only candidates that are objects with a non-empty `class` and
#'   `prior_string`; with none left it counts as invalid.
#' @keywords internal
#' @noRd
llm_dialog_turn <- function(dialog_state, user_text, context_text,
                            provider = "gemini", model = NULL, api_key = NULL,
                            base_url = NULL, async = FALSE) {
  has_text <- is.character(user_text) && nzchar(trimws(user_text))
  if (length(dialog_state) > 0 && !has_text)
    stop("Type a reply to the questions before sending.", call. = FALSE)
  note <- if (has_text) user_text else "Begin the elicitation."
  model <- model %||% llm_default_model(provider)
  kept <- .llm_cap_history(dialog_state, 8L, 16000L)
  # temperature 0 keeps local elicitations reproducible; Gemini 3 is advised to keep its default
  temp <- if (provider == "local") 0 else NULL
  out <- llm_chat(provider, model,
                  system = .llm_system(.llm_dialog_prompt(), context_text),
                  messages = c(.llm_pairs_to_messages(kept),
                               list(list(role = "user", content = note))),
                  json_schema = .llm_dialog_schema(), temperature = temp,
                  max_tokens = 4096, api_key = api_key, base_url = base_url,
                  timeout = .llm_timeout(provider), async = async)
  .llm_then(out, function(res) {
    checked <- .llm_check_dialog(res$json)
    list(raw_text = res$text, parsed = checked$parsed, problem = checked$problem,
         model = res$model, served_model = res$served_model, provider = provider,
         usage = res$usage, next_state = c(kept, list(list(q = note, a = res$text))))
  }, async)
}

#' @noRd
.llm_text_field <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))

#' @noRd
.llm_check_dialog <- function(p) {
  if (!is.list(p) || !isTRUE(p$phase %in% c("elicit", "propose"))) {
    return(list(parsed = NULL, problem = "The reply is not an elicit or propose object."))
  }
  if (!identical(p$phase, "propose")) return(list(parsed = p, problem = NULL))
  cands <- if (is.list(p$candidates) && is.null(names(p$candidates))) p$candidates else list()
  usable <- Filter(function(cd) is.list(cd) && .llm_text_field(cd$class) &&
                     .llm_text_field(cd$prior_string), cands)
  if (!length(usable)) {
    return(list(parsed = NULL, problem = paste0(
      "The proposal has no usable candidates: each candidate must be an object ",
      "with a class and a prior_string.")))
  }
  p$candidates <- usable
  list(parsed = p, problem = NULL)
}

#' Strip control characters and cap length of user-supplied LLM text
#'
#' @param text Input text.
#' @param max_len Maximum retained characters.
#' @return Cleaned text.
#' @keywords internal
#' @noRd
sanitize_llm_text <- function(text, max_len = 4000) {
  if (is.null(text) || length(text) != 1L || is.na(text) || !nzchar(text)) return("")
  text <- gsub("[\\x00-\\x08\\x0b\\x0c\\x0e-\\x1f\\x7f]", " ", text, perl = TRUE)
  substr(text, 1, max_len)
}

#' Render model-written markdown as restricted HTML
#'
#' Raw HTML tags are escaped before rendering; images are replaced by their
#' alt text, so no remote image is fetched; afterwards only `http`, `https`,
#' and `mailto` link targets survive (others become `"#"`), and links open
#' in a new tab with `rel="noopener noreferrer"`.
#'
#' @param text Markdown text.
#' @return An HTML string.
#' @keywords internal
#' @noRd
render_llm_markdown <- function(text) {
  safe <- gsub("<(?=[A-Za-z/!?])", "&lt;", text %||% "", perl = TRUE)
  html <- as.character(shiny::markdown(safe))
  # code spans escape the pre-escaped < a second time; the entity stays text
  html <- gsub("&amp;lt;", "&lt;", html, fixed = TRUE)
  html <- gsub("(?i)<img\\b[^>]*?\\balt=\"([^\"]*)\"[^>]*>", "[image: \\1]", html, perl = TRUE)
  html <- gsub("(?i)<img\\b[^>]*>", "[image]", html, perl = TRUE)
  html <- gsub("(?i)\\b(href|src)\\s*=\\s*\"(?!(https?:|mailto:))[^\"]*\"",
               "\\1=\"#\"", html, perl = TRUE)
  html <- gsub("(?i)\\b(href|src)\\s*=\\s*'[^']*'", "\\1=\"#\"", html, perl = TRUE)
  gsub("<a ", "<a rel=\"noopener noreferrer\" target=\"_blank\" ", html, fixed = TRUE)
}
