suppressPackageStartupMessages({
  library(scdbayes)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(brms)
  library(bayesplot)
  library(posterior)
  library(loo)
  library(promises)
})

list2env(mget(c("llm_ask", "llm_base_url", "llm_chat", "llm_default_instructions",
               "llm_default_model", "llm_default_provider", "llm_dialog_turn",
               "llm_is_local_host", "llm_model_label", "llm_privacy_note",
               "llm_test_connection", "render_llm_markdown", "sanitize_llm_text"),
             envir = asNamespace("scdbayes"), inherits = FALSE),
         envir = environment())

.password_input <- function(inputId, label, value = "", placeholder = NULL) {
  pw <- shiny::passwordInput(inputId, label, value = value, placeholder = placeholder)
  pw$children <- lapply(pw$children, function(ch) {
    if (inherits(ch, "shiny.tag") && identical(ch$name, "input")) {
      ch$attribs$autocomplete <- "new-password"
    }
    ch
  })
  pw
}

# 10 MB upload cap when launched without run_app(); row/case limits come from check_data_size()
if (is.null(getOption("shiny.maxRequestSize")))
  options(shiny.maxRequestSize = 10 * 1024^2)

# demo server
.scdbayes_hosted <- tolower(trimws(Sys.getenv("SCDBAYES_HOSTED", ""))) %in% c("1", "true", "yes", "on")
.demo_max_rows  <- 1000
.demo_max_cases <- 15

if (requireNamespace("future", quietly = TRUE) && inherits(future::plan(), "sequential"))
  future::plan(future::multisession,
               workers = max(2L, min(4L, parallel::detectCores() - 1L, na.rm = TRUE)))

brms_backend       <- getOption("scdbayes.backend", "cmdstanr")
brms_threads       <- getOption("scdbayes.threads", brms::threading(2))
brms_stanc_options <- getOption("scdbayes.stanc_options", list("O1"))
brms_cache_dir     <- getOption("scdbayes.cache_dir",
                                file.path(tempdir(), "scdbayes_models"))
if (!dir.exists(brms_cache_dir)) dir.create(brms_cache_dir, recursive = TRUE)

# stanc_options is a cmdstanr argument; the rstan backend rejects it
.stan_model_args <- function() {
  if (identical(brms_backend, "cmdstanr")) list(stanc_options = brms_stanc_options) else list()
}

# shared Gemini key caps: calls per day and per hour, calls per hashed client address per hour, characters per hour
# counters live in one R process: run the app single-process, or the cap multiplies by worker count
.llm_shared <- new.env(parent = emptyenv())
.llm_shared$calls <- data.frame(time = numeric(0), client = character(0))
.llm_shared$chars <- data.frame(time = numeric(0), n = numeric(0))
# salt drawn from a fresh time/pid seed; the session's .Random.seed is restored afterwards
.llm_shared_salt <- local({
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv())
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
          else rm(".Random.seed", envir = globalenv()))
  set.seed(NULL)
  paste(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
})
.llm_limit <- function(env, default) {
  v <- suppressWarnings(as.numeric(Sys.getenv(env, "")))
  if (is.na(v) || v <= 0) default else v
}
.llm_shared_charge <- function(n, now = as.numeric(Sys.time())) {
  .llm_shared$chars <- rbind(.llm_shared$chars, data.frame(time = now, n = n))
}
.llm_shared_gate <- function(client, n_chars, now = as.numeric(Sys.time())) {
  .llm_shared$calls <- .llm_shared$calls[now - .llm_shared$calls$time < 86400, , drop = FALSE]
  .llm_shared$chars <- .llm_shared$chars[now - .llm_shared$chars$time < 3600, , drop = FALSE]
  hour <- .llm_shared$calls[now - .llm_shared$calls$time < 3600, , drop = FALSE]
  msg <- if (nrow(.llm_shared$calls) >= .llm_limit("SCDBAYES_AI_SHARED_DAILY", 300))
    "The shared AI quota for the last 24 hours is used up. Enter a personal key, choose Local model, or try again later."
  else if (nrow(hour) >= .llm_limit("SCDBAYES_AI_SHARED_CALLS", 60))
    "The shared AI quota is used up for this hour. Enter a personal key, choose Local model, or try again later."
  else if (sum(hour$client == client) >= .llm_limit("SCDBAYES_AI_CLIENT_CALLS", 20))
    "This connection reached its shared AI limit for this hour. Enter a personal key or try again later."
  else if (sum(.llm_shared$chars$n) + n_chars > .llm_limit("SCDBAYES_AI_SHARED_CHARS", 300000))
    "The shared AI text budget is used up for this hour. Enter a personal key, choose Local model, or try again later."
  if (!is.null(msg)) return(msg)
  .llm_shared$calls <- rbind(.llm_shared$calls, data.frame(time = now, client = client))
  .llm_shared_charge(n_chars, now)
  NULL
}

# GEMINI_MODEL is kept for older deployments; SCDBAYES_LLM_* variables are read by llm_default_*()
if (nzchar(Sys.getenv("GEMINI_MODEL")) && is.null(getOption("scdbayes.gemini_model")))
  options(scdbayes.gemini_model = Sys.getenv("GEMINI_MODEL"))
# no startup probe of a local server: the default comes from configured env/options only
.llm_default <- if (.scdbayes_hosted) "gemini" else llm_default_provider(check_local = FALSE)

# empty-state placeholder; icon keys into one shared inline SVG set
.empty_icons <- list(
  table        = '<rect x="3" y="4" width="18" height="16" rx="2"/><path d="M3 9.5h18M9.5 9.5V20M15.5 9.5V20"/>',
  columns      = '<rect x="3" y="4" width="18" height="16" rx="2"/><path d="M12 4v16"/><path d="M6 9h3M6 13h3M15 9h3M15 13h3"/>',
  features     = '<path d="M10 6h11M10 12h11M10 18h11"/><path d="M3 6.5 4.5 8 7 5.5M3 12.5 4.5 14 7 11.5M3 18.5 4.5 20 7 17.5"/>',
  plot_line    = '<path d="M3 3v18h18"/><path d="M6.5 15l3.5-4.5 3 3 4.5-6"/><circle cx="6.5" cy="15" r="1" fill="#CBD5E1" stroke="none"/><circle cx="10" cy="10.5" r="1" fill="#CBD5E1" stroke="none"/><circle cx="13" cy="13.5" r="1" fill="#CBD5E1" stroke="none"/><circle cx="17.5" cy="7.5" r="1" fill="#CBD5E1" stroke="none"/>',
  bars         = '<path d="M3 3v18h18"/><path d="M7.5 18v-5M12 18V9M16.5 18V6"/>',
  trend        = '<path d="M3 3v18h18"/><path d="M6 16.5 18 8.5" stroke-dasharray="3 2.5"/><circle cx="7" cy="17" r="1" fill="#CBD5E1" stroke="none"/><circle cx="10.5" cy="14" r="1" fill="#CBD5E1" stroke="none"/><circle cx="13.5" cy="13" r="1" fill="#CBD5E1" stroke="none"/><circle cx="17" cy="9.5" r="1" fill="#CBD5E1" stroke="none"/>',
  compare      = '<path d="M3 3v18h18"/><path d="M12.5 4.5V19" stroke-dasharray="3 2.5"/><path d="M5.5 15.5H11M14 8h5.5"/>',
  transform    = '<path d="M4 7h12"/><path d="M13 4l3 3-3 3"/><path d="M20 17H8"/><path d="M11 14l-3 3 3 3"/>',
  trace        = '<path d="M3 3v18h18"/><path d="M5.5 13c1-4 1.6-6 2.5-6s1.4 7 2.5 7 1.5-8 2.5-8 1.6 9 2.6 9 1.4-3 2.4-5"/>',
  forest       = '<path d="M12 3.5v17" stroke-dasharray="3 2.5"/><path d="M4.5 7H11"/><circle cx="7.5" cy="7" r="1.3" fill="#CBD5E1" stroke="none"/><path d="M9 12h7.5"/><circle cx="13" cy="12" r="1.3" fill="#CBD5E1" stroke="none"/><path d="M6 17h9"/><circle cx="10.5" cy="17" r="1.3" fill="#CBD5E1" stroke="none"/>',
  cases        = '<circle cx="9" cy="8" r="3.5"/><path d="M3.5 20c0-3 2.4-5 5.5-5s5.5 2 5.5 5"/><path d="M15.5 5a3.5 3.5 0 0 1 0 6.8"/><path d="M17.5 15.3c1.9.8 3 2.4 3 4.7"/>',
  export       = '<path d="M12 4v9"/><path d="M8.5 9.5 12 13l3.5-3.5"/><path d="M4.5 16.5v2a2 2 0 0 0 2 2h11a2 2 0 0 0 2-2v-2"/>',
  studies      = '<circle cx="12" cy="12" r="2.5"/><circle cx="5" cy="5" r="2"/><circle cx="19" cy="5" r="2"/><circle cx="5" cy="19" r="2"/><circle cx="19" cy="19" r="2"/><path d="M6.4 6.4l3.7 3.7M17.6 6.4l-3.7 3.7M6.4 17.6l3.7-3.7M17.6 17.6l-3.7-3.7"/>',
  sliders      = '<path d="M4 7h8"/><circle cx="15" cy="7" r="2"/><path d="M17 7h3"/><path d="M4 12h3"/><circle cx="10" cy="12" r="2"/><path d="M12 12h8"/><path d="M4 17h10"/><circle cx="17" cy="17" r="2"/><path d="M19 17h1"/>',
  gauge        = '<path d="M6.3 18.7A8 8 0 1 1 17.7 18.7"/><path d="M12 13l3.5-4"/><circle cx="12" cy="13" r="1.2" fill="#CBD5E1" stroke="none"/>',
  interactions = '<path d="M2 18h1.4c1.3 0 2.5-.6 3.3-1.7l6.1-8.6c.7-1.1 2-1.7 3.3-1.7H22"/><path d="m18 2 4 4-4 4"/><path d="M2 6h1.9c1.5 0 2.9.9 3.6 2.2"/><path d="M22 18h-5.9c-1.3 0-2.6-.7-3.3-1.8l-.5-.8"/><path d="m18 14 4 4-4 4"/>',
  predict      = '<path d="M3 3v18h18"/><path d="M6 13.5c4-1 7-3 12-4"/><path d="M6 10.5c4-1 6.5-3.5 12-6" stroke-dasharray="2.5 2.5"/><path d="M6 16.5c4-.5 8-1 12-2" stroke-dasharray="2.5 2.5"/>',
  model        = '<path d="M9 4c-1.6 0-2.3 1-2.3 2.7V17.3C6.7 19 6 20 4.4 20"/><path d="M4.5 10.5h6"/><path d="M13.5 10l5.5 8M19 10l-5.5 8"/>'
)

.empty_icon <- function(key) {
  if (is.null(key) || !nzchar(key) || is.null(.empty_icons[[key]])) return(NULL)
  HTML(paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="48" height="48" viewBox="0 0 24 24" ',
    'fill="none" stroke="#CBD5E1" stroke-width="1.5" stroke-linecap="round" ',
    'stroke-linejoin="round" aria-hidden="true" style="display: block; margin: 0 auto 20px;">',
    .empty_icons[[key]], '</svg>'))
}

# Ask AI star icon; id keeps the gradient unique
.ask_star_svg <- function(size = 24, id = "askstar") {
  HTML(paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="', size, '" height="', size,
    '" viewBox="0 0 24 24" aria-hidden="true" style="vertical-align: middle; overflow: visible;">',
    '<defs><linearGradient id="', id, '" x1="0" y1="0" x2="1" y2="1">',
    '<stop offset="0" stop-color="#a855f7"/><stop offset="1" stop-color="#6d28d9"/>',
    '</linearGradient></defs>',
    '<g fill="url(#', id, ')">',
    '<path d="M11 1.5l2.1 5.9 5.9 2.1-5.9 2.1L11 17.5l-2.1-5.9L3 9.5l5.9-2.1z"/>',
    '<path d="M19 13.5l.9 2.5 2.5.9-2.5.9-.9 2.5-.9-2.5-2.5-.9 2.5-.9z"/>',
    '<path d="M5.5 15l.7 1.9 1.9.7-1.9.7-.7 1.9-.7-1.9-1.9-.7 1.9-.7z"/>',
    '</g></svg>'))
}

plotly_figure <- function(outputId, label, ...) {
  tags$div(role = "figure", `aria-label` = label, plotly::plotlyOutput(outputId, ...))
}

empty_state <- function(icon, msg, max_width = "400px") {
  div(style = "padding: 60px 40px; text-align: center;",
    div(style = paste0("max-width: ", max_width, "; margin: 0 auto;"),
      .empty_icon(icon),
      tags$p(style = "font-size: 19px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;",
        HTML(msg))))
}

# R/ helpers sourced before the ui_* fragments that reference them
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source, local = FALSE))

# fragment functions sourced into global env so ui.R can reference them
source("ui_model.R", local = FALSE)
source("ui_results.R", local = FALSE)
source("ui_meta.R", local = FALSE)
