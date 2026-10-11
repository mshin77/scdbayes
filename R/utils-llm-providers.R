# Provider adapters behind llm_chat()

.llm_providers <- function() c("gemini", "openai", "anthropic", "local")

.llm_label <- function(provider) {
  c(gemini = "Gemini", openai = "OpenAI", anthropic = "Anthropic",
    local = "Local model")[[provider]]
}

.llm_setting <- function(option, env) {
  v <- getOption(option, Sys.getenv(env, ""))
  if (is.character(v) && length(v) == 1L && nzchar(trimws(v))) trimws(v) else NULL
}

#' Default model id for an LLM provider
#'
#' The single place that names a fallback model per provider. Administrators
#' override it with the option `scdbayes.<provider>_model` (for example
#' `options(scdbayes.gemini_model = "gemini-3.5-flash")`), or with
#' `SCDBAYES_LLM_MODEL` / `options(scdbayes.llm_model = )`, which applies to
#' the configured default provider. Pin the model a published analysis was
#' run with.
#'
#' @param provider One of `"gemini"`, `"openai"`, `"anthropic"`, `"local"`.
#' @return Model id string.
#' @examples
#' llm_default_model("gemini")
#' llm_default_model("local")
#' @noRd
llm_default_model <- function(provider = c("gemini", "openai", "anthropic", "local")) {
  provider <- match.arg(provider)
  defaults <- list(gemini = "gemini-3.8-flash", openai = "gpt-6-luna",
                   anthropic = "claude-haiku-4-5", local = "gemma3:4b")
  admin <- if (identical(.llm_setting("scdbayes.llm_provider", "SCDBAYES_LLM_PROVIDER"), provider))
    .llm_setting("scdbayes.llm_model", "SCDBAYES_LLM_MODEL")
  getOption(paste0("scdbayes.", provider, "_model"), admin %||% defaults[[provider]])
}

#' Base URL of the local OpenAI-compatible model server
#'
#' Read from `options(scdbayes.llm_base_url = )` or the environment variable
#' `SCDBAYES_LLM_BASE_URL`; falls back to Ollama's default
#' `http://localhost:11434/v1`.
#'
#' @return URL string.
#' @examples
#' llm_base_url()
#' @noRd
llm_base_url <- function() {
  .llm_setting("scdbayes.llm_base_url", "SCDBAYES_LLM_BASE_URL") %||%
    "http://localhost:11434/v1"
}

#' Whether a base URL points at the machine running the app
#'
#' Uses the same strict parse and loopback list as the address guard of
#' `llm_chat()`: the host must be exactly `localhost`, `127.0.0.1`, or `::1`
#' (written `[::1]` in a URL), ignoring case and a trailing dot.
#'
#' @param base_url URL string.
#' @return Logical scalar; `FALSE` for any other host or a URL that does not
#'   parse.
#' @examples
#' llm_is_local_host("http://localhost:11434/v1")
#' llm_is_local_host("http://[::1]:8080/v1")
#' llm_is_local_host("https://api.example.org/v1")
#' @noRd
llm_is_local_host <- function(base_url) {
  u <- .llm_parse_url(base_url %||% "")
  !is.null(u) && u$host %in% .llm_loopback_hosts
}

.llm_loopback_hosts <- c("localhost", "127.0.0.1", "::1")

#' Split an http(s) URL into scheme, host, and port
#'
#' Strict parse: an authority with user info (`@`), a host with characters
#' outside `A-Z a-z 0-9 . _ -`, or a numeric host that is not a dotted-decimal
#' IPv4 address (decimal, octal, hex, or short forms) gives `NULL`. When
#' `curl::curl_parse_url()` exists its host must agree.
#'
#' @param url URL string.
#' @return List with `scheme`, `host` (lower case, no brackets or trailing
#'   dot), `port`, and `ipv6`, or `NULL`.
#' @noRd
.llm_parse_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url)) return(NULL)
  m <- regmatches(url, regexec("^([Hh][Tt][Tt][Pp][Ss]?)://([^/?#]*)([/?#].*)?$", url))[[1]]
  if (length(m) < 3) return(NULL)
  hp <- regmatches(m[3], regexec("^(\\[[0-9A-Fa-f:.]+\\]|[A-Za-z0-9._-]+)(:([0-9]{1,5}))?$",
                                 m[3]))[[1]]
  if (!length(hp)) return(NULL)
  scheme <- tolower(m[2])
  ipv6 <- startsWith(hp[2], "[")
  host <- sub("\\.$", "", tolower(gsub("^\\[|\\]$", "", hp[2])))
  labels <- strsplit(host, ".", fixed = TRUE)[[1]]
  numeric_host <- !ipv6 && length(labels) && all(grepl("^(0x[0-9a-f]*|[0-9]+)$", labels))
  if (!nzchar(host) || numeric_host && !is.integer(.llm_ipv4(host))) return(NULL)
  curl_host <- if (requireNamespace("curl", quietly = TRUE) &&
                   "curl_parse_url" %in% getNamespaceExports("curl"))
    tryCatch(curl::curl_parse_url(url)$host, error = function(e) NA_character_)
  if (!is.null(curl_host) && !identical(sub("\\.$", "", tolower(gsub("^\\[|\\]$", "", curl_host %||% ""))),
                                        host)) return(NULL)
  list(scheme = scheme, host = host, ipv6 = ipv6,
       port = if (nzchar(hp[4])) as.integer(hp[4]) else if (scheme == "https") 443L else 80L)
}

#' @noRd
.llm_ipv4 <- function(host) {
  labels <- strsplit(host, ".", fixed = TRUE)[[1]]
  if (length(labels) != 4 || !all(grepl("^(0|[1-9][0-9]{0,2})$", labels))) return(NULL)
  oct <- as.integer(labels)
  if (any(oct > 255L)) NULL else oct
}

#' @noRd
.llm_ipv6 <- function(host) {
  tail4 <- regmatches(host, regexpr("[0-9.]+$", host))
  if (length(tail4) && grepl(".", tail4, fixed = TRUE)) {
    o <- .llm_ipv4(tail4)
    if (is.null(o)) return(NULL)
    host <- sub("[0-9.]+$", sprintf("%x:%x", o[1] * 256L + o[2], o[3] * 256L + o[4]), host)
  }
  halves <- regmatches(host, regexpr("::", host, fixed = TRUE), invert = TRUE)[[1]]
  if (length(halves) > 2) return(NULL)
  split <- function(s) if (nzchar(s)) strsplit(s, ":", fixed = TRUE)[[1]] else character(0)
  left <- split(halves[1])
  right <- if (length(halves) == 2) split(halves[2]) else character(0)
  fill <- 8L - length(left) - length(right)
  if (length(halves) == 2 && fill < 1L || length(halves) == 1 && fill != 0L) return(NULL)
  hx <- c(left, rep("0", fill), right)
  if (!all(grepl("^[0-9a-f]{1,4}$", hx))) return(NULL)
  strtoi(hx, 16L)
}

#' @noRd
.llm_v4_private <- function(o) {
  o[1] %in% c(0L, 10L, 127L) || o[1] >= 224L ||
    o[1] == 100L && o[2] >= 64L && o[2] <= 127L || o[1] == 169L && o[2] == 254L ||
    o[1] == 172L && o[2] >= 16L && o[2] <= 31L || o[1] == 192L && o[2] == 168L ||
    o[1] == 192L && o[2] == 0L && o[3] == 0L || o[1] == 198L && o[2] %in% c(18L, 19L)
}

#' @noRd
.llm_v6_private <- function(h) {
  zero5 <- all(h[1:5] == 0L)
  all(h[1:6] == 0L) || zero5 && h[6] == 0xffffL ||
    bitwAnd(h[1], 0xfe00L) == 0xfc00L || bitwAnd(h[1], 0xffc0L) %in% c(0xfe80L, 0xfec0L) ||
    bitwAnd(h[1], 0xff00L) == 0xff00L || h[1] == 0x64L && h[2] == 0xff9bL ||
    h[1] == 0x2002L || h[1] == 0x2001L && h[2] == 0L ||
    all(h[1:4] == 0L) && h[5] == 0xffffL && h[6] == 0L || h[1] == 0x100L && all(h[2:4] == 0L)
}

#' @noRd
.llm_addr_private <- function(addr) {
  a <- tolower(gsub("^\\[|\\]$|%.*$", "", addr))
  v4 <- .llm_ipv4(a)
  if (!is.null(v4)) return(.llm_v4_private(v4))
  v6 <- .llm_ipv6(a)
  is.null(v6) || .llm_v6_private(v6)
}

#' @noRd
.llm_resolve <- function(host) {
  if (!requireNamespace("curl", quietly = TRUE)) return(NULL)
  tryCatch(curl::nslookup(host, multiple = TRUE, error = FALSE), error = function(e) NULL)
}

#' Classify the host of a model server URL and pin the address it cleared
#'
#' `"invalid"` for a URL `.llm_parse_url()` refuses; `"loopback"` for exactly
#' `localhost`, `127.0.0.1`, or `::1`; `"private"` for literal or resolved
#' addresses in 0/8, 10/8, 127/8, 100.64/10, 169.254/16, 172.16/12,
#' 192.168/16, multicast, IPv6 unspecified, loopback, unique-local,
#' link-local, IPv4-mapped, NAT64, 6to4, or Teredo space, and for metadata host names;
#' `"unresolved"` when a host name has no address; `"public"` otherwise.
#'
#' A `"public"` host name also returns a `CURLOPT_RESOLVE` entry naming the
#' exact addresses that passed, so the request connects to those and a
#' second lookup cannot answer with a private address (DNS rebinding).
#' Literal addresses need no entry because no lookup happens.
#'
#' @param base_url URL string.
#' @return List with `status` and `pin` (`NULL` unless a resolved public host).
#' @noRd
.llm_host_check <- function(base_url) {
  u <- .llm_parse_url(base_url)
  if (is.null(u)) return(list(status = "invalid", pin = NULL))
  if (u$host %in% .llm_loopback_hosts) return(list(status = "loopback", pin = NULL))
  if (u$host %in% c("metadata.google.internal", "metadata") || grepl("\\.localhost$", u$host))
    return(list(status = "private", pin = NULL))
  literal <- u$ipv6 || !is.null(.llm_ipv4(u$host))
  if (u$ipv6 && is.null(.llm_ipv6(u$host))) return(list(status = "invalid", pin = NULL))
  addrs <- if (literal) u$host else .llm_resolve(u$host)
  if (!length(addrs)) return(list(status = "unresolved", pin = NULL))
  if (any(vapply(addrs, .llm_addr_private, logical(1))))
    return(list(status = "private", pin = NULL))
  list(status = "public",
       pin = if (!literal) sprintf("%s:%d:%s", u$host, u$port, paste(addrs, collapse = ",")))
}

#' @noRd
.llm_host_status <- function(base_url) .llm_host_check(base_url)$status

#' Whether a host is a private, link-local, or cloud-metadata address
#'
#' `TRUE` unless the host status is `"public"` or `"loopback"`, so
#' malformed, unresolvable, and non-canonical numeric hosts count as private.
#'
#' @param base_url URL string.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.llm_private_host <- function(base_url) {
  !.llm_host_status(base_url) %in% c("public", "loopback")
}

#' @noRd
.llm_flag <- function(option, env) {
  v <- getOption(option, Sys.getenv(env, ""))
  isTRUE(v) || tolower(as.character(v %||% "")[1]) %in% c("1", "true", "yes")
}

#' @noRd
.llm_allow_private <- function() .llm_flag("scdbayes.llm_allow_private", "SCDBAYES_LLM_ALLOW_PRIVATE")

#' @noRd
.llm_origin <- function(url) {
  u <- .llm_parse_url(url)
  if (is.null(u)) NA_character_ else paste(u$scheme, u$host, u$port)
}

#' Refuse a model server URL that reaches internal addresses
#'
#' Malformed URLs (user info, odd host forms, non-http schemes) are always
#' refused. A loopback host passes only for `provider = "local"` at the
#' scheme, host, and port of the administrator setting `llm_base_url()`, so
#' a URL typed in the app cannot reach other services on the server.
#' Private, unresolvable, and other loopback addresses pass only with
#' `options(scdbayes.llm_allow_private = TRUE)` or
#' `SCDBAYES_LLM_ALLOW_PRIVATE=true`. A cleared host name is pinned to the
#' addresses that passed the check, so the request connects to those rather
#' than to whatever a second lookup returns.
#'
#' @param base_url URL string or `NULL` (provider default endpoint).
#' @param provider Provider id.
#' @return Invisibly, a `CURLOPT_RESOLVE` entry pinning the cleared address,
#'   or `NULL` when no pin applies; an error when the host is refused.
#' @noRd
.llm_check_host <- function(base_url, provider = "local") {
  if (is.null(base_url)) return(invisible(NULL))
  if (is.null(.llm_parse_url(base_url)))
    stop(paste("The model server address must be an http(s) URL with a plain host name or",
               "dotted IP address and no user info."), call. = FALSE)
  if (!.llm_plain_path(base_url))
    stop(paste("The model server address must not carry a query (?), fragment (#),",
               "backslash, encoded character, or '.'/'..' path segment."), call. = FALSE)
  if (.llm_allow_private()) return(invisible(NULL))
  checked <- .llm_host_check(base_url)
  status <- checked$status
  admin_loopback <- identical(provider, "local") &&
    identical(.llm_origin(base_url), .llm_origin(llm_base_url()))
  if (status == "public") return(invisible(checked$pin))
  if (status == "loopback" && admin_loopback) return(invisible(NULL))
  stop(switch(status,
    loopback = paste("A loopback model server address is accepted only for the local provider",
                     "at the address set by SCDBAYES_LLM_BASE_URL or options(scdbayes.llm_base_url)."),
    unresolved = "The model server host name could not be resolved.",
    paste("The model server address is a private, link-local, or metadata address.",
          "Set options(scdbayes.llm_allow_private = TRUE) or",
          "SCDBAYES_LLM_ALLOW_PRIVATE=true to allow it.")), call. = FALSE)
}

#' @noRd
.llm_plain_path <- function(base_url) {
  path <- sub("^[A-Za-z]+://[^/]*", "", base_url)
  !grepl("[?#%\\\\[:space:]]", base_url) &&
    !any(strsplit(path, "/", fixed = TRUE)[[1]] %in% c(".", ".."))
}

#' @noRd
.is_promise <- function(x) inherits(x, "promise")

#' @noRd
.llm_async_ok <- function() {
  all(vapply(c("promises", "curl", "later"), requireNamespace, logical(1), quietly = TRUE))
}

#' Choose the default LLM provider
#'
#' Uses `options(scdbayes.llm_provider = )` or `SCDBAYES_LLM_PROVIDER` when
#' set. Otherwise picks `"gemini"` when the shared `GEMINI_API_KEY` is
#' present, then `"local"` when a local base URL is configured
#' (`SCDBAYES_LLM_BASE_URL` or `options(scdbayes.llm_base_url = )`), and
#' returns `NA` otherwise. No network request is sent unless `check_local`
#' is `TRUE`; then `"local"` also requires the server to answer a model
#' listing.
#'
#' @param check_local Probe the configured local server; defaults to
#'   `getOption("scdbayes.llm_probe_local", FALSE)`.
#' @return Provider id or `NA_character_`.
#' @examples
#' llm_default_provider(check_local = FALSE)
#' @noRd
llm_default_provider <- function(check_local = getOption("scdbayes.llm_probe_local", FALSE)) {
  set <- .llm_setting("scdbayes.llm_provider", "SCDBAYES_LLM_PROVIDER")
  if (!is.null(set) && set %in% .llm_providers()) return(set)
  if (nzchar(Sys.getenv("GEMINI_API_KEY", ""))) return("gemini")
  configured <- .llm_setting("scdbayes.llm_base_url", "SCDBAYES_LLM_BASE_URL")
  if (is.null(configured)) return(NA_character_)
  if (!isTRUE(check_local)) return("local")
  ok <- isTRUE(llm_test_connection("local", base_url = configured, timeout = 2)$ok)
  if (ok) "local" else NA_character_
}

#' Test the connection to an LLM provider
#'
#' For `"local"` sends one `GET <base_url>/models`; for cloud providers sends
#' one minimal chat request. Never returns or echoes the key.
#'
#' @inheritParams llm_chat
#' @param async For cloud providers, return a promise (see `llm_chat()`).
#' @return A list with `ok`, `model`, `latency_ms`, and `message`, or a
#'   promise of that list.
#' @examples
#' if (interactive()) {
#'   llm_test_connection("local", base_url = llm_base_url())
#' }
#' @noRd
llm_test_connection <- function(provider = c("gemini", "openai", "anthropic", "local"),
                                model = NULL, api_key = NULL, base_url = NULL,
                                timeout = 15, async = FALSE) {
  provider <- match.arg(provider)
  model <- model %||% llm_default_model(provider)
  async <- isTRUE(async) && .llm_async_ok()
  t0 <- Sys.time()
  wrap <- function(x) c(
    if (inherits(x, "error")) list(ok = FALSE, message = .llm_scrub(conditionMessage(x), api_key))
    else list(ok = TRUE, message = x),
    list(model = model,
         latency_ms = round(as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000)))
  if (provider != "local") {
    out <- tryCatch(
      llm_chat(provider, model, system = "Reply with the single word OK.",
               messages = list(list(role = "user", content = "Connection test.")),
               max_tokens = 2048, api_key = api_key, base_url = base_url,
               timeout = timeout, async = async),
      error = function(e) e)
    if (.is_promise(out))
      return(promises::then(out, onFulfilled = function(r) wrap("Connected."), onRejected = wrap))
    return(wrap(if (inherits(out, "error")) out else "Connected."))
  }
  base_url <- base_url %||% llm_base_url()
  pin <- tryCatch(.llm_check_host(base_url, "local"), error = function(e) e)
  if (inherits(pin, "error")) return(wrap(pin))
  req <- list(url = paste0(sub("/+$", "", base_url), "/models"),
              headers = if (nzchar(api_key %||% "")) c(Authorization = paste("Bearer", api_key))
                        else character(0),
              pin = pin)
  listing <- function(sent) {
    if (!identical(as.integer(sent$status), 200L))
      stop(sprintf("Local server answered HTTP %s.", sent$status), call. = FALSE)
    body <- tryCatch(jsonlite::fromJSON(sent$text, simplifyVector = FALSE), error = function(e) NULL)
    ids <- vapply(body$data %||% list(), function(m) as.character(m$id %||% ""), "")
    if (length(ids) && !any(c(model, paste0(model, ":latest")) %in% ids))
      stop(sprintf("Model '%s' is not installed (server has %s). Run: ollama pull %s",
                   model, .llm_scrub(paste(ids, collapse = ", "), api_key, 120L), model), call. = FALSE)
    "Connected."
  }
  if (isTRUE(async))
    return(promises::then(.llm_send_async(req, timeout),
                          onFulfilled = function(sent) wrap(tryCatch(listing(sent), error = function(e) e)),
                          onRejected = wrap))
  wrap(tryCatch({
    r <- httr::GET(req$url, httr::add_headers(.headers = req$headers), httr::timeout(timeout),
                   .llm_curl_config(req$pin))
    listing(list(status = httr::status_code(r), text = httr::content(r, "text", encoding = "UTF-8")))
  }, error = function(e) e))
}

#' Data-use note for an LLM provider
#'
#' @param provider Provider id.
#' @param base_url Base URL for `"local"`.
#' @return A character string for the app's privacy notice.
#' @examples
#' llm_privacy_note("local", "http://localhost:11434/v1")
#' llm_privacy_note("openai")
#' @noRd
llm_privacy_note <- function(provider, base_url = NULL) {
  base_url <- base_url %||% llm_base_url()
  switch(provider,
    local = if (llm_is_local_host(base_url))
      "Local model: requests go to a model server on this machine and are not sent to a third party."
      else sprintf(paste("Local model on a remote host (%s): requests leave this server and go to",
                         "that host. Check how that host stores requests."),
                   sub("^[a-zA-Z]+://([^/]+).*$", "\\1", base_url)),
    gemini = paste("Gemini: with a free-tier (unpaid) key, Google may use submitted content and",
                   "responses to develop its products, and human reviewers may read them",
                   "(https://ai.google.dev/gemini-api/terms). In the EEA, Switzerland, and",
                   "the UK, Google applies its paid-service data terms to all use."),
    openai = paste("OpenAI: API inputs and outputs are not used for training unless the key's",
                   "organization opts in, and abuse-monitoring logs are kept up to 30 days",
                   "(https://developers.openai.com/api/docs/guides/your-data)."),
    anthropic = paste("API provider \"anthropic\": inputs and outputs from its commercial API are",
                      "not used for training by default (https://www.anthropic.com/legal/commercial-terms)."),
    "")
}

#' Display label for a provider and model
#'
#' @param provider Provider id.
#' @param model Model id; defaults to `llm_default_model()`.
#' @return A string such as `"Gemini (gemini-3.8-flash)"`.
#' @examples
#' llm_model_label("gemini")
#' llm_model_label("local", "gemma3:4b")
#' @noRd
llm_model_label <- function(provider = "gemini", model = NULL) {
  provider <- match.arg(provider, .llm_providers())
  sprintf("%s (%s)", .llm_label(provider), model %||% llm_default_model(provider))
}

.llm_model_ok <- function(model, provider = "gemini") {
  # only gemini places the model id in the URL path; other ids travel in the JSON body
  allowed <- if (identical(provider, "gemini")) "^[A-Za-z0-9._:-]+$" else "^[A-Za-z0-9._:/-]+$"
  is.character(model) && length(model) == 1L && !is.na(model) && nchar(model) <= 128L &&
    grepl(allowed, model) && !grepl("..", model, fixed = TRUE)
}

.llm_scrub <- function(x, api_key = NULL, max_chars = 200L) {
  x <- paste(as.character(x), collapse = " ")
  if (!is.null(api_key) && nzchar(api_key)) x <- gsub(api_key, "[key]", x, fixed = TRUE)
  x <- gsub("(AIza[0-9A-Za-z_-]{8,}|sk-[0-9A-Za-z_-]{8,})", "[key]", x, perl = TRUE)
  if (nchar(x) > max_chars) paste0(substr(x, 1, max_chars), "...") else x
}

.compact <- function(x) Filter(Negate(is.null), x)

.gemini_thinking <- function(model) {
  # 2.5 models document thinkingBudget; later models take thinkingLevel
  if (grepl("^gemini-2\\.5", model)) list(thinkingBudget = 512L)
  else if (grepl("^gemini-", model)) list(thinkingLevel = "low")
  else NULL
}

.llm_build <- function(provider, model, system, messages, json_schema,
                       temperature, max_tokens, api_key, base_url) {
  key <- api_key %||% ""
  switch(provider,
    gemini = list(
      url = paste0(base_url %||% "https://generativelanguage.googleapis.com/v1beta",
                   "/models/", utils::URLencode(model, reserved = TRUE), ":generateContent"),
      headers = c(`x-goog-api-key` = key),
      body = .compact(list(
        systemInstruction = if (nzchar(system)) list(parts = list(list(text = system))),
        contents = lapply(messages, function(m) list(
          role = if (identical(m$role, "assistant")) "model" else "user",
          parts = list(list(text = m$content)))),
        generationConfig = .compact(list(
          temperature = temperature,
          maxOutputTokens = as.integer(max_tokens),
          responseMimeType = if (!is.null(json_schema)) "application/json",
          responseJsonSchema = json_schema,
          thinkingConfig = .gemini_thinking(model)))))),
    openai = ,
    local = list(
      url = paste0(sub("/+$", "", base_url %||% if (provider == "local")
        "http://localhost:11434/v1" else "https://api.openai.com/v1"),
        "/chat/completions"),
      headers = if (nzchar(key)) c(Authorization = paste("Bearer", key)) else character(0),
      body = .compact(c(
        list(model = model,
             messages = c(if (nzchar(system)) list(list(role = "system", content = system)),
                          lapply(messages, function(m) list(role = m$role, content = m$content))),
             temperature = temperature),
        stats::setNames(list(as.integer(max_tokens)),
                        if (provider == "openai") "max_completion_tokens" else "max_tokens"),
        list(response_format = if (!is.null(json_schema)) list(
          type = "json_schema",
          json_schema = list(name = "scdbayes_response", schema = json_schema,
                             strict = FALSE)))))),
    anthropic = list(
      url = paste0(sub("/+$", "", base_url %||% "https://api.anthropic.com"), "/v1/messages"),
      headers = c(`x-api-key` = key, `anthropic-version` = "2023-06-01"),
      body = .compact(list(
        model = model,
        max_tokens = as.integer(max_tokens),
        system = if (nzchar(system)) system,
        messages = lapply(messages, function(m) list(role = m$role, content = m$content)),
        temperature = temperature,
        output_config = if (!is.null(json_schema))
          list(format = list(type = "json_schema", schema = json_schema)))))
  )
}

#' @noRd
.llm_curl_config <- function(pin)
  do.call(httr::config, .compact(list(followlocation = 0L, resolve = pin)))

.llm_send <- function(req, timeout) {
  resp <- httr::POST(req$url, httr::add_headers(.headers = req$headers),
                     httr::timeout(timeout), .llm_curl_config(req$pin),
                     body = req$body, encode = "json")
  list(status = httr::status_code(resp),
       text = httr::content(resp, "text", encoding = "UTF-8"))
}

.llm_send_async <- function(req, timeout) {
  promises::promise(function(resolve, reject) {
    h <- curl::new_handle(timeout = timeout, followlocation = 0L)
    if (!is.null(req$pin)) curl::handle_setopt(h, resolve = req$pin)
    if (!is.null(req$body))
      curl::handle_setopt(h, post = TRUE,
        postfields = as.character(jsonlite::toJSON(req$body, auto_unbox = TRUE, null = "null")))
    curl::handle_setheaders(h, .list = as.list(c(req$headers, `Content-Type` = "application/json")))
    pool <- curl::new_pool()
    curl::curl_fetch_multi(req$url, handle = h, pool = pool,
      done = function(res) {
        txt <- rawToChar(res$content)
        Encoding(txt) <- "UTF-8"
        resolve(list(status = res$status_code, text = txt))
      },
      fail = function(msg) reject(simpleError(msg)))
    poll <- function() {
      curl::multi_run(timeout = 0, poll = TRUE, pool = pool)
      if (length(curl::multi_list(pool))) later::later(poll, 0.1)
    }
    poll()
  })
}

.llm_finish_code <- function(raw) {
  raw <- raw %||% "STOP"
  if (raw %in% c("STOP", "stop", "end_turn", "stop_sequence")) "stop"
  else if (raw %in% c("MAX_TOKENS", "length", "max_tokens")) "truncated"
  else if (raw %in% c("refusal")) "refusal"
  else if (raw %in% c("SAFETY", "RECITATION", "BLOCKLIST", "PROHIBITED_CONTENT",
                      "SPII", "IMAGE_SAFETY", "LANGUAGE", "content_filter")) "blocked"
  else "other"
}

.llm_parse <- function(provider, res) {
  switch(provider,
    gemini = {
      cand <- if (length(res$candidates)) res$candidates[[1]]
      parts <- Filter(function(p) !isTRUE(p$thought) && is.character(p$text),
                      cand$content$parts %||% list())
      block <- res$promptFeedback$blockReason
      list(text = paste(vapply(parts, `[[`, "", "text"), collapse = "\n"),
           raw_finish = cand$finishReason %||% block %||% "STOP",
           finish = if (is.null(cand) && !is.null(block)) "blocked"
                    else .llm_finish_code(cand$finishReason),
           usage = list(input = res$usageMetadata$promptTokenCount,
                        output = res$usageMetadata$candidatesTokenCount))
    },
    openai = ,
    local = {
      choice <- if (length(res$choices)) res$choices[[1]]
      refusal <- choice$message$refusal
      list(text = as.character(choice$message$content %||% ""),
           raw_finish = choice$finish_reason %||% "stop",
           finish = if (is.character(refusal) && nzchar(refusal)) "refusal"
                    else .llm_finish_code(choice$finish_reason),
           refusal = refusal,
           usage = list(input = res$usage$prompt_tokens,
                        output = res$usage$completion_tokens))
    },
    anthropic = {
      blocks <- Filter(function(b) identical(b$type, "text"), res$content %||% list())
      list(text = paste(vapply(blocks, `[[`, "", "text"), collapse = "\n"),
           raw_finish = res$stop_reason %||% "end_turn",
           finish = .llm_finish_code(res$stop_reason),
           usage = list(input = res$usage$input_tokens,
                        output = res$usage$output_tokens))
    })
}

.llm_status_hint <- function(status) {
  code <- as.character(status %||% "")[1]
  hint <- c(`401` = "the key was rejected", `403` = "the key lacks access to this model",
            `404` = "the model or endpoint was not found",
            `429` = "the rate limit or quota was reached; try again later",
            `3` = "the server answered with a redirect, which is not followed",
            `4` = "the request was rejected; check the model id and settings",
            `5` = "the provider had a server error; try again later")
  out <- c(hint[code], hint[substr(code, 1L, 1L)], "the request failed")
  unname(out[!is.na(out)][1])
}

.llm_read <- function(provider, model, sent, api_key, json) {
  label <- .llm_label(provider)
  if (!identical(as.integer(sent$status), 200L))
    stop(sprintf("%s API error (HTTP %s): %s.", label, sent$status, .llm_status_hint(sent$status)),
         call. = FALSE)
  res <- tryCatch(jsonlite::fromJSON(sent$text, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (!is.list(res))
    stop(sprintf("%s returned a response that is not JSON.", label), call. = FALSE)
  out <- .llm_parse(provider, res)
  if (out$finish == "blocked")
    stop(sprintf("%s blocked the response (%s). Rephrase the request.", label,
                 .llm_scrub(out$raw_finish, api_key)), call. = FALSE)
  if (out$finish == "refusal")
    stop(sprintf("%s declined to answer: %s", label,
                 .llm_scrub(out$refusal %||% "no reason given", api_key)), call. = FALSE)
  if (out$finish == "truncated" && (json || !nzchar(trimws(out$text))))
    stop(sprintf("%s response was cut off at the token limit. Ask a shorter question.", label),
         call. = FALSE)
  if (!nzchar(trimws(out$text)))
    stop(sprintf("%s returned an empty response (%s).", label,
                 .llm_scrub(out$raw_finish, api_key)), call. = FALSE)
  parsed <- if (json) tryCatch(jsonlite::fromJSON(out$text, simplifyVector = FALSE),
                               error = function(e) NULL)
  served <- if (identical(provider, "gemini")) res$modelVersion else res$model
  list(text = out$text, finish = out$finish, model = model,
       served_model = if (is.character(served) && length(served) == 1L) served else NA_character_,
       usage = out$usage, json = parsed)
}

#' Send one chat request to an LLM provider
#'
#' One interface over four providers: `"gemini"` (`generateContent`),
#' `"openai"` (Chat Completions), `"anthropic"` (Messages), and `"local"`
#' (an OpenAI-compatible server such as Ollama). Finish reasons are mapped to `"stop"`,
#' `"truncated"`, `"blocked"`, or `"refusal"`; blocked, refused, empty, and
#' (for JSON output) truncated responses raise an error. Error text never
#' contains the API key or the body of an error response; a non-200 answer
#' reports only its HTTP status and a generic reason. Redirects are not
#' followed.
#'
#' @details
#' Two modes run side by side. The local mode (`"local"`) posts to an
#' OpenAI-compatible base URL with no key, for example Ollama serving Gemma at
#' `http://localhost:11434/v1`. The cloud mode (`"gemini"`, `"openai"`,
#' `"anthropic"`) posts to the provider's API with its own key and model.
#' Administrators configure defaults without code changes:
#' \describe{
#'   \item{`SCDBAYES_LLM_PROVIDER`}{default provider (`llm_default_provider()`).}
#'   \item{`SCDBAYES_LLM_MODEL`}{model for that provider (`llm_default_model()`).}
#'   \item{`SCDBAYES_LLM_BASE_URL`}{local endpoint (`llm_base_url()`).}
#'   \item{`GEMINI_API_KEY`}{shared server key, used for `"gemini"` only.}
#' }
#' Each has an option counterpart (`scdbayes.llm_provider`,
#' `scdbayes.llm_model`, `scdbayes.llm_base_url`). `llm_test_connection()`
#' checks a provider before use.
#'
#' A base URL is parsed strictly: user info (`user@host`), non-http schemes,
#' numeric hosts other than dotted-decimal IPv4, a query (`?`), a fragment
#' (`#`), percent-encoding, backslashes, and `.`/`..` path segments are
#' refused, so a request goes to the base path followed by the endpoint. Host names
#' are resolved, and a host whose literal or resolved address is private
#' (RFC 1918), shared, link-local, loopback, cloud-metadata, IPv6
#' unique-local, link-local, IPv4-mapped, 6to4 (2002::/16), or Teredo
#' (2001::/32) is refused, so the app cannot
#' be pointed at internal services. The local provider is meant for a
#' server the administrator configured: a loopback host (`localhost`,
#' `127.0.0.1`, `::1`) passes only at the scheme, host, and port of
#' `llm_base_url()`, so a URL typed in the app must pass the same check.
#' `SCDBAYES_LLM_ALLOW_PRIVATE=true` (or
#' `options(scdbayes.llm_allow_private = TRUE)`) lifts the address block for
#' a model server on a private network.
#'
#' @param provider One of `"gemini"`, `"openai"`, `"anthropic"`, `"local"`.
#' @param model Model id; defaults to `llm_default_model()`.
#' @param system System instruction text.
#' @param messages List of `list(role = "user" | "assistant", content = )`.
#'   Messages with empty content are dropped.
#' @param json_schema Optional JSON Schema (as a list) for structured output.
#' @param temperature Sampling temperature; `NULL` omits the field, for
#'   models that accept only their default.
#' @param max_tokens Maximum output tokens.
#' @param api_key API key; required except for `"local"`.
#' @param base_url Optional endpoint base URL; for `"local"` defaults to
#'   `llm_base_url()`.
#' @param timeout Request timeout in seconds.
#' @param async If `TRUE`, returns a promise and performs the request
#'   without blocking the R process; without `promises`, `curl`, and
#'   `later` installed the request is sent synchronously through `httr`.
#' @return A list with `text`, `finish`, `model` (the requested model),
#'   `served_model` (the model version the provider reports serving, `NA`
#'   when the response names none), `usage`, and `json` (the parsed object
#'   when `json_schema` is given and parsing succeeds), or a promise of that
#'   list.
#' @examples
#' if (interactive()) {
#'   llm_chat("local", messages = list(list(role = "user", content = "Hello")))
#' }
#' @noRd
llm_chat <- function(provider = c("gemini", "openai", "anthropic", "local"),
                     model = NULL, system = "", messages = list(),
                     json_schema = NULL, temperature = NULL, max_tokens = 2048,
                     api_key = NULL, base_url = NULL, timeout = 60, async = FALSE) {
  provider <- match.arg(provider)
  model <- model %||% llm_default_model(provider)
  if (!nzchar(model)) stop("No model id given.", call. = FALSE)
  if (!.llm_model_ok(model, provider))
    stop("The model id may hold only letters, digits, '.', '_', ':', '-' (and '/' outside gemini).",
         call. = FALSE)
  if (provider != "local" && !nzchar(api_key %||% ""))
    stop(sprintf("An API key is required for %s.", .llm_label(provider)), call. = FALSE)
  msgs <- Filter(function(m) is.character(m$content) && nzchar(trimws(m$content)), messages)
  if (!length(msgs)) stop("No message text to send.", call. = FALSE)
  base_url <- if (is.null(base_url) || !nzchar(trimws(base_url))) NULL else trimws(base_url)
  if (provider == "local") base_url <- base_url %||% llm_base_url()
  pin <- .llm_check_host(base_url, provider)
  async <- isTRUE(async) && .llm_async_ok()
  req <- .llm_build(provider, model, system %||% "", msgs, json_schema,
                    temperature, max_tokens, api_key, base_url)
  req$pin <- pin
  json <- !is.null(json_schema)
  if (!isTRUE(async)) {
    sent <- tryCatch(.llm_send(req, timeout), error = function(e)
      stop(sprintf("%s request failed: %s", .llm_label(provider),
                   .llm_scrub(conditionMessage(e), api_key)), call. = FALSE))
    return(.llm_read(provider, model, sent, api_key, json))
  }
  promises::then(
    .llm_send_async(req, timeout),
    onFulfilled = function(sent) .llm_read(provider, model, sent, api_key, json),
    onRejected = function(e) stop(sprintf("%s request failed: %s", .llm_label(provider),
                                          .llm_scrub(conditionMessage(e), api_key)), call. = FALSE))
}
