# ui.R - Bayesian SCD Meta-Analysis
# unifiedRandomEffectsUI is in R/ui-random-effects.R

ui <- fluidPage(
  lang = "en",
  shinyjs::useShinyjs(),
  shinybusy::add_busy_spinner(
    spin = "fading-circle",
    position = "full-page",
    color = "#337ab7",
    height = "100px",
    width = "100px",
    onstart = FALSE
  ),
  tags$div(
    id = "loading-splash",
    style = "position:fixed;top:0;left:0;width:100%;height:100%;background:#fff;z-index:99999;display:flex;flex-direction:column;align-items:center;justify-content:center;",
    tags$div(
      style = "width:50px;height:50px;border:4px solid #e5e7eb;border-top:4px solid #337ab7;border-radius:50%;animation:spin 0.8s linear infinite;"
    ),
    tags$style(HTML("@keyframes spin{to{transform:rotate(360deg)}}"))
  ),
  tags$script(HTML("$(document).on('shiny:connected shiny:disconnected',function(){$('#loading-splash').fadeOut(300)});setTimeout(function(){$('#loading-splash').fadeOut(300)},15000);")),
  tags$script(HTML(
    "$(document).on('shiny:connected', function(){",
    "  $('#main_nav a[data-value=\"home\"]').parent().hide();",
    "  $('.navbar-brand').css({cursor:'pointer', 'user-select':'none'})",
    "    .attr({tabindex: '0', role: 'link', title: 'Home'})",
    "    .on('click keydown', function(e){",
    "      if (e.type === 'keydown' && e.key !== 'Enter' && e.key !== ' ') return;",
    "      e.preventDefault();",
    "      $('a[data-value=\"home\"]').tab('show');",
    "    });",
    "});"
  )),
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "description", content = "Bayesian multilevel modeling, meta-analysis, and power analysis for single-case designs, built on brms and Stan."),
    tags$link(rel = "canonical", href = "https://www.scdbayes.org/"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:site_name", content = "scdbayes"),
    tags$meta(property = "og:title", content = "scdbayes: Bayesian Single-Case Design Analysis"),
    tags$meta(property = "og:description", content = "Bayesian multilevel modeling, meta-analysis, and power analysis for single-case designs, built on brms and Stan."),
    tags$meta(property = "og:url", content = "https://www.scdbayes.org/"),
    tags$meta(property = "og:image", content = "https://www.scdbayes.org/images/logo.png"),
    tags$meta(name = "twitter:card", content = "summary"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "logo.svg"),
    tags$link(rel = "shortcut icon", href = "logo.svg"),
    tags$style(HTML("
      .password-wrapper { position: relative; }
      .password-wrapper input[type='password'],
      .password-wrapper input[type='text'] { padding-right: 36px; }
      .password-toggle {
        position: absolute; right: 8px; top: 50%; transform: translateY(-50%);
        background: none; border: none; cursor: pointer; color: #888;
        padding: 2px 4px; font-size: 13px; line-height: 1; z-index: 2;
      }
      .password-toggle:hover { color: #333; }
    ")),
    tags$script(HTML("
      $(document).on('shiny:value shiny:inputchanged shiny:bound', function() {
        setTimeout(function() {
          $('input[type=\"password\"]').each(function() {
            if ($(this).parent().hasClass('password-wrapper')) return;
            $(this).wrap('<div class=\"password-wrapper\"></div>');
            var btn = $('<button type=\"button\" class=\"password-toggle\" title=\"Show/hide\" aria-label=\"Show API key\" aria-pressed=\"false\"><i class=\"fa fa-eye\"></i></button>');
            $(this).after(btn);
          });
        }, 100);
      });
      $(document).on('click', '.password-toggle', function(e) {
        e.preventDefault();
        var input = $(this).siblings('input');
        var icon = $(this).find('i');
        var show = input.attr('type') === 'password';
        input.attr('type', show ? 'text' : 'password');
        icon.toggleClass('fa-eye', !show).toggleClass('fa-eye-slash', show);
        $(this).attr({'aria-pressed': String(show), 'aria-label': show ? 'Hide API key' : 'Show API key'});
      });
      $(document).ready(function() {
        $('input[type=\"password\"]').each(function() {
          if ($(this).parent().hasClass('password-wrapper')) return;
          $(this).wrap('<div class=\"password-wrapper\"></div>');
          var btn = $('<button type=\"button\" class=\"password-toggle\" title=\"Show/hide\" aria-label=\"Show API key\" aria-pressed=\"false\"><i class=\"fa fa-eye\"></i></button>');
          $(this).after(btn);
        });
      });
    "))
  ),
  includeCSS("www/styles.css"),
  tags$a(href = "#main-content", class = "skip-link", tabindex = "0", "Skip to main content"),
  tags$div(role = "status", `aria-live` = "polite", `aria-atomic` = "true",
           id = "status_region", class = "sr-only"),
  tags$h1(class = "sr-only", "scdbayes: Bayesian single-case design analysis"),
  if (.scdbayes_hosted) tags$div(
    class = "demo-banner", role = "note",
    tags$strong("Demo: "),
    sprintf("small data only (up to %d cases, %s rows); no identifiable data. ",
            .demo_max_cases, format(.demo_max_rows, big.mark = ",")),
    "The full R package is released after validation; ",
    tags$a(href = "mailto:mshin2@ilstu.edu?subject=scdbayes%20R%20package", "request early access"), ".",
    tags$button(type = "button", class = "demo-close", `aria-label` = "Close demo note",
                onclick = "this.parentNode.remove()", HTML("&times;"))),
  tags$script(HTML(
    "$(document).on('shiny:connected', function(){",
    "  Shiny.setInputValue('viewport_width', window.innerWidth);",
    "});",
    "var vwTimer = null;",
    "$(window).on('resize orientationchange', function(){",
    "  clearTimeout(vwTimer);",
    "  vwTimer = setTimeout(function(){ if (Shiny.setInputValue) Shiny.setInputValue('viewport_width', window.innerWidth); }, 250);",
    "});",
    "$(function(){",
    "  var panel = $('#app-main > .container-fluid > .tab-content').first();",
    "  if (!panel.length) panel = $('#app-main .tab-content').first();",
    "  if (panel.length) { panel.attr({id: 'main-content', tabindex: '-1'}); }",
    "  else { $('#app-main').attr('tabindex', '-1'); $('.skip-link').attr('href', '#app-main'); }",
    "});",
    # dialog semantics for every modal; focus returns to the trigger on close
    # links sending a Shiny input are not always focused on click, so the last pressed control counts
    "var modalTrigger = null, lastTrigger = null;",
    "$(document).on('mousedown keydown focusin', 'a, button, [role=button]', function(){",
    "  if (!$(this).closest('.modal').length) lastTrigger = this;",
    "});",
    "$(document).on('show.bs.modal', '.modal', function(){",
    "  var a = document.activeElement;",
    "  modalTrigger = lastTrigger && document.body.contains(lastTrigger) ? lastTrigger",
    "    : (a && a !== document.body && !$(a).closest('.modal').length ? a : null);",
    "});",
    "$(document).on('shown.bs.modal', '.modal', function(){",
    "  var t = $(this).find('.modal-title').first();",
    "  if (t.length && !t.attr('id')) t.attr('id', 'modal-title-' + Date.now());",
    "  $(this).attr({role: 'dialog', 'aria-modal': 'true', 'aria-labelledby': t.attr('id') || null});",
    "});",
    "$(document).on('hidden.bs.modal', '.modal', function(){",
    "  if (modalTrigger && document.body.contains(modalTrigger)) modalTrigger.focus();",
    "  modalTrigger = null;",
    "});"
  )),
  tags$main(
    id = "app-main", role = "main",
    navbarPage(
      windowTitle = "scdbayes: Bayesian Single-Case Design Analysis",
      title = tagList(
        "scdbayes",
        tags$small(style = "font-weight: 400; color: #64748B; margin-left: 6px;",
                   as.character(utils::packageVersion("scdbayes")))
      ),
      id = "main_nav",

      tabPanel(
        "Home", value = "home",
        fluidRow(
          column(width = 2,
            wellPanel(
              style = "padding: 0; border: none; box-shadow: none; background: transparent;",
              tags$ul(
                class = "nav nav-pills nav-stacked",
                id = "home_nav_menu",
                tags$li(class = "active", tags$a(href = "#about-tab", `data-toggle` = "tab", "About")),
                tags$li(tags$a(href = "#features-tab", `data-toggle` = "tab", "Features")),
                tags$li(tags$a(href = "#data-format-tab", `data-toggle` = "tab", "Data Format")),
                tags$li(tags$a(href = "#quick-start-tab", `data-toggle` = "tab", "Quick Start")),
                tags$li(tags$a(href = "#cyber-tab", `data-toggle` = "tab", "Cybersecurity")),
                tags$li(tags$a(href = "#resources-tab", `data-toggle` = "tab", "Resources")),
                tags$li(tags$a(href = "#support-tab", `data-toggle` = "tab", "Support"))
              )
            )
          ),
          column(width = 10,
            tags$div(class = "tab-content",
              tags$div(id = "about-tab", class = "tab-pane active",
                div(class = "markdown-content",
                    tags$img(src = "images/logo.png", alt = "scdbayes logo", align = "right", width = 150, height = 174),
                    includeMarkdown("markdown/about.md"))),
              tags$div(id = "features-tab", class = "tab-pane",
                div(class = "markdown-content", includeMarkdown("markdown/features.md"))),
              tags$div(id = "data-format-tab", class = "tab-pane",
                div(class = "markdown-content", includeMarkdown("markdown/data_format.md"))),
              tags$div(id = "quick-start-tab", class = "tab-pane",
                div(class = "markdown-content", includeMarkdown("markdown/quick_start.md"))),
              tags$div(id = "cyber-tab", class = "tab-pane",
                div(class = "markdown-content", includeMarkdown("markdown/cybersecurity.md"),
                  tags$ul(class = "privacy-extra",
                    tags$li("No usage tracking: clicks, inputs, and data are not logged")))),
              tags$div(id = "resources-tab", class = "tab-pane",
                div(class = "markdown-content", includeMarkdown("markdown/resources.md"))),
              tags$div(id = "support-tab", class = "tab-pane",
                div(class = "markdown-content", includeMarkdown("markdown/support.md")))
            )
          )
        )
      ),

      tabPanel(
        "Single-Case Design", value = "scd",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            conditionalPanel(
              condition = "input.scd_nav == 'import'",
              data_sidebar("", c(
                "Select a dataset" = "",
                "AB Design (3 cases)" = "ab",
                "Reversal/ABAB (3 cases)" = "reversal",
                "Multiple Baseline (4 cases)" = "mbd",
                "Multiple Probe (3 cases)" = "mp",
                "Alternating Treatment (3 cases)" = "atd",
                "Changing Criterion (3 cases)" = "ccd"
              ), study_required = FALSE)
            ),
            conditionalPanel(
              condition = "input.scd_nav == 'setup'",
              model_sidebar()
            ),
            conditionalPanel(
              condition = "input.scd_nav == 'fit'",
              fit_sidebar()
            ),
            conditionalPanel(
              condition = "input.scd_nav == 'effects'",
              effects_sidebar()
            ),
            conditionalPanel(
              condition = "input.scd_nav == 'percase'",
              percase_sidebar()
            ),
            conditionalPanel(
              condition = "input.scd_nav == 'export'",
              export_sidebar()
            )
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              id = "scd_nav",
              tabPanel(
                "Data", value = "import",
                data_panels("", TRUE)
              ),

              tabPanel("Model", value = "setup", uiOutput("model_tab_ui")),

      tabPanel("Diagnostics", value = "fit",     uiOutput("fit_tab_ui")),

      tabPanel("Decisions",  value = "effects", uiOutput("effects_tab_ui")),

      tabPanel("Per Case", value = "percase", uiOutput("percase_tab_ui")),

      # Export — inside Single-Case Design
      tabPanel(
        "Export", value = "export",
        conditionalPanel(
          condition = "!output.modelFitted && !output.dataUploaded",
          empty_state("export",
            'Load data, choose settings, then select <strong style="color: #0c1f4a;">Fit</strong>.')
        ),
        conditionalPanel(
          condition = "output.dataUploaded || output.modelFitted",
          div(style = "max-width: 960px; margin: 0 auto; padding: 24px;",

            # R Code
            div(style = "margin-bottom: 28px;",
              tags$h2("R Code", style = "color: #0c1f4a; font-weight: 600; margin-bottom: 12px;"),
              verbatimTextOutput("liveCodeDisplay"),
              tags$button(id = "copyCode", type = "button", class = "btn btn-default btn-sm",
                onclick = paste0(
                  "(function(){var el=document.getElementById('liveCodeDisplay');",
                  "var t=el?el.innerText:'';",
                  "function report(s){Shiny.setInputValue('copyCodeResult',{status:s,t:Date.now()},{priority:'event'});}",
                  "function fb(){var a=document.createElement('textarea');a.value=t;a.setAttribute('readonly','');",
                  "a.style.position='fixed';a.style.opacity=0;document.body.appendChild(a);a.select();",
                  "var ok=false;try{ok=document.execCommand('copy');}catch(e){}",
                  "document.body.removeChild(a);report(ok?'ok':'fail');}",
                  "if(!t){report('fail');return;}",
                  "if(navigator.clipboard&&window.isSecureContext){",
                  "navigator.clipboard.writeText(t).then(function(){report('ok');},fb);}else{fb();}})();"),
                "Copy")
            ),

            hr(style = "border-color: #E2E8F0;"),

            # Downloads
            div(style = "margin-top: 20px;",
              tags$h2("Downloads", style = "color: #0c1f4a; font-weight: 600; margin-bottom: 16px;"),
              fluidRow(
                column(3, downloadButton("downloadData", "Data", class = "btn-default w-100 mb-2")),
                column(3, shinyjs::disabled(downloadButton("downloadModel", "Model", class = "btn-default w-100 mb-2"))),
                column(3, shinyjs::disabled(downloadButton("downloadCode", "Script", class = "btn-default w-100 mb-2"))),
                column(3, shinyjs::disabled(downloadButton("downloadEffects", "Decisions", class = "btn-default w-100 mb-2")))
              ),
              fluidRow(
                column(6, downloadButton("downloadAiAudit", "AI audit log (JSON)",
                                         class = "btn-default w-100 mb-2",
                                         title = "Prompts, responses, and adopted priors for a reproducibility supplement"))
              ),
              # Meta-analytic exports have moved to Meta-Analysis > Export.
            )
          )
        )
      )
            )
          )
        )
      ),

      tabPanel(
        "Meta-Analysis", value = "meta",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            conditionalPanel(
              condition = "input.meta_nav == 'meta_data'",
              meta_data_sidebar()
            ),
            conditionalPanel(
              condition = "input.meta_nav == 'meta_model'",
              meta_model_sidebar()
            ),
            conditionalPanel(
              condition = "input.meta_nav == 'meta_fit'",
              meta_fit_sidebar()
            ),
            conditionalPanel(
              condition = "input.meta_nav == 'meta_effects'",
              meta_effects_sidebar()
            ),
            conditionalPanel(
              condition = "input.meta_nav == 'meta_perstudy'",
              meta_perstudy_sidebar()
            ),
            conditionalPanel(
              condition = "input.meta_nav == 'meta_export'",
              meta_export_sidebar()
            )
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              id = "meta_nav",
              tabPanel("Data",     value = "meta_data",    uiOutput("meta_data_tab_ui")),
              tabPanel("Model",    value = "meta_model",   uiOutput("meta_model_tab_ui")),
              tabPanel("Diagnostics", value = "meta_fit",     uiOutput("meta_fit_tab_ui")),
              tabPanel("Decisions",  value = "meta_effects", uiOutput("meta_effects_tab_ui")),
              tabPanel("Per Study", value = "meta_perstudy", uiOutput("meta_perstudy_tab_ui")),
              tabPanel("Export",   value = "meta_export",  uiOutput("meta_export_tab_ui"))
            )
          )
        )
      ),

      tabPanel(
        "Power Analysis", value = "power",
        div(id = "power-panel",
        sidebarLayout(
          sidebarPanel(width = 3,
            tags$h2("Analysis", style = "color: #0c1f4a; font-weight: 600;"),
            selectInput("power_target", "Analysis type",
              choices = c("Single-study intervention effect" = "single",
                          "Pooled meta-analysis effect" = "pooled",
                          "Difference between two study categories" = "category"), selected = "single"),
            helpText("Bayesian power simulation."),
            selectInput("power_design", "Design type",
              choices = c("AB" = "ab", "Reversal" = "reversal",
                          "Multiple Baseline" = "multiple_baseline"),
              selected = "multiple_baseline"),
            conditionalPanel(
              condition = "input.power_design == 'reversal'",
              radioButtons("power_reversal_decision", "Reversal decision",
                choices = c("All three transitions" = "all_transitions",
                            "First transition only" = "first_transition"),
                selected = "all_transitions"),
              helpText("With all transitions, each step is judged one-sided in its expected direction",
                       "and all must pass, so the Type I rate is at most 1 − threshold; read the effect = 0 row.")
            ),
            selectInput("power_family", "Outcome family",
              choices = c("Gaussian" = "gaussian",
                          "Poisson" = "poisson",
                          "Neg. binomial" = "negbinomial"),
              selected = "gaussian"),
            hr(),
            tags$h2("Sample size", style = "color: #0c1f4a; font-weight: 600;"),
            numericInput("power_n_cases", "Cases per study", value = 5, min = 1, max = 30, step = 1),
            conditionalPanel("input.power_target == 'pooled'",
              numericInput("power_n_studies", "Studies", value = 4, min = 2, max = 100, step = 1)),
            conditionalPanel("input.power_target == 'category'",
              numericInput("power_category_a", "Studies in category A", value = 4, min = 2, max = 100, step = 1),
              numericInput("power_category_b", "Studies in category B", value = 4, min = 2, max = 100, step = 1),
              numericInput("power_common_effect", "Category A intervention effect magnitude", value = 0.5, min = 0, max = 5, step = 0.1),
              helpText("Each study belongs to A or B. The effect grid specifies the B minus A difference in the benefit direction.")),
            conditionalPanel("input.power_target != 'single'",
              numericInput("power_study_sd", "Between-study effect SD", value = 0.2, min = 0, max = 5, step = 0.05)),
            numericInput("power_n_phase_a", "Baseline observations per case", value = 5, min = 3, max = 30, step = 1),
            numericInput("power_n_phase_b", "Intervention observations per case", value = 5, min = 3, max = 30, step = 1),
            hr(),
            tags$h2("Meaningful effect", style = "color: #0c1f4a; font-weight: 600;"),
            textInput("power_effect_sizes", "Effect magnitudes or category differences (up to 10)",
                      value = "0, 0.5, 1.0"),
            selectInput("power_coefficient", "Target coefficient",
              choices = c("Level change" = "level",
                          "Trend" = "trend",
                          "Slope change" = "slope_change"),
              selected = "level"),
            radioButtons("power_goal", "Intervention goal",
              choices = c("Increase" = "increase", "Decrease" = "decrease"),
              selected = "increase", inline = TRUE),
            helpText("Enter effect magnitudes of zero or greater. The intervention goal sets the direction."),
            hr(),
            tags$h2("Within-case", style = "color: #0c1f4a; font-weight: 600;"),
            div(style = "display: flex; align-items: center; gap: 6px;",
              numericInput("power_autocorr", "AR(1)", value = 0.2, min = 0, max = 0.9, step = 0.05),
              div(style = "margin-top: 24px;", info_link("ar1"))
            ),
            div(style = "display: flex; align-items: center; gap: 6px;",
              numericInput("power_icc", "ICC", value = 0.2, min = 0, max = 0.95, step = 0.05),
              div(style = "margin-top: 24px;", info_link("icc"))
            ),
            div(style = "display: flex; align-items: center; gap: 6px;",
              numericInput("power_slope_sd", "Between-case effect SD",
                           value = 0.7, min = 0, max = 2, step = 0.05),
              div(style = "margin-top: 24px;", info_link("slope_sd"))
            ),
            helpText("Effect variation uses residual-SD units for Gaussian outcomes and log-rate units for counts. Zero sets the same effect across cases."),
            hr(),
            div(style = "display: flex; align-items: center; gap: 6px;",
              tags$h2("Decision rule", style = "color: #0c1f4a; font-weight: 600; margin: 0;"),
              info_link("power_decision_rule")
            ),
            radioButtons("power_decision_rule",
              tags$span(class = "sr-only", "Decision rule"),
              choices = c("Two-sided direction detection (pd)" = "prob_direction",
                          "Meaningful benefit in the selected direction" = "prob_meaningful",
                          "Two-sided effect outside ROPE" = "rope"),
              selected = "prob_meaningful"),
            div(style = "display: flex; align-items: center; gap: 6px;",
              numericInput("power_prob_threshold", "Posterior probability threshold",
                           value = 0.975, min = 0.5, max = 0.999, step = 0.005),
              div(style = "margin-top: 24px;", info_link("pd"))
            ),
            conditionalPanel(
              condition = "input.power_decision_rule != 'prob_direction'",
              div(style = "display: flex; align-items: center; gap: 6px;",
                numericInput("power_rope", "Meaningful-effect threshold / ROPE half-width",
                             value = 0.1, min = 0.001, max = 5, step = 0.05),
                div(style = "margin-top: 24px;", info_link("rope"))
              )
            ),
            hr(),
            tags$h2("MCMC", style = "color: #0c1f4a; font-weight: 600;"),
            # Agresti-Coull MCSE sqrt(p(1-p)/(n+4)) with p = (k+2)/(n+4): at p = .5, n = 200 gives .035
            numericInput("power_n_sims", "Replicates", value = 200, min = 20, max = 500, step = 10),
            tags$p(style = "color: #64748B; font-size: 13px; margin-top: -8px;",
              "More replicates reduce simulation uncertainty."),
            numericInput("power_iter",   "Iterations", value = 2000, min = 500, max = 4000, step = 250),
            numericInput("power_seed",   "Random seed", value = 2026, min = 1, step = 1),
            hr(),
            div(style = "display: flex; align-items: center; gap: 8px;",
              actionButton("runPowerSim", "Run simulation",
                           class = "btn btn-primary",
                           icon = icon("play")),
              info_popup_link("showPowerGuide", title = "Methodology references")
            )
          ),
          mainPanel(width = 9,
            tags$p(role = "status", textOutput("powerStatus")),
            conditionalPanel(
              condition = "!output.powerReady",
              empty_state("gauge",
                'Choose settings, then select <strong style="color: #0c1f4a;">Run simulation</strong>.')
            ),
            conditionalPanel(
              condition = "output.powerReady",
              plotly_figure("powerPlot",
                "Line chart of estimated power by effect size with Monte Carlo error bars and reference lines at .50 and .80",
                height = "420px"),
              hr(),
              DT::dataTableOutput("powerTable"),
              downloadButton("downloadPowerPlan", "Export specification and results")
            )
          )
        ))
      )
    )
  ),

  # Ask AI navbar item added client-side; toggles the side panel, mounted on first open via input$askAiFab
  tags$script(HTML("
    $(document).on('shiny:connected', function(){
      if ($('.ask-ai-nav').length) return;
      var star = '<svg width=\"24\" height=\"24\" viewBox=\"0 0 24 24\" aria-hidden=\"true\" focusable=\"false\" style=\"overflow:visible;vertical-align:middle;\">'
        + '<defs><linearGradient id=\"navstar\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"1\">'
        + '<stop offset=\"0\" stop-color=\"#a855f7\"/><stop offset=\"1\" stop-color=\"#6d28d9\"/></linearGradient></defs>'
        + '<g fill=\"url(#navstar)\">'
        + '<path d=\"M11 1.5l2.1 5.9 5.9 2.1-5.9 2.1L11 17.5l-2.1-5.9L3 9.5l5.9-2.1z\"/>'
        + '<path d=\"M19 13.5l.9 2.5 2.5.9-2.5.9-.9 2.5-.9-2.5-2.5-.9 2.5-.9z\"/>'
        + '<path d=\"M5.5 15l.7 1.9 1.9.7-1.9.7-.7 1.9-.7-1.9-1.9-.7 1.9-.7z\"/></g></svg>';
      var li = $('<li class=\"ask-ai-nav\"><a href=\"#\" role=\"button\" title=\"Ask AI (optional methodology help)\" aria-controls=\"ask-panel\" aria-expanded=\"false\">'
        + star + '<span>Ask AI</span></a></li>');
      li.find('a').on('click', function(e){ e.preventDefault(); scdAsk(); })
        .on('keydown', function(e){ if (e.key === ' ') { e.preventDefault(); scdAsk(); } });
      var last = $('.navbar-nav a[data-value=\"power\"]').parent();
      if (last.length) last.after(li); else $('.navbar-nav').first().append(li);
    });
    function scdAsk(open){
      var on = open === undefined ? !document.body.classList.contains('ask-open') : open;
      document.body.classList.toggle('ask-open', on);
      $('.ask-ai-nav a').attr('aria-expanded', on);
      if (on) {
        Shiny.setInputValue('askAiFab', Date.now(), {priority:'event'});
        $('.ask-ai-nav').removeClass('has-error').find('a').attr('aria-label', 'Ask AI');
      } else $('.ask-ai-nav a').focus();
      setTimeout(function(){ $(window).trigger('resize'); }, 250);
      if (on) $('#ask_user_input').focus();
    }
    // the first open mounts the panel from the server; focus the box once Shiny binds it
    $(document).on('shiny:bound', function(e){
      if (e.target.id === 'ask_user_input' && document.body.classList.contains('ask-open')) e.target.focus();
    });
    function scdAskFix(msg){
      if (!msg) return;
      $('#ask-last-error').hide();
      scdAsk(true);
      (function put(n){
        var t = $('#ask_user_input');
        if (!t.length) return n > 0 && setTimeout(function(){ put(n - 1); }, 200);
        t.val('Explain this app message and how to fix it: ' + msg).trigger('change').focus();
      })(25);
    }
    $(document).on('keydown', function(e){
      if (e.key !== 'Escape' || !document.body.classList.contains('ask-open') || $('.modal:visible').length) return;
      if ($(e.target).closest('#ask-panel, .ask-ai-nav').length) scdAsk(false);
    });
    $(document).on('click', '.ask-chip', function(){
      $('#ask_user_input').val(this.getAttribute('data-q')).trigger('change').focus();
    });
    $(document).on('keydown', '#ask_user_input, #dialog_user_input', function(e){
      if (e.key !== 'Enter' || !(e.ctrlKey || e.metaKey)) return;
      e.preventDefault();
      var send = this.id === 'ask_user_input' ? '#askSend' : '#dialogStart';
      $(this).trigger('change');
      setTimeout(function(){ $(send).click(); }, 50);
    });
    // error and warning notifications and output errors get an Ask AI to fix button
    var scdErrTimer = null;
    function scdScanErrors(){
      clearTimeout(scdErrTimer);
      scdErrTimer = setTimeout(function(){
        $('.shiny-notification-error .shiny-notification-content-text, .shiny-notification-warning .shiny-notification-content-text, .shiny-output-error:not(.shiny-output-error-silent):not(.shiny-output-error-validation)')
          .filter(function(){ return $(this).is(':visible') && !$(this).children('.ask-fix-btn').length; })
          .each(function(){
            var el = $(this), msg = $.trim(el.text()).slice(0, 400);
            if (!msg) return;
            $('<button type=\"button\" class=\"btn btn-xs ask-fix-btn\">Ask AI to fix</button>')
              .on('click', function(){ scdAskFix(msg); }).appendTo(el);
            window.scdLastErr = msg;
            $('#ask-last-error-text').text(msg);
            $('#ask-last-error').show();
            if (!document.body.classList.contains('ask-open'))
              $('.ask-ai-nav').addClass('has-error').find('a').attr('aria-label', 'Ask AI, new app message');
          });
      }, 300);
    }
    // streaming log text (pre, verbatim output) never holds the error elements, so it does not trigger a scan
    new MutationObserver(function(muts){
      if (muts.some(function(m){ return m.type === 'attributes' || !$(m.target).closest('pre, .shiny-text-output').length; }))
        scdScanErrors();
    }).observe(document.body, {childList: true, subtree: true, attributes: true, attributeFilter: ['class']});
    $(document).on('shown.bs.tab', scdScanErrors);
  ")),

  tags$aside(id = "ask-panel", class = "ask-panel", role = "complementary", `aria-label` = "Ask AI",
    tags$div(class = "ask-panel-head",
      tags$span(.ask_star_svg(20, "panelstar"), " Ask AI"),
      tags$button(type = "button", class = "ask-panel-close", `aria-label` = "Close Ask AI",
                  onclick = "scdAsk(false)", HTML("&times;"))),
    tags$div(id = "ask-last-error", class = "ask-last-error", role = "status", style = "display: none;",
      tags$span(id = "ask-last-error-text"),
      tags$button(type = "button", class = "btn btn-xs ask-fix-btn",
                  onclick = "scdAskFix(window.scdLastErr)", "Ask AI to fix"),
      tags$button(type = "button", class = "ask-err-dismiss", `aria-label` = "Dismiss message",
                  onclick = "$('#ask-last-error').hide()", HTML("&times;"))),
    tags$div(id = "ask-panel-body", class = "ask-panel-body")),

  # stable, shareable URLs for top-level and Home tabs (#meta-analysis, #quick-start, ...)
  # and download links announced as unavailable until their result exists
  tags$script(HTML("
    (function(){
      var MAIN = {'home':'home', 'single-case-design':'scd', 'meta-analysis':'meta', 'power-analysis':'power'};
      var HOME = ['about','features','data-format','quick-start','cybersecurity','resources','support'];
      var HOME_ID = {'cybersecurity':'cyber'};
      function slugOf(map, v){ for (var k in map) if (map[k] === v) return k; return null; }
      function homeTarget(slug){ return '#' + (HOME_ID[slug] || slug) + '-tab'; }
      function openHash(){
        var h = (location.hash || '').replace(/^#/, '').toLowerCase();
        if (MAIN[h]) { $('#main_nav a[data-value=\"' + MAIN[h] + '\"]').tab('show'); return; }
        if (HOME.indexOf(h) >= 0) {
          $('#main_nav a[data-value=\"home\"]').tab('show');
          $('#home_nav_menu a[href=\"' + homeTarget(h) + '\"]').tab('show');
        }
      }
      function setHash(slug){
        if (!window.history || !history.replaceState) return;
        history.replaceState(null, '', slug ? '#' + slug : location.pathname + location.search);
      }
      $(document).on('shiny:connected', function(){ setTimeout(openHash, 0); });
      $(window).on('hashchange', openHash);
      $(document).on('shown.bs.tab', '#main_nav a[data-toggle=\"tab\"]', function(){
        var slug = slugOf(MAIN, this.getAttribute('data-value'));
        if (slug) setHash(slug === 'home' ? '' : slug);
      });
      $(document).on('shown.bs.tab', '#home_nav_menu a[data-toggle=\"tab\"]', function(){
        var id = (this.getAttribute('href') || '').replace(/^#/, '').replace(/-tab$/, '');
        for (var k in HOME_ID) if (HOME_ID[k] === id) id = k;
        setHash(id === 'about' ? '' : id);
      });

      function syncDownloads(){
        $('a.shiny-download-link').each(function(){
          var off = !this.getAttribute('href');
          this.setAttribute('aria-disabled', off ? 'true' : 'false');
          if (off) this.setAttribute('tabindex', '-1'); else this.removeAttribute('tabindex');
        });
      }
      function syncTabState(){
        $('a[data-toggle=\"tab\"][role=\"tab\"]').each(function(){
          var selected = $(this).parent().hasClass('active');
          $(this).attr({'aria-selected': String(selected), 'aria-expanded': String(selected)});
        });
      }
      $(document).on('shiny:connected shiny:value shiny:recalculated shown.bs.tab', function(){
        setTimeout(function(){ syncDownloads(); syncTabState(); }, 0);
      });
    })();
  "))
)
