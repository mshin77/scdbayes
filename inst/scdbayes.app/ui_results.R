# Peer tabs under Single-Case Design: Fit, Effects, Per Case.
# empty_state() is defined in global.R; this file uses it directly.

stale_banner <- function(refit_id = "refitFromResults") {
  conditionalPanel(
    condition = "output.modelStale",
    div(class = "status-main-warning",
        "Model settings changed since last fit. Results may be outdated.",
        actionButton(refit_id, "Refit", class = "btn-sm btn-primary",
                     style = "margin-left: 8px;")))
}

# Fit
fit_sidebar <- function() {
  tagList(
    tags$h3("Trace Plots", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    uiOutput("traceParamSelector"),
    hr(),
    tags$h3("Posterior Predictive", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    numericInput("pp_ndraws", "Draws", value = 50, min = 10, max = 500, step = 10),
    hr(),
    tags$h3("Visual-analysis checks", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    checkboxGroupInput("ppc_stats", "Features",
      choices = list(
        "Level (mean)"        = "level",
        "Trend (slope)"       = "trend",
        "Variability (SD)"    = "variability",
        "Absence (≤ 0)"  = "absence",
        "Immediacy (TSTE)"    = "immediacy",
        "Non-overlap (Tau)"   = "nonoverlap",
        "Autocorrelation"     = "autocorrelation",
        "Initial performance" = "initial"),
      selected = c("initial", "level", "trend", "variability", "absence",
                   "immediacy", "nonoverlap", "autocorrelation")),
    radioButtons("ppc_mode", "Simulate",
      choices = list("Same participants" = "same", "New participants" = "new"),
      selected = "same"),
    tags$p(style = "font-size: 13px; color: #64748B; margin: 4px 0 8px;",
      "Same: fit for the observed cases. New: draw new case effects from the population posterior (generalization)."),
    numericInput("ppc_ndraws", "Draws", value = 500, min = 50, max = 1000, step = 50),
    uiOutput("ppcOptionsUI"),
    actionButton("runPpcFeatures", "Run checks", class = "btn-primary w-100"),
    div(style = "margin-top: 8px;",
      downloadButton("downloadPpcSummary", "Summary CSV", class = "btn-default btn-sm w-100 mb-1"),
      downloadButton("downloadPpcDraws", "Draws CSV", class = "btn-default btn-sm w-100"))
  )
}

fit_main <- function() {
  tagList(
    stale_banner("refitFromFit"),
    conditionalPanel(
      condition = "!output.modelFitted",
      empty_state("trace",
        'Click <strong style="color: #0c1f4a;">Fit</strong> in the Model tab to generate results')
    ),
    conditionalPanel(
      condition = "output.modelFitted",
      uiOutput("samplerHealthBanner"),
      fluidRow(
        column(6, class = "col-xs-12 col-md-3 col-lg-2", div(class = "stat-box",
          div(class = "stat-value", textOutput("rhatMax")),
          div(class = "stat-label", "Max R-hat ", info_link("rhat")))),
        column(6, class = "col-xs-12 col-md-3 col-lg-2", div(class = "stat-box",
          div(class = "stat-value", textOutput("essMin")),
          div(class = "stat-label", "Min ESS ", info_link("ess")))),
        column(6, class = "col-xs-12 col-md-3 col-lg-2", div(class = "stat-box",
          div(class = "stat-value", textOutput("bayesR2")),
          div(class = "stat-label", "Bayes R-sq ", info_link("bayes_r2")))),
        column(6, class = "col-xs-12 col-md-3 col-lg-2", div(class = "stat-box",
          div(class = "stat-value", textOutput("looic")),
          div(class = "stat-label", "LOO-IC ", info_link("looic"))))
      ),
      verbatimTextOutput("modelSummary"),
      hr(),
      fluidRow(
        column(6, verbatimTextOutput("convergenceDiagnostics")),
        column(6, verbatimTextOutput("modelFitStats"))
      ),
      tabsetPanel(
        id = "fit_tabs", type = "pills",
        tabPanel("Trace", uiOutput("tracePlotsBox")),
        tabPanel("PP Check",
          tags$p(style = "font-size: 13px; color: #475569; margin: 8px 0 12px;",
            "Compares model-simulated data with the observed data. ",
            "Look for systematic misfit, not a pass/fail cutoff."),
          tabsetPanel(
            type = "pills",
            tabPanel("Visual features",
              tags$p(style = "font-size: 13px; color: #475569; margin: 8px 0 10px;",
                info_link("ppc_features"),
                "Checks visual-analysis features; p below 0.025 or above 0.975 flags misfit. ",
                "Pick features, then Run checks."),
              plotOutput("ppcOverlay", height = "auto"),
              hr(),
              plotOutput("ppcFeatureDensity", height = "460px"),
              hr(),
              uiOutput("ppcCaseStatUI"),
              plotOutput("ppcCaseDensity", height = "auto"),
              hr(),
              plotOutput("ppcImmediacy", height = "auto"),
              hr(),
              DT::dataTableOutput("ppcFeatureTable")),
            tabPanel("Density", plotOutput("ppCheckDensity", height = "450px")),
            tabPanel("Stat", plotOutput("ppCheckStat", height = "450px")),
            tabPanel("By Group", plotOutput("ppCheckGrouped", height = "450px")),
            tabPanel("By Case", plotOutput("ppCheckByCase", height = "auto"))
          )),
        tabPanel("Residuals", plotly_figure("residualPlots",
          "Scatter plot of residuals against fitted values with a loess smoother and zero reference line",
          height = "500px"))
      )
    )
  )
}

# Effects
effects_sidebar <- function() {
  tagList(
    numericInput("results_ci_level", "Credible Interval (%)",
                 value = 95, min = 50, max = 99.9, step = 0.1),
    numericInput("results_digits", "Decimal Places",
                 value = 3, min = 1, max = 6, step = 1),
    hr(),
    tags$h3("Posterior decisions", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    numericInput("results_mcid", "Minimum clinically important difference (optional)",
                 value = NULL, min = 0, step = 0.1),
    helpText("Smallest meaningful effect, in link units. Adds P(effect > MCID); leave blank to skip.")
  )
}

effects_main <- function() {
  tagList(
    stale_banner("refitFromEffects"),
    conditionalPanel(
      condition = "!output.modelFitted",
      empty_state("forest",
        'Click <strong style="color: #0c1f4a;">Fit</strong> in the Model tab to view effects')
    ),
    conditionalPanel(
      condition = "output.modelFitted",
      tabsetPanel(
        id = "effectsTabset", type = "pills",
        tabPanel("Fixed Effects",
          uiOutput("ratioNoteFixed"),
          DT::dataTableOutput("fixedEffectsTable"),
          plotly_figure("fixedEffectsPlot",
            "Coefficient plot of fixed-effect posterior means with credible intervals", height = "500px")),
        tabPanel("Random Effects",
          fluidRow(
            column(6, verbatimTextOutput("varianceComponents")),
            column(6, verbatimTextOutput("iccResults"))
          ),
          plotly_figure("varianceDecomposition",
            "Bar chart of the proportion of variance attributed to each random-effect component", height = "400px")),
        tabPanel("Posterior decisions",
          tags$h2("Fixed-effect posterior decisions", class = "section-header"),
          uiOutput("ratioNoteDecisions"),
          DT::dataTableOutput("posteriorESTable"),
          tags$h2("Per-case posterior decisions",
                  style = "color: #0c1f4a; font-weight: 600; margin-top: 20px;"),
          DT::dataTableOutput("perCaseESTable"),
          tags$h2("Variance components",
                  style = "color: #0c1f4a; font-weight: 600; margin-top: 20px;"),
          DT::dataTableOutput("varianceComponentsTable"),
          plotly_figure("effectSizePlot",
            "Posterior decision summaries for treatment terms with credible intervals", height = "500px")),
        tabPanel("Phase Effects", plotly_figure("phaseEffects",
          "Outcome by phase with phase-level estimates", height = "500px")),
        tabPanel("Marginal contrast", uiOutput("marginalContrastPanel")),
        tabPanel("Interactions",  uiOutput("crossLevelPanel")),
        tabPanel("Predictions",   uiOutput("posteriorPredictionsPanel"))
      )
    )
  )
}

# Per Case
percase_sidebar <- function() {
  tagList(
    tags$h3("Custom Hypothesis", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    textInput("customHypothesis", tags$span(class = "sr-only", "Custom hypothesis"),
              placeholder = "e.g., time_A + trend_AB > 0"),
    actionButton("runCustomHypothesis", "Test", class = "btn-primary w-100"),
    hr(),
    tags$h3("Model Comparison", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    actionButton("runModelComparison", "Compare", class = "btn-primary w-100 mb-3"),
    numericInput("boot_n", "Posterior draws", value = 200, min = 50, max = 1000, step = 50),
    actionButton("runBootstrap", "Variance components / ICC", class = "btn-primary w-100"),
    hr(),
    checkboxInput("run_sensitivity", "Sensitivity analyses", value = FALSE),
    conditionalPanel(
      condition = "input.run_sensitivity",
      checkboxGroupInput("sensitivity_types", "Analyses:",
        choices = c("Treatment prior (default / narrow / diffuse)" = "prior",
                    "Student-t likelihood" = "outlier"),
        selected = "prior"))
  )
}

percase_main <- function() {
  tagList(
    stale_banner("refitFromPerCase"),
    conditionalPanel(
      condition = "!output.modelFitted",
      empty_state("cases",
        'Click <strong style="color: #0c1f4a;">Fit</strong> in the Model tab to view per-case results')
    ),
    conditionalPanel(
      condition = "output.modelFitted",
      tabsetPanel(
        id = "percaseTabset", type = "pills",
        tabPanel("Trajectories", plotly_figure("individualTrajectories",
          "Observed and model-fitted outcome trajectories for each case", height = "auto")),
        tabPanel("Hypothesis Tests",
          DT::dataTableOutput("hypothesisTestTable"),
          hr(),
          DT::dataTableOutput("customHypothesisTable")),
        tabPanel("Model Comparison",
          fluidRow(
            column(6, verbatimTextOutput("modelComparisonResults")),
            column(6, verbatimTextOutput("bootstrapResults"))
          ),
          DT::dataTableOutput("modelComparisonTable"),
          conditionalPanel(
            condition = "input.run_sensitivity",
            hr(),
            tags$h2("Sensitivity Analysis",
                    style = "color: #0c1f4a; font-weight: 600; margin-top: 10px;"),
            verbatimTextOutput("sensitivityResults"),
            plotly_figure("sensitivityPlots",
              "Treatment effect estimates compared across sensitivity analyses", height = "400px")
          ),
          hr(),
          tags$h2("Sequential Bayesian Updating",
                  style = "color: #0c1f4a; font-weight: 600; margin-top: 10px;"),
          tags$p(style = "font-size: 15px; color: #374151; margin-bottom: 10px;",
            "Refit the model adding one case at a time to track how the effect estimate stabilizes."),
          actionButton("runSequentialUpdate", "Update", class = "btn-primary mb-3"),
          plotly_figure("sequentialUpdatePlot",
            "Line chart of the phase effect estimate and credible band as cases are added", height = "400px"),
          DT::dataTableOutput("sequentialUpdateTable"))
      )
    )
  )
}

# Export
export_sidebar <- function() {
  tagList(
    tags$h3("Report options", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    selectInput("report_format", "Format",
      choices = c("HTML" = "html", "PDF" = "pdf", "Word" = "docx"),
      selected = "html"),
    checkboxGroupInput("report_sections", "Sections",
      choices = c("Methods" = "methods", "Results" = "results", "Tables" = "tables",
                  "Figures" = "figures", "Diagnostics" = "diagnostics",
                  "AI audit log appendix (when AI was used)" = "ai_audit"),
      selected = c("methods", "results", "tables", "figures")),
    shinyjs::disabled(downloadButton("downloadReport", "Download report", class = "btn-primary w-100")),
    helpText("Report, model, script, and decisions downloads are available after a fit.")
  )
}

