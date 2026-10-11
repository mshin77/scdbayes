# Sidebar / main fragments for Meta-Analysis sub-tabs.
# All input/output IDs use the `meta_` prefix to keep state independent from
# the Single-Case Design tab and avoid DOM collisions.

.meta_empty <- function(icon_class, msg) {
  empty_state(icon_class, msg, max_width = "480px")
}

.meta_stale_banner <- function(refit_id = "meta_refitFromResults") {
  conditionalPanel(
    condition = "output.metaModelStale",
    div(class = "status-main-warning",
        "Model settings changed since last fit. Results may be outdated.",
        actionButton(refit_id, "Refit", class = "btn-sm btn-primary",
                     style = "margin-left: 8px;")))
}

meta_data_sidebar <- function() {
  data_sidebar("meta_", c(
    "Select a dataset" = "",
    "Meta-Analysis (4 studies)" = "meta"
  ), study_required = TRUE)
}

meta_model_sidebar <- function() {
  tagList(
    actionButton("meta_runModel", "Fit",
                class = "btn btn-primary btn-block"),
    conditionalPanel(
      condition = "output.metaModelFitted && !output.metaModelRefitting && !output.metaModelStale",
      div(class = "status-sidebar-success", "Model fitted")),
    conditionalPanel(
      condition = "output.metaModelRefitting",
      div(class = "status-sidebar-warning", "Re-fitting...")),
    conditionalPanel(
      condition = "output.metaModelStale && !output.metaModelRefitting",
      div(class = "status-sidebar-warning", "Settings changed; click Fit to update.")),
    hr(),

    selectInput("meta_research_design", "Design Type",
      choices = list("AB Design" = "ab", "Reversal (ABAB)" = "reversal",
                     "Multiple Baseline" = "multiple_baseline",
                     "Multiple Probe" = "multiple_probe",
                     "Alternating Treatment" = "alternating",
                     "Changing Criterion" = "changing_criterion"),
      selected = "multiple_baseline"),
    hr(),

    selectInput("meta_intervention_goal", "Intervention goal",
      choices = list(
        "Increase desirable behavior" = "increase",
        "Decrease undesirable behavior" = "decrease"
      ),
      selected = "increase"),
    selectInput("meta_outcome_type", "Outcome type",
      choices = list(
        "Continuous (test scores, percent on task)" = "continuous",
        "Count (frequency of behavior, words read)" = "count",
        "Yes/no (target behavior occurred)" = "binary",
        "Percent / proportion (decimal 0-1)" = "proportion",
        "Other (pick a specific distribution)" = "other"
      ),
      selected = "continuous"),
    conditionalPanel(
      condition = "input.meta_outcome_type == 'other'",
      selectInput("meta_model_family", "Specific distribution",
        choices = list(
          "Continuous" = list("Gaussian" = "gaussian", "Student-t (heavy-tailed)" = "student",
                              "Skew-normal" = "skew_normal", "Gamma (log link)" = "Gamma"),
          "Count" = list("Poisson" = "poisson", "Neg. Binomial" = "negbinomial",
                         "Zero-infl. Poisson" = "zero_inflated_poisson"),
          "Proportion" = list("Beta" = "beta", "Binomial (successes / trials)" = "binomial"),
          "Ordinal" = list("Cumulative logit" = "cumulative"),
          "Binary" = list("Bernoulli" = "bernoulli")),
        selected = "gaussian"),
      conditionalPanel(
        condition = "input.meta_model_family == 'binomial'",
        selectInput("meta_trials_var", "Trials column", choices = NULL))),
    conditionalPanel(
      condition = paste("input.meta_outcome_type == 'count' ||",
                        "(input.meta_outcome_type == 'other' &&",
                        "['poisson', 'negbinomial', 'zero_inflated_poisson', 'zero_inflated_negbinomial']",
                        ".indexOf(input.meta_model_family) >= 0)"),
      selectInput("meta_count_outcome_units", "Recorded outcome units", choices = c("Counts" = "count", "Rate per exposure unit" = "rate")),
      selectInput("meta_exposure_var", "Exposure (offset) column", choices = c("None" = "")),
      helpText("Rate values are multiplied by exposure and rounded to counts. Counts remain unchanged."),
      helpText("Adds offset(log(column)) so effects describe the rate per unit of exposure (e.g., session minutes).")),
    div(style = "margin: -6px 0 10px; font-size: 13px; color: #64748B;",
      info_link("mixed_outcomes"), " Outcomes of different types or scales"),
    selectInput("meta_measure_var", "Measure column (fit a subset)",
                choices = c("(none: all rows)" = "")),
    conditionalPanel(
      condition = "input.meta_measure_var != ''",
      selectInput("meta_measure_value", "Fit on", choices = NULL, multiple = TRUE)),
    div(style = "display: flex; align-items: center; gap: 6px;",
      checkboxInput("meta_standardize", "Standardize outcome within case (residual SD)",
                    value = FALSE),
      info_link("standardize_case")),
    uiOutput("meta_standardizeNotice"),
    hr(),
    actionButton("meta_resetSettings", "Reset",
                 icon = icon("arrow-rotate-left"),
                 class = "btn btn-default btn-block btn-sm",
                 title = "Restore defaults")
  )
}

meta_fit_sidebar <- function() {
  tagList(
    tags$h3("Trace Plots", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    uiOutput("meta_traceParamSelector"),
    hr(),
    tags$h3("Posterior Predictive", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    numericInput("meta_pp_ndraws", "Draws", value = 50, min = 10, max = 500, step = 10)
  )
}

meta_effects_sidebar <- function() {
  tagList(
    numericInput("meta_ci_level", "Credible Interval (%)",
      value = 95, min = 50, max = 99.9, step = 0.1),
    hr(),
    tags$h3("Forest", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    selectInput("meta_forest_sort", "Sort By",
      choices = list("Effect size" = "effect", "Name" = "name"),
      selected = "effect"),
    sliderInput("meta_forest_height", "Minimum plot height (px)",
      min = 300, max = 1000, value = 500, step = 50),
    hr(),
    tags$h3("Sensitivity", style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    checkboxInput("meta_run_sensitivity", "Sensitivity analyses", value = FALSE),
    conditionalPanel(
      condition = "input.meta_run_sensitivity",
      checkboxGroupInput("meta_sensitivity_types", "Analyses:",
        choices = c("Treatment prior (default / narrow / diffuse)" = "prior",
                    "Student-t likelihood" = "outlier"),
        selected = "prior"))
  )
}

meta_perstudy_sidebar <- function() {
  tagList(
    tags$h3("Custom Hypothesis", id = "meta_customHypLabel",
            style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    tags$div(`aria-labelledby` = "meta_customHypLabel",
        textInput("meta_customHypothesis", tags$span(class = "sr-only", "Custom hypothesis"),
                  placeholder = "e.g., time_A + trend_AB > 0")),
    actionButton("meta_runCustomHypothesis", "Test", class = "btn-primary w-100")
  )
}

meta_export_sidebar <- function() {
  tagList(
    tags$h3("Effect sizes ", info_link("first_stage_es"),
            style = "color: #64748B; font-weight: 600; font-size: 13px;"),
    tags$p(style = "font-size: 13px; color: #64748B;",
      "LRR1, LRR2, NAP, and Tau with sampling variances for every A-to-B contrast.",
      "The R templates read this file."),
    downloadButton("downloadMetaES", "Download first-stage ES", class = "btn-primary w-100"),
    tags$p(style = "font-size: 13px; color: #64748B; margin-top: 12px;",
      "Posterior case effects from the multilevel model are shrunken and correlated: descriptive",
      "only, not for re-pooling."),
    downloadButton("downloadMetaPosteriorES", "Download posterior case effects (descriptive)",
                   class = "btn-default w-100")
  )
}

meta_model_main <- function(data = NULL, outcome = NULL, moderators = NULL) {
  columns <- names(data)
  numeric_columns <- columns[vapply(data, is.numeric, logical(1))]
  selected_outcome <- intersect(outcome, numeric_columns)
  if (!length(selected_outcome)) selected_outcome <- c(intersect("outcome", numeric_columns), numeric_columns)[1]
  tagList(
    .meta_stale_banner("meta_refitFromModel"),
    conditionalPanel(
      condition = "!output.hasMultipleStudies",
      .meta_empty("studies",
        "Load a multi-study dataset in Meta-Analysis > Data first.")
    ),
    conditionalPanel(
      condition = "output.hasMultipleStudies",
      wellPanel(
        style = "background-color: #f9fafb; border: 1px solid #e5e7eb; padding: 12px;",
        tags$h2("Model Specification",
                style = "color: #0c1f4a; font-weight: 600; margin: 0 0 6px;"),
        withMathJax(),
        uiOutput("meta_modelEquation"),
        uiOutput("meta_modelEquationLatex")
      ),
      conditionalPanel(
        condition = "input.meta_standardize",
        wellPanel(
          style = "background-color: #f9fafb; border: 1px solid #e5e7eb; padding: 12px;",
          tags$h2("Within-case residual SD ", info_link("standardize_case"),
                  style = "color: #0c1f4a; font-weight: 600; margin: 0 0 6px;"),
          tags$p(style = "font-size: 13px; color: #374151; margin: 0 0 8px;",
            "Each tier's outcome is divided by its residual SD.",
            "Biased under autocorrelation: use as a sensitivity analysis."),
          uiOutput("meta_stdWarning"),
          DT::dataTableOutput("meta_stdSigmaTable"))),
      tabsetPanel(id = "meta_model_tabs",
      tabPanel("Fixed Effects",
        selectInput("meta_outcome_var", "Outcome Variable", choices = numeric_columns, selected = selected_outcome),
        radioButtons("meta_formula_mode", "Formula Mode",
          choices = list("Guided" = "guided", "Custom" = "custom"),
          selected = "guided", inline = TRUE),
        conditionalPanel(
          condition = "input.meta_formula_mode == 'guided'",
          uiOutput("meta_piecewiseTermsUI"),
          selectInput("meta_moderator_vars", "Moderators", choices = columns,
                      selected = intersect(moderators, columns), multiple = TRUE),
          checkboxInput("meta_cross_level_interactions", "Cross-level interactions", value = FALSE)),
        conditionalPanel(
          condition = "input.meta_formula_mode == 'custom'",
          textAreaInput("meta_custom_fixed_formula", tags$span(class = "sr-only", "Custom fixed-effects formula"),
            placeholder = "e.g., phase * (designType) + complexityMeasure",
            rows = 2, width = "100%")),
        checkboxInput("meta_center_moderators", "Center moderator variables", value = FALSE),
        checkboxInput("meta_scale_moderators", "Scale moderator variables", value = FALSE)
      ),
      tabPanel("Random Effects",
        tags$p(style = "color: #64748B; font-size: 13px; margin: 8px 0;",
          "Three-level structure: case nested within study."),
        unifiedRandomEffectsUI("metaRandomEffects"),
        hr(),
        checkboxInput("meta_include_ar1", "AR(1) autocorrelation", value = FALSE),
        conditionalPanel(condition = "input.meta_include_ar1",
          uiOutput("meta_arSpacingWarning"),
          checkboxInput("meta_ar_covariance", "Estimate AR covariance", value = FALSE),
          numericInput("meta_ar_prior_sd", "AR(1) prior SD",
                       value = 0.5, min = 0.1, max = 2, step = 0.1)),
        checkboxInput("meta_heterogeneous_variance",
                      "Heterogeneous variance across phases", value = FALSE)
      ),
      tabPanel("MCMC",
        fluidRow(
          column(6,
            numericInput("meta_n_chains", "Chains", value = 2, min = 2, max = 8),
            numericInput("meta_n_iter", "Iterations", value = 2000, min = 1500, max = 20000, step = 500),
            numericInput("meta_n_warmup", "Warmup", value = 1000, min = 500, max = 10000, step = 250)
          ),
          column(6,
            div(style = "display: flex; align-items: center; gap: 6px;",
              numericInput("meta_adapt_delta", "Adapt Delta", value = 0.95, min = 0.80, max = 0.999, step = 0.01),
              div(style = "margin-top: 24px;", info_link("adapt_delta"))
            ),
            numericInput("meta_max_treedepth", "Max Tree Depth", value = 12, min = 8, max = 15),
            checkboxInput("meta_parallel_chains", "Parallel chains", value = TRUE)
          )
        )
      ),
      tabPanel("Priors",
        fluidRow(
          column(6,
            div(style = "display: flex; align-items: center; gap: 8px;",
              tags$label("Prior Type", class = "control-label"),
              info_popup_link("meta_showPriorGuide", title = "Prior choice guide"),
              actionLink("meta_showPriorWizard", "Help me pick",
                         style = "color: #337ab7; font-size: 13px; margin-left: 8px;",
                         title = "3-question decision tree")
            ),
            radioButtons("meta_prior_type", tags$span(class = "sr-only", "Prior type"),
              choices = list("Weakly informative" = "default",
                             "Informative" = "informative",
                             "Meta-analytic" = "meta_analytic",
                             "Sensitivity" = "sensitivity"),
              selected = "default"),
            conditionalPanel(
              condition = "input.meta_prior_type == 'meta_analytic' || input.meta_prior_type == 'informative'",
              helpText("Enter the prior mean on the link scale. The informative prior uses half the weakly informative SD."),
              numericInput("meta_prior_effect_mean", "Expected effect (prior mean, link scale)",
                           value = 0, min = -3, max = 3, step = 0.1)),
            conditionalPanel(
              condition = "input.meta_prior_type == 'meta_analytic'",
              numericInput("meta_prior_effect_sd", "Prior Uncertainty (SD, link scale)",
                           value = 0.3, min = 0.1, max = 2, step = 0.1)),
            hr(),
            div(style = "display: flex; flex-wrap: wrap; gap: 6px; align-items: center;",
              actionButton("meta_runPriorPredictive", "Preview",
                           icon = icon("eye"), class = "btn-default btn-sm",
                           title = "sample_prior='only' brm + pp_check"),
              actionButton("meta_showPriorOverride", "Edit priors",
                           icon = icon("pen"), class = "btn-default btn-sm",
                           title = "Per-class override"),
              actionButton("meta_applyReplicationPreset", "Replication",
                           icon = icon("clone"), class = "btn-default btn-sm",
                           title = "Normal(earlier estimate, inflation x earlier SE) on the treatment coefficient"),
              actionButton("meta_askAiPriors", tagList(icon("wand-magic-sparkles"), "Co-design priors (AI)"),
                           class = "btn-primary btn-sm",
                           title = "Optional: elicit priors in a dialog with an AI assistant")
            )
          ),
          column(6,
            plotOutput("meta_priorsPlot", height = "300px"),
            conditionalPanel(
              condition = "output.meta_priorPredictiveReady",
              hr(),
              tags$h3("Prior predictive check",
                      style = "color: #64748B; font-weight: 600; font-size: 13px;"),
              plotOutput("meta_priorPredictivePlot", height = "260px"),
              uiOutput("meta_priorPredictiveNote")
            )
          )
        ),
        verbatimTextOutput("meta_priorDisplay")
      )
    )
    )
  )
}

meta_fit_main <- function() {
  tagList(
    .meta_stale_banner("meta_refitFromFit"),
    conditionalPanel(
      condition = "!output.hasMultipleStudies",
      .meta_empty("studies",
        "Load a multi-study dataset in Meta-Analysis > Data first.")
    ),
    conditionalPanel(
      condition = "output.hasMultipleStudies && !output.metaModelFitted",
      .meta_empty("trace",
        'Click <strong style="color: #0c1f4a;">Fit</strong> in the Model tab to generate results')
    ),
    conditionalPanel(
      condition = "output.hasMultipleStudies && output.metaModelFitted",
      fluidRow(
        column(6, class = "col-xs-12 col-md-3 col-lg-2", div(class = "stat-box",
          div(class = "stat-value", textOutput("meta_rhatMax")),
          div(class = "stat-label", "Max R-hat"))),
        column(6, class = "col-xs-12 col-md-3 col-lg-2", div(class = "stat-box",
          div(class = "stat-value", textOutput("meta_essMin")),
          div(class = "stat-label", "Min ESS"))),
        column(6, class = "col-xs-12 col-md-3 col-lg-2", div(class = "stat-box",
          div(class = "stat-value", textOutput("meta_bayesR2")),
          div(class = "stat-label", "Bayes R-sq"))),
        column(6, class = "col-xs-12 col-md-3 col-lg-2", div(class = "stat-box",
          div(class = "stat-value", textOutput("meta_looic")),
          div(class = "stat-label", "LOO-IC")))
      ),
      verbatimTextOutput("meta_modelSummary"),
      hr(),
      tabsetPanel(
        id = "meta_fit_tabs", type = "pills",
        tabPanel("Trace", uiOutput("meta_tracePlotsBox")),
        tabPanel("PP Check",
          tabsetPanel(
            type = "pills",
            tabPanel("Density", plotOutput("meta_ppCheckDensity", height = "450px")),
            tabPanel("Stat",    plotOutput("meta_ppCheckStat",    height = "450px")),
            tabPanel("By Group",plotOutput("meta_ppCheckGrouped", height = "450px")),
            tabPanel("By Case", plotOutput("meta_ppCheckByCase",  height = "450px"))
          )),
        tabPanel("Residuals", plotly_figure("meta_residualPlots",
          "Scatter plot of residuals against fitted values with a zero reference line", height = "500px"))
      )
    )
  )
}

meta_perstudy_main <- function() {
  tagList(
    .meta_stale_banner("meta_refitFromPerStudy"),
    conditionalPanel(
      condition = "!output.hasMultipleStudies",
      .meta_empty("studies",
        "Load a multi-study dataset in Meta-Analysis > Data first.")
    ),
    conditionalPanel(
      condition = "output.hasMultipleStudies && !output.metaModelFitted",
      .meta_empty("cases",
        'Click <strong style="color: #0c1f4a;">Fit</strong> in Meta-Analysis > Model to view per-study effects.')
    ),
    conditionalPanel(
      condition = "output.hasMultipleStudies && output.metaModelFitted",
      tabsetPanel(
        id = "meta_perstudyTabset", type = "pills",
        tabPanel("Estimates",
          DT::dataTableOutput("perStudyEffectsTable"),
          plotly_figure("perStudyEffectsPlot",
            "Dot plot of per-study treatment effect posterior means with credible intervals", height = "auto")),
        tabPanel("Hypothesis Tests",
          DT::dataTableOutput("meta_hypothesisTestTable"),
          hr(),
          DT::dataTableOutput("meta_customHypothesisTable"))
      )
    )
  )
}

meta_effects_main <- function() {
  tagList(
    .meta_stale_banner("meta_refitFromEffects"),
    conditionalPanel(
      condition = "!output.hasMultipleStudies",
      .meta_empty("studies",
        "Load a multi-study dataset in Meta-Analysis > Data first.")
    ),
    conditionalPanel(
      condition = "output.hasMultipleStudies && !output.metaModelFitted",
      .meta_empty("forest",
        'Click <strong style="color: #0c1f4a;">Fit</strong> in Meta-Analysis > Model to render the meta-analytic views.')
    ),
    conditionalPanel(
      condition = "output.hasMultipleStudies && output.metaModelFitted",
      tabsetPanel(
        id = "meta_effectsTabset", type = "pills",
        tabPanel("Forest",
          plotly_figure("forestPlot",
            "Forest plot of per-case treatment effects with credible intervals", height = "auto")),
        tabPanel("Heterogeneity",
          verbatimTextOutput("heterogeneityResults")),
        tabPanel("Sensitivity",
          conditionalPanel(
            condition = "!input.meta_run_sensitivity",
            .meta_empty("sliders",
              'Tick <strong style="color: #0c1f4a;">Sensitivity analyses</strong> in the sidebar to refit with default, narrow, and diffuse treatment priors and a Student-t likelihood.')
          ),
          conditionalPanel(
            condition = "input.meta_run_sensitivity",
            verbatimTextOutput("meta_sensitivityResults"),
            plotly_figure("meta_sensitivityPlots",
              "Treatment effect estimates compared across sensitivity analyses", height = "400px")
          ))
      )
    )
  )
}
