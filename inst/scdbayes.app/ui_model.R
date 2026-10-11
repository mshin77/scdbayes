model_sidebar <- function() {
  tagList(
    actionButton("runModel", "Fit",
                class = "btn btn-primary btn-block"),
    conditionalPanel(
      condition = "output.modelFitted && !output.modelRefitting && !output.modelStale",
      div(class = "status-sidebar-success", "Model fitted")),
    conditionalPanel(
      condition = "output.modelRefitting",
      div(class = "status-sidebar-warning", "Re-fitting...")),
    conditionalPanel(
      condition = "output.modelStale && !output.modelRefitting",
      div(class = "status-sidebar-warning", "Settings changed; click Fit to update.")),
    hr(),

    selectInput("research_design", "Design Type",
      choices = list("AB Design" = "ab", "Reversal (ABAB)" = "reversal",
                     "Multiple Baseline" = "multiple_baseline",
                     "Multiple Probe" = "multiple_probe",
                     "Alternating Treatment" = "alternating",
                     "Changing Criterion" = "changing_criterion"),
      selected = "ab"),
    conditionalPanel(
      condition = "output.dataUploaded",
      div(class = "status-sidebar-info", uiOutput("designAiRecommendation")),
      actionLink("reDetectDesign", "Redetect",
                 icon = icon("rotate-right"),
                 style = "font-size: 13px; margin-top: 4px;")),
    hr(),

    selectInput("intervention_goal", "Intervention goal",
      choices = list(
        "Increase desirable behavior" = "increase",
        "Decrease undesirable behavior" = "decrease"
      ),
      selected = "increase"),
    selectInput("outcome_type", "Outcome type",
      choices = list(
        "Continuous (test scores, percent on task)" = "continuous",
        "Count (frequency of behavior, words read)" = "count",
        "Yes/no (target behavior occurred)" = "binary",
        "Percent / proportion (decimal 0-1)" = "proportion",
        "Other (pick a specific distribution)" = "other"
      ),
      selected = "continuous"),
    conditionalPanel(
      condition = "input.outcome_type == 'other'",
      selectInput("model_family", "Specific distribution",
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
        condition = "input.model_family == 'binomial'",
        selectInput("trials_var", "Trials column", choices = NULL))),
    conditionalPanel(
      condition = "['poisson', 'negbinomial', 'zero_inflated_poisson', 'zero_inflated_negbinomial'].indexOf(input.model_family) >= 0",
      selectInput("count_outcome_units", "Recorded outcome units", choices = c("Counts" = "count", "Rate per exposure unit" = "rate")),
      selectInput("exposure_var", "Exposure (offset) column", choices = c("None" = "")),
      helpText("Rate values are multiplied by exposure and rounded to counts. Counts remain unchanged."),
      helpText("Adds offset(log(column)) so effects describe the rate per unit of exposure (e.g., session minutes).")),
    conditionalPanel(
      condition = "output.dataUploaded",
      div(class = "status-sidebar-info", uiOutput("distributionAiRecommendation"))),
    hr(),
    actionButton("resetSettings", "Reset",
                 icon = icon("arrow-rotate-left"),
                 class = "btn btn-default btn-block btn-sm",
                 title = "Restore defaults without clearing the uploaded data")
  )
}

model_main <- function() {
  tagList(
    conditionalPanel(
      condition = "output.modelRunning",
      shinyWidgets::progressBar(id = "mcmc_progress", value = 0, total = 100,
                  title = "", display_pct = TRUE, status = "info", striped = TRUE),
      tags$div(
        style = "height: 200px; overflow-y: auto; background-color: #1e1e1e; padding: 10px;",
        tags$pre(style = "color: #00ff00; font-family: monospace; font-size: 13px; margin: 0;",
                 verbatimTextOutput("modelProgress")))
    ),

    conditionalPanel(
      condition = "!output.dataUploaded",
      empty_state("model",
        'Upload a file or select an <strong style="color: #0c1f4a;">Example Dataset</strong> in the Data tab to specify the model.')
    ),

    conditionalPanel(
      condition = "output.dataUploaded",
      wellPanel(
        style = "background-color: #f9fafb; border: 1px solid #e5e7eb; padding: 12px;",
        div(style = "display: flex; align-items: center; gap: 6px; margin-bottom: 6px;",
          tags$h2("Model Specification", style = "color: #0c1f4a; font-weight: 600; margin: 0;"),
          info_popup_link("showEquationGuide", title = "Notation guide")
        ),
        uiOutput("modelFormulaText"),
        withMathJax(),
        uiOutput("modelEquation")
      ),

      tabsetPanel(id = "model_tabs",
      tabPanel("Fixed Effects",
        selectInput("outcome_var", "Outcome Variable", choices = NULL),
        radioButtons("formula_mode", "Formula Mode",
          choices = list("Guided" = "guided", "Custom" = "custom"),
          selected = "guided", inline = TRUE),
        conditionalPanel(
          condition = "input.formula_mode == 'guided'",
          uiOutput("piecewiseTermsUI"),
          selectInput("moderator_vars", "Moderators", choices = NULL, multiple = TRUE),
          checkboxInput("cross_level_interactions", "Cross-level interactions", value = FALSE)),
        conditionalPanel(
          condition = "input.formula_mode == 'custom'",
          textAreaInput("custom_fixed_formula", tags$span(class = "sr-only", "Custom fixed-effects formula"),
            placeholder = "e.g., phase * (designType) + complexityMeasure", rows = 2, width = "100%")),
        checkboxInput("center_moderators", "Center moderator variables", value = FALSE),
        checkboxInput("scale_moderators", "Scale moderator variables", value = FALSE)
      ),
      tabPanel("Random Effects",
        unifiedRandomEffectsUI("randomEffects"),
        hr(),
        checkboxInput("include_ar1",
          tagList("AR(1) autocorrelation ", info_link("ar1"), " ",
                  uiOutput("autocorrAlert", inline = TRUE)),
          value = FALSE),
        conditionalPanel(condition = "input.include_ar1",
          uiOutput("arSpacingWarning"),
          checkboxInput("ar_covariance", "Estimate AR covariance", value = FALSE),
          numericInput("ar_prior_sd", "AR(1) prior SD",
                       value = 0.5, min = 0.1, max = 2, step = 0.1),
          helpText("Normal(0, SD) prior on the AR(1) coefficient, truncated to (-1, 1). Larger SDs push mass toward the bounds.")),
        checkboxInput("heterogeneous_variance", "Heterogeneous variance across phases", value = FALSE)
      ),
      tabPanel("MCMC",
        fluidRow(
          column(6,
            numericInput("n_chains", "Chains", value = 4, min = 2, max = 8),
            numericInput("n_iter", "Iterations", value = 2000, min = 1500, max = 20000, step = 500),
            numericInput("n_warmup", "Warmup", value = 1000, min = 500, max = 10000, step = 250)
          ),
          column(6,
            div(style = "display: flex; align-items: center; gap: 6px;",
              numericInput("adapt_delta", "Adapt Delta", value = 0.95, min = 0.80, max = 0.999, step = 0.01),
              div(style = "margin-top: 24px;", info_link("adapt_delta"))
            ),
            numericInput("max_treedepth", "Max Tree Depth", value = 12, min = 8, max = 15),
            checkboxInput("parallel_chains", "Parallel chains", value = TRUE)
          )
        )
      ),
      tabPanel("Priors",
        fluidRow(
          column(6,
            div(style = "display: flex; align-items: center; gap: 8px;",
              tags$label("Prior Type", class = "control-label"),
              info_popup_link("showPriorGuide", title = "Prior choice guide"),
              actionLink("showPriorWizard", "Help me pick",
                         style = "color: #337ab7; font-size: 13px; margin-left: 8px;",
                         title = "3-question decision tree")
            ),
            radioButtons("prior_type", tags$span(class = "sr-only", "Prior type"),
              choices = list("Weakly informative" = "default",
                             "Informative" = "informative",
                             "Meta-analytic" = "meta_analytic",
                             "Sensitivity" = "sensitivity"),
              selected = "default"),
            conditionalPanel(
              condition = "input.prior_type == 'meta_analytic' || input.prior_type == 'informative'",
              helpText("Enter the prior mean on the link scale. The informative prior uses half the weakly informative SD."),
              numericInput("prior_effect_mean", "Expected effect (prior mean, link scale)", value = 0, min = -3, max = 3, step = 0.1)),
            conditionalPanel(
              condition = "input.prior_type == 'meta_analytic'",
              numericInput("prior_effect_sd", "Prior Uncertainty (SD, link scale)", value = 0.3, min = 0.1, max = 2, step = 0.1)),
            hr(),
            div(style = "display: flex; flex-wrap: wrap; gap: 6px; align-items: center;",
              actionButton("runPriorPredictive", "Preview",
                           icon = icon("eye"), class = "btn-default btn-sm",
                           title = "sample_prior='only' brm + pp_check"),
              actionButton("showPriorOverride", "Edit priors",
                           icon = icon("pen"), class = "btn-default btn-sm",
                           title = "Per-class override"),
              actionButton("applyReplicationPreset", "Replication",
                           icon = icon("clone"), class = "btn-default btn-sm",
                           title = "Normal(earlier estimate, inflation x earlier SE) on the treatment coefficient"),
              actionButton("askAiPriors", tagList(icon("wand-magic-sparkles"), "Co-design priors (AI)"),
                           class = "btn-primary btn-sm",
                           title = "Optional: elicit priors in a dialog with an AI assistant")
            )
          ),
          column(6,
            plotOutput("priorsPlot", height = "300px"),
            conditionalPanel(
              condition = "output.priorPredictiveReady",
              hr(),
              tags$h3("Prior predictive check",
                      style = "color: #64748B; font-weight: 600; font-size: 13px;"),
              plotOutput("priorPredictivePlot", height = "260px"),
              uiOutput("priorPredictiveNote")
            )
          )
        ),
        verbatimTextOutput("priorDisplay")
      )
    )
    )
  )
}

