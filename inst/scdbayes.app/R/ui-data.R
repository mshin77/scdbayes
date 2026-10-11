# Shared Data-subtab generators used by both Single-Case Design (prefix "") and
# Meta-Analysis (prefix "meta_"). Output/input ids are prefix-keyed so the two
# tabs render the same panels over the shared data store without DOM id clashes.

data_sidebar <- function(prefix = "", example_choices = NULL, study_required = FALSE) {
  id <- function(s) paste0(prefix, s)

  base_rows <- tagList(
    tags$tr(tags$td(tags$code("case")),    tags$td("text/numeric")),
    tags$tr(tags$td(tags$code("time")),    tags$td("numeric")),
    tags$tr(tags$td(tags$code("phase")),   tags$td("text")),
    tags$tr(tags$td(tags$code("outcome")), tags$td("numeric"))
  )
  study_row <- if (study_required)
    tags$tr(tags$td(tags$code("study")), tags$td("text/numeric (2+)")) else NULL
  optional_note <- if (study_required) "Optional: moderators."
    else tagList("Optional: ", tags$code("study"), ", moderators.")

  tagList(
    tags$h2("Load Data", style = "color: #0c1f4a; font-weight: 600; margin-bottom: 10px;"),
    fileInput(id("file1"), "Upload File",
              accept = c(".csv", ".xlsx", ".xls"),
              placeholder = "CSV or Excel...",
              buttonLabel = icon("folder-open")),
    hr(),
    selectizeInput(id("sample_dataset"), "Example Datasets",
      choices = example_choices,
      selected = "",
      options = list(placeholder = "Select a dataset")),
    hr(),
    tags$h2("Data Format", style = "color: #0c1f4a; font-weight: 600; margin-bottom: 8px;"),
    tags$table(class = "table table-sm table-striped",
      style = "font-size: 15px;",
      tags$thead(tags$tr(tags$th("Column"), tags$th("Type"))),
      tags$tbody(study_row, base_rows)
    ),
    tags$p(style = "font-size: 15px; color: #374151; margin-top: -4px;", optional_note),
    conditionalPanel(
      condition = paste0("output.", id("dataUploaded")),
      hr(),
      uiOutput(id("validationIssues")),
      uiOutput(id("validationWarnings")),
      uiOutput(id("validationRecommendationsGeneral"))
    )
  )
}

data_panels <- function(prefix = "", include_between_phase = TRUE) {
  id <- function(s) paste0(prefix, s)
  up <- paste0("output.", id("dataUploaded"))

  placeholder <- function(icon, suffix) {
    conditionalPanel(
      condition = paste0("!", up),
      empty_state(icon,
        paste0('Upload a file or select an <strong style="color: #0c1f4a;">Example Dataset</strong> ', suffix)))
  }

  between_phase <- if (include_between_phase) tabPanel("Between-Phase",
    placeholder("compare", "to view between-phase effect sizes"),
    conditionalPanel(condition = up,
      div(style = "display: flex; align-items: center; margin-bottom: 10px;",
        tags$span(style = "font-size: 15px; color: #64748B;",
                  "Descriptive (non-model) summaries supporting visual analysis: select metrics for between-phase comparison")
      ),
      conditionalPanel(
        condition = paste0("output.", id("isCCD")),
        radioButtons(id("ccd_analysis_option"), "CCD comparison",
          choices = list(
            "Baseline vs last subphase (recommended)" = "baseline_last",
            "Adjacent subphases (supplementary)" = "consecutive"
          ),
          selected = "baseline_last"),
        helpText(info_link("ccd_contrast"),
                 "The baseline-to-last-subphase contrast is the main summary; adjacent-subphase nonoverlap is expected by design.")
      ),
      fluidRow(
        column(4,
          selectInput(id("va_es_metrics"), "Metrics",
            choices = list(
              "Parametric" = list("LRRd" = "LRRd", "LRRi" = "LRRi"),
              "Non-Overlap" = list("NAP" = "NAP", "Tau" = "Tau", "Tau-BC" = "Tau_BC",
                                   "PND" = "PND", "PEM" = "PEM", "PAND" = "PAND",
                                   "IRD" = "IRD")
            ),
            selected = c("NAP", "Tau"),
            multiple = TRUE)
        ),
        column(4,
          radioButtons(id("va_es_improvement"), "Intervention direction",
            choices = c("increase", "decrease"), selected = "increase", inline = TRUE)
        ),
        column(4,
          numericInput(id("va_es_confidence"), "CI level",
                       value = 0.95, min = 0.50, max = 0.99, step = 0.01)
        )
      ),
      fluidRow(
        column(4,
          selectInput(id("va_es_scale"), "Outcome scale (LRR)",
            choices = list("Count" = "count", "Rate" = "rate", "Percentage" = "percentage",
                           "Proportion" = "proportion", "Other" = "other"),
            selected = "other")),
        column(8, uiOutput(id("atdReferenceUI")))
      ),
      helpText(info_link("va_effect_sizes"),
        "LRR needs a ratio-scale outcome with a true zero.",
        "Prefer NAP, Tau, or LRR over PND, PEM, and PAND."),
      DT::dataTableOutput(id("vaEffectSizeTable")),
      div(class = "data-section",
        tags$h2("Immediacy of Effect ", info_link("immediacy"), class = "section-header"),
        tags$p(style = "font-size: 15px; color: #374151; margin-bottom: 10px;",
          "Mean of the last 3 points before each phase change versus the first 3 after it.",
          "Positive values are changes in the direction the design predicts (back toward baseline at a withdrawal)."),
        DT::dataTableOutput(id("immediacyTable"))),
      div(class = "data-section",
        tags$h2("Reversibility ", info_link("reversibility"), class = "section-header"),
        tags$p(style = "font-size: 15px; color: #374151; margin-bottom: 10px;",
          "NAP of the initial baseline against each later baseline.",
          "NAP <= .85 means the behavior returned, so reversibility is met."),
        DT::dataTableOutput(id("reversibilityTable"))),
      conditionalPanel(
        condition = paste0("output.", id("isCCD")),
        div(class = "data-section",
          tags$h2("Percentage of Conforming Data ", info_link("pcd"), class = "section-header"),
          tags$p(style = "font-size: 15px; color: #374151; margin-bottom: 10px;",
            "Share of criterion-phase points inside the acceptable range around each criterion level.",
            "The range width scales with p."),
          numericInput(id("pcd_proportion"), "Range proportion p", value = 0.5,
                       min = 0.05, max = 0.95, step = 0.05),
          DT::dataTableOutput(id("ccdPcdTable")))))
  ) else NULL

  panels <- list(
      tabPanel("Data Table",
        placeholder("table", "to get started"),
        conditionalPanel(condition = up,
          DT::dataTableOutput(id("dataTable")))
      ),
      tabPanel("Variables",
        placeholder("columns", "to view variables"),
        conditionalPanel(condition = up,
          uiOutput(id("columnRoleSelectors")),
          div(class = "data-section",
            tags$h2("Continuous Variables", class = "section-header"),
            DT::dataTableOutput(id("continuousVarsTable"))),
          div(class = "data-section",
            tags$h2("Categorical Variables", class = "section-header"),
            DT::dataTableOutput(id("categoricalVarsTable"))))
      ),
      tabPanel("Features",
        placeholder("features", "to view features"),
        conditionalPanel(condition = up,
          div(class = "data-section",
            tags$h2("Case-Level Features", class = "section-header"),
            DT::dataTableOutput(id("caseFeaturesTable"))),
          div(class = "data-section",
            tags$h2("Key Feature Distributions", class = "section-header"),
            plotly_figure(id("featureDistPlot"),
              "Distributions of case-level features across cases", height = "400px")))
      ),
      tabPanel("Plot",
        placeholder("plot_line", "to view plots"),
        conditionalPanel(condition = up,
          fluidRow(
            column(2, checkboxInput(id("show_trend_lines"), "Trend lines", value = FALSE)),
            column(3, checkboxInput(id("show_phase_lines"), "Phase transition lines", value = TRUE)),
            column(2, checkboxInput(id("show_phase_means"), "Phase means", value = FALSE)),
            column(3, uiOutput(id("facet_var_selector")))
          ),
          fluidRow(
            column(6, sliderInput(id("plot_height"), "Height",
                                  min = 300, max = 2400, value = 600, step = 50, post = " px")),
            column(6, sliderInput(id("plot_width"), "Width",
                                  min = 280, max = 2000, value = 900, step = 20, post = " px"))
          ),
          div(style = "overflow: auto; max-height: 80vh; border: 1px solid #e2e8f0; border-radius: 4px;",
            uiOutput(id("rawDataPlotUI"))
          ))
      ),
      tabPanel("Within-Phase",
        placeholder("bars", "to view within-phase patterns"),
        conditionalPanel(condition = up,
          DT::dataTableOutput(id("phaseDescriptivesTable")))
      ),
      tabPanel("Baseline Trend",
        placeholder("trend", "to view baseline trend tests"),
        conditionalPanel(condition = up,
          tags$p(style = "font-size: 15px; color: #374151; margin-bottom: 10px;",
            info_link("baseline_trend"),
            "Trend NAP compares the last three baseline points with earlier ones.",
            "NAP > 0.85 flags an already improving baseline."),
          DT::dataTableOutput(id("baselineTrendTable")))
      ),
      tabPanel("WWC Check",
        placeholder("table", "to screen the design against the WWC 5.0 standards"),
        conditionalPanel(condition = up,
          tags$h2("WWC Screening ", info_link("wwc_check"), class = "section-header"),
          fluidRow(
            column(4, numericInput(id("wwc_ioa_value"), "IOA value (optional)",
                                   value = NULL, min = 0, max = 100, step = 0.01)),
            column(4, numericInput(id("wwc_ioa_coverage"), "Share of data points with IOA",
                                   value = NULL, min = 0, max = 1, step = 0.05)),
            column(4, selectInput(id("wwc_ioa_type"), "IOA index",
                                  choices = c("Percentage agreement (>= .80)" = "percentage",
                                              "Cohen's kappa (>= .60)" = "kappa")))),
          uiOutput(id("wwcSummary")),
          div(class = "data-section",
            tags$h2("Experiments", class = "section-header"),
            DT::dataTableOutput(id("wwcExperimentTable"))),
          div(class = "data-section",
            tags$h2("Phases per Tier", class = "section-header"),
            DT::dataTableOutput(id("wwcTierTable"))),
          div(class = "data-section",
            tags$h2("Baseline Trend (risk of bias)", class = "section-header"),
            DT::dataTableOutput(id("wwcTrendTable"))),
          div(class = "data-section",
            tags$h2("Reversibility (risk of bias)", class = "section-header"),
            DT::dataTableOutput(id("wwcReversibilityTable"))),
          tags$p(style = "font-size: 15px; color: #374151; margin-top: 10px;",
            "Findings that meet standards proceed to effect sizes (BC-SMD, LRR, NAP/Tau)",
            "in Meta-Analysis > Export."))
      ),
      between_phase,
      tabPanel("Processed Data",
        conditionalPanel(condition = paste0("!", up),
          empty_state("transform",
            "Upload data to view the processed model variables (phase-centered time, level dummies, slope-change predictors).")),
        conditionalPanel(condition = up,
          tags$p(style = "font-size: 15px; color: #374151; margin-bottom: 10px;",
            "Model variables: time (", tags$code("time_A"), "), level change (",
            tags$code("level_AB"), "), and slope change (", tags$code("trend_AB"), ")."),
          DT::dataTableOutput(id("processedDataTable")))
      )
  )
  panels <- Filter(Negate(is.null), panels)

  tagList(
    conditionalPanel(
      condition = up,
      fluidRow(
        column(3, numericInput(id("display_digits"), "Decimal places", value = 2, min = 0, max = 6, step = 1))
      )
    ),
    do.call(tabsetPanel, c(list(id = id("data_tabs")), panels))
  )
}
