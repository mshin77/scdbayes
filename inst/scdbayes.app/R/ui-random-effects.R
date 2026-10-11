unifiedRandomEffectsUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
      tags$h2(HTML("<strong>Random Effects</strong>"), style = "color: #0c1f4a; margin: 0;"),
      actionButton(ns("detectStructure"), "Detect", class = "btn btn-sm btn-default")
    ),
    uiOutput(ns("structureAlert")),

    fluidRow(
      column(6,
        selectInput(ns("groupingVars"), "Grouping Variables:",
          choices = c("Loading data..." = ""),
          multiple = TRUE, selectize = TRUE, width = "100%"),
        selectInput(ns("slopeVars"), "Random Slopes (optional):",
          choices = c("None" = "", "Phase" = "phase", "Time" = "time"),
          multiple = TRUE, selectize = TRUE, width = "100%"),
        div(class = "mt-2",
          actionButton(ns("buildNested"), "Nested", class = "btn btn-sm btn-default"),
          actionButton(ns("buildCrossClass"), "Crossed", class = "btn btn-sm btn-default")
        )
      ),
      column(6,
        div(style = "display: flex; align-items: center; gap: 6px; margin-bottom: 4px;",
          tags$label("Random Effects Formula", `for` = ns("randomFormula")),
          actionLink(ns("showREGuide"), icon("info-circle"),
                     style = "color: #337ab7; font-size: 13px;",
                     title = "Random effects syntax guide")
        ),
        textAreaInput(ns("randomFormula"),
          label = NULL,
          value = "(1|case)",
          placeholder = "e.g., (1|case) + (1|study)",
          rows = 3, width = "100%")
      )
    ),

    uiOutput(ns("formulaWarnings"))
  )
}
