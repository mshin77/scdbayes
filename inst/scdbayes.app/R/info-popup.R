# info-popup
# uniform ⓘ actionLink + modalDialog wrapper

# icon link; pair the id with register_info_popup() or a manual observeEvent
info_popup_link <- function(id, title = "More info") {
  shiny::tags$a(
    href = "#",
    role = "button",
    title = title,
    `aria-label` = title,
    style = "color: #337ab7; font-size: 13px; cursor: pointer;",
    onclick = sprintf(
      "Shiny.setInputValue('%s', Math.random(), {priority: 'event'}); return false;",
      id
    ),
    # role=button: Space activates like a native button; Enter already fires click on a link
    onkeydown = "if (event.key === ' ' || event.key === 'Spacebar') { event.preventDefault(); this.click(); }",
    shiny::icon("info-circle")
  )
}

# Standard modal body. citation is shown as a small footer attribution.
info_popup_modal <- function(title, body, citation = NULL, size = "m") {
  body_tag <- if (inherits(body, "shiny.tag") || inherits(body, "shiny.tag.list")) {
    body
  } else {
    shiny::HTML(body)
  }
  citation_tag <- if (!is.null(citation) && nzchar(citation)) {
    shiny::tags$p(
      style = "margin-top: 16px; padding-top: 10px; border-top: 1px solid #e5e7eb; color: #64748B; font-size: 13px;",
      shiny::HTML(citation)
    )
  } else NULL

  # one modal is open at a time, so a fixed title id is unique
  shiny::tagAppendAttributes(
    shiny::modalDialog(
      title = shiny::tags$span(id = "info-popup-title", shiny::HTML(as.character(title))),
      shiny::div(style = "font-size: 15px; line-height: 1.6;", body_tag),
      citation_tag,
      size = size,
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    ),
    role = "dialog", `aria-modal` = "true", `aria-labelledby` = "info-popup-title"
  )
}

# Wire a static popup in one call. body and citation may be character or tag.
register_info_popup <- function(input, id, title, body, citation = NULL, size = "m") {
  shiny::observeEvent(input[[id]], {
    shiny::showModal(info_popup_modal(title, body, citation, size))
  })
}

# popup definitions keyed by id stem
info_popup_specs <- list(
  ar1 = list(
    title = "AR(1) autocorrelation",
    body = "<p>AR(1) describes dependence between neighboring residuals within a case.</p><p>Account for serial dependence when estimating intervention effects and uncertainty.</p>",
    citation = "Hox, J. J. (2010). <em>Multilevel analysis: Techniques and applications</em> (2nd ed.). Routledge."
  ),
  icc = list(
    title = "Intraclass correlation (ICC)",
    body = "<p>For a Gaussian random-intercept model, ICC is between-case variance divided by total variance.</p><p>Other families and random-slope models require a model-specific variance definition.</p>",
    citation = "Hox, J. J. (2010). <em>Multilevel analysis: Techniques and applications</em> (2nd ed.). Routledge."
  ),
  adapt_delta = list(
    title = "Adapt Delta and Max Tree Depth",
    body = "<p>Adapt Delta controls the target acceptance probability during warmup. Raising it can reduce divergences, with slower sampling.</p><p>Max Tree Depth limits trajectory length. Inspect all sampling diagnostics before interpreting estimates.</p>",
    citation = "Stan Development Team. (n.d.). <em>How to diagnose and resolve convergence problems</em>. <a href=\"https://mc-stan.org/learn-stan/diagnostics-warnings.html\" target=\"_blank\" rel=\"noopener noreferrer\">https://mc-stan.org/learn-stan/diagnostics-warnings.html</a>"
  ),
  rhat = list(
    title = "R-hat (potential scale reduction)",
    body = "<p>R-hat compares chains; values near 1 indicate agreement.</p><p>Use R-hat below 1.01 together with effective sample sizes and divergence checks. R-hat alone does not establish convergence.</p>",
    citation = "Stan Development Team. (n.d.). <em>How to diagnose and resolve convergence problems</em>. <a href=\"https://mc-stan.org/learn-stan/diagnostics-warnings.html\" target=\"_blank\" rel=\"noopener noreferrer\">https://mc-stan.org/learn-stan/diagnostics-warnings.html</a>"
  ),
  ess = list(
    title = "Effective sample size (ESS)",
    body = "<p>ESS expresses posterior precision as an equivalent number of independent draws.</p><p>Inspect bulk ESS for central summaries and tail ESS for interval endpoints.</p>",
    citation = "Stan Development Team. (n.d.). <em>How to diagnose and resolve convergence problems</em>. <a href=\"https://mc-stan.org/learn-stan/diagnostics-warnings.html\" target=\"_blank\" rel=\"noopener noreferrer\">https://mc-stan.org/learn-stan/diagnostics-warnings.html</a>"
  ),
  looic = list(
    title = "LOO-IC (leave-one-out information criterion)",
    body = "<p>LOO estimates predictive performance for held-out observations. LOO-IC equals minus twice the expected log predictive density.</p><p>Compare differences with their standard errors and inspect Pareto-k diagnostics. For repeated observations, choose a holdout unit suited to the prediction task.</p>",
    citation = "Vehtari, A., Gelman, A., &amp; Gabry, J. (2017). Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC. <em>Statistics and Computing, 27</em>, 1413&ndash;1432. <a href=\"https://doi.org/10.1007/s11222-016-9696-4\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1007/s11222-016-9696-4</a>"
  ),
  bayes_r2 = list(
    title = "Bayesian R&sup2;",
    body = "<p>Bayesian R-squared summarizes explained variation for each posterior draw.</p><p>Inspect its credible interval alongside predictive checks; it does not establish a causal intervention effect.</p>",
    citation = "Gelman, A., Goodrich, B., Gabry, J., &amp; Vehtari, A. (2019). R-squared for Bayesian regression models. <em>The American Statistician, 73</em>(3), 307&ndash;309. <a href=\"https://doi.org/10.1080/00031305.2018.1549100\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1080/00031305.2018.1549100</a>"
  ),
  credible_interval = list(
    title = "95% credible interval (CrI)",
    body = "<p>The equal-tailed 95% credible interval runs from the 2.5th to the 97.5th posterior percentile.</p><p>Interpret it conditional on the model, data, and priors. It differs from a highest-density interval.</p>",
    citation = "Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., &amp; Rubin, D. B. (2013). <em>Bayesian data analysis</em> (3rd ed.). CRC Press."
  ),
  power_decision_rule = list(
    title = "Decision rule",
    body = "<p>A simulated dataset counts as a detection when its posterior meets the selected rule.</p><ul><li>Direction: sufficient posterior mass on either side of zero.</li><li>Meaningful benefit: sufficient mass beyond the threshold in the intervention direction.</li><li>Outside ROPE: sufficient mass outside the equivalence interval.</li></ul><p>For all-transition reversal tests, every transition must meet its required rule.</p>",
    citation = "Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., &amp; L&uuml;decke, D. (2019). Indices of effect existence and significance in the Bayesian framework. <em>Frontiers in Psychology, 10</em>, Article 2767. <a href=\"https://doi.org/10.3389/fpsyg.2019.02767\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.3389/fpsyg.2019.02767</a>"
  ),
  pd = list(
    title = "pd: probability of direction",
    body = "<p>Probability of direction is the larger posterior probability above or below zero.</p><p>Report it with effect magnitude and a credible interval; it does not measure practical importance.</p>",
    citation = "Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., &amp; L&uuml;decke, D. (2019). Indices of effect existence and significance in the Bayesian framework. <em>Frontiers in Psychology, 10</em>, Article 2767. <a href=\"https://doi.org/10.3389/fpsyg.2019.02767\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.3389/fpsyg.2019.02767</a>"
  ),
  rope = list(
    title = "ROPE: region of practical equivalence",
    body = "<p>The ROPE contains effects considered practically equivalent to zero.</p><p>This app reports the full posterior share inside that interval. Choose a context-specific width; defaults are conventions.</p>",
    citation = "Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., &amp; L&uuml;decke, D. (2019). Indices of effect existence and significance in the Bayesian framework. <em>Frontiers in Psychology, 10</em>, Article 2767. <a href=\"https://doi.org/10.3389/fpsyg.2019.02767\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.3389/fpsyg.2019.02767</a>"
  ),
  slope_sd = list(
    title = "Between-case effect SD",
    body = "<p>Between-case effect SD describes variation in intervention effects across cases.</p><p>Gaussian simulations use residual-SD units; count simulations use log-rate units. Zero specifies a common effect.</p>",
    citation = "Hox, J. J. (2010). <em>Multilevel analysis: Techniques and applications</em> (2nd ed.). Routledge."
  ),
  marginal_contrast = list(
    title = "Population-averaged (marginal) contrast",
    body = "<p>A marginal contrast averages predictions over the fitted distribution of new cases.</p><p>On nonlinear links it differs from a contrast that holds random effects at zero. Check the comparison sessions and outcome scale.</p>",
    citation = "Hox, J. J. (2010). <em>Multilevel analysis: Techniques and applications</em> (2nd ed.). Routledge."
  ),
  ppc_features = list(
    title = "Visual-analysis posterior predictive checks",
    body = "<p>Posterior predictive checks compare observed visual-analysis features with simulated data from the fitted model.</p><p>Inspect level, trend, variability, dependence, and phase transitions. Extreme discrepancies suggest model misfit; unflagged checks do not prove fit.</p>",
    citation = "Grekov, P., &amp; Pustejovsky, J. E. (2026). A gentle introduction to Bayesian posterior predictive checking for single-case researchers. <em>Journal of Behavioral Education</em>. Advance online publication. <a href=\"https://doi.org/10.1007/s10864-025-09613-8\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1007/s10864-025-09613-8</a>"
  ),
  ccd_contrast = list(
    title = "Changing-criterion comparisons",
    body = "<p>Compare baseline with the final criterion subphase for the main summary.</p><p>Adjacent subphase comparisons are supplementary: criterion changes can produce nonoverlap by design.</p>",
    citation = "Manolov, R., Solanas, A., &amp; Sierra, V. (2020). Changing criterion designs: Integrating methodological and data analysis recommendations. <em>The Journal of Experimental Education, 88</em>(2), 335&ndash;350. <a href=\"https://doi.org/10.1080/00220973.2018.1553838\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1080/00220973.2018.1553838</a>"
  ),
  mixed_outcomes = list(
    title = "Outcomes of different types or scales",
    body = "<p>Analyze distinct outcome types on suitable scales before combining evidence.</p><ul><li>Use one compatible effect-size metric per synthesis.</li><li>Align intervention direction and account for dependent outcomes.</li><li>Check scale conversions in sensitivity analyses.</li></ul>",
    citation = "Hox, J. J. (2010). <em>Multilevel analysis: Techniques and applications</em> (2nd ed.). Routledge.<br><br>Chen, M., &amp; Pustejovsky, J. E. (2024). Multi-level meta-analysis of single-case experimental designs using robust variance estimation. <em>Psychological Methods, 29</em>(3), 537&ndash;560. <a href=\"https://doi.org/10.1037/met0000510\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1037/met0000510</a>"
  ),
  standardize_case = list(
    title = "Standardize outcome within case",
    body = "<p>Gaussian outcomes can be divided by a case-specific residual SD to place them on a common scale.</p><p>The OLS scale estimate ignores serial dependence. Treat this option as a sensitivity analysis; tiers too short to estimate the scale are excluded.</p>",
    citation = "Chen, M., &amp; Pustejovsky, J. E. (2024). Multi-level meta-analysis of single-case experimental designs using robust variance estimation. <em>Psychological Methods, 29</em>(3), 537&ndash;560. <a href=\"https://doi.org/10.1037/met0000510\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1037/met0000510</a>"
  ),
  bc_smd = list(
    title = "Bayesian BC-SMD and pooling",
    body = "<p>BC-SMD divides the model-implied treatment difference by the untreated outcome SD at the comparison session.</p><p>Check comparison times, Gaussian assumptions, and prior sensitivity. Bayesian estimates use posterior draws without the REML small-sample correction.</p><p>Pooling uses study estimates and their standard errors in a normal approximation. It does not remove first-stage uncertainty or bias.</p>",
    citation = "Pustejovsky, J. E., Hedges, L. V., &amp; Shadish, W. R. (2014). Design-comparable effect sizes in multiple baseline designs: A general modeling framework. <em>Journal of Educational and Behavioral Statistics, 39</em>(5), 368&ndash;393. <a href=\"https://doi.org/10.3102/1076998614547577\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.3102/1076998614547577</a>"
  ),
  first_stage_es = list(
    title = "First-stage effect sizes",
    body = "<p>First-stage exports contain case-level effect sizes and standard errors; sampling variance is SE squared.</p><p>Check phase stability and serial dependence. Do not re-pool shrunken case estimates from an already pooled model.</p>",
    citation = "Pustejovsky, J. E. (2018). Using response ratios for meta-analyzing single-case designs with behavioral outcomes. <em>Journal of School Psychology, 68</em>, 99&ndash;112. <a href=\"https://doi.org/10.1016/j.jsp.2018.02.003\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1016/j.jsp.2018.02.003</a><br><br>Chen, M., &amp; Pustejovsky, J. E. (2024). Multi-level meta-analysis of single-case experimental designs using robust variance estimation. <em>Psychological Methods, 29</em>(3), 537&ndash;560. <a href=\"https://doi.org/10.1037/met0000510\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1037/met0000510</a>"
  ),
  va_effect_sizes = list(
    title = "Between-phase effect sizes",
    body = "<p>LRR measures proportionate change for ratio-scale outcomes; NAP and Tau summarize nonoverlap.</p><p>Check zero phase means, time trends, and serial dependence. Independent-observation intervals may be too narrow when observations are correlated.</p>",
    citation = "Pustejovsky, J. E. (2018). Using response ratios for meta-analyzing single-case designs with behavioral outcomes. <em>Journal of School Psychology, 68</em>, 99&ndash;112. <a href=\"https://doi.org/10.1016/j.jsp.2018.02.003\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1016/j.jsp.2018.02.003</a><br><br>Chen, M., &amp; Pustejovsky, J. E. (2024). Multi-level meta-analysis of single-case experimental designs using robust variance estimation. <em>Psychological Methods, 29</em>(3), 537&ndash;560. <a href=\"https://doi.org/10.1037/met0000510\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1037/met0000510</a>"
  ),
  atd_contrast = list(
    title = "Alternating-treatment comparisons",
    body = "<p>Compare treatment conditions directly with the reference and with each other.</p><p>These contrasts do not rule out sequence or carryover effects. Tau-BC is omitted because no preceding baseline defines the comparison.</p>",
    citation = "Manolov, R., &amp; Onghena, P. (2018). Analyzing data from single-case alternating treatments designs. <em>Psychological Methods, 23</em>(3), 480&ndash;504. <a href=\"https://doi.org/10.1037/met0000133\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1037/met0000133</a>"
  ),
  immediacy = list(
    title = "Immediacy of effect",
    body = "<p>Immediacy compares the last three observations before a transition with the first three afterward.</p><p>Short phases use available observations. Positive values follow the expected intervention or withdrawal direction; this summary captures abrupt level changes.</p>",
    citation = "Manolov, R., &amp; Onghena, P. (2022). Defining and assessing immediacy in single-case experimental designs. <em>Journal of the Experimental Analysis of Behavior, 118</em>, 462&ndash;492. <a href=\"https://doi.org/10.1002/jeab.799\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1002/jeab.799</a>"
  ),
  reversibility = list(
    title = "Reversibility",
    body = "<p>Compare later baseline phases with the initial baseline to assess return toward baseline.</p><p>The app uses NAP of 0.85 as a screening threshold. Inspect the graph; this threshold alone does not establish reversibility.</p>",
    citation = "What Works Clearinghouse. (2022). <em>What Works Clearinghouse procedures and standards handbook, version 5.0</em>. Institute of Education Sciences. <a href=\"https://ies.ed.gov/ncee/wwc/Handbooks\" target=\"_blank\" rel=\"noopener noreferrer\">https://ies.ed.gov/ncee/wwc/Handbooks</a>"
  ),
  pcd = list(
    title = "Percentage of conforming data",
    body = "<p>Percentage of conforming data is the share of criterion-phase observations inside an acceptable range around the criterion.</p><p>Inspect the range setting and criterion steps before interpreting the percentage.</p>",
    citation = "Manolov, R., Solanas, A., &amp; Sierra, V. (2020). Changing criterion designs: Integrating methodological and data analysis recommendations. <em>The Journal of Experimental Education, 88</em>(2), 335&ndash;350. <a href=\"https://doi.org/10.1080/00220973.2018.1553838\" target=\"_blank\" rel=\"noopener noreferrer\">https://doi.org/10.1080/00220973.2018.1553838</a>"
  ),
  baseline_trend = list(
    title = "Baseline trend (risk of bias)",
    body = "<p>The app screens whether baseline observations already trend in the intervention direction.</p><p>A flagged trend requires inspection. The piecewise model includes baseline trend; the flag does not automatically exclude data.</p>",
    citation = "What Works Clearinghouse. (2022). <em>What Works Clearinghouse procedures and standards handbook, version 5.0</em>. Institute of Education Sciences. <a href=\"https://ies.ed.gov/ncee/wwc/Handbooks\" target=\"_blank\" rel=\"noopener noreferrer\">https://ies.ed.gov/ncee/wwc/Handbooks</a>"
  ),
  wwc_check = list(
    title = "WWC design screening",
    body = "<p>This screening aid applies selected WWC design rules.</p><p>Outcome validity, confounds, and a complete visual review still require assessment. The screening result is not an official WWC determination.</p>",
    citation = "What Works Clearinghouse. (2022). <em>What Works Clearinghouse procedures and standards handbook, version 5.0</em>. Institute of Education Sciences. <a href=\"https://ies.ed.gov/ncee/wwc/Handbooks\" target=\"_blank\" rel=\"noopener noreferrer\">https://ies.ed.gov/ncee/wwc/Handbooks</a>"
  )
)

# registers every spec once; the UI id is paste0("info_", key)
register_info_popups <- function(input, specs = info_popup_specs) {
  for (key in names(specs)) {
    sp <- specs[[key]]
    local({
      input_id <- paste0("info_", key)
      spec <- sp
      shiny::observeEvent(input[[input_id]], {
        shiny::showModal(info_popup_modal(spec$title, spec$body, spec$citation))
      })
    })
  }
}

# Convenience: link with id matching the spec key.
info_link <- function(key) {
  spec <- info_popup_specs[[key]]
  if (is.null(spec)) stop("Unknown info_popup spec key: ", key)
  info_popup_link(paste0("info_", key), title = spec$title)
}
