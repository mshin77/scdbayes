# Package index

## Getting Started

Launch the Shiny app

- [`run_app()`](https://mshin77.github.io/scdbayes/reference/run_app.md)
  : Launch the scdbayes Shiny application

## Data Structure Detection

Identify design, family, hierarchy, and column roles

- [`detect_design_type()`](https://mshin77.github.io/scdbayes/reference/detect_design_type.md)
  : Detect SCD design type
- [`detect_hierarchical_structure()`](https://mshin77.github.io/scdbayes/reference/detect_hierarchical_structure.md)
  : Detect hierarchical structure in data
- [`detect_hierarchy_levels()`](https://mshin77.github.io/scdbayes/reference/detect_hierarchy_levels.md)
  : Detect hierarchy levels in SCD data
- [`find_case_var()`](https://mshin77.github.io/scdbayes/reference/find_case_var.md)
  : Locate the case-level grouping column by common SCED name patterns
- [`find_cluster_var()`](https://mshin77.github.io/scdbayes/reference/find_cluster_var.md)
  : Locate a mid-level cluster grouping column
- [`find_nest_var()`](https://mshin77.github.io/scdbayes/reference/find_nest_var.md)
  : Detect a within-case nesting variable by name pattern then structure
- [`find_phase_var()`](https://mshin77.github.io/scdbayes/reference/find_phase_var.md)
  : Locate the phase column by common SCED name patterns
- [`find_study_var()`](https://mshin77.github.io/scdbayes/reference/find_study_var.md)
  : Locate the study- / institution-level grouping column
- [`find_time_var()`](https://mshin77.github.io/scdbayes/reference/find_time_var.md)
  : Locate the time column by common SCED name patterns
- [`tier_vars()`](https://mshin77.github.io/scdbayes/reference/tier_vars.md)
  : Grouping columns that define one data series (tier)
- [`phase_role()`](https://mshin77.github.io/scdbayes/reference/phase_role.md)
  : Design role of phase labels
- [`reversal_terms()`](https://mshin77.github.io/scdbayes/reference/reversal_terms.md)
  : Reversal-design piecewise terms
- [`recommend_distribution_family()`](https://mshin77.github.io/scdbayes/reference/recommend_distribution_family.md)
  : Recommend distribution family based on outcome data
- [`validate_scd_data()`](https://mshin77.github.io/scdbayes/reference/validate_scd_data.md)
  : Validate SCD data
- [`check_data_size()`](https://mshin77.github.io/scdbayes/reference/check_data_size.md)
  : Evaluate dataset size against per-task server limits
- [`wwc_design_check()`](https://mshin77.github.io/scdbayes/reference/wwc_design_check.md)
  : WWC single-case design standards screening check

## Data Preparation

Process phases, compute case-level features, generate synthetic data

- [`process_scd_phases()`](https://mshin77.github.io/scdbayes/reference/process_scd_phases.md)
  : Build piecewise regression coding for single-case design phases
- [`consolidate_phase_indicators()`](https://mshin77.github.io/scdbayes/reference/consolidate_phase_indicators.md)
  : Consolidate binary phase indicator columns into a single phase
  column
- [`standardize_by_case()`](https://mshin77.github.io/scdbayes/reference/standardize_by_case.md)
  : Re-express the outcome in within-case residual-SD units
- [`safe_column_names()`](https://mshin77.github.io/scdbayes/reference/safe_column_names.md)
  : Rename data columns to syntactic names
- [`compute_autocorrelation_features()`](https://mshin77.github.io/scdbayes/reference/compute_autocorrelation_features.md)
  : Lag-1 autocorrelation per phase per case
- [`compute_case_features()`](https://mshin77.github.io/scdbayes/reference/compute_case_features.md)
  : Per-case descriptive features
- [`analyze_covariates()`](https://mshin77.github.io/scdbayes/reference/analyze_covariates.md)
  : Analyze continuous and categorical covariates
- [`analyze_design_patterns()`](https://mshin77.github.io/scdbayes/reference/analyze_design_patterns.md)
  : Analyze design patterns
- [`simulate_scd_data()`](https://mshin77.github.io/scdbayes/reference/simulate_scd_data.md)
  : Simulate SCD data

## Modeling

Bayesian piecewise model formulas, priors, and fits via brms

- [`default_random_effects()`](https://mshin77.github.io/scdbayes/reference/default_random_effects.md)
  : Default random-effects structure of a single-case data set
- [`build_default_random_effects()`](https://mshin77.github.io/scdbayes/reference/build_default_random_effects.md)
  : Random effects with explicit grouping columns
- [`build_comparison_models()`](https://mshin77.github.io/scdbayes/reference/build_comparison_models.md)
  : Build candidate model formulas
- [`has_random_slope()`](https://mshin77.github.io/scdbayes/reference/has_random_slope.md)
  : Detect a random slope in a model formula
- [`treatment_coef()`](https://mshin77.github.io/scdbayes/reference/treatment_coef.md)
  : Identify the treatment coefficient of a piecewise SCD model
- [`add_exposure_offset()`](https://mshin77.github.io/scdbayes/reference/add_exposure_offset.md)
  : Add an exposure offset to a count-model formula
- [`generate_adaptive_priors()`](https://mshin77.github.io/scdbayes/reference/generate_adaptive_priors.md)
  : Generate adaptive priors based on data characteristics
- [`generate_prior_recommendations()`](https://mshin77.github.io/scdbayes/reference/generate_prior_recommendations.md)
  : Generate prior recommendations
- [`assemble_brms_priors()`](https://mshin77.github.io/scdbayes/reference/assemble_brms_priors.md)
  : Assemble brms priors from app-level inputs
- [`priors_to_brms()`](https://mshin77.github.io/scdbayes/reference/priors_to_brms.md)
  : Convert prior list to brms prior object
- [`is_prior_string()`](https://mshin77.github.io/scdbayes/reference/is_prior_string.md)
  : Check that a prior string is a single distribution call
- [`is_safe_formula()`](https://mshin77.github.io/scdbayes/reference/is_safe_formula.md)
  : Check that a model formula uses only data columns and brms term
  syntax
- [`is_safe_hypothesis()`](https://mshin77.github.io/scdbayes/reference/is_safe_hypothesis.md)
  : Check that a hypothesis string uses only model parameters and
  arithmetic
- [`run_scd_analysis()`](https://mshin77.github.io/scdbayes/reference/run_scd_analysis.md)
  : Main analysis function
- [`run_model_comparison()`](https://mshin77.github.io/scdbayes/reference/run_model_comparison.md)
  : Model comparison via LOO-CV
- [`run_sequential_update()`](https://mshin77.github.io/scdbayes/reference/run_sequential_update.md)
  : Sequential Bayesian updating
- [`run_sensitivity_analysis()`](https://mshin77.github.io/scdbayes/reference/run_sensitivity_analysis.md)
  : Run sensitivity analysis with different priors and outlier handling

## Diagnostics

Convergence diagnostics and variance-component bootstrap

- [`compute_convergence_diagnostics()`](https://mshin77.github.io/scdbayes/reference/compute_convergence_diagnostics.md)
  : Convergence diagnostics for Bayesian models
- [`bootstrap_variance_components()`](https://mshin77.github.io/scdbayes/reference/bootstrap_variance_components.md)
  : Posterior variance components and ICCs
- [`prior_predictive_check()`](https://mshin77.github.io/scdbayes/reference/prior_predictive_check.md)
  : Prior predictive check for a fitted SCD model

## Posterior Predictive Checking

Posterior predictive checks on visual-analysis features and overlay
draws

- [`posterior_predictive_features()`](https://mshin77.github.io/scdbayes/reference/posterior_predictive_features.md)
  : Posterior predictive checks on single-case visual-analysis features
- [`ppc_overlay_draws()`](https://mshin77.github.io/scdbayes/reference/ppc_overlay_draws.md)
  : Posterior-predictive line draws for a spaghetti overlay
- [`ppc_replicates()`](https://mshin77.github.io/scdbayes/reference/ppc_replicates.md)
  : Posterior predictive replicate datasets

## Posterior Decisions

Family-native posterior decision summaries, phase contrasts, and
marginal contrasts

- [`default_rope()`](https://mshin77.github.io/scdbayes/reference/default_rope.md)
  : Default region of practical equivalence for treatment coefficients
- [`summarize_fixed_effect_decisions()`](https://mshin77.github.io/scdbayes/reference/summarize_fixed_effect_decisions.md)
  : Posterior decision summary for fixed-effect coefficients
- [`summarize_per_case_effects()`](https://mshin77.github.io/scdbayes/reference/summarize_per_case_effects.md)
  : Per-case posterior decision summary
- [`summarize_cross_case_consistency()`](https://mshin77.github.io/scdbayes/reference/summarize_cross_case_consistency.md)
  : Posterior probability of consistent intervention direction across
  cases
- [`summarize_variance_components()`](https://mshin77.github.io/scdbayes/reference/summarize_variance_components.md)
  : Posterior summary of variance components
- [`compute_phase_contrast()`](https://mshin77.github.io/scdbayes/reference/compute_phase_contrast.md)
  : Posterior phase contrast on the response scale
- [`marginal_contrast()`](https://mshin77.github.io/scdbayes/reference/marginal_contrast.md)
  : Population-averaged counterfactual contrast at a common session
- [`summarize_phase_descriptives()`](https://mshin77.github.io/scdbayes/reference/summarize_phase_descriptives.md)
  : Pre-model descriptive phase summary

## Effect Sizes and Meta-Analysis

Bayesian design-comparable BC-SMD per study and Bayesian random-effects
pooling

- [`bc_smd_bayes()`](https://mshin77.github.io/scdbayes/reference/bc_smd_bayes.md)
  : Bayesian design-comparable between-case standardized mean difference
- [`meta_bayes()`](https://mshin77.github.io/scdbayes/reference/meta_bayes.md)
  : Bayesian random-effects meta-analysis of study effect sizes
- [`calculate_meta_summary()`](https://mshin77.github.io/scdbayes/reference/calculate_meta_summary.md)
  : Calculate meta-analytic summary statistics

## Power Analysis

Simulation-based power for SCD multilevel Bayesian models

- [`run_power_simulation()`](https://mshin77.github.io/scdbayes/reference/run_power_simulation.md)
  : Bayesian power and design analysis by simulation
