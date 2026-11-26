# Analyze design patterns

Analyze design patterns

## Usage

``` r
analyze_design_patterns(data)
```

## Arguments

- data:

  A data frame with case and phase columns.

## Value

A list with phase_patterns, temporal_structure, baseline_length,
classification_logic, statistical_implications, and recommendations.

## Examples

``` r
d <- data.frame(
  case = rep(c("C1", "C2"), each = 10),
  phase = rep(c("A", "B"), 10),
  time = rep(1:10, 2),
  outcome = rnorm(20)
)
analyze_design_patterns(d)
#> $phase_patterns
#> [1] "A-B-A-B-A-B-A-B-A-B (in 2/2 cases)"
#> 
#> $temporal_structure
#> [1] "Concurrent phase changes"
#> 
#> $baseline_length
#> [1] "Mean: 5.0 sessions (range: 5-5)"
#> 
#> $classification_logic
#> [1] "Detected 2-phase design across 2 cases with consistent A-B-A-B-A-B-A-B-A-B pattern. This suggests a simple AB comparison design suitable for basic treatment effect analysis."
#> 
#> $statistical_implications
#> $statistical_implications[[1]]
#> [1] "Small sample size (< 5 cases) may limit generalizability and power"
#> 
#> $statistical_implications[[2]]
#> [1] "Two-phase design allows for level and trend analysis"
#> 
#> $statistical_implications[[3]]
#> [1] "Consistent phase patterns support pooled analysis across cases"
#> 
#> 
#> $stat_agent_rec
#> [1] "With 2 cases and A-B-A-B-A-B-A-B-A-B pattern, recommend individual case-level analysis approach. Report family-native posterior decisions (probability of direction, ROPE, probability of meaningful change)."
#> 
#> $method_agent_rec
#> [1] "Design classification: alternating.  Ensure adequate baseline stability before intervention phases."
#> 
#> $validity_agent_rec
#> [1] "Reversal design supports internal validity through experimental control demonstration. Assess reversibility of target behavior."
#> 
#> $optim_agent_rec
#> [1] "Consider expanding to at least 5 cases for multilevel modeling."
#> 
```
