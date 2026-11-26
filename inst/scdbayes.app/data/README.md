# Example datasets

Each CSV bundled with the `scdbayes` Shiny app is a simulated example whose
phase counts and phase lengths follow the WWC (2022) SCD design thresholds
unless noted. The files illustrate design structure only; no WWC rating is
implied. Values are simulated with `set.seed(2026)` and parameters drawn from
the published SCD methodology literature.

## ab_design.csv

- Design: AB across 3 participants
- Phase pattern per case: A(5) -> B(10)
- Note: a 2-phase AB contains a single demonstration of effect, short of the three independent demonstrations the WWC design standards call for.

## reversal_design.csv

- Design: ABAB reversal/withdrawal
- Phase pattern per case: A(6) -> B(5) -> A(5) -> B(5)
- Phase lengths follow the WWC (2022) thresholds: first baseline >= 6, subsequent phases >= 5, four phases total.

## changing_criterion_design.csv

- Design: changing criterion
- Phase pattern per case: A(5) -> B1(4) -> B2(4) -> B3(4)
- `criterion` column: criterion level per subphase (22, 28, 30); NA in baseline.
- WWC (2022): three criterion changes establish three intervention-effect demonstrations.
- Note: the 5-point baseline and 4-point subphases fall below the Without Reservations data-point thresholds (first baseline >= 6, other phases >= 5); `wwc_design_check()` screens this file as Meets WWC Standards With Reservations.

## multiple_baseline_design.csv

- Design: multiple baseline across 4 participants
- Phase pattern: P1 A(6)/B(10), P2 A(8)/B(10), P3 A(10)/B(10), P4 A(12)/B(10)
- Phase lengths follow the WWC (2022) thresholds: first baseline within each tier >= 6, subsequent phases >= 5, staggered transitions, concurrent (vertical comparison) layout.

## multiple_probe_design.csv

- Design: multiple probe across 3 participants
- Phase pattern: P1 A(6)/B(10), P2 A(6)/B(8), P3 A(7)/B(5)
- Probes at sessions 1-3 (overlap), in the session each earlier tier starts intervention, and 3 sessions immediately before intervention; baseline sessions in between are planned missing and have no rows (NA outcomes would be read as unplanned missing data).
- `wwc_design_check()` screens this file as Meets WWC Standards Without Reservations.

## alternating_treatment_design.csv

- Design: alternating treatment, three conditions (A, B, C)
- Pattern per case: rotating A->B->C with 6 observations per condition
- Phase lengths follow the WWC (2022) thresholds: >= 5 data points per condition, at most 1 contiguous (within the max-2 rule).

## meta_analysis.csv

- Design: 4 studies x 3 participants, multiple-baseline-across-participants per study
- Staggered baselines within each study: A(6)/A(8)/A(10) -> B
- Moderators: `intervention_type` (Type_A / B / C), `age_group` (Adolescent / Child)
- Schema modeled after Shin, Hart, & Simmons (2024) `wp_data.csv` at [OSF 8WBTA](https://doi.org/10.17605/OSF.IO/8WBTA).

## References

- What Works Clearinghouse. (2022). *What Works Clearinghouse procedures and standards handbook, version 5.0* (Chapter 6: Single-case design standards). U.S. Department of Education, Institute of Education Sciences, National Center for Education Evaluation and Regional Assistance. Public domain.
- Moeyaert, M., Ugille, M., Ferron, J. M., Beretvas, S. N., & Van den Noortgate, W. (2014). The influence of the design matrix on treatment effect estimates in the quantitative analyses of single-subject experimental design research. *Behavior Modification*, 38(5), 665-704.
- Pustejovsky, J. E., Swan, D. M., & English, K. W. (2020). How many tiers do we need? Type I errors and power in multiple baseline designs. *Perspectives on Behavior Science*, 43, 691-704.
- Shin, M., Hart, S. L., & Simmons, M. (2024). Meta-analysis of single-case design research: Application of multilevel modeling. *School Psychology, 39*(6), 625-635. https://doi.org/10.1037/spq0000637
