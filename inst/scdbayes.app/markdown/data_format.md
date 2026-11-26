## Data Format

One row per session, with these columns.

| Column | Type | Description |
|--------|------|-------------|
| `case` | text/numeric | Case identifier |
| `time` | numeric | Session number |
| `phase` | text | Condition (A, B, ...) |
| `outcome` | numeric | Measured behavior |
| `study` | text | Study (optional; meta-analysis) |
| moderators | text/numeric | Optional covariates |

- Phase labels: A (baseline), B (intervention), M (maintenance), G (generalization), or numbered subphases such as B1, B2, B3
- Reversal phases are numbered automatically
- Names like "participant" or "session" are detected
