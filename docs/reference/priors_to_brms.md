# Convert prior list to brms prior object

Internal helper that converts the list returned by
`assemble_brms_priors` into a stacked `brmsprior`.

## Usage

``` r
priors_to_brms(priors)
```

## Arguments

- priors:

  Named list of prior specifications.

## Value

A `brmsprior` object.
