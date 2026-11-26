# Launch the scdbayes Shiny application

Launch the scdbayes Shiny application

## Usage

``` r
run_app(...)
```

## Arguments

- ...:

  Arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Called for its side effect (launches the app).

## Examples

``` r
if (interactive()) {
  run_app()
}
```
