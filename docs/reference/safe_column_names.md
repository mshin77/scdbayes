# Rename data columns to syntactic names

Column names reach model formulas, where text such as
`"x+I(file.create('f'))"` would run as code. Each name is trimmed, every
character outside `A-Z a-z 0-9 . _` becomes `.`, the reserved `...` and
`..1`, `..2`, ... get an `X` prefix, and
[`make.names()`](https://rdrr.io/r/base/make.names.html) with
`unique = TRUE` fixes the rest, so every result matches
`^[A-Za-z.][A-Za-z0-9._]*$` and parses as one symbol.

## Usage

``` r
safe_column_names(data)
```

## Arguments

- data:

  Data frame.

## Value

`data` with syntactic, unique names. The attribute `"renamed"` is a
character vector of new names named by the original names, for the
changed columns only.

## Examples

``` r
d <- safe_column_names(data.frame(`a b` = 1, `x+I(q())` = 2, check.names = FALSE))
attr(d, "renamed")
#>        a b   x+I(q()) 
#>      "a.b" "x.I.q..." 
```
