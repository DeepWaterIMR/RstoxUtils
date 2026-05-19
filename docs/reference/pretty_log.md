# Round to pretty log breaks

Rounds values to human-readable breaks suitable for logarithmic scales.

## Usage

``` r
pretty_log(x, f = round)
```

## Arguments

- x:

  numeric vector to round

- f:

  rounding function: [`floor`](https://rdrr.io/r/base/Round.html),
  [`ceiling`](https://rdrr.io/r/base/Round.html) or
  [`round`](https://rdrr.io/r/base/Round.html)

## Value

A numeric vector of the same length as `x`.

## See also

[`round_any`](https://deepwaterimr.github.io/RstoxUtils/reference/round_any.md)

## Author

Mikko Vihtakari
