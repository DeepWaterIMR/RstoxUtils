# Guess column names from a list of candidates

Uses fuzzy matching ([`agrep`](https://rdrr.io/r/base/agrep.html)) to
guess (column) names from a list of allowed character strings.

## Usage

``` r
guess_colname(cols, df, candidates = coln_search_words)
```

## Arguments

- cols:

  Character vector of approximate column names to be guessed

- df:

  data frame containing column names

- candidates:

  a switch argument or a character vector giving the candidates to be
  used in matching. The
  [`coln_search_words`](https://deepwaterimr.github.io/RstoxUtils/reference/coln_search_words.md)
  function is used by default.

## Value

A named character vector mapping each required column name in `cols` to
the best-matching column name found in `df`.

## Author

Mikko Vihtakari, Conrad Helgeland
