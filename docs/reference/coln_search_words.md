# A list of search words used to find column names in Excel sheets

A list containing search words used in
[`guess_colname`](https://deepwaterimr.github.io/RstoxUtils/reference/guess_colname.md)
function.

## Usage

``` r
coln_search_words(column, return_name = FALSE)
```

## Arguments

- column:

  A required column name as character string

- return_name:

  Logical. Should name of `column` be returned instead of value from the
  list? Used in internal conditional functions.

## Details

The function accepts following required column names: `expedition`,
`station`, `type`, `sample_name`, `longitude`, `latitude`, `date`,
`bottom_depth`, `gear`, `from`, `to`, `responsible` and `comment`.

## Author

Mikko Vihtakari
