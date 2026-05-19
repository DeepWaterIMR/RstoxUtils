# Read and process NMD Biotic xml files for further use in the BioticExplorer

A wrapper for
[`processBioticFile`](https://deepwaterimr.github.io/RstoxUtils/reference/processBioticFile.md)
allowing processing multiple files simultaneously

## Usage

``` r
processBioticFiles(
  files,
  lengthUnit = "cm",
  weightUnit = "g",
  removeEmpty = TRUE,
  coreDataOnly = FALSE,
  returnOriginal = TRUE,
  dataTable = TRUE,
  convertColumns = FALSE
)
```

## Arguments

- files:

  character string specifying the file path to the xml file. Accepts
  only one file at the time.

- lengthUnit:

  character string specifying the unit for length output. Alternatives:
  "mm", "cm" or "m".

- weightUnit:

  character string specifying the unit for weight output. Alternatives:
  "g" or "kg".

- removeEmpty:

  logical indicating whether empty columns should be removed from
  output. This option also influences "coreData" columns.

- coreDataOnly:

  logical indicating whether only important core columns should be
  picked from data. See
  [`coreDataList`](https://deepwaterimr.github.io/RstoxUtils/reference/coreDataList.md)
  for list of core columns for each data type.

- returnOriginal:

  logical indicating whether the original data (`$mission` through
  `$agedetermination`) should be returned together with combined data.

- dataTable:

  logical indicating whether the output should be returned as
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)s
  instead of [data.frame](https://rdrr.io/r/base/data.frame.html)s.
  Setting this to `TRUE` speeds up further calculations using the data
  (but requires the
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  syntax).

- convertColumns:

  logical indicating whether the column types should be converted. See
  `link{convertColumnTypes}`. Setting this to `FALSE` considerably
  speeds up the function.

## Value

Returns a list of Biotic data with `$mission`, `$fishstation`,
`$catchsample`, `$individual` and `$agedetermination` data frames. The
`$stnall` and `$indall` data frames are merged from `$fishstation` and
`$catchsample` (former) and `$fishstation`, `$catchsample`,
`$individual` and `$agedetermination` (latter).

## See also

Other Biotic functions:
[`print.bioticProcData()`](https://deepwaterimr.github.io/RstoxUtils/reference/print.bioticProcData.md),
[`processBioticFile()`](https://deepwaterimr.github.io/RstoxUtils/reference/processBioticFile.md)

## Author

Mikko Vihtakari (Institute of Marine Research)
