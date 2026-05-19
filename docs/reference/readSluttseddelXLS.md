# Read landing data from annually compiled Excel files

Reads and compiles landing (sales note / sluttseddel) data from the
annually compiled Excel files stored on the IMR server. Requires access
to the IMR intranet and the Excel files to be in place.

## Usage

``` r
readSluttseddelXLS(
  species,
  dataDir,
  years = 1977:2022,
  dropMissingMainArea = TRUE
)
```

## Arguments

- species:

  Character string defining the species using the FDIR Norwegian species
  names. `NULL` returns all species.

- dataDir:

  Character vector defining the path to the folder where Excel files are
  located (typically "sluttseddel_xls_ferdige_År")

- years:

  Integer vector defining the years to read. Files in later years tend
  to be a mess and this option avoids crashes. Use `NULL` to try opening
  all files.

- dropMissingMainArea:

  Logical indicating whether catches with missing main area should be
  dropped.

## Value

A data.frame with columns `year`, `month`, `main_area`, `sub_area`,
`ices_area`, `gear_id`, `gear_category`, `gear`, and `weight` (and
`species` when `species = NULL`).

## See also

Other Landings functions:
[`downloadLandings()`](https://deepwaterimr.github.io/RstoxUtils/reference/downloadLandings.md)
