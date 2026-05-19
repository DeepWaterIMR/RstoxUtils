# Read landing data from annually compiled Excel files

Read landing data from annually compiled Excel files

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

## See also

Other Landings functions:
[`downloadLandings()`](https://deepwaterimr.github.io/RstoxUtils/reference/downloadLandings.md)
