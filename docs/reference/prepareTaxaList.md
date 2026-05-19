# Prepare taxa list

Wrapper for
[`prepareTaxaList`](https://rdrr.io/pkg/BioticExplorerServer/man/prepareTaxaList.html).
Uses the updated IMR reference API. See BioticExplorerServer for full
documentation.

## Usage

``` r
prepareTaxaList(verbose = FALSE)
```

## Arguments

- verbose:

  Logical. Print status messages during download. Defaults to `FALSE`.

## Value

A data.table of taxon codes and names from the IMR reference API.

## See also

Other Biotic functions:
[`prepareCruiseSeriesList()`](https://deepwaterimr.github.io/RstoxUtils/reference/prepareCruiseSeriesList.md),
[`prepareGearList()`](https://deepwaterimr.github.io/RstoxUtils/reference/prepareGearList.md),
[`print.bioticProcData()`](https://deepwaterimr.github.io/RstoxUtils/reference/print.bioticProcData.md),
[`processBioticFile()`](https://deepwaterimr.github.io/RstoxUtils/reference/processBioticFile.md),
[`processBioticFiles()`](https://deepwaterimr.github.io/RstoxUtils/reference/processBioticFiles.md)
