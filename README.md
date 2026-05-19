# RstoxUtils
**Utility functions for the Stox Project. R package, updated 2026-05-19.**

This package contains utility functions for stock assessment and data flow within the Institute of Marine Research (IMR). The package has two purposes: 1) To function as a showcase and developmental platform for functions that may be included in the future releases of the Stox Project. 2) To provide a collection of functions needed in the internal workflow of the Deep-sea species group at IMR.

## Installation

The package requires the [**RstoxData**](https://github.com/StoXProject/RstoxData/releases) [Stox project](https://github.com/StoXProject) package to function. These packages can be installed by following the links for each package or using the [**devtools**](https://cran.r-project.org/web/packages/devtools/index.html) package. The **RstoxUtils** package can be installed using **devtools** once all Stox project packages are installed correctly. 


``` r
devtools::install_github("DeepWaterIMR/RstoxUtils", upgrade = "never")
```

The **RstoxUtils** uses multiple GIS packages developed for R. You may have to update these (Packages -> Update -> Select all -> Install updates in R Studio). 



If the installation of a dependency fails, try installing those packages manually (using RStudio or `install.packages`).

## Usage

See the [website](https://deepwaterimr.github.io/RstoxUtils), [function reference](https://deepwaterimr.github.io/RstoxUtils/reference/index.html) and specific articles for the use of the package:

 1. [Sales note data](https://deepwaterimr.github.io/RstoxUtils/articles/LandingsData.html)
 2. [ERS data](https://deepwaterimr.github.io/RstoxUtils/articles/ERSdata.html)
 3. [Biotic data](https://deepwaterimr.github.io/RstoxUtils/articles/BioticData.html)

The package also includes functions not covered by the vignettes above: `processBioticFile()` / `processBioticFiles()` for reading and merging NMD Biotic XML files into data.table objects; `readFdirCodes()` for updating the bundled FDIR species and gear code list; `read.pos()` for reading IMR vessel position tracking files; and `extractLogbook()` for legacy confidential logbook data from IMR servers. Reference data (cruise series, gear lists, taxa lists) can be refreshed using `prepareCruiseSeriesList()`, `prepareGearList()`, and `prepareTaxaList()`, which delegate to [BioticExplorerServer](https://github.com/DeepWaterIMR/BioticExplorerServer).

Note that the strata functions from this package have been moved to the [RstoxStrata](https://deepwaterimr.github.io/RstoxStrata/index.html) package and the entire IMR Biotic database can be accessed using the [BioticExplorerServer package](https://github.com/DeepWaterIMR/BioticExplorerServer).

## Citation

When using use the package for stock assessment or scientific articles, please cite it:


``` r
citation("RstoxUtils")
#> To cite package 'RstoxUtils' in publications use:
#> 
#>   Vihtakari M (2026). _RstoxUtils: Utility Functions for the Stox
#>   Project Within the Institute of Marine Research, Norway_. R package
#>   version 0.5.0, <https://deepwaterimr.github.io/RstoxUtils>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {RstoxUtils: Utility Functions for the Stox Project Within the Institute of Marine Research, Norway},
#>     author = {Mikko Vihtakari},
#>     year = {2026},
#>     note = {R package version 0.5.0},
#>     url = {https://deepwaterimr.github.io/RstoxUtils},
#>   }
```

## Contributions and contact information

Any contributions to the package are more than welcome. Please contact the package creator Mikko Vihtakari (<mikko.vihtakari@hi.no>) to discuss your ideas on improving the package or place a request in the issues section. 
