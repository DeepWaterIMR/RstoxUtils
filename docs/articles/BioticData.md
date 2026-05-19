# Open and visualize Biotic data

Packages required to run the examples:

``` r

# Package names
packages <- c("RstoxUtils", "ggplot2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  
  if("RstoxUtils" %in% packages[!installed_packages]) {
    remotes::install_github("DeepWaterIMR/RstoxUtils", upgrade = "never")
  }
  
  installed_packages <- packages %in% rownames(installed.packages())
  install.packages(packages[!installed_packages])
}

# Load the packages to the workspace
invisible(lapply(packages, function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))}))
```

The IMR database data are distributed as [.xml files of a certain
structure](https://confluence.imr.no/display/API/Biotic+V3+API+documentation).
The
[`RstoxData::readXmlFile`](https://rdrr.io/pkg/RstoxData/man/readXmlFile.html)
function reads these files, but due to the data architecture,
information is spread across multiple data frames in the standard
format. The
[`RstoxUtils::processBioticFile`](https://deepwaterimr.github.io/RstoxUtils/reference/processBioticFile.md)
function combines the data in station-based (`stnall` element in the
list architecture) and individual fish-based (`indall`) formats. The
[`RstoxUtils::processBioticFiles`](https://deepwaterimr.github.io/RstoxUtils/reference/processBioticFiles.md)
function does the same for multiple .xml files.

These functions may come in handy for reports and software development
where only few xml files are needed. If you want to access the entire
Biotic database, it is recommended to use the the
[BioticExplorerServer](https://github.com/DeepWaterIMR/BioticExplorerServer)
package.

``` r

xml.example <- system.file("extdata", "example.xml", package = "RstoxUtils")

standard.format <- RstoxData::readXmlFile(xml.example)

## The data are as a list organized under multiple data.frames
names(standard.format)
#>  [1] "missions"                       "mission"                       
#>  [3] "fishstation"                    "catchsample"                   
#>  [5] "individual"                     "prey"                          
#>  [7] "agedetermination"               "preylengthfrequencytable"      
#>  [9] "copepodedevstagefrequencytable" "tag"                           
#> [11] "metadata"

## Station-based data are organized under 3 data frames
dim(standard.format$mission)
#> [1]  6 11
dim(standard.format$fishstation)
#> [1] 59 74
dim(standard.format$catchsample)
#> [1] 84 36

## Individual-based data have 2 additional data frames
dim(standard.format$individual)
#> [1] 446  56
dim(standard.format$agedetermination)
#> [1] 52 39

## The user has to merge these data frames to work with the data
## RstoxUtils addresses this issue and merges the data

library(RstoxUtils)

Utils.format <- RstoxUtils::processBioticFile(xml.example)

## Station-based data can now be found from 1 data frame

dim(Utils.format$stnall)
#> [1] 84 55

## The same applies for individual-based-data 
dim(Utils.format$indall)
#> [1] 446  62

## The uniting ID tags in the Utils format are
# missionid, startyear, serialno, catchsampleid and sometimes cruise for station based format
# The abovementioned and specimenid
```
