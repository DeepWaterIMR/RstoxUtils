# Package index

## Package documentation

Quick access to the package documentation

- [`RstoxUtils`](https://deepwaterimr.github.io/RstoxUtils/reference/RstoxUtils-package.md)
  [`RstoxUtils-package`](https://deepwaterimr.github.io/RstoxUtils/reference/RstoxUtils-package.md)
  : Utility functions for the Stox Project

## Download Norwegian sales note data

Functions to download and access sales note data (landings from
commercial fisheries) distributed by the Directorate of Fisheries in
Norway. See the [landings data
vignette](https://deepwaterimr.github.io/RstoxUtils/articles/LandingsData.html)
for examples.

- [`downloadLandings()`](https://deepwaterimr.github.io/RstoxUtils/reference/downloadLandings.md)
  : Download sales note data for a species from the IMR database
- [`readSluttseddelXLS()`](https://deepwaterimr.github.io/RstoxUtils/reference/readSluttseddelXLS.md)
  : Read landing data from annually compiled Excel files

## ERS functions

Functions to download and access Electronic Reporting System data on
exact positions and times of commercial fishing events by the Norwegian
fleet for vessels \>15 m distributed by the Directorate of Fisheries in
Norway. See the [ERS data
vignette](https://deepwaterimr.github.io/RstoxUtils/articles/ERSdata.html)
for examples.

- [`downloadERS()`](https://deepwaterimr.github.io/RstoxUtils/reference/downloadERS.md)
  :

  Download Electronic Reporting System (ERS) catch data into a folder
  from the [Norwegian Directorate of Fisheries
  webpage](https://www.fiskeridir.no/statistikk-tall-og-analyse/data-og-statistikk-om-yrkesfiske/apne-data-elektronisk-rapportering-ers)

- [`extractERS()`](https://deepwaterimr.github.io/RstoxUtils/reference/extractERS.md)
  : Extract ERS information from detailed Norwegian fisheries logbooks

- [`extractLogbook()`](https://deepwaterimr.github.io/RstoxUtils/reference/extractLogbook.md)
  : Legacy function to extract ERS information from detailed Norwegian
  fisheries logbooks from Excel files on the IMR server

## Biotic functions

Functions to open IMR biotic data from xml files. See the
[vignette](https://deepwaterimr.github.io/RstoxUtils/articles/BioticData.html)
for details. Note that the
[BioticExplorerDatabase](https://github.com/DeepWaterIMR/BioticExplorerServer)
package offers a more flexible way to search and filter the entire
Biotic database.

- [`processBioticFile()`](https://deepwaterimr.github.io/RstoxUtils/reference/processBioticFile.md)
  : Read and process a NMD Biotic xml file for further use in the
  BioticExplorer

- [`processBioticFiles()`](https://deepwaterimr.github.io/RstoxUtils/reference/processBioticFiles.md)
  : Read and process NMD Biotic xml files for further use in the
  BioticExplorer

- [`print(`*`<bioticProcData>`*`)`](https://deepwaterimr.github.io/RstoxUtils/reference/print.bioticProcData.md)
  :

  Print processed NMD Biotic data (`bioticProcData`) objects

## Other functions

Other potentially helpful functions.

- [`read.pos()`](https://deepwaterimr.github.io/RstoxUtils/reference/read.pos.md)
  : Read and compile IMR vessel position files from a folder for cruise
  reports

## Definitions

Lists of data definitions, codes, and classifications used by the IMR
and FDir and functions used to generate them. See the
[vignette](https://deepwaterimr.github.io/RstoxUtils/articles/CodeDefinitions.html)
to explore the tables

- [`cruiseSeriesList`](https://deepwaterimr.github.io/RstoxUtils/reference/cruiseSeriesList.md)
  : NMD/IMR cruise series list
- [`gearList`](https://deepwaterimr.github.io/RstoxUtils/reference/gearList.md)
  : NMD/IMR gear code list
- [`taxaList`](https://deepwaterimr.github.io/RstoxUtils/reference/taxaList.md)
  : NMD/IMR taxa code list
- [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)
  : List of Norwegian Directorate of Fisheries logbook codes
- [`prepareCruiseSeriesList()`](https://deepwaterimr.github.io/RstoxUtils/reference/prepareCruiseSeriesList.md)
  : Prepare cruise series list
- [`prepareGearList()`](https://deepwaterimr.github.io/RstoxUtils/reference/prepareGearList.md)
  : Prepare gear list
- [`prepareTaxaList()`](https://deepwaterimr.github.io/RstoxUtils/reference/prepareTaxaList.md)
  : Prepare taxa list
- [`readFdirCodes()`](https://deepwaterimr.github.io/RstoxUtils/reference/readFdirCodes.md)
  : Retrieve the Norwegian Directorate of Fisheries codes from a code
  list

## Internal helper functions

Internal functions dealing with data and graphics for basemap. These
functions are run on the background and only needed if you modify your
maps beyond the options offered by ggOceanMaps.

- [`guess_colname()`](https://deepwaterimr.github.io/RstoxUtils/reference/guess_colname.md)
  : Guess column names from a list of candidates
- [`convertColumnTypes()`](https://deepwaterimr.github.io/RstoxUtils/reference/convertColumnTypes.md)
  : Converts column types in a data frame to (hopefully) correct types

## Datasets

Example datasets. Note that the [Norwegian Statistical Fishing
Areas](https://mikkovihtakari.github.io/ggOceanMaps/reference/fdir_main_areas.html)
(hovedområde) as well as [ICES
areas](https://mikkovihtakari.github.io/ggOceanMaps/reference/ices_areas.html)
can be accessed through the [ggOceanMaps
package](https://mikkovihtakari.github.io/ggOceanMaps).

- [`ers_example_data`](https://deepwaterimr.github.io/RstoxUtils/reference/ers_example_data.md)
  : Example ERS data
- [`salesnote_example_data`](https://deepwaterimr.github.io/RstoxUtils/reference/salesnote_example_data.md)
  : Example sales note data downloaded through the API
- [`salesnote_xls_data`](https://deepwaterimr.github.io/RstoxUtils/reference/salesnote_xls_data.md)
  : Example historic sales note data extracted from Excel sheets on IMR
  server
