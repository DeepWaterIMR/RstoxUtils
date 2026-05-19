# Extract ERS information from detailed Norwegian fisheries logbooks

Extracts information from detailed Norwegian fisheries logbook files
downloaded using the
[`downloadERS`](https://deepwaterimr.github.io/RstoxUtils/reference/downloadERS.md)
function.

## Usage

``` r
extractERS(
  path,
  species = NULL,
  translate_header = TRUE,
  method = "start",
  language = "norwegian",
  print.filename = FALSE,
  ncores = 1
)
```

## Arguments

- path:

  Character argument defining the path to the folder where data are
  located. This can either be the entire folder in which case all the
  .zip files will be read or a single .zip file.

- species:

  Character argument defining the species. Species names can be given in
  Norwegian, English or Latin depending on the `language` argument.
  Species names are sampled from
  [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)`$speciesCodes`.
  `NULL` returns all species in the ERS data.

- translate_header:

  Logical indicating whether to translate the original column names in
  data selecting useful columns. If `FALSE`, the original columns are
  returned without modifications. If `TRUE`, columns are modified. See
  Return for details.

- method:

  Character argument specifying the method for position, time and depth
  extraction. Alternatives: `"start"` takes the reported start values,
  `"end"` extracts the reported end values and `"average"` takes an
  average of start and end values.

- language:

  Character argument in lower case specifying the language of species
  names in the returned data table. Alternatives: `"norwegian"`,
  `"english"`, or `"latin"`, The species names are acquired from
  [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)`$speciesCodes`.

- print.filename:

  Logical indicating whether the file name should be printed to console
  while processing. Useful for debugging.

- ncores:

  Integer giving the number of cores to be used when combining the
  files. Speeds up the calculus.

## Value

If `translate_header = TRUE`, a data.table containing following columns:

- `"year"` year of the fishing event.

- `"month"` month of the fishing event.

- `"id"` identification number of the fishing event.

- `"vesselName"` name of the vessel.

- `"radioCallSign"` call signal of the vessel.

- `"EEZ"` reported exclusive economic zone of the fishing event.

- `"ICESArea"` reported FAO area of the fishing event.

- `"FDIRMainArea"` reported main area (hovedomraade) of the fishing
  event.

- `"date"` date and time of the fishing event acquired based on the
  `method` argument.

- `"lon"` longitude coordinate for the fishing event in decimal degrees
  acquired based on the `method` argument.

- `"lat"` latitude coordinate for the fishing event in decimal degrees
  acquired based on the `method` argument.

- `"depth"` reported fishing depth in meters acquired based on the
  `method` argument.

- `"gear"` gear name translated from `gearId` based on
  [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)`$gearCodes`.

- `"gearGroup"` reported gear group according to the Norwegian
  Directorate of Fisheries.

- `"gearCategory"` gear category translated from `gearId` based on
  [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)`$gearCodes`.

- `"gearSpecification"` reported gear specification according to the
  Norwegian Directorate of Fisheries.

- `"gearMeshSize"` reported gear mesh size.

- `"gearAmount"` reported gear amount (number of hooks etc.).

- `"gearMeshSize"` reported issues with the gear.

- `"fishTime"` reported fishing time in minutes.

- `"fishDistance"` reported fishing distance in meters.

- `"targetSp"` target species based on the FAO definition: the species
  with highest weight in catch.

- `"catchSp"` reported catch species based on the `species`, `language`
  and `subspecies` arguments. Translated from the species ID
  (FANGSTART_NS or FANGSTART_FAO) based on
  [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)`$speciesCodes`.

- `"weight"` reported catch of the `catchSp` in kilograms.

Note that NA catch species (`Art - FDIR`) are filtered out when
`translate_header = TRUE`. This can include some records of fishing with
no catch.

If `translate_header = FALSE`, the data are returned without filtering
or modifications with the Norwegian column names.

## Details

This function extracts information from the Norwegian fisheries logbooks
provided in the `.zip` format on the [Norwegian Directorate of Fisheries
(FDIR)
webpage](https://www.fiskeridir.no/statistikk-tall-og-analyse/data-og-statistikk-om-yrkesfiske/apne-data-elektronisk-rapportering-ers).
Use the
[`downloadERS`](https://deepwaterimr.github.io/RstoxUtils/reference/downloadERS.md)
function to download these data on your computer. Note that at the time
of writing, FDIR only provided ERS data for \> 15 m vessels due to
unclarities in the Norwegian Privacy Act. If you want to access ERS data
from smaller vessels, use the legacy function
[`extractLogbook`](https://deepwaterimr.github.io/RstoxUtils/reference/extractLogbook.md),
which reads the confidential ERS data on IMR servers.

## See also

Other ERS functions:
[`downloadERS()`](https://deepwaterimr.github.io/RstoxUtils/reference/downloadERS.md),
[`extractLogbook()`](https://deepwaterimr.github.io/RstoxUtils/reference/extractLogbook.md)

## Author

Mikko Vihtakari (Institute of Marine Research)
