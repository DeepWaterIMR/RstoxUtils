# Legacy function to extract ERS information from detailed Norwegian fisheries logbooks from Excel files on the IMR server

Extracts ERS information from Excel files on the IMR server.

## Usage

``` r
extractLogbook(
  path,
  species,
  method = "start",
  language = "norwegian",
  subspecies = FALSE,
  remove.sensitive = TRUE,
  print.filename = FALSE
)
```

## Arguments

- path:

  Character argument defining the path to the folder where data are
  located

- species:

  Character argument defining the species. Species names can be given in
  Norwegian, English or Latin. Species names are sampled from
  [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)`$speciesCodes`.

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

- subspecies:

  Logical. Should NS codes (i.e. species sub-categories) be used to
  translate species names? If `FALSE`, FAO codes are used leading to
  actual species names.

- remove.sensitive:

  Logical indicating whether the function should remove sensitive data
  fields.

- print.filename:

  Logical indicating whether the file name should be printed to console
  while processing. Useful for debugging.

## Value

A data.table containing following columns:

- `"year"` year of the fishing event.

- `"month"` month of the fishing event.

- `"lon"` longitude coordinate for the fishing event in decimal degrees
  acquired based on the `method` argument.

- `"lat"` latitude coordinate for the fishing event in decimal degrees
  acquired based on the `method` argument.

- `"gearId"` reported gear ID (REDSKAP_NS) in the .psv files

- `"gear"` gear name translated from `gearId` based on
  [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)`$gearCodes`.

- `"gearCat"` gear category translated from `gearId` based on
  [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)`$gearCodes`.

- `"fishTime"` reported fishing time.

- `"effort"` reported fishing effort (number of hooks etc.)

- `"dist"` reported fishing distance in meters.

- `"depth"` reported fishing depth in meters acquired based on the
  `method` argument.

- `"targetSpFAO"` target species based on the FAO definition: the
  species with highest mass in catch.

- `"targetSpRep"` target species reported by the fishermen.

- `"species"` reported catch species based on the `species`, `language`
  and `subspecies` arguments. Translated from the species ID
  (FANGSTART_NS or FANGSTART_FAO) based on
  [`FDIRcodes`](https://deepwaterimr.github.io/RstoxUtils/reference/FDIRcodes.md)`$speciesCodes`.

- `"mass"` reported catch of the `species` in kilograms.

## Details

This function extracts information from the Norwegian fisheries logbooks
provided in the `.psv` format on the IMR server. Note that these data
are confidential and protected by the Norwegian Privacy Act
(personvernloven). Keep the data on the server or in another safe place
(not on the cloud). Do not send the input data around on email. The
`remove.sensitive` argument can be used to disconnect the output from
the confidential input data. This output can be distributed to people
who have not signed the confidentiality statement (students, ICES
colleagues, etc.) also using email. The data are property of the
Norwegian Directorate of Fisheries and you should consult them if you
plan to publish any parts of the dataset.

Note that the structure of logbook files is not confidential and
therefore this function can be distributed freely. The data required by
the function cannot (do not send those data on email).

There are many duplicate records in the logbook files on the server.
This function removes these duplicates automatically, but the process
takes time and is not very controlled. If you want to be sure about the
data you include, copy the desired files to another folder (in a safe
place) and run the function for that folder.

## See also

Other ERS functions:
[`downloadERS()`](https://deepwaterimr.github.io/RstoxUtils/reference/downloadERS.md),
[`extractERS()`](https://deepwaterimr.github.io/RstoxUtils/reference/extractERS.md)

## Author

Mikko Vihtakari (Institute of Marine Research)
