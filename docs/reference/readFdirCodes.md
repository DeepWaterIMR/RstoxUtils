# Retrieve the Norwegian Directorate of Fisheries codes from a code list

This function retrieves codes used in the electronic logbook data from
an Excel sheet published by the Directorate of Fisheries. This list is
already supplied in the package and the function is only required to
update the codes.

## Usage

``` r
readFdirCodes(
  path,
  speciesSheet = "B-Fiskeslag",
  speciesStartRow = 19,
  speciesHeaderRow = 17,
  gearSheet = "A7-Redskap",
  gearStartRow = 8
)
```

## Arguments

- path:

  Character string specifying the path to the Excel file downloaded from
  the [Directorate of Fisheries
  webpage](https://www.fiskeridir.no/Yrkesfiske/Rapportering-ved-landing/Kodeliste).

- speciesSheet:

  Character string specifying the name of the tab containing species
  codes.

- speciesStartRow:

  Integer specifying the `skip` argument for
  [`read_xlsx`](https://readxl.tidyverse.org/reference/read_excel.html)
  in the species code tab.

- speciesHeaderRow:

  Integer specifying row number of header in the species code tab.

- gearSheet:

  Character string specifying the name of the tab containing species
  codes.

- gearStartRow:

  Integer specifying the `skip` argument for
  [`read_xlsx`](https://readxl.tidyverse.org/reference/read_excel.html)
  in the gear code tab.

## Details

The function has been written for [the code list Excel
sheet](https://www.fiskeridir.no/Yrkesfiske/Rapportering-ved-landing/Kodeliste)
published on 2020-10-30. You may have to adjust the function depending
on changes in newer versions of the file.

## Author

Mikko Vihtakari
