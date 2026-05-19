# Download Electronic Reporting System (ERS) catch data into a folder from the [Norwegian Directorate of Fisheries webpage](https://www.fiskeridir.no/statistikk-tall-og-analyse/data-og-statistikk-om-yrkesfiske/apne-data-elektronisk-rapportering-ers)

Download Electronic Reporting System (ERS) catch data into a folder from
the [Norwegian Directorate of Fisheries
webpage](https://www.fiskeridir.no/statistikk-tall-og-analyse/data-og-statistikk-om-yrkesfiske/apne-data-elektronisk-rapportering-ers)

## Usage

``` r
downloadERS(
  dest,
  years = 2011:as.integer(format(Sys.Date(), "%Y")),
  overwrite = "yes",
  fdir_url = "https://register.fiskeridir.no/vms-ers/ERS/"
)
```

## Arguments

- dest:

  File path as character where the data should be downloaded to.

- years:

  An integer vector of years to download. The default downloads all
  years.

- overwrite:

  Either `"yes"`, `"no"` or `"force"`. The "yes" option overwrites
  annual .zip files only if the were modified on the webpage compared to
  existing files, "no" returns an error if there are existing .zip files
  in `dest` and "force" downloads all .zip files from the website again
  overwriting all existing files.

- fdir_url:

  Character defining the URL where data should be downloaded from.
  You'll only need to modify this if the source URL has changed. In that
  case, send an email to the maintainer such that the address can be
  updated.

## Value

Returns a data frame showing which files were modified. Downloads data
into folder specified by the `dest` argument.

## Details

Please note that this function downloads large amounts of data and
places then as zip files in the folder defined by `dest`. Use the
[`extractERS`](https://deepwaterimr.github.io/RstoxUtils/reference/extractERS.md)
to extract information from the downloaded files.

## See also

Other ERS functions:
[`extractERS()`](https://deepwaterimr.github.io/RstoxUtils/reference/extractERS.md),
[`extractLogbook()`](https://deepwaterimr.github.io/RstoxUtils/reference/extractLogbook.md)

## Author

Mikko Vihtakari
