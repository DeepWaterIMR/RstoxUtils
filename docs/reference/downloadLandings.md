# Download sales note data for a species from the IMR database

The function downloads sales note/landings ("sluttseddel") data from IMR
database. Requires access to the intranet.

## Usage

``` r
downloadLandings(
  species,
  years = NULL,
  returned_data = "both",
  separate = TRUE
)
```

## Arguments

- species:

  any species identification name in `FDIRcodes$speciesCodes` as
  character. Only one species at the time allowed.

- years:

  an integer vector of years to download. If `NULL` (default), all years
  are downloaded. Please note that this option can take very long time
  and lead to huge datasets.

- returned_data:

  character argument specifying what type of data should be returned.
  Use `"sales notes"` to return the original sales note data from the
  server, `"summary"` to only return summarized catches, or `"both"` to
  return both sales notes and summarized catches in a list.

- separate:

  logical indicating whether years should be downloaded as separate API
  calls or as one call. Setting this to `FALSE` can save time, but lead
  to unexpected behavior because the download sometimes gets truncated
  for large datasets. Only relevant when the `years` argument contains
  multiple years.

## Value

Depends on `returned_data`: a list of sales note data frames when
`"sales notes"`; a data.frame with columns `year`, `date`, `month`,
`gear_id`, `vessel_length`, `main_area`, `sub_area`, `nation`, `weight`
when `"summary"`; or a list containing both when `"both"` (default).

## See also

Other Landings functions:
[`readSluttseddelXLS()`](https://deepwaterimr.github.io/RstoxUtils/reference/readSluttseddelXLS.md)

## Author

Mikko Vihtakari

## Examples

``` r
if (FALSE) { # \dontrun{
downloadLandings("brugde") # Basking shark, all years
downloadLandings("kveite", years = 2000:2001) # halibut, 2000-01
} # }
```
