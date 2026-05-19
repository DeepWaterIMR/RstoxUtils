# Read and compile IMR vessel position files from a folder for cruise reports

Compiles the standard IMR vessel position files into a data.table for
further analyses

## Usage

``` r
read.pos(path, log.file = FALSE)
```

## Arguments

- path:

  Character string defining the path to the `/CRUISE_LOG/TRACK/` folder

- log.file:

  Logical indicating whether the file from `path` has `.log` extension.
  If false, `.csv` is assumed.

## Value

A data.table containing all position information from the
`/CRUISE_LOG/TRACK/` folder

## Author

Mikko Vihtakari
