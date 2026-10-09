# AGENTS.md

This file provides guidance to Codex (Codex.ai/code) when working with code in this repository.

## Package Overview

**RstoxUtils** is an R package providing utility functions for stock assessment and data flow at the Institute of Marine Research (IMR), Norway. It serves as both a developmental platform for functions destined for the [StoX Project](https://github.com/StoXProject) and a collection of internal workflow tools for IMR's deep-sea species group.

Key dependency: requires the [RstoxData](https://github.com/StoXProject/RstoxData/releases) package from the StoX project, which is not on CRAN.

## Common Commands

All commands run from an R console in the package root directory.

```r
# Load package during development
devtools::load_all()

# Document (run after editing roxygen2 comments)
devtools::document()

# Build and check
devtools::check()

# Install locally
devtools::install()

# Build pkgdown website
pkgdown::build_site()

# Run a single function interactively (see debugging params at top of each file)
# Each R file contains commented debugging parameters at the top of function bodies
```

There is no automated test suite (`testthat` or otherwise). Testing is done interactively using the example datasets bundled in the package (`ers_example_data`, `salesnote_example_data`, `salesnote_xls_data`).

## Architecture

The package is organized into three functional areas, each with paired download/extract or prepare functions:

### ERS (Electronic Reporting System) — Norwegian Fisheries Logbooks
- [R/downloadERS.R](R/downloadERS.R): Downloads `.zip` files of public ERS data from the Norwegian Directorate of Fisheries (FDIR) web server. Compares `Last-Modified` headers to avoid redundant downloads.
- [R/extractERS.R](R/extractERS.R): Reads downloaded `.zip` files, parses the semicolon-delimited CSV inside each archive, translates Norwegian column names to English, filters by species using `FDIRcodes$speciesCodes`, and resolves positions/times based on `method` ("start", "end", or "average"). Uses `pbmcapply` for optional parallel processing.
- [R/extractLogbook.R](R/extractLogbook.R): Legacy function for confidential ERS data in `.psv` format stored on IMR servers (not publicly accessible). Uses `lapply` + `txtProgressBar` sequentially (parallel call is commented out).

### Sales Notes / Landings Data
- [R/downloadLandings.R](R/downloadLandings.R): Downloads sales note ("sluttseddel") data from the IMR intranet API (`tomcat7.imr.no`). Requires IMR intranet access.
- [R/readSluttseddelXLS.R](R/readSluttseddelXLS.R): Reads historical sales note data from Excel files on IMR servers.

### NMD Biotic XML Data
- [R/processBiotic_functions.R](R/processBiotic_functions.R): Reads and flattens the hierarchical NMD Biotic XML format (via `RstoxData::readXmlFile`) into a list of joined data frames: `$mission`, `$fishstation`, `$catchsample`, `$individual`, `$agedetermination`, `$stnall` (station + catch), `$indall` (full join through to age data). `processBioticFile()` handles one file; `processBioticFiles()` loops over multiple files. Returns a `bioticProcData` S3 class with a custom `print` method.

### Reference Data (Bundled Datasets)
- `FDIRcodes` (`data/FDIRcodes.rda`): List of FDIR code tables — `$speciesCodes` (Norwegian/English/Latin/FAO/NS codes), `$gearCodes` (gear names and categories). This is the central lookup used throughout ERS and landings functions.
- `taxaList`, `gearList`, `cruiseSeriesList`: IMR-specific code lists, updated via `prepareTaxaList()`, `prepareGearList()`, `prepareCruiseSeriesList()`.
- [R/readFdirCodes.R](R/readFdirCodes.R): Parses the FDIR Excel code list to regenerate `FDIRcodes`. Run this when FDIR updates their codes, then re-save as package data.

### Utility Functions
- [R/internal_utility_functions.R](R/internal_utility_functions.R): Small helpers (`FS`, `LS` for ggplot unit conversion; `round_any`, `pretty_log`; `quiet`, `tryCatchWE`).
- [R/read.pos.R](R/read.pos.R): Reads position data files.

## Key Conventions

- **Roxygen2** is used for all documentation. Run `devtools::document()` after any `#'` comment changes.
- **Debugging parameters**: each main function file has a commented block at the top of the function body (e.g., `# species = "Snabeluer"; path = '...'`) that replicates manual interactive testing. Preserve these when editing.
- **Column name encoding**: ERS CSV files use Norwegian characters. `extractERS` substitutes `å`/`ø` with `X` using `dplyr::rename_with` before matching against the hard-coded `columns` named vector — be careful when adding new columns.
- **magrittr pipe** (`%>%`) is used throughout; the native pipe (`|>`) is not used.
- **data.table vs dplyr**: Biotic functions use `data.table` syntax internally; ERS/landings functions use `dplyr`/`vroom`/`readr`.
- The `FDIRcodes` dataset must be loaded (via `data(FDIRcodes)` or `devtools::load_all()`) for ERS and landings functions to work.
- **`convertColumns` in `processBioticFile`**: defaults to `FALSE` and is not well tested — the conversion blocks were written with copy-paste errors that have been corrected but the full path has limited real-world usage.
- **`readSluttseddelXLS`**: uses `print()` (stdout) rather than `message()` (stderr) for progress reporting, so progress output cannot be suppressed with `suppressMessages()`.
