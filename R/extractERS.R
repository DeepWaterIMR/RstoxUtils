#' @title Extract ERS information from detailed Norwegian fisheries logbooks
#' @description Extracts information from detailed Norwegian fisheries logbook files downloaded using the \code{\link{downloadERS}} function.
#' @param path Character argument defining the path to the folder where data are located. This can either be the entire folder in which case all the .zip files will be read or a single .zip file.
#' @param species Character argument defining the species. Species names can be given in Norwegian, English or Latin depending on the \code{language} argument. Species names are sampled from \code{\link{FDIRcodes}$speciesCodes}. \code{NULL} returns all species in the ERS data.
#' @param method Character argument specifying the method for position, time and depth extraction. Alternatives: \code{"start"} takes the reported start values, \code{"end"} extracts the reported end values and \code{"average"} takes an average of start and end values.
#' @param language Character argument in lower case specifying the language of species names in the returned data table. Alternatives: \code{"norwegian"}, \code{"english"}, or \code{"latin"}, The species names are acquired from \code{\link{FDIRcodes}$speciesCodes}.
#' @param translate_header Logical indicating whether to translate the original column names in data selecting useful columns. If \code{FALSE}, the original columns are returned without modifications. If \code{TRUE}, columns are modified. See Return for details.
#' @param print.filename Logical indicating whether the file name should be printed to console while processing. Useful for debugging.
#' @param ncores Integer giving the number of cores to be used when combining the files. Speeds up the calculus.
#' @details This function extracts information from the Norwegian fisheries logbooks provided in the \code{.zip} format on the \href{https://www.fiskeridir.no/statistikk-tall-og-analyse/data-og-statistikk-om-yrkesfiske/apne-data-elektronisk-rapportering-ers}{Norwegian Directorate of Fisheries (FDIR) webpage}. Use the \code{\link{downloadERS}} function to download these data on your computer. Note that at the time of writing, FDIR only provided ERS data for > 15 m vessels due to unclarities in the Norwegian Privacy Act. If you want to access ERS data from smaller vessels, use the legacy function \code{\link{extractLogbook}}, which reads the confidential ERS data on IMR servers.
#'
#' @return If \code{translate_header = TRUE}, a data.table containing following columns:
#' \itemize{
#' \item \code{"year"} year of the fishing event.
#' \item \code{"month"} month of the fishing event.
#' \item \code{"id"} identification number of the fishing event.
#' \item \code{"vesselName"} name of the vessel.
#' \item \code{"radioCallSign"} call signal of the vessel.
#' \item \code{"EEZ"} reported exclusive economic zone of the fishing event.
#' \item \code{"ICESArea"} reported FAO area of the fishing event.
#' \item \code{"FDIRMainArea"} reported main area (hovedomraade) of the fishing event.
#' \item \code{"date"} date and time of the fishing event acquired based on the \code{method} argument.
#' \item \code{"lon"} longitude coordinate for the fishing event in decimal degrees acquired based on the \code{method} argument.
#' \item \code{"lat"} latitude coordinate for the fishing event in decimal degrees acquired based on the \code{method} argument.
#' \item \code{"depth"} reported fishing depth in meters acquired based on the \code{method} argument.
#' \item \code{"gear"} gear name translated from \code{gearId} based on \code{\link{FDIRcodes}$gearCodes}.
#' \item \code{"gearGroup"} reported gear group according to the Norwegian Directorate of Fisheries.
#' \item \code{"gearCategory"} gear category translated from \code{gearId} based on \code{\link{FDIRcodes}$gearCodes}.
#' \item \code{"gearSpecification"} reported gear specification according to the Norwegian Directorate of Fisheries.
#' \item \code{"gearMeshSize"} reported gear mesh size.
#' \item \code{"gearAmount"} reported gear amount (number of hooks etc.).
#' \item \code{"gearMeshSize"} reported issues with the gear.
#' \item \code{"fishTime"} reported fishing time in minutes.
#' \item \code{"fishDistance"} reported fishing distance in meters.
#' \item \code{"targetSp"} target species based on the FAO definition: the species with highest weight in catch.
#' \item \code{"catchSp"} reported catch species based on the \code{species}, \code{language} and \code{subspecies} arguments. Translated from the species ID (FANGSTART_NS or FANGSTART_FAO) based on \code{\link{FDIRcodes}$speciesCodes}.
#' \item \code{"weight"} reported catch of the \code{catchSp} in kilograms.
#' }
#'
#' Note that NA catch species (\code{Art - FDIR}) are filtered out when \code{translate_header = TRUE}. This can include some records of fishing with no catch.
#'
#' If \code{translate_header = FALSE}, the data are returned without filtering or modifications with the Norwegian column names.
#' @family ERS functions
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @export

# path = '/Users/a22357/ownCloud/Workstuff/Data/Norwegian Electronic Logbooks/Data/ERS data'
# species = "snabeluer"; translate_header = TRUE; method = "start"; language = "norwegian"; subspecies = FALSE; print.filename = FALSE; ncores = parallel::detectCores() - 2
extractERS <- function(path, species = NULL, translate_header = TRUE, method = "start", language = "norwegian", print.filename = FALSE, ncores = parallel::detectCores() - 2) {

  ## Load species list ####

  splist <- FDIRcodes$speciesCodes %>% stats::na.omit()
  gearlist <- FDIRcodes$gearCodes

  ## Checks

  if(!is.character(language) | !language %in% tolower(colnames(splist))) stop("language has to be one of the following: 'norwegian', 'english' or 'latin'")

  ## Define species

  spvec <- splist %>% dplyr::pull(tolower(language))
  sp <- paste0(
    "^", gsub("\\)", "\\\\)",
              gsub("\\(", "\\\\(", tolower(species), perl = TRUE)),
    "$")

  if(!any(grepl(sp, spvec, ignore.case = TRUE))) {
    stop(paste0(species, " not found from ", "FDIRcodes$speciesCodes[,", language, "]"))
  }

  if(length(unique(grep(sp, spvec, ignore.case = TRUE,value = TRUE))) > 1) {
    stop(paste0("Multiple matches with ", species, ": ", paste(unique(grep(sp, spvec, ignore.case = TRUE,value = TRUE), collapse = ", "))))
  }

  spCode <-
    splist %>%
    dplyr::slice(grep(sp, spvec, ignore.case = TRUE)) %>%
    dplyr::pull(idFAO)

  ## Define columns

  columns <-
    c(
      year = "Relevant Xr",
      id = "Melding ID",
      vesselName = "FartXynavn (ERS)",
      radioCallSign = "Radiokallesignal (ERS)",
      EEZ = "Sone (kode)",
      ICESArea = "OmrXdegruppering start (kode)",
      FDIRMainArea = "HovedomrXde start (kode)",
      dateStart = "Starttidspunkt",
      dateEnd = "Stopptidspunkt",
      lonStart = "Startposisjon lengde",
      latStart = "Startposisjon bredde",
      lonEnd = "Stopposisjon lengde",
      latEnd = "Stopposisjon bredde",
      gear = "Redskap FDIR",
      gearGroup = "Redskap - gruppe",
      gearSpecification = "Redskapsspesifikasjon",
      gearMeshSize = "Redskap maskevidde",
      gearAmount = "Redskap mengde",
      gearIssues = "Redskap problem",
      fishTime = "Varighet", # mins
      fishDistance = "Trekkavstand", # meters
      depthStart = "Havdybde start",
      depthEnd = "Havdybde stopp",
      targetSp = "Hovedart FAO", # target species, most in catch
      catchSp = "Art FAO",
      weight = "Rundvekt" # kg
    )

  ## Find files

  if(grepl(".zip$", path)) {
    files <- path
  } else {
    files <- dir(path, pattern = ".zip", full.names = TRUE)
  }

  # pb <- utils::txtProgressBar(min = 1, max = length(files) + 5, style = 3)
  # utils::setTxtProgressBar(pb, 1)

  ## Reading loop ####

  out <- pbmcapply::pbmclapply(seq_along(files), function(i) {
    # lapply(seq_along(files), function(i) {
    if(print.filename) print(files[i])

    # utils::setTxtProgressBar(pb, i + 1)

    ## ####

    fileyear <- unlist(strsplit(files[i], "/"))
    fileyear <- gsub("\\D", "", fileyear[length(fileyear)])

    x <- unz(
      files[i],
      paste0("elektronisk-rapportering-ers-", fileyear, "-fangstmelding-dca.csv")
    ) %>%
      vroom::vroom(
        delim = ";", col_types = readr::cols(),
        locale = readr::locale(decimal_mark = ",")
      ) %>%
      suppressWarnings()

    if(!translate_header) return(x)

    # Rename and select relevant columns
    x <- x %>%
      dplyr::rename_with(~ gsub("\u00e5|\u00f8", "X", .x)) %>%
      dplyr::select(unname(columns)) %>%
      dplyr::rename(dplyr::all_of(columns)) %>%
      dplyr::filter(!is.na(.data$catchSp)) # Remove NAs from the catch species column

    if(nrow(x) == 0) return(x)

    # Species ###

    x <- x %>%
      dplyr::mutate(
        catchSpCode = dplyr::recode(
          .data$catchSp, !!!setNames(splist$idFAO, splist$norwegian))
      )

    if(language == "english") {
      x <- x %>%
        dplyr::mutate(
          targetSp = dplyr::recode(
            .data$targetSp, !!!setNames(splist$english, splist$norwegian)),
          catchSp = dplyr::recode(
            .data$catchSp, !!!setNames(splist$english, splist$norwegian))
        )
    }

    if(language == "latin") {
      x <- x %>%
        dplyr::mutate(
          targetSp = dplyr::recode(
            .data$targetSp, !!!setNames(splist$latin, splist$norwegian)),
          catchSp = dplyr::recode(
            .data$catchSp, !!!setNames(splist$latin, splist$norwegian))
        )
    }

    # if(!is.null(species)) {
    #   x <- x %>%
    #     dplyr::filter(grepl(sp, .data$catchSp, ignore.case = TRUE))
    # }

    if(!is.null(species)) {
      x <- x %>%
        dplyr::filter(.data$catchSpCode %in% spCode) %>%
        dplyr::select(-.data$catchSpCode)
    }

    if(nrow(x) == 0) return(x)

    ### Column classes and values

    if(method == "start") {

      x <- x %>%
        dplyr::mutate(
          date =
            dplyr::case_when(
              nchar(dateStart) < 11 ~
                as.POSIXct(
                  paste(.data$dateStart, "00:00:01"),
                  format = "%d.%m.%Y %H:%M:%S", tz = "UTC"),
              .default =
                as.POSIXct(.data$dateStart,
                           format = "%d.%m.%Y %H:%M:%S", tz = "UTC")
            ),
          lon = .data$lonStart,
          lat = .data$latStart,
          depth = abs(.data$depthStart)
        )

    } else if (method == "end") {

      x <- x %>%
        dplyr::mutate(
          date =
            dplyr::case_when(
              nchar(dateEnd) < 11 ~
                as.POSIXct(
                  paste(.data$dateEnd, "00:00:01"),
                  format = "%d.%m.%Y %H:%M:%S", tz = "UTC"),
              .default =
                as.POSIXct(.data$dateEnd,
                           format = "%d.%m.%Y %H:%M:%S", tz = "UTC")
            ),
          lon = .data$lonEnd,
          lat = .data$latEnd,
          depth = abs(.data$depthEnd)
        )

    } else {

      #### Averages
      x <- x %>%
        dplyr::mutate(
          dateStart =
            dplyr::case_when(
              nchar(dateStart) < 11 ~
                as.POSIXct(
                  paste(.data$dateStart, "00:00:01"),
                  format = "%d.%m.%Y %H:%M:%S", tz = "UTC"),
              .default =
                as.POSIXct(.data$dateStart,
                           format = "%d.%m.%Y %H:%M:%S", tz = "UTC")
            ),
          dateEnd =
            dplyr::case_when(
              nchar(dateEnd) < 11 ~
                as.POSIXct(
                  paste(.data$dateEnd, "00:00:01"),
                  format = "%d.%m.%Y %H:%M:%S", tz = "UTC"),
              .default =
                as.POSIXct(.data$dateEnd,
                           format = "%d.%m.%Y %H:%M:%S", tz = "UTC")
            )
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          date = dateStart + (dateEnd - dateStart)/2,
          depth = mean(abs(.data$depthStart), abs(.data$depthEnd)),
          lon = mean(.data$lonStart, .data$lonEnd),
          lat = mean(.data$latStart, .data$latEnd)
        ) %>%
        dplyr::ungroup()
    }

    ### Export

    x %>%
      dplyr::select(-dplyr::matches("Start|End"))
  },
  mc.cores = ncores
  )

  ## ####

  ## Compile and return

  dplyr::bind_rows(out) %>%
    dplyr::distinct() %>% # Remove duplicate rows
    dplyr::left_join(
      gearlist %>% dplyr::select(.data$gearName, .data$gearCategory),
      by = dplyr::join_by("gear" == "gearName")
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c("targetSp", "catchSp", "ICESArea", "FDIRMainArea", "EEZ", "gear",
          "gearGroup", "gearSpecification", "gearIssues"),
        factor)) %>%
    dplyr::filter(.data$weight > 0) %>%
    dplyr::arrange(.data$year, .data$date, .data$lat, .data$lon,
                   .data$weight) %>%
    dplyr::mutate(
      month = format(date,"%m")
    ) %>%
    dplyr::relocate(
      c("year", "month", "id", "vesselName", "radioCallSign", "EEZ", "ICESArea",
        "FDIRMainArea", "date", "lon", "lat", "depth", "gear", "gearGroup",
        "gearCategory", "gearSpecification", "gearMeshSize", "gearAmount", "gearIssues",
        "fishTime", "fishDistance", "targetSp", "catchSp", "weight"
      )
    ) %>%
    droplevels()

}
