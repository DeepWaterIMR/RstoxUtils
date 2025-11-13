#' @title Download sales note data for a species from the IMR database
#' @description The function downloads sales note/landings ("sluttseddel") data from IMR database. Requires access to the intranet.
#' @param species any species identification name in \code{FDIRcodes$speciesCodes} as character. Only one species at the time allowed.
#' @param years an integer vector of years to download. If \code{NULL} (default), all years are downloaded. Please note that this option can take very long time and lead to huge datasets.
#' @param returned_data character argument specifying what type of data should be returned. Use \code{"sales notes"} to return the original sales note data from the server, \code{"summary"} to only return summarized catches, or \code{"both"} to return both sales notes and summarized catches in a list.
#' @param separate logical indicating whether years should be downloaded as separate API calls or as one call. Setting this to \code{FALSE} can save time, but lead to unexpected behavior because the download sometimes gets truncated for large datasets. Only relevant when the \code{years} argument contains multiple years.
#' @author Mikko Vihtakari
#' @family Landings functions
#' @examples \dontrun{
#' downloadLandings("brugde") # Basking shark, all years
#' downloadLandings("kveite", years = 2000:2001) # halibut, 2000-01
#' }
#' @export

# species  <- "blåkveite"; years <- c(1900:2020)
downloadLandings <- function(species, years = NULL, returned_data = "both", separate = TRUE) {

  ## Set up variables

  splist <- as.data.frame(FDIRcodes$speciesCodes)
  dest <- tempfile(fileext = ".xml")
  APIpath <- "http://tomcat7.imr.no:8080/apis/nmdapi/landing/v2?version=2.0&type=search"

  ## Try the connection

  if(!RCurl::url.exists(APIpath)) stop("No connection to the server. Make sure you are connected to the IMR intranet")

  ## Find the species code

  species <- paste0("^", species, "$")
  if(grepl("(|)", species)) species <- gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", species))
  tmp <- sapply(colnames(splist), function(x) grep(species, splist[,x], ignore.case = TRUE))

  if(all(sapply(tmp, function(k) length(k) == 0))) {
    message(paste(species, "not found from FDIRcodes$speciesCodes. Returning NULL."))
    return(NULL)
  }

  if(sum(sapply(tmp, function(k) length(k) == 1)) > 1) {
    stop(
      paste(species, "was matched to",
            paste(names(sapply(tmp, function(k) length(k) == 1)[sapply(tmp, function(k) length(k) == 1)]), collapse = ", "),
            ". Cannot extract information from multiple columns."
      )
    )
  }

  # spCol <- names(tmp[sapply(tmp, function(k) length(k) == 1)])
  spRow <- unlist(unname(tmp[sapply(tmp, function(k) length(k) == 1)]))
  spCode <- splist[spRow, "idNS"]

  spCode <- ifelse(nchar(spCode) == 3, paste0(0, spCode), spCode)

  ## Set up the download path

  if(is.null(years) & !separate) {
    DownloadPath <- paste0(APIpath, "&Art_kode=", spCode)
  } else if(is.null(years) & separate) {
    DownloadPath <- sapply(2005:as.integer(format(Sys.Date(), "%Y")), function(k) {
      paste0(APIpath, "&Art_kode=", spCode, "&Fangstar=", k)
    })
  } else if(separate) {
    DownloadPath <- sapply(years, function(k) {
      paste0(APIpath, "&Art_kode=", spCode, "&Fangstar=", k)
    })
  } else {
    DownloadPath <- paste0(APIpath, "&Art_kode=", spCode, "&Fangstar=", paste(years, collapse = ","))
  }

  ## Download the data from the database

  out <- lapply(DownloadPath, function(k) {

    status <- suppressMessages(
      suppressWarnings(try(utils::download.file(k, dest), silent = TRUE)))

    if(inherits(status, "try-error")) {

      ## Warn if not found

      warning(paste("Species code", spCode, "with years", paste(k, collapse = ","), "not found from the database. Returning NULL"))
      return(NULL)

    } else {

      ## Read the data

      sales_notes <- RstoxData::readXmlFile(dest)

      sn_sum <-
        sales_notes$Produkt %>%
        dplyr::select(
          "Fangst\u00e5r", "SisteFangstdato", "Redskap_kode", "St\u00f8rsteLengde",
          "Hovedomr\u00e5de_kode", "Lokasjon_kode", "Fart\u00f8ynasjonalitet_kode",
          "Rundvekt") %>%
        stats::setNames(c("year", "date", "gear_id", "vessel_length", "main_area",
                          "sub_area", "nation", "weight")) %>%
        dplyr::mutate(date = as.Date(.data$date, format = "%d.%m.%Y")) %>%
        dplyr::filter(!is.na(.data$weight) & .data$weight > 0) %>%
        dplyr::mutate(month = lubridate::month(.$date), .before = "date") %>%
        dplyr::mutate_at(
          dplyr::vars(.data$main_area, .data$sub_area, .data$gear_id), as.integer) %>%
        dplyr::arrange(dplyr::desc(date))

      if(returned_data == "both") {
        c(sales_notes, list(summary = sn_sum))
      } else if(returned_data == "summary") {
        sn_sum
      } else {
        sales_notes
      }
    }
  })

  # Return
  do.call(Map, c(f = rbind, out, fill = TRUE))
}
