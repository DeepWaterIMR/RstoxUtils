#' @title Download Electronic Reporting System (ERS) catch data into a folder from the \href{https://www.fiskeridir.no/statistikk-tall-og-analyse/data-og-statistikk-om-yrkesfiske/apne-data-elektronisk-rapportering-ers}{Norwegian Directorate of Fisheries webpage}
#' @param dest File path as character where the data should be downloaded to.
#' @param years An integer vector of years to download. The default downloads all years.
#' @param overwrite Either \code{"yes"}, \code{"no"} or \code{"force"}. The "yes" option overwrites annual .zip files only if the were modified on the webpage compared to existing files, "no" returns an error if there are existing .zip files in \code{dest} and "force" downloads all .zip files from the website again overwriting all existing files.
#' @param fdir_url Character defining the URL where data should be downloaded from. You'll only need to modify this if the source URL has changed. In that case, send an email to the maintainer such that the address can be updated.
#' @details Please note that this function downloads large amounts of data and places then as zip files in the folder defined by \code{dest}. Use the  \code{\link{extractERS}} to extract information from the downloaded files.
#' @return Returns nothing. Downloads data into folder specified by the \code{dest} argument.
#' @family ERS functions
#' @author Mikko Vihtakari
#' @export

# species = "snabeluer"; years = 2011:as.integer(format(Sys.Date(), "%Y")); dest = '/Users/a22357/ownCloud/Workstuff/Data/Norwegian Electronic Logbooks/Data/ERS data'; overwrite = "yes"; fdir_url = "https://register.fiskeridir.no/vms-ers/ERS/"
downloadERS <- function(
    dest, years = 2011:as.integer(format(Sys.Date(), "%Y")),
    overwrite = "yes", fdir_url = "https://register.fiskeridir.no/vms-ers/ERS/"
) {

  if(missing(dest)) stop("Define the destination folder (dest) where the data should be downloaded to.")

  existing_files <- dir(dest, pattern = "ers-.*\\.zip", full.names = TRUE)

  if(overwrite == "force") message("Overwrite set to force. Downloading and overwriting all (possibly) existing files.")

  lapply(years, function(k) {

    message("Year ", k)
    file_name <- paste0("elektronisk-rapportering-ers-", k, ".zip")
    file_url <- normalizePath(file.path(fdir_url, file_name))

    if(overwrite == "force") {

      response <- httr::HEAD(file_url)

      message("Downloading and writing")
      message(grep(k, existing_files, value = TRUE))
      httr::GET(
        file_url,
        httr::write_disk(file.path(dest, file_name), overwrite = TRUE),
        httr::progress()
      )

    } else if(overwrite == "no") {

      if(any(grepl(k, existing_files))) {
        message("File found from the disk and overwrite set to 'no'. Skipping...")
      } else {
        message("File not found from the disk. Downloading and writing.")
        message(grep(k, existing_files, value = TRUE))
        httr::GET(
          file_url,
          httr::write_disk(file.path(dest, file_name), overwrite = TRUE),
          httr::progress()
        )
      }
    } else if(overwrite == "yes") {
      response <- httr::HEAD(file_url)
      last_mod_fdir <- as.POSIXct(
        httr::headers(response)$`last-modified`,
        format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"
      )

      last_mod_disk <- file.info(grep(k, existing_files, value = TRUE))$mtime

      if(length(last_mod_disk) == 0) {

        message("File not found from the disk. Downloading and writing.")
        # message(grep(k, existing_files, value = TRUE))
        httr::GET(
          file_url,
          httr::write_disk(file.path(dest, file_name), overwrite = TRUE),
          httr::progress()
        )
      } else if(last_mod_fdir > last_mod_disk) {

        message("Updates found from the server. Downloading and overwriting.")
        # message(grep(k, existing_files, value = TRUE))
        httr::GET(
          file_url,
          httr::write_disk(file.path(dest, file_name), overwrite = TRUE),
          httr::progress()
        )
      } else {

        message("The file has not been modified on the server since the latest download. Skipping...")
        # message("The file ", grep(k, existing_files, value = TRUE), " left untouched")
      }
    }

  })

  message("Download finished. See ", dest)
}
