#' @title Download landings data for a species from the IMR database
#' @description THe function downloads landings ("sluttseddel") data from IMR database. Requires access to the intranet. 
#' @param species any species identification name in \code{FDIRcodes$speciesCodes} as character. Only one species at the time allowed.
#' @param years an integer vector of years to download. Defaults to all years. Please note that this option can take very long time and lead to huge datasets.
#' @author Mikko Vihtakari
#' @importFrom RstoxData readXmlFile
#' @export

# species  <- "brugde"; years <- c(1900:2020)
downloadLandings <- function(species, years = c(1900:2020)) {
  
  ## Set up variables
  
  splist <- as.data.frame(FDIRcodes$speciesCodes)
  dest <- tempfile(fileext = ".xml")
  APIpath <- "http://tomcat7.imr.no:8080/apis/nmdapi/landing/v2?version=2.0&type=search"
  
  ## Find the species code
  
  species <- paste0("^", species, "$")
  tmp <- sapply(colnames(splist), function(x) grep(species, splist[,x], ignore.case = TRUE))
  
  if(all(sapply(tmp, function(k) length(k) == 0))) stop(paste(species, "not found from FDIRcodes$speciesCodes"))
  
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
  
  DownloadPath <- paste0(APIpath, "&Art_kode=", spCode, "&Fangstar=", paste(years, collapse = ","))
  
  ## Download the data from the database 
  
  status <- suppressMessages(suppressWarnings(try(utils::download.file(DownloadPath, dest), silent = TRUE)))
  
  if(class(status) == "try-error") {
    
    ## Stop processing if not found
    
    stop(paste("Species code", spCode, "with years", paste(years, collapse = ","), "not found from the database."))
    
  } else {
    
    ## Read the data
    
    RstoxData::readXmlFile(dest)
    
  }
}
