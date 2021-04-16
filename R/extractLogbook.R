#' @title Extract information from detailed Norwegian fisheries logbooks
#' @description Extracts information from detailed Norwegian fisheries logbook files
#' @param path Character argument defining the path to the folder where data are located
#' @param species Character argument defining the species. Species names can be given in Norwegian, English or Latin. Species names are sampled from \code{\link{FDIRcodes}$speciesCodes}. 
#' @param method Character argument specifying the method for position, time and depth extraction. Alternatives: \code{"start"} takes the reported start values, \code{"end"} extracts the reported end values and \code{"average"} takes an average of start and end values. 
#' @param language Character argument in lower case specifying the language of species names in the returned data table. Alternatives: \code{"norwegian"}, \code{"english"}, or \code{"latin"}, The species names are acquired from \code{\link{FDIRcodes}$speciesCodes}.
#' @param subspecies Logical. Should NS codes (i.e. species sub-categories) be used to translate species names? If \code{FALSE}, FAO codes are used leading to actual species names. 
#' @param remove.sensitive Logical indicating whether the function should remove sensitive data fields.
#' @details This function extracts information from the Norwegian fisheries logbooks provided in the \code{.psv} format on the IMR server. Note that these data are confidential and protected by the Norwegian Privacy Act (personvernloven). Keep the data on the server or in another safe place (not on the cloud). Do not send the input data around on email. The \code{remove.sensitive} argument can be used to disconnect the output from the confidential input data. This output can be distributed to people who have not signed the confidentiality statement (students, ICES colleagues, etc.) also using email. The data are property of the Norwegian Directorate of Fisheries and you should consult them if you plan to publish any parts of the dataset. 
#' 
#' Note that the structure of logbook files is not confidential and therefore this function can be distributed freely. The data required by the function cannot (do not send those data on email).
#'
#' There are many duplicate records in the logbook files on the server. This function removes these duplicates automatically, but the process takes time and is not very controlled. If you want to be sure about the data you include, copy the desired files to another folder (in a safe place) and run the function for that folder. 
#'   
#' @return A data.table containing following columns:
#' \itemize{
#' \item \code{"year"} year of the fishing event.
#' \item \code{"month"} month of the fishing event.
#' \item \code{"lon"} longitude coordinate for the fishing event in decimal degrees acquired based on the \code{method} argument.
#' \item \code{"lat"} latitude coordinate for the fishing event in decimal degrees acquired based on the \code{method} argument.
#' \item \code{"gearId"} reported gear ID (REDSKAP_NS) in the .psv files
#' \item \code{"gear"} gear name translated from \code{gearId} based on \code{\link{FDIRcodes}$gearCodes}.
#' \item \code{"gearCat"} gear category translated from \code{gearId} based on \code{\link{FDIRcodes}$gearCodes}.
#' \item \code{"fishTime"} reported fishing time.
#' \item \code{"effort"} reported fishing effort (number of hooks etc.)
#' \item \code{"dist"} reported fishing distance in meters. 
#' \item \code{"depth"} reported fishing depth in meters acquired based on the \code{method} argument.
#' \item \code{"targetSpFAO"} target species based on the FAO definition: the species with highest mass in catch.
#' \item \code{"targetSpRep"} target species reported by the fishermen. 
#' \item \code{"species"} reported catch species based on the \code{species}, \code{language} and \code{subspecies} arguments. Translated from the species ID (FANGSTART_NS or FANGSTART_FAO) based on \code{\link{FDIRcodes}$speciesCodes}.
#' \item \code{"mass"} reported catch of the \code{species} in kilograms. 
#' }
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @import data.table
#' @importFrom parallel mclapply
#' @importFrom geosphere geomean
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

extractLogbook <- function(path, species, method = "start", language = "norwegian", subspecies = FALSE, remove.sensitive = TRUE) {
 
  ## Load species list ####
  
  splist <- data.table::as.data.table(FDIRcodes$speciesCodes)
  gearlist <- data.table::as.data.table(FDIRcodes$gearCodes)
  
  ## Checks
  
  if(!is.character(language) | !language %in% tolower(colnames(splist))) stop("language has to be one of the following: 'norwegian', 'english' or 'latin'")
  
  ## Define species
  
  spvec <- splist[, tolower(language), with = FALSE][[1]]
  sp <- paste0("^", species, "$")
  
  if(!any(grepl(sp, spvec, ignore.case = TRUE))) {
    stop(paste0(species, " not found from ", "FDIRcodes$speciesCodes[,", language, "]"))
  }
  
  if(length(unique(grep(sp, spvec, ignore.case = TRUE,value = TRUE))) > 1) {
    stop(paste0("Multiple matches with ", species, ": ", paste(unique(grep(sp, spvec, ignore.case = TRUE,value = TRUE), collapse = ", "))))
  }
  
  if(subspecies) {
    spCode <- splist[, idNS][grep(sp, spvec, ignore.case = TRUE)]
  } else {
    spCode <- splist[, idFAO][grep(sp, spvec, ignore.case = TRUE)]
  }
  
  ## Definitions
   
  columns <- c(
    year = "FANGST",
    dateStart = "STARTTIDSPUNKT",
    dateEnd = "STOPPTIDSPUNKT",
    lonStart = "START_LG",
    latStart = "START_LT",
    lonEnd = "STOPP_LG",
    latEnd = "STOPP_LT",
    gearId = "REDSKAP_NS",
    gear = "REDSKAP",
    fishTime = "VARIGHET", # mins
    effort = "INNSATS", # number of hooks in the line or total length of net
    dist = "TREKK_AVSTAND_METER", # meters
    depthStart = "HAV_DYBDE_START",
    depthEnd = "HAV_DYBDE_STOPP",
    targetSpFAO = "HOVEDART_FAO", # target species, most in catch
    targetSpRep = "HOVEDART_NS", # target species, reported by the fishermen
    species = if(subspecies) "FANGSTART_NS" else "FANGSTART_FAO", # Created during species filtering
    speciesFAO = "FANGSTART_FAO", # FAO code for the catch species
    speciesNS = "FANGSTART_NS", # NS code for the catch species
    mass = "RUNDVEKT" # kg
  )
  
  ## Find files
  
  files <- paste(path, dir(path, recursive = TRUE, pattern = ".psv"), sep = "/")
  
  pb <- utils::txtProgressBar(min = 1, max = length(files) + 5, style = 3)
  utils::setTxtProgressBar(pb, 1)
  
  ## Reading loop ####
  
  out <- #pbmcapply::pbmclapply
    lapply(seq_along(files), function(i) {
      # print(i)
      
      utils::setTxtProgressBar(pb, i + 1)
      
      ## ####
      
      x <- data.table::fread(files[i], sep = "|", header = FALSE, stringsAsFactors = FALSE, nrows = 1, showProgress = FALSE)
      colns <-  unlist(unname(x[1,]))
      colns <- gsub("(\xc5R)|(\xd8R)", "", colns, perl = TRUE)
      
      x <- data.table::fread(files[i], sep = "|", header = FALSE, stringsAsFactors = FALSE, dec = ",", skip = 1, showProgress = FALSE)
      
      names(x) <- colns
      
      if(subspecies) { # Remove missing catch species
        x <- x[!is.na(x$FANGSTART_NS),] 
      } else {
        x <- x[!is.na(x$FANGSTART_FAO),] 
      }
      
      # Species ###
      
      if(subspecies) {
        x <- x[FANGSTART_NS %in% spCode,]
      } else {
        x <- x[FANGSTART_FAO %in% spCode,]
      }
      
      x[,HOVEDART_NS := splist[x, get(language), on = c(idNS = "HOVEDART_NS")]]
      
      sps <- splist[idFAO %in% unique(x$HOVEDART_FAO),]
      sps <- sps[!duplicated(sps$idFAO),]
      
      x[,HOVEDART_FAO := sps[x, get(language), on = c(idFAO = "HOVEDART_FAO")]]
      
      if(nrow(x) > 0) {
        
        ## Columns
        
        if(all(unname(columns) %in% names(x))) {
          
          x <- x[, unname(columns), with = FALSE]
          names(x) <- names(columns)
          
        } else {
          
          missing_cols <- unname(columns)[!unname(columns) %in% names(x)]
          existing_cols <- unname(columns)[unname(columns) %in% names(x)]
          
          x <- x[, unname(existing_cols), with = FALSE]
          names(x) <- names(columns[columns %in% existing_cols])
        }
        
        if(subspecies) {
          x[,species := splist[x, get(language), on = c(idNS = "speciesNS")]] 
        } else {
          x[,species := splist[x, get(language), on = c(idFAO = "speciesFAO")]]
        }
        
        
        ### Column classes and values
        
        if(method == "start") {
          
          x[, date := as.POSIXct(dateStart, tz = "UTC")]
          x[, lon := lonStart]
          x[, lat := latStart]
          x[, depth := abs(depthStart)]
          
          data.table::set(x, i = which(x[["depth"]] < 15), j = "depth", value = NA)
          
        } else if (method == "end") {
          
          x[, date := as.POSIXct(dateEnd, tz = "UTC")]
          x[, lon := lonEnd]
          x[, lat := latEnd]
          x[, depth := abs(depthEnd)]
          
          data.table::set(x, i = which(x[["depth"]] < 15), j = "depth", value = NA)
          
        } else {
          
          #### Average date
          x[, dateStart := as.POSIXct(dateStart, tz = "UTC")]
          x[, dateEnd := as.POSIXct(dateEnd, tz = "UTC")]
          x[, date := dateStart + (dateEnd - dateStart)/2]
          
          #### Average location
          z <- parallel::mclapply(1:nrow(x), function(i) {
            out <- x[i,]
            tmp <- geosphere::geomean(cbind(x = c(out$lonStart, out$lonEnd), y = c(out$latStart, out$latEnd)))
            out$lon <- tmp[1]
            out$lat <- tmp[2]
            out
          })
          
          x <- do.call(rbind, z)
          
          #### Average depth
          
          x[, depthStart := abs(depthStart)]
          x[, depthEnd := abs(depthEnd)]
          data.table::set(x, i = which(x[["depthStart"]] < 15), j = "depthStart", value = NA)
          data.table::set(x, i = which(x[["depthEnd"]] < 15), j = "depthEnd", value = NA)
          
          x[, depth := rowMeans(.SD), .SDcols = c("depthStart", "depthEnd")]
          
        }
        
        ### To factor
        
        tmp <- c("targetSpFAO", "targetSpRep", "species")
        x[,(tmp):= lapply(.SD, as.factor), .SDcols = tmp]
        
        ### Select columns
        x[, grep("Start|End", names(x), value = TRUE) := NULL]
        
        x
      } else {
        NULL
      }
      
      
      ## ###
    })#, mc.cores = parallel::detectCores() - 4)
  
  
  ## ####
  
  lb <- data.table::rbindlist(out)
  lb <- unique(lb) # Remove duplicate rows
  
  utils::setTxtProgressBar(pb, length(files) + 2)
  
  data.table::set(lb, i = which(is.na(lb[["gearId"]])), j = "gearId", value = 80)
  
  lb$gear <- gearlist[lb, gearName, on = c(idGear = "gearId")]
  lb$gearCat <- gearlist[lb, gearCategory, on = c(idGear = "gearId")]
  
  utils::setTxtProgressBar(pb, length(files) + 3)
  
  ## Remove 0 mass
  
  lb <- lb[mass != 0]
  
  ## Sort
  
  lb <- lb[order(date, lat, lon, mass)]
  
  ## Distance to land (to remove wrongly reported values)
  
  ## Change year to fishing year
  
  lb[, year := format(as.Date(date, format="%d/%m/%Y"),"%Y")]
  lb[, month:= format(date,"%m")]
  
  utils::setTxtProgressBar(pb, length(files) + 4)
  
  ## Column orderlb
  
  data.table::setcolorder(lb, c("year", "month", "date", "lon", "lat", "gearId", "gear", "gearCat", "fishTime", "effort", "dist", "depth","targetSpFAO", "targetSpRep", "species", "mass"))
  
  ## Remove sensitive data
    
  if(remove.sensitive) lb[, date := NULL]
  
  ## Return 
  
  utils::setTxtProgressBar(pb, length(files) + 5)
  
  droplevels(lb)
}
