#' @title Read and compile IMR vessel position files from a folder
#' @description Compiles the standard IMR vessel position files into a data.table for further analyses
#' @param path Character string defining the path to the \code{/CRUISE_LOG/TRACK/} folder
#' @param log.file Logical indicating whether the file from \code{path} has \code{.log} extension. If false, \code{.csv} is assumed.
#' @return A data.table containing all position information from the \code{/CRUISE_LOG/TRACK/} folder
#' @author Mikko Vihtakari
#' @importFrom data.table rbindlist
#' @export

# path = "Data/TRACK/"; log.file = FALSE
read.pos <- function(path, log.file = FALSE) {
  
  files <- dir(path, pattern = ifelse(log.file, ".log", ".csv"), full.names = TRUE)
  
  # i = 11
  tmp <- lapply(seq_along(files), function(i) {
     print(files[i])
    
    if(!log.file) {
      
      ## The header line
      
      hd <- readLines(files[i], n = 1)
      hd <- gsub("\"", "", hd)
      hd <- strsplit(hd, ",,")
      hd <- strsplit(c(sapply(hd, function(k) gsub(",", "", k))), ":")
      
      tmp <- lapply(hd, function(k) k[2])
      names(tmp) <- sapply(hd, function(k) k[1])
      hd <- tmp
      hd$Date <- as.Date(hd$Date, format = "%d.%m.%Y")
      
      ## Position data
      
      out <- utils::read.csv(files[i], skip = 1)
      names(out)[names(out) == "Time"] <- "Date"
      out[out$Date == "","Date"] <- out$Date[nrow(out)-1]
      
      out$Date <- as.POSIXct(paste(hd$Date, out$Date), format = "%Y-%m-%d %H:%M:%S", 
                             tz = "UTC")
      
      test <- as.POSIXct(paste(hd$Date, "00:00:00"), 
                         format = "%d.%m.%Y %H:%M:%S", tz = "UTC") == out$Date[nrow(out)]
      if(is.na(test)) test <- FALSE
      
      if(test) {
        out$Date[nrow(out)] <- out$Date[nrow(out)] + 60*60*24 # Correct the last observation
      }
      
      names(out) <- tolower(names(out))
      
    } else {
      
      out <- utils::read.csv(files[i], header = FALSE)
      
      names(out) <- c("ship", "cruise", "date", "time", NA, "ref.no", "loc.st.no", NA, NA, NA, NA, "log", "latitude", "longitude", "depth", NA, NA, NA, NA, "water.temp", "wind", "wind.dir", NA, NA, NA, NA, NA, NA, NA, "speed", "heading", NA, NA)
      
      out$date <- as.Date(out$date, format = "%d.%m.%Y")
      out$time <- gsub("\\s", "0", format(out$time, width = 6))
      
      out$date <- as.POSIXct(paste(out$date, out$time), tz = "UTC", format = "%Y-%m-%d %H%M%S")
      
      out <- out[!is.na(names(out))]
    }
    
    ## Position data
    
    out[c("latitude", "longitude")] <- lapply(out[c("latitude", "longitude")], trimws)
    out <- out[out$latitude != "" & out$longitude != "" & out$latitude != -1 & out$longitude != -1,]
    
    ### Fix latiitude
    
    dec <- as.numeric(substr(out$latitude, 1, 2))
    min <- as.numeric(trimws(gsub("[a-zA-Z]", "", substr(out$latitude, 3, nchar(out$latitude)))))
    
    out$latitude <- ifelse(grepl("N", out$latitude), dec + min/60, dec - min/60)
    
    ### Fix longitude
    
    dec <- as.numeric(substr(out$longitude, 1, 3))
    min <- as.numeric(trimws(gsub("[a-zA-Z]", "", substr(out$longitude, 4, nchar(out$longitude)))))
    
    out$longitude <- ifelse(grepl("E", out$longitude), dec + min/60, dec - min/60)
    
    ## Return out
    
    return(out)
  })
  
  out <- data.table::rbindlist(tmp)
  out <- out[order(log),]
  
  out[, which(unlist(lapply(out, function(x) !all(is.na(x))))), with = FALSE]
}



