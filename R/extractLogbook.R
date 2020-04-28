#' @title Extract information from detailed Norwegian fisheries logbooks
#' @description Extracts information from detailed Norwegian fisheries logbook files
#' @param path Character argument defining the path to the folder where data are located
#' @param species Character argument defining the species. Species names are given in Norwegian (see Håndbok for prøvetaking). Some of the available names are peculiar, but most commercially valuable species should work.
#' @param method Character argument specifying the method for position, time and depth extraction. Alternatives: \code{"start"} takes the reported start values, \code{"end"} extracts the reported end values and \code{"average"} takes an average of start and end values. 
#' @param remove.sensitive Logical indicating whether the function should remove sensitive data fields.
#' @details This function extracts information from the Norwegian fisheries logbooks provided in the \code{.psv} format on the IMR server. Note that these data are confidential and protected by the Norwegian Privacy Act (personvernloven). Keep the data on the server or in another safe place (not on the cloud). Do not send the input data around on email. The \code{remove.sensitive} argument can be used to disconnect the output from the confidential input data. This output can be distributed to people who have not signed the confidentiality statement (students, ICES colleagues, etc.) also using email. The data are property of the Norwegian Directorate of Fisheries and you should consult them if you plan to publish any parts of the dataset. 
#' 
#' Note that the structure of logbook files is not confidential and therefore this function can be distributed freely. The data required by the function cannot (do not send those data on email).
#' 
#' The mess with species names may be solved in the future. For now, you just have to know what a species you are interested in is called in these data. 
#' @return A data.table containing the desired information.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @import data.table
#' @importFrom parallel mclapply
#' @importFrom geosphere geomean
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

extractLogbook <- function(path, species, method = "start", remove.sensitive = TRUE) {
  
  columns <- c(
    year = "FANGST",
    date_start = "STARTTIDSPUNKT",
    date_end = "STOPPTIDSPUNKT",
    lon_start = "START_LG",
    lat_start = "START_LT",
    lon_end = "STOPP_LG",
    lat_end = "STOPP_LT",
    gear_id = "REDSKAP_NS",
    gear = "REDSKAP",
    fish_time = "VARIGHET", # mins
    effort = "INNSATS", # number of hooks in the line or total length of net
    dist = "TREKK_AVSTAND_METER", # meters
    target_sp = "HOVEDART_FAO", # target species, most in catch
    species = "FANGSTART", # caught species
    depth_start = "HAV_DYBDE_START",
    depth_end = "HAV_DYBDE_STOPP",
    mass = "RUNDVEKT" # kg
  )
  
  files <- paste(path, dir(path, recursive = TRUE, pattern = ".psv"), sep = "/")
  
  file = files[1]
  
  pb <- utils::txtProgressBar(min = 1, max = length(files) + 4, style = 3)
  
  out <- #pbmcapply::pbmclapply
    lapply(seq_along(files), function(i) {
      # print(file)
      
      utils::setTxtProgressBar(pb, i)
      
      ## ####
      
      x <- data.table::fread(files[i], sep = "|", header = FALSE, stringsAsFactors = FALSE, nrows = 1)
      colns <-  unlist(unname(x[1,]))
      colns <- gsub("(\xc5R)|(\xd8R)", "", colns, perl = TRUE)
      
      x <- data.table::fread(files[i], sep = "|", header = FALSE, stringsAsFactors = FALSE, dec = ",", skip = 1)
      
      names(x) <- colns
      
      # Species ###
      
      x[, FANGSTART := ascii_to_nor(x$FANGSTART)]
      
      # spnams <- sort(unique(x$FANGSTART))
      # if(!hovedart %in% spnams) {
      #   if(target_sp == "blåkveite") stop("Did not find ", target_sp, ", but found ", paste(grep("blå|kveite", spnams, value = TRUE, ignore.case = TRUE), collapse = ", "))
      # }
      
      if(length(species) > 1) {
        tmp <- species
      } else {
        tmp <- paste(species, collapse = "|")
      }
      
      x <- x[grepl(tmp, x$FANGSTART, ignore.case = TRUE),]
      
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
        
        ### Column classes and values
        
        if(method == "start") {
          
          x[, date := as.POSIXct(date_start, tz = "UTC")]
          x[, lon := lon_start]
          x[, lat := lat_start]
          x[, depth := abs(depth_start)]
          
          set(x, i = which(x[["depth"]] < 15), j = "depth", value = NA)
          
        } else if (method == "end") {
          
          x[, date := as.POSIXct(date_end, tz = "UTC")]
          x[, lon := lon_end]
          x[, lat := lat_end]
          x[, depth := abs(depth_end)]
          
          set(x, i = which(x[["depth"]] < 15), j = "depth", value = NA)
          
        } else {
          
          #### Average date
          x[, date_start := as.POSIXct(date_start, tz = "UTC")]
          x[, date_end := as.POSIXct(date_end, tz = "UTC")]
          x[, date := date_start + (date_end - date_start)/2]
          
          #### Average location
          z <- parallel::mclapply(1:nrow(x), function(i) {
            out <- x[i,]
            tmp <- geosphere::geomean(cbind(x = c(out$lon_start, out$lon_end), y = c(out$lat_start, out$lat_end)))
            out$lon <- tmp[1]
            out$lat <- tmp[2]
            out
          })
          
          x <- do.call(rbind, z)
          
          #### Average depth
          
          x[, depth_start := abs(depth_start)]
          x[, depth_end := abs(depth_end)]
          set(x, i = which(x[["depth_start"]] < 15), j = "depth_start", value = NA)
          set(x, i = which(x[["depth_end"]] < 15), j = "depth_end", value = NA)
          
          x[, depth := rowMeans(.SD), .SDcols = c("depth_start", "depth_end")]
          
          # x$depth <- unname(apply(x[c("depth_start", "depth_end")], 1, function(x) mean(x, na.rm = TRUE)))
          
        }
        
        ## øæå addition
        x <- rapply(x, f = ascii_to_nor, classes = "character", how = "replace")
        
        ### To factor
        
        tmp <- c("target_sp", "species")
        x[,(tmp):= lapply(.SD, as.factor), .SDcols = tmp]
        
        ### Select columns
        x[, grep("_start|_end", names(x), value = TRUE) := NULL]
        
        x
      } else {
        NULL
      }
      
      
      ## ####
    })#, mc.cores = parallel::detectCores() - 4)
  
  
  lb <- data.table::rbindlist(out)
  lb <- unique(lb) # Remove duplicate rows
  
  utils::setTxtProgressBar(pb, length(files) + 1)
  
  lb$gear_id <- as.character(lb$gear_id)
  lb$gear <- as.character(lb$gear)
  
  set(lb, i = which(is.na(lb[["gear_id"]])), j = "gear_id", value = 80)
  
  data(gearList)
  if(!all(unique(lb$gear_id) %in% gearList$gear_id)) stop("gear_list gear_id does not match with lb gear_id")
  
  lb[, gear_id := factor(gear_id, levels = sort(as.numeric(unique(gear_id))))]
  
  lb[.(gearList), gear := gear_name, on = c("gear_id")]
  lb[.(gearList), gear_category := gear_category, on = c("gear_id")]
  
  utils::setTxtProgressBar(pb, length(files) + 2)
  
  ## Remove 0 mass
  
  lb <- lb[mass != 0]
  
  ## Sort
  
  lb <- lb[order(date, lat, lon, mass)]
  
  ## Distance to land (to remove wrongly reported values)
  
  ## Change year to fishing year
  
  lb[, year := format(as.Date(date, format="%d/%m/%Y"),"%Y")]
  lb[, month:= format(date,"%m")]
  
  utils::setTxtProgressBar(pb, length(files) + 3)
  
  ## Column order
  setcolorder(lb, c("year", "month", "date", "lon", "lat", "gear_id", "gear", "gear_category", "fish_time", "effort", "dist", "target_sp", "species", "depth", "mass"))
  
  ## Remove sensitive data
    
  if(remove.sensitive) lb[, date := NULL]
  
  ## Return 
  
  utils::setTxtProgressBar(pb, length(files) + 4)
  
  droplevels(lb)
}
