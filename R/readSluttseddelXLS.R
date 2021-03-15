#' @title A list of search words used to find column names in Excel sheets
#' @description A list containing search words used in \code{\link{guess_colname}} function.
#' @param column A required column name as character string
#' @param return_name Logical. Should name of \code{column} be returned instead of value from the list? Used in internal conditional functions.
#' @details The function accepts following required column names: \code{expedition}, \code{station}, \code{type}, \code{sample_name}, \code{longitude}, \code{latitude}, \code{date}, \code{bottom_depth}, \code{gear}, \code{from}, \code{to}, \code{responsible} and \code{comment}.
#' @author Mikko Vihtakari
#' @export

coln_search_words <- function(column, return_name = FALSE) {
  
  candidates <- list(
    year = "(fangst\u00e5r)",
    month = "landingsmnd",
    main_area = "hovedomr\u00e5de",
    sub_area = "(lokalitet)",
    ices_area = "ices",
    gear_id = "(redskap)|(reiskap)",
    gear = "beskrivelse",
    species = "(namn)|(navn)",
    mass = "kvantum"
  )
  
  if(!column %in% names(candidates)) {
    stop(paste(column, "column type not set"))
  } else {
    if(return_name) names(candidates[column]) else {
      candidates[[column]]
    }
  }
  
  
}

#' @title Guess column names from a list of candidates
#' @description Uses fuzzy matching (\code{\link[base]{agrep}}) to guess (column) names from a list of allowed character strings.
#' @param cols Character vector of approximate column names to be guessed
#' @param df data frame containing column names
#' @param candidates a switch argument or a character vector giving the candidates to be used in matching. The \code{\link{coln_search_words}} function is used by default.
#' @author Mikko Vihtakari, Conrad Helgeland
#' @export

# cols = required_cols; df = dt; candidates = coln_search_words

guess_colname <- function(cols, df, candidates = coln_search_words) {
  
  sapply(cols, function(k) {
    
    if(any(k %in% colnames(df))) {
      candidates(k, return_name = TRUE)
    } else {
      colnames(df)[grep(candidates(k), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(df)), perl = TRUE), ignore.case = TRUE, perl = FALSE)][1]
    }
    
  })
}

## ####

#' @title Read landing data from annually compiled Excel files
#' @param species Character string defining the species using the FDIR Norwegian species names
#' @param dataDir Character vector defining the path to the folder where Excel files are located (typically "sluttseddel_xls_ferdige_År")
#' @importFrom utils data
#' @importFrom dplyr left_join recode
#' @importFrom stats setNames
#' @export

# dataDir = "~/ownCloud/Workstuff/Data/Landings data/sluttseddel_xls_ferdige_År"; species = "Kveite"
readSluttseddelXLS <- function(species, dataDir) {
  
  dirs <- list.dirs(dataDir, full.names = FALSE, recursive = FALSE)
  COLS <- c("year", "month", "main_area", "sub_area", "ices_area", "gear_id", "gear", "species", "mass")
  
  # i = 1
  x <- lapply(seq_along(dirs), function(i) {
    print(dirs[i])
    
    # Files ###
    DR <- paste(dataDir, dirs[i], sep = "/")
    files <- dir(DR, pattern = ".xls|.xlsx")
    files <- files[!grepl("^\\~\\$", files)]
    
    if(any(grepl("bunn", files, ignore.case = TRUE))) {
      files <- grep("bunn", files, value = TRUE, ignore.case = TRUE)
    }
    
    ## The file reading loop ###
    # j = 1; target_sp = species; colns = COLS
    out <- lapply(seq_along(files), function(j, target_sp = species, colns = COLS) {
      print(files[j])
      
      # Read ###
      
      dt <- try(readxl::read_excel(paste(dataDir, dirs[i], files[j], sep = "/"), 2), silent = TRUE)
      
      if(any(class(dt) == "try-error")) {
        
        dt2 <- try(readxl::read_excel(paste(dataDir, dirs[i], files[j], sep = "/"), 1), silent = TRUE)
        
        if(any(class(dt2) == "try-error")) {
          stop("The readxl package failed to open the ", files[j], " file in ", dirs[i], " folder. \n This error is most likely due to problems in file saving. Try saving the file as newer .xlsx format and make sure to delete/rename the old file. \n\n The exact error returned by the readxl package: \n\n", dt)
        } else {
          dt <- dt2
          message("Used 1st sheet")
        }
      }
      
      nrow_1 <- nrow(dt)
      
      if(nrow_1 < 100) {
        dt <- readxl::read_excel(paste(dataDir, dirs[i], files[j], sep = "/"), 1)
        nrow_2 <- nrow(dt)
        
        message("Used 1st sheet")
        
        if(nrow_1 > nrow_2) {
          stop("found only ", nrow_1, " rows for ", files[j], ". Check the file and run again.")
        }
      }
      
      # Columns ###
      
      org_cols <- guess_colname(colns, df = dt)
      
      if(is.na(unname(org_cols[names(org_cols) == "sub_area"]))) {
        dt$lokalitet <- NA
        org_cols <- guess_colname(colns, df = dt)
      }
      
      if(!all(unname(org_cols) %in% colnames(dt))) {
        stop("column name ", paste(unname(org_cols)[!unname(org_cols) %in% colnames(dt)], collapse = ", "), " not found from ", files[j])
      }
      
      dt <- dt[unname(org_cols)]
      colnames(dt) <- names(org_cols)
      
      # Species ###
      
      spnams <- sort(unique(dt$species))
      
      if(!target_sp %in% spnams) {
        stop("Did not find ", target_sp, ", but found ", paste(grep(target_sp, spnams, value = TRUE, ignore.case = TRUE), collapse = ", "))
      }
      
      dt <- dt[dt$species == target_sp,]
      
      # Return
      dt[!names(dt) %in% "species"]
      
    })
    
    tmp <- do.call(rbind, out)
    
    tmp <- tmp[!is.na(tmp$main_area),]
    
    tmp
    
  })
  
  ## ####
  
  dt <- do.call(rbind, x)
  
  dt$year <- as.integer(dt$year) # factor(dt$year, levels = sort(as.numeric(unique(dt$year))))
  dt$month <- as.integer(dt$month) # factor(as.numeric(dt$month), levels = sort(unique(as.numeric(dt$month))))
  
  dt$main_area <- factor(as.numeric(as.character(dt$main_area)), levels = sort(unique(as.numeric(as.character(dt$main_area)))))
  
  dt$sub_area <- factor(as.numeric(as.character(dt$sub_area)), levels = sort(unique(as.numeric(as.character(dt$sub_area)))))
  dt$sub_area <- factor(ifelse(is.na(dt$sub_area) | dt$sub_area == 0, NA, paste(as.character(dt$main_area), as.character(dt$sub_area), sep = "-")))
  
  dt$ices_area <- factor(dt$ices_area, levels = sort(unique(dt$ices_area)))
  
  dt$gear_id <- factor(as.numeric(dt$gear_id), levels = sort(unique(as.numeric(dt$gear_id))))
  
  # data("FDIRcodes", package = "RstoxUtils")
  
  gear_list <- FDIRcodes$gearCodes#readxl::read_excel("Data/gear list.xlsx")
  names(gear_list)[names(gear_list) == "idGear"] <- "gear_id"
  names(gear_list)[names(gear_list) == "gearName"] <- "gear"
  names(gear_list)[names(gear_list) == "gearCategory"] <- "gear_category"
  
  gear_list$gear_id <- factor(gear_list$gear_id, levels = as.character(sort(as.numeric(gear_list$gear_id))))
  gear_list$gear_category <- factor(gear_list$gear_category, levels = unique(gear_list$gear_category))
  if(any(duplicated(gear_list$gear_id))) stop("Duplicated gear IDs in the gear_list")
  
  dt$gear <- factor(dplyr::recode(dt$gear_id, !!!stats::setNames(gear_list$gear, gear_list$gear_id)), 
                    levels = as.character(gear_list$gear))
  
  dt <- dt[dt$mass > 0,]
  
  dt <- dplyr::left_join(dt, gear_list[c("gear_id", "gear_category")], by = "gear_id")
  
  dt <- dt[c("year", "month", "main_area", "sub_area", "ices_area", "gear_id", "gear_category", "gear", "mass")]
  
  # dt$mass <- dt$mass/1000  # Mass in metric tons
  # dt <- dt[dt$mass < 4000,] # Remove the 4048 t outlier
  dt <- droplevels(dt)
  
}