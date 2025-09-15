#' @title A list of search words used to find column names in Excel sheets
#' @description A list containing search words used in \code{\link{guess_colname}} function.
#' @param column A required column name as character string
#' @param return_name Logical. Should name of \code{column} be returned instead of value from the list? Used in internal conditional functions.
#' @details The function accepts following required column names: \code{expedition}, \code{station}, \code{type}, \code{sample_name}, \code{longitude}, \code{latitude}, \code{date}, \code{bottom_depth}, \code{gear}, \code{from}, \code{to}, \code{responsible} and \code{comment}.
#' @keywords internal
#' @author Mikko Vihtakari

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
    weight = "kvantum"
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
#' @family helpers
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
#' @param species Character string defining the species using the FDIR Norwegian species names. \code{NULL} returns all species.
#' @param dataDir Character vector defining the path to the folder where Excel files are located (typically "sluttseddel_xls_ferdige_År")
#' @param years Integer vector defining the years to read. Files in later years tend to be a mess and this option avoids crashes. Use \code{NULL} to try opening all files.
#' @param dropMissingMainArea Logical indicating whether catches with missing main area should be dropped.
#' @importFrom utils data
#' @importFrom dplyr left_join recode
#' @importFrom stats setNames
#' @family Landings functions
#' @export

# dataDir = "~/ownCloud/Workstuff/Data/Landings data/sluttseddel_xls_ferdige_År/"; species = "Snabeluer"; dropMissingMainArea = TRUE; years = 1977:2022
readSluttseddelXLS <- function(species, dataDir, years = 1977:2022, dropMissingMainArea = TRUE) {

  ## Test connection

  if(!dir.exists(dataDir)) stop("Could not find dataDir. If the path is defined toward the server, make sure you are connected to the IMR intranet")

  ## Define files

  dirs <- list.dirs(dataDir, full.names = FALSE, recursive = FALSE)

  if(!is.null(years)) {
    dirs <- dirs[grepl(paste(years, collapse = "|"), dirs)]
  }

  COLS <- c("year", "month", "main_area", "sub_area", "ices_area", "gear_id", "gear", "species", "weight")

  # i = 1
  x <- lapply(seq_along(dirs), function(i) {
    print(dirs[i])

    # Files ###
    DR <- paste(dataDir, dirs[i], sep = "/")
    files <- dir(DR, pattern = ".xls|.xlsx")
    files <- files[!grepl("^\\~\\$", files)]

    if(any(grepl("sluttseddel", files, ignore.case = TRUE))) {
      files <- grep("sluttseddel", files, value = TRUE, ignore.case = TRUE)
    }

    if(any(grepl("FDIR", files, ignore.case = TRUE)) & length(files) > 1) {
      if(any(grepl("korr", files, ignore.case = TRUE))) {

        tmp <- sapply(strsplit(grep("korr", files, value = TRUE), "_"),
                      function(g) utils::tail(g,1))
        if(length(tmp) > 1) tmp <- tmp[which.max(as.integer(gsub("\\D", "", tmp)))]

        files <- grep(tmp, files, value = TRUE, ignore.case = TRUE)
      }
    }

    ## The file reading loop ###
    # j = 1; target_sp = species; colns = COLS
    out <- lapply(seq_along(files), function(j, target_sp = species, colns = COLS) {
      print(files[j])

      # Read ###

      dt <- try(readxl::read_excel(paste(dataDir, dirs[i], files[j], sep = "/"), 2), silent = TRUE)

      if(!inherits(dt, "try-error")) {
        if(ncol(dt) < 8) {
          dt <- try(readxl::read_excel(paste(dataDir, dirs[i], files[j], sep = "/"), 3),
                    silent = TRUE)
        }
      }

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

      if(is.null(species)) return(dt)

      spnams <- sort(unique(dt$species))

      if(!target_sp %in% spnams) {
        alt_name <- paste(grep(target_sp, spnams, value = TRUE, ignore.case = TRUE), collapse = ", ")
        message("Did not find ", target_sp, ifelse(alt_name == "", alt_name, paste(", but found", alt_name)), ". Returning NULL.")
        return(NULL)
      }

      dt <- dt[dt$species == target_sp,]

      # Return
      dt[!names(dt) %in% "species"]

    })

    tmp <- dplyr::bind_rows(out)

    if(dropMissingMainArea) tmp <- tmp[!is.na(tmp$main_area),]

    tmp

  })

  ## ####

  dt <- do.call(rbind, x)

  dt$sub_area <- factor(as.numeric(as.character(dt$sub_area)), levels = sort(unique(as.numeric(as.character(dt$sub_area)))))
  dt$sub_area <- factor(ifelse(is.na(dt$sub_area) | dt$sub_area == 0, NA, paste(as.character(dt$main_area), as.character(dt$sub_area), sep = "-")))

  dt$ices_area <- factor(dt$ices_area, levels = sort(unique(dt$ices_area)))

  dt <- dt %>%
    dplyr::mutate_at(
      dplyr::vars(.data$year, .data$month, .data$main_area, .data$sub_area, .data$gear_id),
      as.integer)

  # dt$gear_id <- factor(as.numeric(dt$gear_id), levels = sort(unique(as.numeric(dt$gear_id))))

  gear_list <- FDIRcodes$gearCodes
  names(gear_list)[names(gear_list) == "idGear"] <- "gear_id"
  names(gear_list)[names(gear_list) == "gearName"] <- "gear"
  names(gear_list)[names(gear_list) == "gearCategory"] <- "gear_category"

  # gear_list$gear_id <- factor(gear_list$gear_id, levels = as.character(sort(as.numeric(gear_list$gear_id))))
  gear_list$gear_category <- factor(gear_list$gear_category, levels = unique(gear_list$gear_category))
  if(any(duplicated(gear_list$gear_id))) stop("Duplicated gear IDs in the gear_list")

  dt$gear <- factor(
    dplyr::recode(
      dt$gear_id, !!!stats::setNames(gear_list$gear, gear_list$gear_id)),
    levels = as.character(gear_list$gear))

  dt <- dt[dt$weight > 0,]

  dt <- dplyr::left_join(dt, gear_list[c("gear_id", "gear_category")], by = "gear_id")


  dt <- dt[c("year", "month","main_area", "sub_area", "ices_area", "gear_id", "gear_category", "gear", if(is.null(species)) {"species"}, "weight")]

  droplevels(dt)

}
