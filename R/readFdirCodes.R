#' @title Retrieve the Norwegian Directorate of Fisheries codes from a code list
#' @description This function retrieves codes used in the electronic logbook data from an Excel sheet published by the Directorate of Fisheries. This list is already supplied in the package and the function is only required to update the codes.
#' @param path Character string specifying the path to the Excel file downloaded from the \href{https://www.fiskeridir.no/Yrkesfiske/Rapportering-ved-landing/Kodeliste}{Directorate of Fisheries webpage}.
#' @param speciesSheet Character string specifying the name of the tab containing species codes.
#' @param speciesStartRow Integer specifying the \code{skip} argument for \code{\link[readxl]{read_xlsx}} in the species code tab.
#' @param speciesHeaderRow Integer specifying row number of header in the species code tab.
#' @param gearSheet Character string specifying the name of the tab containing species codes.
#' @param gearStartRow Integer specifying the \code{skip} argument for \code{\link[readxl]{read_xlsx}} in the gear code tab.
#' @details The function has been written for \href{https://www.fiskeridir.no/Yrkesfiske/Rapportering-ved-landing/Kodeliste}{the code list Excel sheet} published on 2020-10-30. You may have to adjust the function depending on changes in newer versions of the file.
#' @author Mikko Vihtakari
#' @export

# path = "~/Desktop/Kodeliste-landing-171219.xlsx"; speciesSheet = "B-Fiskeslag"; speciesStartRow = 19; speciesHeaderRow = 17; gearSheet = "A7-Redskap"; gearStartRow = 8
readFdirCodes <- function(path,
                          speciesSheet = "B-Fiskeslag",
                          speciesStartRow = 19,
                          speciesHeaderRow = 17,
                          gearSheet = "A7-Redskap",
                          gearStartRow = 8
) {

  ## Species codes

  dt <- suppressMessages(readxl::read_xlsx(path = path, sheet = speciesSheet, skip = speciesStartRow, col_names = FALSE))
  header <- suppressMessages(readxl::read_xlsx(path = path, sheet = speciesSheet, col_names = FALSE, range = paste0("A", speciesHeaderRow, ":", LETTERS[ncol(dt)], speciesHeaderRow)))

  colnames(dt) <- as.character(header[1,])

  dt <- dt[c("Tall", "FAO", "Norsk navn", "Engelsk navn", "Latinsk navn")]
  dt <- dt[rowSums(is.na(dt)) != ncol(dt), ]
  colnames(dt) <- c("idNS", "idFAO", "norwegian", "english", "latin")

  dt$idNS <- suppressWarnings(as.numeric(dt$idNS))
  dt <- dt[!is.na(dt$idNS),]
  dt <- dt[!duplicated(dt$idNS),]
  dt$norwegian <- trimws(gsub("\\*", "", dt$norwegian))
  dt$english <- trimws(gsub("\\*", "", dt$english))

  speciesCodes <- dt

  ## Gear codes

  dt <- suppressMessages(readxl::read_xlsx(path = path, sheet = gearSheet, skip = gearStartRow, col_names = FALSE))
  dt <- dt[,1:2]
  colnames(dt) <- c("idGear", "gearName")
  dt <- dt[!is.na(dt$idGear),]
  dt <- dt %>%
    dplyr::mutate(
      gearCategory = dplyr::case_when(
        idGear %in% 10:19 ~ "Seines",
        idGear %in% c(20:29, 45) ~ "Gillnets",
        idGear %in% 30:39 ~ "Hook gears",
        idGear %in% 40:49 ~ "Traps and fykes",
        idGear %in% c(50:52,55:59,61) ~ "Bottom trawls",
        idGear %in% c(53:54) ~ "Pelagic trawls",
        .default = "Other"
      ),
      Hovedgruppe = dplyr::case_when(
        idGear %in% 10:19 ~ "Not",
        idGear %in% c(20:49, 61) ~ "Konvensjonelle",
        idGear %in% c(50:59) ~ "Traal",
        .default = "Annet"
      ),
      Subgruppe = dplyr::case_when(
        idGear %in% 10:19 ~ "Not",
        idGear %in% c(20:29, 45) ~ "Garn",
        idGear %in% 30:39 ~ "Krokredskap",
        idGear %in% 40:49 ~ "Bur og ruser",
        idGear %in% c(50:59) ~ "Traal",
        idGear %in% c(61) ~ "Snurrevad",
        idGear %in% c(70:79) ~ "Harpun/kanon",
        idGear %in% c(90) ~ "Oppdrett/uspesifisert",
        .default = "Andre redskap"
      )
    )

  gearCodes <- dt

  ## Return

  list(speciesCodes = speciesCodes, gearCodes = gearCodes)

}
