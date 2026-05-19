#' @title List of Norwegian Directorate of Fisheries logbook codes
#' @description A named list with two elements: \code{$speciesCodes} (FDIR species codes with FAO codes, Norwegian, English, and Latin names) and \code{$gearCodes} (FDIR gear codes with Norwegian/English categories). Use \code{\link{readFdirCodes}} to update from the FDIR Excel code list.
#' @docType data
#' @keywords datasets
#' @name FDIRcodes
#' @usage data(FDIRcodes)
#' @format A list of two data frames
#' @source Norwegian Directorate of Fisheries (\url{https://www.fiskeridir.no})
"FDIRcodes"

#' @title NMD/IMR taxa code list
#' @description Use the \code{\link[BioticExplorerServer]{prepareTaxaList}} function to update the list.
#' @docType data
#' @keywords datasets
#' @name taxaList
#' @format A data table
#' @source Institute of Marine Research (\url{https://imr.no})
"taxaList"

#' @title NMD/IMR gear code list
#' @description Use the \code{\link[BioticExplorerServer]{prepareGearList}} function to update the list.
#' @docType data
#' @keywords datasets
#' @name gearList
#' @format A data table
#' @source Institute of Marine Research (\url{https://imr.no})
"gearList"

#' @title NMD/IMR cruise series list
#' @description Use the \code{\link[BioticExplorerServer]{prepareCruiseSeriesList}} function to update the list. Need to be updated every time data from the database are downloaded.
#' @docType data
#' @keywords datasets
#' @name cruiseSeriesList
#' @format A data table
#' @source Institute of Marine Research (\url{https://imr.no})
"cruiseSeriesList"

#' @title Example ERS data
#' @description Example ERS data for beaked redfish. Use the \code{\link{extractERS}} function to produce similar data.
#' @docType data
#' @keywords datasets
#' @name ers_example_data
#' @format A tibble
#' @source Norwegian Directorate of Fisheries (\url{https://www.fiskeridir.no/})
"ers_example_data"

#' @title Example sales note data downloaded through the API
#' @description Example sales note data for beaked redfish. Use the \code{\link{downloadLandings}} function to produce similar data.
#' @docType data
#' @keywords datasets
#' @name salesnote_example_data
#' @format A tibble
#' @source Norwegian Directorate of Fisheries (\url{https://www.fiskeridir.no/})
"salesnote_example_data"

#' @title Example historic sales note data extracted from Excel sheets on IMR server
#' @description Example sales note data for beaked redfish. Use the \code{\link{readSluttseddelXLS}} function to produce similar data.
#' @docType data
#' @keywords datasets
#' @name salesnote_xls_data
#' @format A tibble
#' @source Institute of Marine Research (\url{https://imr.no}) and Norwegian Directorate of Fisheries (\url{https://www.fiskeridir.no/})
"salesnote_xls_data"
