#' @title Prepare cruise series list
#' @description Wrapper for \code{\link[BioticExplorerServer]{prepareCruiseSeriesList}}.
#' Uses the updated IMR reference API. See BioticExplorerServer for full documentation.
#' @return A data.table of cruise series metadata from the IMR reference API.
#' @importFrom BioticExplorerServer prepareCruiseSeriesList
#' @family Biotic functions
#' @export
prepareCruiseSeriesList <- function() {
  BioticExplorerServer::prepareCruiseSeriesList()
}

#' @title Prepare gear list
#' @description Wrapper for \code{\link[BioticExplorerServer]{prepareGearList}}.
#' Uses the updated IMR reference API. See BioticExplorerServer for full documentation.
#' @return A data.table of gear codes and categories from the IMR reference API.
#' @importFrom BioticExplorerServer prepareGearList
#' @family Biotic functions
#' @export
prepareGearList <- function() {
  BioticExplorerServer::prepareGearList()
}

#' @title Prepare taxa list
#' @description Wrapper for \code{\link[BioticExplorerServer]{prepareTaxaList}}.
#' Uses the updated IMR reference API. See BioticExplorerServer for full documentation.
#' @param verbose Logical. Print status messages during download. Defaults to \code{FALSE}.
#' @return A data.table of taxon codes and names from the IMR reference API.
#' @importFrom BioticExplorerServer prepareTaxaList
#' @family Biotic functions
#' @export
prepareTaxaList <- function(verbose = FALSE) {
  BioticExplorerServer::prepareTaxaList(verbose = verbose)
}
