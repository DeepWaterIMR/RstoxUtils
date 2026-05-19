#' @title Prepare cruise series list
#' @description Wrapper for \code{\link[BioticExplorerServer]{prepareCruiseSeriesList}}.
#' Uses the updated IMR reference API. See BioticExplorerServer for full documentation.
#' @importFrom BioticExplorerServer prepareCruiseSeriesList
#' @export
prepareCruiseSeriesList <- function() {
  BioticExplorerServer::prepareCruiseSeriesList()
}

#' @title Prepare gear list
#' @description Wrapper for \code{\link[BioticExplorerServer]{prepareGearList}}.
#' Uses the updated IMR reference API. See BioticExplorerServer for full documentation.
#' @importFrom BioticExplorerServer prepareGearList
#' @export
prepareGearList <- function() {
  BioticExplorerServer::prepareGearList()
}

#' @title Prepare taxa list
#' @description Wrapper for \code{\link[BioticExplorerServer]{prepareTaxaList}}.
#' Uses the updated IMR reference API. See BioticExplorerServer for full documentation.
#' @param verbose Logical. Print status messages during download. Defaults to \code{FALSE}.
#' @importFrom BioticExplorerServer prepareTaxaList
#' @export
prepareTaxaList <- function(verbose = FALSE) {
  BioticExplorerServer::prepareTaxaList(verbose = verbose)
}
