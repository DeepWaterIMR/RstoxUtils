.onLoad <- function(libname, pkgname) {
  options("rgdal_show_exportToProj4_warnings"="none")
}

.onAttach <- function(libname, pkgname) {
  options(timeout = max(6000, getOption("timeout"))) 
}

# Define global variables
## Define global variables

utils::globalVariables(c(".", "FANGSTART_NS", "FANGSTART_FAO", "dateEnd", "dateStart", "depth", "depthEnd", "depthStart", "gear", "gearCat", "gearId", "gearName", "idFAO", "individualweight", "lat", "latEnd", "latStart", "lon", "lonEnd", "lonStart", "mass", "stationstartdate", "stationstarttime", "stationstopdate", "stationstoptime", "gearCategory", "FDIRcodes", "fishingAreasNor", "icesAreas", "description", "cruiseseriescode", "gearcategory", "language"))
