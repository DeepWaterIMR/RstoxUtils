# .onLoad <- function(libname, pkgname) {
# }

.onAttach <- function(libname, pkgname) {
  options(timeout = max(10000, getOption("timeout")))
}

# Define global variables
## Define global variables

utils::globalVariables(c(".", ".data", "FANGSTART_NS", "FANGSTART_FAO", "HOVEDART_NS", "HOVEDART_FAO", "dateEnd", "dateStart", "depth", "depthEnd", "depthStart", "gear", "gearCat", "gearId", "gearName", "idFAO", "idNS", "individualweight", "lat", "latEnd", "latStart", "lon", "lonEnd", "lonStart", "mass", "stationstartdate", "stationstarttime", "stationstopdate", "stationstoptime", "gearCategory", "FDIRcodes", "fishingAreasNor", "icesAreas", "description", "cruiseseriescode", "language", "Elevation.relative.to.sea.level", "area.km2", "nautical_mile", "area", "catchpartnumber", "commonname", "numberofreads", "platform", "preferredagereading", "serialnumber", "specimenid", "startyear", "gearcategory"))
