#' @title North East Atlantic Greenland halibut survey stations
#' @docType data
#' @keywords datasets
#' @name nea_ghl_survey_stations
#' @usage data(nea_ghl_survey_stations)
#' @format A dataframe 
#' @source Institute of Marine Research (\url{https://www.imr.no})
"nea_ghl_survey_stations"

#' @title North East Atlantic Greenland original strata areas
#' @docType data
#' @keywords datasets
#' @name nea_ghl_original_strata
#' @usage data(nea_ghl_original_strata)
#' @format A dataframe 
#' @source Institute of Marine Research (\url{https://www.imr.no})
"nea_ghl_original_strata"

#' @title List of Norwegian Directorate of Fisheries lobbook codes
#' @docType data
#' @keywords datasets
#' @name FDIRcodes
#' @usage data(FDIRcodes)
#' @format A list of data tables
#' @source Norwegian Directorate of Fisheries (\url{https://www.fiskeridir.no/English})
"FDIRcodes"

#' @title Major fishing zones of Norway
#' @docType data
#' @keywords datasets shapefiles
#' @family shapefiles
#' @name fishingAreasNor
#' @format \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} in decimal degrees (+init=epsg:4326) containing major fishing zones defined by the Norwegian Directorate of Fisheries.
#' @source \href{https://kart.fiskeridir.no/stat}{Norwegian Directorate of Fisheries}
#' @import sp
"fishingAreasNor"

#' @title ICES fishing areas
#' @description Food and Agriculture Organization Major Fishing Area 27 (i.e. ICES region) fishing area polygons. The polygons are not cut with land as opposed to the areas distributed on the ICES website (see \code{BioticExplorerServer:::prepareICESareas}). This makes the polygons smaller and consequent calculations quicker. Use this dataset instead of the function.
#' @docType data
#' @keywords datasets shapefiles
#' @family shapefiles
#' @name icesAreas
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} in decimal degrees (+init=epsg:4326).
#' @source \href{https://gis.ices.dk/sf/index.html}{International Council for the Exploration of the Sea}
#' @importFrom sp SpatialPolygonsDataFrame
"icesAreas"

#' @title NMD/IMR taxa code list
#' @description Use the \code{\link{prepareTaxaList}} function to update the list.
#' @docType data
#' @keywords datasets
#' @name taxaList
#' @format A data table 
#' @source Institute of Marine Research (\url{https://imr.no})
"taxaList"

#' @title NMD/IMR gear code list
#' @description Use the \code{\link{prepareGearList}} function to update the list.
#' @docType data
#' @keywords datasets
#' @name gearList
#' @format A data table 
#' @source Institute of Marine Research (\url{https://imr.no})
"gearList"

#' @title NMD/IMR cruise series list
#' @description Use the \code{\link{prepareCruiseSeriesList}} function to update the list. Need to be updated every time data from the database are downloaded.
#' @docType data
#' @keywords datasets
#' @name cruiseSeriesList
#' @format A data table 
#' @source Institute of Marine Research (\url{https://imr.no})
"cruiseSeriesList"