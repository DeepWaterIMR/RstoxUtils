#' @title Get centroid of a Norwegian main fishing area 
#' @description Returns a centroid of a Norwegian Directorate of Fisheries main fishing area (hoverområde) in decimal degrees
#' @param name Integer vector giving the names of main areas
#' @param return.sp Logical indicating whether a SpatialPoints object or data frame should be returned.
#' @return A SpatialPoints object if \code{return.sp = TRUE}. Otherwise, a data frame
#' @author Mikko Vihtakari
#' @importFrom sp coordinates
#' @importFrom rgeos gCentroid
#' @export
#' @examples mainAreaCentroid(1:70)

# name = 70
mainAreaCentroid <- function(name, return.sp = FALSE) {
  
  if(return.sp) {
    rgeos::gCentroid(fishingAreasNor[fishingAreasNor@data$FID %in% name,])
  } else {
    data.frame(sp::coordinates(rgeos::gCentroid(fishingAreasNor[fishingAreasNor@data$FID %in% name,], byid = TRUE)))
  }
}
