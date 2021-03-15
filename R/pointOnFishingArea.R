#' @title Detect statistical fishing area from decimal degree coordinates
#' @description Detects Norwegian Directorate of Fisheries main fishing areas (hoverområde) and FAO/ICES fisheries areas from decimal degree coordinates. 
#' @param lon Numeric vector of longitude coordinates as decimal degrees. Must be the same length than \code{lat}
#' @param lat Numeric vector of latitude coordinates as decimal degrees. Must be the same length than \code{lon}
#' @param type A character argument specifying the fisheries area type. Alternatives: \code{"fdir"} for the Norwegian Directorate of Fisheries main fishing areas, \code{"ices"} for FAO/ICES fisheries areas. 
#' @details The function is designed to work with Norwegian data. All FAO fisheries areas are not included.
#' @return Returns a an integer vector with names of areas. Same length than \code{lon} and \code{lat}. \code{NA} is returned, if a point is outside the areas.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @importFrom sp coordinates proj4string CRS over
#' @export
#' @examples 
#' lon = seq(-50, 50, 10)
#' lat = seq(50, 80, length.out = length(lon))
#' dt <- data.frame(lon, lat)
#' dt$main_area <- pointOnFishingArea(dt$lon, dt$lat)
#' dt$ices_area <- pointOnFishingArea(dt$lon, dt$lat, type = "ices")
#' dt[is.na(dt)] <- "NA"
#' 
#' \dontrun{
#' # Norwegian fishing areas
#' 
#' labels <- sp::SpatialPointsDataFrame(rgeos::gCentroid(fishingAreasNor, byid=TRUE), 
#'                                      data = fishingAreasNor@data)
#' labels <- ggspatial::df_spatial(labels)
#' 
#' ggOceanMaps::basemap(limits = raster::extent(fishingAreasNor)[1:4]) +
#'   ggspatial::annotation_spatial(fishingAreasNor, fill = NA, color = "blue") +
#'   ggspatial::geom_spatial_text(data = dt, 
#'                                aes(x = lon, y = lat, label = main_area), 
#'                                size = FS(10), fontface = 2, color = "red") + 
#'   ggspatial::geom_spatial_text(data = labels, aes(x = x, y = y, label = FID),
#'                                size = FS(8), fontface = 2, color = "blue") 
#' 
#' # ICES areas
#' 
#' ggOceanMaps::basemap(data = dt) +
#'   ggspatial::annotation_spatial(icesAreas, fill = NA, color = "blue") +
#'   ggspatial::geom_spatial_text(data = dt, 
#'                                aes(x = lon, y = lat, label = ices_area), 
#'                                size = FS(10), fontface = 2, color = "red")
#' 
#' }


# lon = seq(-50, 50, 10); lat = seq(50, 80, length.out = length(lon))
pointOnFishingArea <- function(lon, lat, type = "fdir") {
  
  if(length(lon) != length(lat)) stop("length of lon and lat vectors must be equal")
  
  points <- data.frame(lon = lon, lat = lat)
  
  sp::coordinates(points) <- c(1,2)
  sp::proj4string(points) <- sp::CRS("EPSG:4326")
  
  if(type == "fdir") {
    return(sp::over(points, fishingAreasNor)$FID)
  } 
  
  if(type == "ices") {
    return(sp::over(points, icesAreas)$Area_Full)
  }
  
}

