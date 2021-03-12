#' @title Detect Norwegian main fishing area from decimal degree coordinates
#' @description Detects Norwegian Directorate of Fisheries main fishing area (hoverområde) from decimal degree coordinates
#' @param lon Numeric vector of longitude coordinates as decimal degrees. Must be the same length than \code{lat}
#' @param lat Numeric vector of latitude coordinates as decimal degrees. Must be the same length than \code{lon}
#' @return Returns a an integer vector with names of main areas. Same length than \code{lon} and \code{lat}. \code{NA} is returned, if a point is outside the areas.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @importFrom sp coordinates proj4string CRS over
#' @examples 
#' @export
#' \dontrun{
#' library(ggOceanMaps)
#' 
#' dt <- data.frame(lon = seq(-50, 50, 10), 
#'                  lat = seq(50, 80, length.out = length(lon)))
#' dt$main_area <- pointOnMainArea(dt$lon, dt$lat)
#' 
#' basemap(limits = raster::extent(fishingAreasNor)[1:4]) +
#'     annotation_spatial(fishingAreasNor, fill = NA, color = "blue") +
#'     geom_spatial_text(data = dt, 
#'                       aes(x = lon, y = lat, label = main_area), 
#'                       size = FS(10), fontface = 2, color = "red") + 
#'     geom_spatial_text(data = labels, aes(x = x, y = y, label = FID),
#'                       size = FS(8), fontface = 2, color = "blue") 
#' }


# lon = seq(-50, 50, 10); lat = seq(50, 80, length.out = length(lon))
pointOnMainArea <- function(lon, lat) {
  
  if(length(lon) != length(lat)) stop("length of lon and lat vectors must be equal")
  
  points <- data.frame(lon = lon, lat = lat)
  
  sp::coordinates(points) <- c(1,2)
  sp::proj4string(points) <- sp::CRS("EPSG:4326")
  
  sp::over(points, fishingAreasNor)$FID
}

