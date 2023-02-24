#' @title Calculate area of strata using only raster data
#' @description The function calculates the area of strata without polygonizing the strata. Useful for checking the results of \code{strataPolygon} function.
#' @param bathy String giving the path to the bathymetry NetCDF file.
#' @param depths Numeric vector giving the cut points for depth strata (see \code{\link[base]{cut}}. Data outside the cut range will be dropped. Use limits of length two exceeding the depths of the region to avoid depth categorization (\code{c(0, 1000)} for instance).
#' @param boundary A \link[sp]{SpatialPolygons}(DataFrame) object, text string defining the file path to a spatial polygon or a numeric vector of length 4 giving the boundaries for the overall region. Should be given as decimal degrees. If numeric vector, the first element defines the minimum longitude, the second element the maximum longitude, the third element the minimum latitude and the fourth element the maximum latitude of the bounding box.
#' @param geostrata A data frame defining the minimum and maximum longitude and latitude for geographically bounded strata. The data frame columns must be ordered as \code{lon.min, lon.max, lat.min, lat.max}. Column names do not matter. Each row in the data frame will be interpreted as separate geographically bounded strata. 
#' @details The function uses the \code{\link[raster]{reclassify}} and \code{\link[raster]{area}} functions to calculate the area of depth strata specified by the \code{depths} argument over a polygon specified by the \code{boundary} and \code{geostrata} arguments. 
#' @return Returns a data frame. The areas are expressed in square kilometers (km2) and nautical miles (nm2).
#' @import sp
#' @rawNamespace import(raster, except = shift)
#' @importFrom rgdal readOGR
#' @importFrom dplyr left_join
#' @export

# Test parameters
# bathy <- "../../GIS/GEBCO bathymetry/GEBCO_2019/GEBCO_2019.nc"; boundary <- c(0, 29, 68, 80); geostrata <- data.frame(lon.min = c(3, 10, 10, 8, 17.3), lon.max = c(16, 17.3, 17.3, 17.3, 29), lat.min = c(76, 73.5, 70.5, 68, 72.5), lat.max = c(80, 76, 73.5, 70.5, 76)); depths <- c(400, 500, 700, 1000, 1500)

# bathy = "/Users/a22357/Downloads/GEBCO_2019/GEBCO_2019.nc"; boundary = c(0, 35, 68, 80); depths = c(400, 500, 700, 1000, 1500); geostrata = data.frame(lon.min = c(0, 0, 0, 8, 17.5), lon.max = c(15, 17.5, 17.5, 17.5, 35), lat.min = c(76, 73.5, 70.5, 68, 72.5), lat.max = c(80, 76, 73.5, 70.5, 76))

# bathy = link; depths = depths.vec; boundary = boundary.path; geostrata = geostrata.df
strataArea <- function(bathy, depths, boundary, geostrata = NULL) {

  ## General checks ####
  
  ### Bathy argument 
  
  if(!file.exists(bathy)) stop("Bathy raster file not found. Check the path in the bathy argument.")
  
  ### The depths argument
  
  if(!(is.vector(depths) & class(depths) %in% c("numeric", "integer"))) {
    stop("The depths parameter has to be a numeric or integer vector.")}
  
  ### The boundary argument
  
  if(grepl("spatialpolygons", class(boundary), ignore.case = TRUE)) {
    
    if(is.null(suppressWarnings(sp::proj4string(boundary)))) {
      stop("boundary misses proj4string argument.")
    } else if(!grepl("+proj=longlat", suppressWarnings(sp::proj4string(boundary)))) {
      stop("boundary has to be defined as decimal degrees")
    }
    
  } else if(inherits(boundary, "character") & length(boundary) == 1) {
    if(!file.exists(boundary)) stop("Boundary shapefile not found. Check your path")
    
    boundary <- rgdal::readOGR(boundary, verbose = FALSE)
    
    if(is.null(suppressWarnings(sp::proj4string(boundary)))) {
      stop("boundary misses proj4string argument.")
    } else if(!grepl("+proj=longlat", suppressWarnings(sp::proj4string(boundary)))) {
      stop("boundary has to be defined as decimal degrees")
    }
    
  } else if(!(is.vector(boundary) & class(boundary) %in% c("numeric", "integer") & length(boundary) == 4)) {
    stop("The boundary parameter has to be a numeric/integer vector of length 4 giving the decimal degree longitude and latitude limits for the strata region OR a character argument giving the location of the shapefile polygon.")
  }
  
  ### The geostrata argument
  
  if(!is.null(geostrata)) {
    if(!(is.data.frame(geostrata) & ncol(geostrata) == 4)) {
      stop("The geostrata argument has to be a data.frame with 4 columns.")
    }
  }
  
  ## Open raster
  
  ras <- raster::raster(bathy)
  
  if(is.null(suppressWarnings(sp::proj4string(ras)))) stop("bathy does not contain coordinate reference information")
  if(!grepl("+proj=longlat", suppressWarnings(sp::proj4string(ras)))) stop("bathy has to be in decimal degree projection. Use 'EPSG:4326'")
  
  ras <- raster::crop(ras, raster::extent(boundary))
  
  if(grepl("spatialpolygons", class(boundary), ignore.case = TRUE)) {
    ras <- raster::mask(ras, boundary)
  }
  
  ## Reclassify raster
  
  if(all(depths >= 0)) depths <- sort(-1 * depths)
  
  depths <- c(-Inf, depths, Inf)
  
  cut_int <- paste(abs(depths[-1]), abs(depths[-length(depths)]), sep = "-")
  cut_df <- data.frame(from = depths[-length(depths)], 
                       to = depths[-1], 
                       average = sapply(strsplit(cut_int, "-"), function(k) mean(as.numeric(k))),
                       interval = cut_int, 
                       stringsAsFactors = FALSE)
  
  cut_matrix <- as.matrix(cut_df[-ncol(cut_df)])
  
  r <- raster::reclassify(ras, rcl = cut_matrix, right = NA)
  
  ## Make the polygons
  
  if(is.null(geostrata)) {
    areas <- tapply(suppressWarnings(raster::area(r, na.rm = TRUE)), r[], sum)
    
    out <- data.frame(average = as.numeric(names(areas)), stringsAsFactors = FALSE)
    out <- dplyr::left_join(out, cut_df, by = "average")
    out$area.km2 <- unname(areas)
    out$area.nm2 <- unname(areas)/1.852^2
    
  } else {
    
    polys <- lapply(1:nrow(geostrata), function(i) {
      x <- geostrata[i,]
      sp::Polygons(list(sp::Polygon(as.matrix(data.frame(lon = c(x$lon.min, x$lon.min, x$lon.max, x$lon.max, x$lon.min), 
                 lat = c(x$lat.min, x$lat.max, x$lat.max, x$lat.min, x$lat.min))))), ID = i)
    })
    
    polys <- sp::SpatialPolygons(polys, proj4string = sp::CRS("EPSG:4326"))
    polys <- sp::SpatialPolygonsDataFrame(polys, geostrata)
    
    tmp <- lapply(1:length(polys), function(i) {
      
      r_out <- raster::crop(r, polys[i,])
      areas <- tapply(suppressWarnings(raster::area(r_out, na.rm = TRUE)), r_out[], sum)
      
      out <- data.frame(average = as.numeric(names(areas)), geostrata.name = LETTERS[i], stringsAsFactors = FALSE, area.km2 = unname(areas), area.nm2 =  unname(areas)/1.852^2)
      out <- dplyr::left_join(out[!is.infinite(out$average),], cut_df[!is.infinite(cut_df$average),], by = "average")
      out <- suppressWarnings(cbind(out, polys@data[i,]))
      
      out[order(out$average, decreasing = TRUE),]
      
    })
    
    out <- do.call(rbind, tmp)
    rownames(out) <- 1:nrow(out)
  }
  
  
  ## Output

  out
}
