#' @title Create polygon shapefiles for depth strata required by Rstox
#' @description A helper function to define strata for stock assesment from GEBCO and ETOPO bathymetry grids.
#' @param bathy String giving the path to the bathymetry NetCDF file.
#' @param depths Numeric vector giving the cut points for depth strata (see \code{\link[base]{cut}}. Data outside the cut range will be dropped. Use limits of length two exceeding the depths of the region to avoid depth categorization (\code{c(0, 1000)} for instance).
#' @param boundary Numeric vector of length 4 giving the boundaries for the overall region. See \code{\link[raster]{extent}}. Should be given as decimal degrees. The first element defines the minimum longitude, the second element the maximum longitude, the third element the minimum latitude and the fourth element the maximum latitude of the bounding box.
#' @param geostrata A data frame defining the minimum and maximum longitude and latitude for geographically bounded strata. The data frame columns must be ordered as \code{lon.min, lon.max, lat.min, lat.max}. Column names do not matter. Each row in the data frame will be interpreted as separate geographically bounded strata. 
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for disconnected polygons which should be removed from the strata. Set to \code{NULL} to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @param remove.holes Single numeric value specifying a threshold (area in km2) for holes which should be removed from the strata. Set to \code{NULL} to bypass the removal. Uses the \link[smoothr]{fill_holes} function. 
#' A single logical argument or a logical vector as long as the number of rows in \code{geostrata} specifying whether holes in the strata polygons should be removed. 
#' @param strata.names A character vector spcifying the names of \code{geostrata}. Not implemented.
#' @param validate.polygons A logical indicating whether the function should retunr valid geometries. This option might considerably change the output, but makes it compatible with GIS software.
#' @param use.python Logical indicating whether the function should use gdal python script (\code{TRUE}; \code{gdal_polygonize.py}) or \code{\link[raster]{rasterToPolygons}} (\code{FALSE}) for polygonization of strata. The python script has a superior computing time, but requires QGIS 2.18 installed on the computer (earlier or later versions won't do). 
#' @details Uses \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}{GEBCO} or \href{https://www.ngdc.noaa.gov/mgg/global/}{ETOPO1} bathymetry grids to define the depth strata. Download the desired grid from the links. The bathymetry grids must be in NetCDF format.
#' @return \link[sp]{SpatialPolygonsDataFrame} containing the estimated strata and information for them including areas. The strata are returned as decimal degrees (WGS84).
#' @references GEBCO Compilation Group (2019) GEBCO 2019 15-arcsecond grid (doi:10.5285/836f016a-33be-6ddc-e053-6c86abc0788e). URL: \url{https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2019/gebco_2019_info.html}.
#' 
#' ETOPO1 1 Arc-Minute Global Relief Model. \url{https://doi.org/10.7289/V5C8276M}.
#' @import raster sp rgeos
#' @importFrom spatialEco remove.holes
#' @importFrom smoothr drop_crumbs
#' @importFrom dplyr left_join
#' @importFrom units set_units
#' @author Mikko Vihtakari
#' @export

# geostrata = data.frame(lon.min = c(0, 0, 0, 8, 17.5), lon.max = c(15, 17.5, 17.5, 17.5, 35), lat.min = c(76, 73.5, 70.5, 68, 72.5), lat.max = c(80, 76, 73.5, 70.5, 76)); boundary = c(0, 35, 68, 80); depths = c(400, 500, 700, 1000, 1500)
# bathy = "/vsigzip//Users/a22357/Downloads/ETOPO1_Ice_g_gdal.grd.gz"
# bathy = "/vsizip/Users/a22357/Downloads/GEBCO_2019.zip/GEBCO_2019.nc"
# bathy = "/Users/a22357/Downloads/GEBCO_2019/GEBCO_2019.nc"
# bathy = "vsigzip/ETOPO1_Ice_g_gdal.grd.gz/ETOPO1_Ice_g_gdal.grd"
# bathy = "/Users/a22357/Dropbox/Workstuff/GIS/GEBCO bathymetry/GEBCO_2014_1D.nc"; boundary = c(5, 50, 69, 82); depths = c(0, 200, 500, 750)
# drop.crumbs = 500; remove.holes = FALSE; strata.names = NULL; validate.polygons = TRUE; use.python = TRUE
# bathy = "/Users/a22357/Downloads/GEBCO_2019/GEBCO_2019.nc"; boundary = c(0, 35, 68, 80); depths = c(400, 500, 700, 1000, 1500); geostrata = data.frame(lon.min = c(0, 0, 0, 8, 17.5), lon.max = c(15, 17.5, 17.5, 17.5, 35), lat.min = c(76, 73.5, 70.5, 68, 72.5), lat.max = c(80, 76, 73.5, 70.5, 76)); drop.crumbs = 200; remove.holes = 1000; strata.names = NULL; validate.polygons = TRUE; use.python = TRUE
strataPolygon <- function(bathy, depths, boundary, geostrata = NULL, drop.crumbs = NULL, remove.holes = NULL, strata.names = NULL, validate.polygons = TRUE, use.python = TRUE) {
  
  ## General checks ####
  
  if(!(is.vector(depths) & class(depths) %in% c("numeric", "integer"))) {
    stop("The depths parameter has to be a numeric or integer vector.")}
  
  if(!(is.vector(boundary) & class(boundary) %in% c("numeric", "integer") & length(boundary) == 4)) {
    stop("The boundary parameter has to be a numeric or integer vector of length 4 giving the decimal degree longitude and latitude limits for the strata region.")
  }
  
  if(!is.null(geostrata)) {
    if(!(is.data.frame(geostrata) & ncol(geostrata) == 4)) {
      stop("The geostrata argument has to be a data.frame with 4 columns.")
    }
  }
  
  if(!is.null(drop.crumbs)) {
    if(!(is.vector(drop.crumbs) & class(drop.crumbs) %in% c("numeric", "integer") & length(drop.crumbs) == 1)) {
      stop("The drop.crumbs parameter has to be a single value.")
    }
  }
  
  ## Open raster
  
  ras <- raster::raster(bathy)
  
  if(is.null(proj4string(ras))) stop("bathy does not contain coordinate reference information")
  if(proj4string(ras) != "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") stop("bathy has to be in decimal degree projection. Use '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'")
  
  ras <- raster::crop(ras, raster::extent(boundary))
  
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
  
  ## Polygonization
  
  if(use.python) {
    pol <- gdalPolygonizeR(r) 
  } else {
    pol <- raster::rasterToPolygons(r)
  }
  
  ## Geostrata ####
  
  if(!is.null(geostrata)) {
    
    # i = 1
    out.pols <- lapply(1:nrow(geostrata), function(i) {
      
      ## ###
      
      tmp <- unname(unlist(geostrata[i,]))
      
      out <- suppressMessages(suppressWarnings(clipShapefile(pol, limits = tmp)))
      
      if(!is.null(drop.crumbs)) {
        out <- suppressWarnings(suppressMessages(smoothr::drop_crumbs(out, units::set_units(drop.crumbs, km^2))))
      }
      
      if(!is.null(remove.holes)) {
        out <- smoothr::fill_holes(out, units::set_units(remove.holes, km^2)) # Alternative: spatialEco::remove.holes(out)
      }
      
      if(validate.polygons & !suppressMessages(suppressWarnings(rgeos::gIsValid(out)))) {
        out <- suppressMessages(suppressWarnings(rgeos::gBuffer(out, byid = TRUE, width = 0)))  
        
        if(!rgeos::gIsValid(out)) stop("Geometry validation did not work for ADD")
      }
      
      ## Combine polygons with same depth
      
      out <- aggregate(out, by = names(out@data[1]), dissolve = TRUE)
      out <- out[order(out@data[1], decreasing = TRUE),]
      
      ## Add names to @data ###
      
      tmp <- out@data
      names(tmp) <- "average"
      
      tmp <- dplyr::left_join(tmp, cut_df, by = "average")
      tmp <- suppressWarnings(cbind(tmp, geostrata[i,], geostrata.name = LETTERS[i]))
      
      out@data <- tmp
      
      ## Return
      
      out
    })
    
    pol <- do.call(raster::bind, out.pols)
    
  } else {
    
    if(!is.null(drop.crumbs)) {
      pol <- suppressWarnings(suppressMessages(smoothr::drop_crumbs(pol, units::set_units(drop.crumbs, km^2))))
    }
    
    if(!is.null(remove.holes)) {
      out <- smoothr::fill_holes(out, units::set_units(remove.holes, km^2)) # Alternative: spatialEco::remove.holes(out)
    }
    
    if(validate.polygons & !rgeos::gIsValid(pol)) {
      pol <- rgeos::gBuffer(pol, byid = TRUE, width = 0) 
      
      if(!rgeos::gIsValid(pol)) stop("Geometry validation did not work for ADD")
    }
    
    tmp <- pol@data
    names(tmp) <- "average"
    tmp.rowns <- rownames(tmp)
    
    tmp <- dplyr::left_join(tmp, cut_df, by = "average")
    
    rownames(tmp) <- tmp.rowns
    
    pol@data <- tmp
    
  }
  
  ## Fix plot order (this might mess things up)
  
  pol@plotOrder <- as.integer(rownames(pol@data))
  
  ## Calculate area ####
  
  pol@data$area.km2 <- raster::area(pol)/1e6
  pol@data$area.nm2 <- pol@data$area.km2/1.852^2
  
  ## Add ID to @data
  
  pol@data$id <- as.numeric(sapply(slot(pol, "polygons"), function(x) slot(x, "ID")))
  
  ## Return
  
  pol
  
}
