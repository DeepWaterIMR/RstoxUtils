#' @title Create polygon shapefiles for depth strata required by Rstox
#' @description A helper function to define strata for stock assesment from GEBCO and ETOPO bathymetry grids.
#' @param bathy String giving the path to the bathymetry NetCDF file.
#' @param depths Numeric vector giving the cut points for depth strata (see \code{\link[base]{cut}}. Data outside the cut range will be dropped. Use limits of length two exceeding the depths of the region to avoid depth categorization (\code{c(0, 1000)} for instance).
#' @param boundary A \link[sp]{SpatialPolygons}(DataFrame) object, text string defining the file path to a spatial polygon or a numeric vector of length 4 giving the boundaries for the overall region. Should be given as decimal degrees. If numeric vector, the first element defines the minimum longitude, the second element the maximum longitude, the third element the minimum latitude and the fourth element the maximum latitude of the bounding box.
#' @param geostrata A data frame defining the minimum and maximum longitude and latitude for geographically bounded strata. The data frame columns must be ordered as \code{lon.min, lon.max, lat.min, lat.max}. Column names do not matter. Each row in the data frame will be interpreted as separate geographically bounded strata. Use \code{NULL} to ignore geostrata.
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for disconnected polygons which should be removed from the strata. Set to \code{NULL} to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @param remove.holes Single numeric value specifying a threshold (area in km2) for holes which should be removed from the strata. Set to \code{NULL} to bypass the removal. Uses the \link[smoothr]{fill_holes} function. 
#' A single logical argument or a logical vector as long as the number of rows in \code{geostrata} specifying whether holes in the strata polygons should be removed. 
#' @details Uses \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}{GEBCO} or \href{https://www.ngdc.noaa.gov/mgg/global/}{ETOPO1} bathymetry grids to define the depth strata. Download the desired grid from the links. The bathymetry grids must be in NetCDF format and defined using decimal degrees.
#' @return \link[sp]{SpatialPolygonsDataFrame} containing the estimated strata and information for them including areas. The strata are returned as decimal degrees (WGS84).
#' @references GEBCO Compilation Group (2019) GEBCO 2019 15-arcsecond grid (doi:10.5285/836f016a-33be-6ddc-e053-6c86abc0788e). URL: \url{https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2019/gebco_2019_info.html}.
#' 
#' ETOPO1 1 Arc-Minute Global Relief Model. \url{https://doi.org/10.7289/V5C8276M}.
#' @import sp rgeos
#' @importFrom rgdal readOGR
#' @rawNamespace import(raster, except = shift)
#' @importFrom smoothr drop_crumbs
#' @importFrom dplyr left_join
#' @importFrom units set_units
#' @importFrom sf as_Spatial st_as_sf
#' @importFrom stars st_as_stars
#' @author Mikko Vihtakari
#' @seealso \code{\link{strataArea}} for calculating strata areas without polygonization. 
#' @export

## Developmental code
# bathy = link; depths = depths.vec; boundary = boundary; geostrata = geostrata.df; drop.crumbs = NULL; remove.holes = NULL
strataPolygon <- function(bathy, depths, boundary, geostrata = NULL, drop.crumbs = NULL, remove.holes = NULL) {
  
  ## General checks ####
  
  ### Bathy argument 
  
  if(!file.exists(bathy)) stop("Bathy raster file not found. Check the path in the bathy argument.")
  
  ### The depths argument
  
  if(!(is.vector(depths) & class(depths) %in% c("numeric", "integer"))) {
    stop("The depths parameter has to be a numeric or integer vector.")}
  
  ### The boundary argument
  
  if(grepl("spatialpolygons", class(boundary), ignore.case = TRUE)) {
    
    if(is.null(sp::proj4string(boundary))) {
      stop("boundary misses proj4string argument.")
    } else if(!grepl("+proj=longlat", sp::proj4string(boundary))) {
      stop("boundary has to be defined as decimal degrees")
    }
    
  } else if(class(boundary) == "character" & length(boundary) == 1) {
    if(!file.exists(boundary)) stop("Boundary shapefile not found. Check your path")
    
    boundary <- rgdal::readOGR(boundary, verbose = FALSE)
    
    if(is.null(sp::proj4string(boundary))) {
      stop("boundary misses proj4string argument.")
    } else if(!grepl("+proj=longlat", sp::proj4string(boundary))) {
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
  
  ### The drop.crumbs argument
  
  if(!is.null(drop.crumbs)) {
    if(!(is.vector(drop.crumbs) & class(drop.crumbs) %in% c("numeric", "integer") & length(drop.crumbs) == 1)) {
      stop("The drop.crumbs parameter has to be a single value.")
    }
  }
  
  ## Open raster ####
  
  ras <- raster::raster(bathy)
  
  if(is.null(sp::proj4string(ras))) stop("bathy does not contain coordinate reference information")
  if(!grepl("+proj=longlat", sp::proj4string(ras))) stop("bathy has to be in decimal degree projection. Use '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'")
  
  ras <- raster::crop(ras, raster::extent(boundary))
  
  if(grepl("spatialpolygons", class(boundary), ignore.case = TRUE)) {
    ras <- raster::mask(ras, boundary)
  }
  
  ## Reclassify raster ####
  
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
  
  ## Polygonization ####
  
  pol <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(r), as_points = FALSE, merge = TRUE))
  
  ### Validate the polygon
  
  if(!suppressMessages(suppressWarnings(rgeos::gIsValid(pol)))) {
    pol <- suppressMessages(suppressWarnings(rgeos::gBuffer(pol, byid = TRUE, width = 0)))  
    
    if(!rgeos::gIsValid(pol)) stop("The initial geometry validation did not work. You are skrewed.")
  }
  
  ### Remove Inf
  
  pol <- pol[pol@data[[1]] != Inf,]
  cut_df <- cut_df[cut_df$average != Inf,]
  
  ## Geostrata ####
  
  if(!is.null(geostrata)) {
    
    # i = 1
    out.pols <- lapply(1:nrow(geostrata), function(i) {
      
      ## ###
      
      tmp <- unname(unlist(geostrata[i,]))
      
      out <- suppressMessages(suppressWarnings(clipShapefile(pol, limits = tmp)))
      
      if(!is.null(drop.crumbs)) {
        out <- suppressWarnings(suppressMessages(smoothr::drop_crumbs(out, units::set_units(drop.crumbs, "km^2", mode = "standard"))))
      }
      
      if(!is.null(remove.holes)) {
        out <- smoothr::fill_holes(out, units::set_units(remove.holes, "km^2", mode = "standard")) # Alternative: spatialEco::remove.holes(out)
      }
      
      if(!suppressMessages(suppressWarnings(rgeos::gIsValid(out)))) {
        out <- suppressMessages(suppressWarnings(rgeos::gBuffer(out, byid = TRUE, width = 0)))  
        
        if(!rgeos::gIsValid(out)) stop("The final geometry validation did not work. Adjust something.")
      }
      
      ## Combine polygons with same depth
      
      out <- raster::aggregate(out, by = names(out@data[1]), dissolve = TRUE)
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
    
    
    if(length(out.pols) == 1) {
      pol <- out.pols[[1]]
    } else {
      pol <- do.call(raster::bind, out.pols)
    }
    
    
  } else {
    
    if(!is.null(drop.crumbs)) {
      pol <- suppressWarnings(suppressMessages(smoothr::drop_crumbs(pol, units::set_units(drop.crumbs, "km^2", mode = "standard"))))
    }
    
    if(!is.null(remove.holes)) {
      pol <- smoothr::fill_holes(pol, units::set_units(remove.holes, "km^2", mode = "standard")) # Alternative: spatialEco::remove.holes(out)
    }
    
    if(!suppressMessages(suppressWarnings(rgeos::gIsValid(pol)))) {
      pol <- suppressMessages(suppressWarnings(rgeos::gBuffer(pol, byid = TRUE, width = 0)))  
      
      if(!rgeos::gIsValid(pol)) stop("The final geometry validation did not work. Adjust something.")
    }
    
    ## Combine polygons with same depth
    
    pol <- raster::aggregate(pol, by = names(pol@data[1]), dissolve = TRUE)
    pol <- pol[order(pol@data[1], decreasing = TRUE),]
    
    tmp <- pol@data
    names(tmp) <- "average"
    # tmp.rowns <- rownames(tmp)
    
    tmp <- dplyr::left_join(tmp, cut_df, by = "average")
    
    # rownames(tmp) <- tmp.rowns
    
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
