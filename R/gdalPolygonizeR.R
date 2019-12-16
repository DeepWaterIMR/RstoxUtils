#' @title Polygonize raster using a Python script and QGIS
#' @description ...
#' @param x raster object to be polygonized
#' @param outshape ...
#' @param gdalformat ...
#' @param pypath ...
#' @param readpoly ...
#' @param quiet ...
#' @param rewrite ...
#' @details Documentation and code tidying has not been written for this function yet.
#' @import raster
#' @author \href{(https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/}{John Baumgartner}. See also \href{https://gis.stackexchange.com/questions/166753/fastest-way-to-convert-big-raster-to-polyline-using-r-or-python}{StackOverflow}.
#' @export

gdalPolygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                            pypath = NULL, readpoly = TRUE, quiet = TRUE, rewrite = TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if(!rewrite)
      if (any(f.exists))
        stop(sprintf('File already exists: %s',
                     toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                    sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}
