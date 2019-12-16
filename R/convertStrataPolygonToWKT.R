#' @title Convert strata spatial polygons to the WKT format required by Rstox
#' @description Converts strata polygons from \code{\link{strataPolygon}} to the WKT format required by Rstox
#' @param x A SpatialPolygonsDataFrame containing the strata polygons. 
#' @return ...
#' @import wicket sp
#' @author Mikko Vihtakari
#' @export

strataPolygonToWKT <- function(x) {
  
  ## Combine by depth level
  
  # depth_levels <- unique(pol@data)
  
  #i <- 1
  # out <- lapply(1:nrow(depth_levels), function(i) {
  #   y <- pol[pol@data$average == depth_levels$average[i],]
  #   x <- unlist(wicket::sp_convert(SpatialPolygons(y@polygons), group = FALSE))
  #   
  #   # x <- gsub(",\\({2}", ",(", x)
  #   # x <- gsub("\\){2},", "),", x)
  #   
  #   dt <- data.frame(strata = paste0("D", depth_levels$interval[i], "_", 1:length(x)), poly = x, stringsAsFactors = FALSE)
  #   
  #   list(data = dt, polygons = y, interval = depth_levels$interval[i])
  #   
  #   #paste0("D", depth_levels$interval[i], "\t", x)
  # })
  # 
  # Out <- do.call(rbind, lapply(out, function(k){k$data}))
  
  # pols <- lapply(out, function(k) k$polygons)
  # names(pols) <- as.character(sapply(out, function(k) k$interval))
  # 
  
  ## Return
  
}
