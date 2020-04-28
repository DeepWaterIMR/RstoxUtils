#' @title Convert font sizes measured as points to ggplot font sizes
#' @description Converts font sizes measured as points (as given by most programs such as MS Word etc.) to ggplot font sizes
#' @param x numeric vector giving the font sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot font sizes
#' @keywords internal
#' @export
#'
FS <- function(x) x/2.845276 # x is the desired font / line size in pt

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by most programs such as Adobe Illustrator etc.) to ggplot font sizes
#' @param x numeric vector giving the lines sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot line sizes
#' @keywords internal
#' @export
#'
LS <- function(x) x/2.13

#' @title Select an element of each vector from a list
#' @description Selects y'th element of each vector from a list
#' @param x list
#' @param y number of element. Must be integer
#' @keywords internal
#' @export
#'
select.element <- function(x,y) sapply(x, "[", y)

#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
#' @keywords internal
#' @author Hadley Wickham
#' @export
#'
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' @title Round to pretty log breaks
#' @param x numeric vector to round
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
#' @keywords internal
#' @author Mikko Vihtakari
#' @seealso \code{\link{round_any}}
#' @export
#'
pretty_log <- function(x, f = round) {
  x <- round(x, 0)

  ifelse(nchar(x) == 1, round_any(x, 5),
    ifelse(nchar(x) == 2 & x < 20, round_any(x, 5),
      ifelse(nchar(x) == 2 & x < 90, round_any(x, 10),
        ifelse(nchar(x) == 2, round_any(x, 100),
          ifelse(nchar(x) == 3 & x < 500, round_any(x, 50),
            ifelse(nchar(x) == 3, round_any(x, 100),
              ifelse(nchar(x) == 4, round_any(x, 1000),
                ifelse(nchar(x) == 5, round_any(x, 10000), round_any(x, 1e5)))))))))
}


#' @title Convert direct UTM distances to kilometers
#' @description Divides \code{x} by 1000 (i.e from meters to kilometers, since polar stereographic are meters from the North Pole). Used for ggplot2 labels.
#' @param x numeric to be converted
#' @keywords internal
#' @export
#'
formatterUTMkm <- function(x){
    x/1000
}

#' @title Return function output quietly
#' @description Returns function output without printed \code{\link{cat}} messages
#' @param x function
#' @keywords internal
#' @author Hadley Wickham
#' @export

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' @title Convert ASCII letters to Norwegian letters in psv files
#' @description Convert ASCII letters to Norwegian letters in psv files
#' @param x vector
#' @keywords internal
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @export

ascii_to_nor <- function(x) {
  
  if(!any(class(x) %in% c("factor", "character"))) stop("x has to be factor or character")
  
  if(class(x) == "factor") {
    FAC <- TRUE
    levs <- levels(x)
    x <- as.character(x)
  } else {
    FAC  <- FALSE
  }
  
  x <- gsub("\xed", "i", gsub("\xc5", "Å", gsub("\xd8", "Ø", gsub("\xe6", "ø", gsub("\xe5", "å", gsub("\xf8", "ø", x))))))
  x[x == ""] <- NA
  
  if(FAC) {
    levs <- gsub("\xc5", "Å", gsub("\xd8", "Ø", gsub("\xe6", "ø", gsub("\xe5", "å", gsub("\xf8", "ø", levs)))))
    factor(x, levels = levs)
  } else {
    x
  }
}

#' @title Convert Norwegian letters to ASCII equivalents
#' @description Convert Norwegian letters to ASCII letters
#' @param x vector
#' @keywords internal
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @export

nor_to_ascii <- function(x) {
  gsub("Å", "\xc5", fixed = TRUE, gsub("Ø", "\xd8", fixed = TRUE, gsub("ø", "\xf8", fixed = TRUE, gsub("å", "\xe5", x, fixed = TRUE))))
}

## Define global variables

utils::globalVariables(c(".", "FANGSTART", "date_end", "date_start", "depth", "depth_end", "depth_start", "gear", "gear_category", "gear_id", "gear_name", "individualweight", "lat", "lat_end", "lat_start", "lon", "lon_end", "lon_start", "mass", "stationstartdate", "stationstarttime", "stationstopdate", "stationstoptime"))

