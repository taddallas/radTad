#' 
#' Calculate the area formed by a bunch of points
#'
#' @param x vector of x values
#' @param y vector of y values
#' @param convexHull (boolean) should the area be a convex hull?
#' 
#' @importFrom sp Polygon

getArea <- function(x,y, convexHull=TRUE){
  rem <- which(is.na(x) | is.na(y))
  if(any(rem)){
    x <- x[-rem]
    y <- y[-rem]
  }
  if(convexHull){
    tmp <- chull(x,y)
    x <- x[tmp]
    y <- y[tmp]
  }
  Polygon(cbind(c(x, x[1]), c(y, y[1])))@area
}


