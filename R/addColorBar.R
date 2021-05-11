#' 
#' Add a color bar (useful for heatmaps or maps)
#'
#' @param x x-position where to plot the color ramp
#' @param y y-position where to plot the color ramp
#' @param z vector of values for the color ramp (doesn't have to be evenly space sequence)
#'
#' @return nothing 
#'
#' @importFrom fields add.image
#'
#' @export
addColorBar <- function(x,y,z, color=rainbow(100)){
  g <- seq(min(z), max(z), length.out=50)
  fields::add.image(xpos=x, ypos=y,
    image.width=0.95, 
    image.height=0.05, 
    z=t(as.matrix(g)),
    col=color)
}



