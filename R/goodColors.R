#' Import some color palettes
#'
#' @param index which palette do you want? (1-7)
#' @param alpha transparency channel
#'
#' @return color palette
#'
#' @importFrom grDevices rainbow
#' @export

goodColors <- function(index=1, alpha=0.5){
  one <- c('#65587f', '#f18867', '#e85f99', '#50bda1')
  two <- c('#21243d', '#ff7c7c', '#ffd082', '#88e1f2', '#9818d6')
  three <- c('#393e46', '#00adb5', '#f38181', '#eaeaea')
  four <- c('#ED6A5A', '#F4F1BB', '#9BC1BC', '#5CA4A9', '#E6EBE0')
  five <- c('#FF7561', '#A458E8', '#6ED3FF', '#05EB0A', '#FFDF3E')
  six <- c('#FF6F47', '#EBC42F', '#8EB88A', '#4D91A3', '#6214C4')
  seven <- rainbow(50)
  return(adjustcolor(list(one, two, three, four, five, six, seven)[[index]], alpha)) 
}
