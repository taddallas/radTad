#' 
#' Plot a linear fit within the bounds of the data, and insert fit stats into plot
#'
#' @param y a vector 
#' @param x a vector
#' @param leg.x x coordinate of legend location
#' @param leg.y y coordinate of legend location
#' @param justMod if TRUE, no segment is drawn, but the model object is returned
#' @param textCol color of line and text.
#' @param ... other graphical parameters taht will be handed to the lines function 
#'
#' @return nothing 
#'
#' @importFrom grDevices adjustcolor colorRampPalette
#' @importFrom graphics lines text
#' @importFrom stats cor lm na.omit predict
#'
#' @export

plotSegments <- function(y,x, 
  leg.x=NULL, leg.y=NULL, 
  justMod = FALSE, textCol=1, ...){
  m <- lm(y~x)
  if(justMod){
    return(m)
  }else{
    lty <- ifelse(summary(m)[[4]][2,4] < 0.05, 1, 5)
    preds <- predict(m, 
      newdata=data.frame(x=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))))
    lines(y=preds, x=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE)), 
      lty=lty, col=textCol, ...)
    b <- format(summary(m)$coefficients[2],digits=2)
    pee <- format(summary(m)$coefficients[8],digits=2)
    if(!is.null(leg.x)){
      text(leg.x, leg.y+1, xpd=TRUE, bquote(beta==.(b)~~'p'==.(pee)), 
        col=textCol)
    }
  }
}

