#' 
#' Plot a linear fit within the bounds of the data, and insert fit stats into plot
#'
#' @param y a vector 
#' @param x a vector
#' @param	forceIntercept (default = NULL) what is the intercept value to use (useful 
#'   for relationships that need to be forced through origin
#' @param legPos position of the legend (which is the slope and p-value; default=NULL)
#'   other values are "topleft", "bottomleft", "topright", "bottomright".
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
	forceIntercept=NULL, legPos=NULL,
  justMod = FALSE, textCol=1, ...){
	if(is.null(forceIntercept)){
	  m <- lm(y~x)
	}else{
	  m <- lm(y ~ 0 + x)
    preds <- predict(m, 
      newdata=data.frame(x=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))))
    lines(y=preds, x=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE)), 
      col=textCol, ...)
		return(m)
	}
  if(justMod){
    return(m)
  }else{
    lty <- ifelse(summary(m)[[4]][2,4] < 0.05, 1, 5)
    preds <- predict(m, 
      newdata=data.frame(x=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))))
    lines(y=preds, x=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE)), 
      lty=lty, col=textCol, ...)
    b <- format(summary(m)$coefficients[2],digits=2)
    pee <- format(summary(m)$coefficients[8],digits=4)
    if(pee < 0.0001){
      p <- '< 0.0001'
    }else{
      p <- paste('= ', pee, sep='')
    }
    if(!is.null(legPos)){
      legend(legPos, xpd=TRUE, legend=bquote(beta==.(b)~';'~'p'~.(p)), 
        col=textCol, bty='n')
    }
  }
}

