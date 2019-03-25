#' 
#' Create correlation matrix plot with Spearman's rank corr output
#'
#' @param x a matrix or data.frame containing variables of interest
#' @param colorz a single color to use for gradient shading
#' @param varNames names to use as labels (default is to use colnames of x)
#'
#' @return nothing but a plot
#' 
#' @importFrom grDevices adjustcolor colorRampPalette
#' @importFrom graphics lines text
#' @importFrom stats cor lm na.omit predict
#' @importFrom lattice panel.splom panel.fill panel.text current.panel.limits
#' @export

makeCorPlot <- function(x, colorz='dodgerblue', varNames=NULL){
  if(is.null(varNames)){
    varNames <- colnames(x)
  }  
  lattice::splom(x, 
	  lower.panel = panel.splom,
    upper.panel = function(x, y, ...) {
      panel.fill(col = colorRampPalette(c('white', 'white', colorz[1]))(9)[ round(cor(x, y, use='pairwise.complete.obs', method='spearman') * 4 + 5)])
      cpl <- current.panel.limits()
      panel.text(mean(cpl$xlim), mean(cpl$ylim), round(cor(x, y, use='pairwise.complete.obs'),2), font=2,
      cex=1.5, adj=c(0.5,-0.7))}, 
    varnames=varNames,
    main='', pch=16, col=adjustcolor(colorz[1], 0.5), 
	  tck=0.005, cex=0.75, xlab = NULL,
    varname.cex = 1, varname.font=2)
}
