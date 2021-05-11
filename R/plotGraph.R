#' @title Plot a graph object
#'
#' @param g graph object
#' @param colz colors to use
#' @param nodeSize Size of nodes
#' @param colVec What vertex attribute should color nodes? (default = NULL)
#' @param edgeWidth edge weights e.g., E(g)$weight? (default=1)
#' 
#' @return a graph plot

plotGraph <- function(g, lay=layout_nicely(g), 
	colz=1:5, nodeSize=10, colVec=NULL, edgeWidth=1){
	if(is.null(colVec)){
		colVec <- rep(1, length(V(g)))
	}
	plot(g, layout=lay, edge.width=edgeWidth, 
		vertex.size=nodeSize, directed=FALSE,
		vertex.color=colz[colVec], vertex.label=NA)
}



