#' 
#' Make a dummy variable from a categorical or factor variable
#'
#' @param x a vector of type character or factor 
#'
#' @return a binary matrix containing dummy variables
#'
#' @export
#'


makeDummy <- function(x){
	x <- as.matrix(x)
	out <- matrix(1, ncol=1, nrow=nrow(x))
	for(i in 1:ncol(x)){
		levs <- unique(na.omit(x[,i]))
		ret <- matrix(0, ncol=length(levs), nrow=nrow(x))
		colnames(ret) <- levs
		for(j in 1:length(levs)){
			ret[,j] <- as.numeric(x[,i]==levs[j])
		}
		out <- cbind(out, ret)
	}
	return(out[,-1])
}
