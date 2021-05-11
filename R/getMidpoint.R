#' 
#' Get midpoint of factor level (useful for plotting)
#'
#' @param x vector of values to cut
#' @param breaks number of breaks
#' 
#' @return midpoint for each point in x

getMidpoint <- function(x, breaks = NULL){ 
	if(is.null(breaks)){
		x1 <- x
	}else{
	  x1 <- cut(x, breaks=breaks, include.lowest=T)
	}
  x2 <- gsub('\\(', '', as.character(x1))
  x2 <- gsub('\\]', '', as.character(x2))
  x2 <- gsub('\\)', '', as.character(x2))
  x2 <- gsub('\\[', '', as.character(x2))
  mids <- sapply(x2, function(z){
    mean(as.numeric(unlist(strsplit(z, ','))))
  })
  return(mids)
}
