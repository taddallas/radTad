#' Get range for factor level (useful for plotting)
#'
#' @param x vector of values to cut
#' @param breaks number of breaks
#' 
#' @return range of values in that factor level for each value in x

getRange <- function(x, breaks = NULL){ 
	if(is.null(breaks)){
		x1 <- x
	}else{
	  x1 <- cut(x, breaks=breaks, include.lowest=T)
	}
  x2 <- gsub('\\(', '', as.character(x1))
  x2 <- gsub('\\]', '', as.character(x2))
  x2 <- gsub('\\)', '', as.character(x2))
  x2 <- gsub('\\[', '', as.character(x2))
  rng <- sapply(x2, function(z){
    paste(unlist(strsplit(z, ',')), collapse=' - ')
  })
  return(rng)
}






