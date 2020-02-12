#' Calculate the spectral radius of a network (in interaction matrix format)
#'
#' @param x interaction matrix
#' 
getSpectral <- function(x){
  rows <- nrow(x)
  cols <- ncol(x)
  tot <- rows+cols

  x2 <- matrix(0, tot, tot)
  x2[1:rows, (rows + 1):tot] <- x
  x2 <- x2 + t(x2)
  out <- Re(eigen(x2)$value[1])
  return(out)
}

