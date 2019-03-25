#' Make the default color of axes and labels white (for presentations with dark background)
#'
#' @param ... additional par arguments (if any) 
#'
#' @return nothing 
#'
#'
#' @export

presFig <- function(...){
  par(fg='white', col.axis='white', col.lab='white', 
    col.main='white', col.sub='white', ...)
}
