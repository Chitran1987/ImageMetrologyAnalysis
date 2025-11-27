#' Returns a 2D X-Y grid/rank-3 tensor
#'
#' @description The function takes in two vectors \eqn{\large \bar{X}} and \eqn{\large \bar{Y}} and returns a rank-3 tensor \eqn{\large T_{m,n,p=2}}
#'
#' @usage grid_2(X, Y)
#'
#'
#' @param X The vector \eqn{\bar{X}}, or \code{X}.
#'
#' @param Y The vector \eqn{\bar{Y}}, or \code{Y}.
#'
#'
#' @details
#' Creates a rank-3 tensor \eqn{\large T_{m,n,p=2}} extablishing the X-Y grid
#' \enumerate{
#' \item The first slice \eqn{\large T_{m,n,1}} is the X-values of the XY-grid
#' \item The second slice \eqn{\large T_{m,n,2}} is the Y-values of the XY-grid
#' }
#'
#'
#'
#' @return The returned value is always a rank-3 tensor of the form checked by \code{\link{is.tensor.rank3}}
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' library(ImageMetrologyAnalysis)
#' library(StatsChitran)
#' X <- seq(1, 5)
#' Y <- seq(-2, 2)
#' grid.tens <- grid_2(X, Y)
#' grid.tens[,,1] #The X-matrix
#' grid.tens[,,2] #The Y-matrix
#'
#'
#' @export
grid_2 <- function(X, Y){
  #Error checking
  if(!is.numeric(X) || !is.numeric(Y)){
    stop("X and Y both should be of type numeric")
  }
  if(!is.vector(X) || !is.vector(Y)){
    stop("X and Y should both be numeric vectors")
  }
  #Introduce the arguments for grid_2_c subroutine
  m.x <- length(X)
  m.y <- length(Y)
  tens <- array(data = 0.0, dim = c(m.y, m.x,2))
  #storage mode for arguments
  storage.mode(X) <- "double"
  storage.mode(Y) <- "double"
  storage.mode(tens) <- "double"
  storage.mode(m.x) <- "integer"
  storage.mode(m.y) <- "integer"
  #Call the function
  res <- .C('grid_2_c', X = X, Y = Y, mx = m.x, my = m.y, tens = tens)
  return(res$tens)
}
