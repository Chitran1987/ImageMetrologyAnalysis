#' Returns a tensor from a function
#'
#' @description The function takes in a funtion \eqn{f(\bar{X}, \bar{Y})} and two vectors \eqn{\bar{X}}, \eqn{\bar{Y}} and returns a rank-3 tensor of the form \code{\link{is.tensor.rank3}}
#'
#' @usage func2D(f, X, Y)
#'
#' @param f The function \eqn{f(\bar{X}, \bar{Y})} with caveats\cr
#' \enumerate{
#' \item The function \eqn{f(\bar{X}, \bar{Y})} should have two arguments of the form \code{f(X, Y)}
#' \item The arguments should be named \code{X} and \code{Y}
#' }
#'
#' @param X The vector \eqn{\bar{X}}, or \code{X}, used as an argument in \eqn{f(\bar{X}, \bar{Y})}, or \code{f(X, Y)}
#'
#' @param Y The vector \eqn{\bar{Y}}, or \code{Y}, used as an argument in \eqn{f(\bar{X}, \bar{Y})} or \code{f(X, Y)}
#'
#'
#' @details
#' Creates a rank-3 tensor out of a 2D funtion \code{f(X, Y)} and vectors \code{X} and \code{Y}
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
#' X <- seq(-10, 10, by = 0.01)
#' Y <- seq(-8, 8, by = 0.01)
#' f <- function(X,Y){
#'   return(sin(3*X+4*Y) + cos(-4*X + 3*Y))
#' }
#' tens <- func2D(f, X, Y)
#' plot2D.arr(tens)
#'
#'
#' @export
func2D <- function(f, X, Y){
  ###error checking###################################
  if(!is.function(f)){
    stop('Argument f needs to be a function')
  }
  if(length(formals(f)) != 2){
    stop('Argument f neds to be a function with exactly two arguments')
  }
  #Check the names of the arguments
  args <- names(formals(f))
  if(!identical(args, c("X", "Y"))){
    stop('f needs to be a function whose arguments are exactly called "X" and "Y" ')
  }
  if( !is.numeric(X) || (length(dim(X)) != 1) ){
    stop('X needs to be a numeric vector')
  }
  if( !is.numeric(Y) || (length(dim(Y)) != 1)){
    stop('Y needs to be a numeric vector')
  }
  ###eroor checking####################################
  XY <- grid_2(X, Y)
  func.mat <- f(X = XY[,,1], Y = XY[,,2])
  tens <- array(data=NA, dim = c(length(Y), length(X), 3))
  tens[,,1] <- func.mat
  tens[,,2:3] <- XY
  return(tens)
}
