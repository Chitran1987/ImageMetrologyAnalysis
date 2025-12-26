#' The simplified sigmoid function
#'
#' @description The sigmoid plateau function in one dimension
#' @usage sigmoid.plat(X, k, left.cut, right.cut)
#'
#' @param X The numeric vector which contains the \eqn{x} values against which the sigmoid vector is needed
#'
#' @param k The sharpness \eqn{k} of the sigmoid function
#'  * higher \eqn{k} implies a sharper cutoff
#'
#' @param left.cut The cutoff value \eqn{x_1} at the left side of the sigmoid plateau
#'
#' @param right.cut The cutoff value \eqn{x_2} at the right side of the sigmoid plateau
#'
#'
#' @details
#' The function returns a vector \eqn{\large \bar{Y}} with individual components \eqn{\large y} against the vector \eqn{\large \bar{X}} with component \eqn{\large x}
#' \deqn{\Large y = \{1/(1 + e^{-k(x - x_1)})\} - \{1/(1 + e^{-k(x - x_2)})\} }
#'
#'
#'
#' @return The returned value is always a vector
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' #Call the relevant libraries
#' library(ImageMetrologyAnalysis)
#'
#' #Build the X-variable and then call the sigmoid.plat() function
#' X <- seq(-10, 10, by=0.01)
#' Y1 <- sigmoid.plat(X, k = 7, left.cut = -8, right.cut = 8)
#' Y2 <- sigmoid.plat(X, k = 2, left.cut = -8, right.cut = 8)
#'
#' #Plot the curves
#' plot(X, Y1, type = 'l', col='blue')
#' lines(X, Y2, type = 'l', col='green')
#' abline(v=c(-8,8), col='red')
#'
#'
#' @export
sigmoid.plat <- function(X, k, left.cut, right.cut){
  ###error checking
  #
  #
  #
  #
  ###error checking

  ###Declare the variables Y and m
  m <- length(X)
  Y <- vector(mode = 'numeric', length = m)
  ###Declare the storage modes
  storage.mode(m) <- 'integer'
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(k) <- 'double'
  storage.mode(left.cut) <- 'double'
  storage.mode(right.cut) <- 'double'
  ###Call the relevant fortran function
  ret.vec <- .C('sigmoid_plat_c', X = X, m = m, Y = Y, k = k, left_cut = left.cut, right_cut = right.cut)
  return(ret.vec$Y)
}
