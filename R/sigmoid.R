#' The simplified sigmoid function
#'
#' @description The sigmoid function in one dimension
#' @usage sigmoid <- function(X, k, cutoff)
#'
#' @param X The numeric vector which contains the \eqn{x} values against which the sigmoid vector is needed
#'
#' @param k The sharpness \eqn{k} of the sigmoid function
#'  * higher \eqn{k} implies a sharper cutoff
#'
#' @param cutoff The cutoff value \eqn{x_0} at which the sigmoid function returns a value of \eqn{1/2}
#'
#'
#' @details
#' The function returns a vector \eqn{\large \bar{Y}} with individual components \eqn{\large y} against the vector \eqn{\large \bar{X}} with component \eqn{\large x}
#' \deqn{\Large y = 1/(1 + e^{-k(x - x_0)})}
#'
#'
#'
#' @return The returned value is always a vector
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' #Import the relevant libraries
#' library(StatsChitran)
#' library(ImageMetrologyAnalysis)
#'
#' #Build the X-variable
#' X <- seq(-10, 10, by=0.01)
#' #Build the Y-variable
#' Y1 <- sigmoid(X, k=1, cutoff = 3)
#' Y2 <- sigmoid(X, k=5, cutoff = 3)
#' #Plot the results
#' plot(X, Y1, type='l', col='blue')
#' lines(X, Y2, type = 'l', col='green')
#' abline(v=3, col='red')
#'
#'
#' @export
sigmoid <- function(X, k, cutoff){
  ######error checking################################
  #
  #
  #
  #
  #
  ######error checking################################

  #####declare m and Y variables######################
  m <- length(X)
  Y <- vector(mode = 'numeric', length = m)

  ###Declare storage modes
  storage.mode(X) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(Y) <- 'double'
  storage.mode(k) <- 'double'
  storage.mode(cutoff) <- 'double'

  ###Call the variable
  res <- .C('sigmoid_c', X = X, m = m, Y = Y, k = k, cut = cutoff)
  return(res$Y)
}
