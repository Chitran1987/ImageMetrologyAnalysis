#' Integrate an entire dataset to get another dataset
#'
#' @description The function takes in equi-dimensional vectors, \eqn{\large \bar{X}} and \eqn{\large \bar{Y}} and solves the equation shown below
#' \deqn{\displaystyle \Large y_1[k] = \int_{x_0}^{x[k]}f(x)\,\mathrm{d}x + y_0 \quad | \quad y = f(x)}\cr
#' It then returns the dataframe consisting of vectors \eqn{\large \bar{X}} and \eqn{\large \bar{Y_1}}
#'
#' @usage integrate.function(X, Y, y0)
#'
#' @param X The numeric vector \eqn{\large \bar{X}}, such that \eqn{\large x[k] \in \bar{X}}
#'
#' @param Y The numeric vector \eqn{\large \bar{Y}}, such that \eqn{\large y[k] \in \bar{Y}}
#'
#'
#' @param y0 The term, \eqn{\large y_0}, acting as the constant of integration
#'
#'
#' @details
#' The function uses the Trapezoidal rule for calculating the integral. Please see \link{integrate.num}
#'
#'
#' @return The returned value is always a two-column dataframe with columns \code{X} and \code{Y}
#' \code{X} contains the values \eqn{x \in \bar{X}}
#' \code{Y} contains the values \eqn{y_1 \in \bar{Y}}
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' rm(list=ls())
#' library(ImageMetrologyAnalysis)
#' #Build the dataset
#' X <- seq(-4*pi, 4*pi, by= 0.001)
#' Y <- cos(X)
#' #Plot the function
#' plot(X, Y, 'l')
#' #Call the integrate function
#' df <- integrate.function(X, Y, y0=0)
#' #Plot the data frame
#' lines(df$X, df$Y, col = 'red')
#' #Use vertical lines as indicators of the phase shift between sine and cosine
#' abline(v=c(0, pi/2, -pi/2))
#'
#'
#' @export
integrate.function <- function(X, Y, y0){
  if(!is.numeric(X) || !is.numeric(Y) || !is.numeric(y0)){
    stop('X, Y and y0 have all got to be of type numeric')
  }
  if(length(X) != length(Y)){
    stop('lengths of the X and Y vectors have to be the same')
  }
  if(length(y0) != 1){
    stop('y0 needs to be a numeric scalar')
  }
  m <- length(X)
  res <- matrix( data = rep(0.0, 2*length(X)), ncol = 2 )
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(y0) <- 'double'
  storage.mode(res) <- 'double'
  storage.mode(m) <- 'integer'
  res.mat <- .C('integrate_function_c', X = X, Y = Y, m = m, y0 = y0, dat = res)
  df <- data.frame(res.mat$dat)
  names(df) <- c('X', 'Y')
  return(df)
}
