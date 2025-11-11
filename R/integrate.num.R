#' Integrates a 1D dataset between specific bounds
#'
#' @description The function provides the closest analogue of the deinite integral shown below
#' \deqn{\large\displaystyle S = \int_{x_0}^{x_1}f(x)\,\mathrm{d}x}
#'
#' @usage integrate.num(X, Y, xmin, xmax, riemann)
#'
#' @param X The numeric vector which contains the \eqn{\large x} values or the independent variable
#'
#' @param Y The numeric vector which contains the \eqn{\large y = f(x)} values or the dependent variable
#'
#' @param xmin The lower bound of the integral, denoted as \eqn{\large x_0}
#'
#' @param xmax The upper bound of the integral, denoted as \eqn{\large x_1}
#'
#' @param riemann A logical bit which toggles between a riemann summation or a trapezoidal approximation.\cr
#' Selects a riemann integral when set (\code{TRUE}).
#'
#' @details
#' For additional details between a Trapezoidal approximation and a Riemann summation, see also \link[StatsChitran:num_integrate]{\code{num_integrate()}}
#' , \link[StatsChitran:num.integrate.riemann]{\code{num.integrate.riemann()}}.\cr
#' For very dense vectors, \eqn{\large \bar{X}} and \eqn{\large \bar{Y}},  a Riemann integral approaches a Trapezoidal in precision estimates.
#'
#'
#' @return The returned value is always a numeric scalar
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' #Call the relevant libraries
#' library(StatsChitran)
#' library(ImageMetrologyAnalysis)
#'
#' rm(list = ls())
#' X <- seq(-10, 10, by=0.005)
#' Y <- X**3
#' plot(X, Y, type = 'l')
#' integrate.num(X, Y, xmin = 0, xmax = 5, riemann = F)
#'
#'
#' @export
integrate.num <- function(X, Y, xmin, xmax, riemann){
  if(!is.numeric(X) || !is.numeric(X) || !is.numeric(xmin) || !is.numeric(xmax) ){
    stop('X, Y, xmin and xmax need to be numeric')
  }
  if(length(X) != length(Y)){
    stop('Lengths of X and Y vectors are different')
  }
  if(length(xmin) != 1 || length(xmax) != 1){
    stop('xmin and xmax should have length = 1')
  }
  if(!is.logical(riemann)){
    stop('riemann is supposed to be of type logical')
  }
  m <- length(X)
  res <- 0.0
  riemann <- as.integer(riemann)
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(xmin) <- 'double'
  storage.mode(xmax) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(riemann) <- 'integer'
  storage.mode(res) <- 'double'
  res <- .C('integrate_c', X = X, Y = Y, m = m, xmin = xmin, xmax = xmax, Riemann = riemann, int_val = res)
  return(res$int_val)
}
