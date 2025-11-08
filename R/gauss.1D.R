#' Gaussian in one dimension
#'
#' @description The function takes in a vector \eqn{\bar{X}} against which it returns a gaussian vector depending on other parameters, like Amplitude, mean and standard-deviation.
#'
#' @usage gauss.1D(X, A, mu, sig)
#' gauss.1D(X, mu, sig)
#' gauss.1D(X, mu)
#' gauss.1D(X)
#'
#' @param X The numeric vector which contains the \eqn{x} values against which the gaussian vector is needed.
#'
#' @param mu The term, \eqn{\mu}, denoting the mean of the gaussian.\cr
#' If not provided, the term defaults to \eqn{\mu = 0}
#'
#' @param sig The term, \eqn{\sigma}, denoting the satndard-deviation of the gaussian.\cr
#' If not provided, the term defaults to \eqn{\sigma = 1}
#'
#' @param A The term, \eqn{A}, denoting the amplitude of the gaussian.\cr
#' If not provided, the term defaults to \eqn{A=1/\sigma \sqrt{2\pi}}
#'
#' @details
#' The function returns a vector \eqn{\bar{Y}} with individual compunents \eqn{y} against the vector \eqn{\bar{X}} with components \eqn{x}\cr
#' \deqn{\large\displaystyle y = A e^{-(x-\mu)^2 / 2\sigma^2} } \cr
#' When \eqn{A}, \eqn{\mu} and \eqn{\sigma} are missing, then the returned vector \eqn{\bar{Y}} is the standard normal distribution against \eqn{\bar{X}}\cr
#' Whenever \eqn{A} is missing, then \eqn{\bar{Y}} is statistically normalized using \eqn{A=1/\sigma \sqrt{2\pi}}\cr
#' When all the arguments are provided and \eqn{A \neq 1/\sigma \sqrt{2\pi}}, then \eqn{\bar{Y}} remains non-normalized
#'
#'
#' @return The returned value is always a vector
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' #Call the relevant libraries
#' library(StatsChitran)
#' library(ImageMetrologyAnalysis)
#'
#' #build the dataset
#' X <- seq(-10, 10, 0.1)
#' Y1 <- gauss.1D(X)
#' Y2 <- gauss.1D(X, mu = 0, sig = 1.5)
#' Y3 <- gauss.1D(X, mu = 0, sig = 2.0)
#' plot(X, Y1, type='l', col = 'black')
#' lines(X, Y2, col = 'blue')
#' lines(X, Y3, col = 'red')
#'
#'
#' @export
gauss.1D <- function(X, A, mu, sig){
  if(missing(mu)){
    mu <- 0
  }
  if(missing(sig)){
    sig <- 1
  }
  if(missing(A)){
    A <- 1/(sqrt(2*pi)*sig)
  }
  x_len <- length(X)

  #define the storage modes
  storage.mode(X) <- 'double'
  storage.mode(A) <- 'double'
  storage.mode(mu) <- 'double'
  storage.mode(sig) <- 'double'
  storage.mode(x_len) <- 'integer'
  res <- .C("gauss_1D_c", x=X, x_len=x_len, y=X, A=A, x0=mu, sig=sig)
  return(res$y)
}
