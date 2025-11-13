#' Fourier Transform in one dimension
#'
#' @description The function takes in vectors \eqn{\bar{X}} and \eqn{\bar{Y}} and returns the Fourier Transfrom of the data
#'
#' @usage fft_1D(X, Y)
#'
#' @param X The X column of the 1-D dataset
#'
#' @param Y The Y column of the 1-D dataset
#'
#'
#' @details
#' Solves for the DFT, \eqn{\large X_k}, using Fortran's Fast FFTW3 backend [FFTW3]<https://www.fftw.org/>. \cr
#' \deqn{\displaystyle \large X_k = \sum_{n=0}^{N-1}x_n e^{-2\pi i k n/N}}
#' Calculates the frequency vector, \eqn{\large \omega_k} using the sampling of the \eqn{\large \bar{X}} variable.\cr
#' Here, \eqn{ \large \omega_0 \leq \omega_k \leq \omega_s} where \eqn{\large \omega_0} and \eqn{\large \omega_s} are the base and the sampling frequencies.\cr
#'
#'
#'
#' @return The returned value is always a Three column dataframe .\cr
#'  * The first column contain \eqn{\large \omega_k}
#'  * The second column contains the amplitude of the complex \eqn{\large X_k}, \eqn{\large ||X_k||}
#'  * The third column contains the phase vector of the complex \eqn{\large X_k}
#'
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
fft_1D <- function(X, Y){
  if( !is.numeric(X) || !is.numeric(Y)){
    stop('X and Y both have to be numeric types')
  }
  if( length(X) != length(Y)){
    stop('X and Y both need to have equal lengths')
  }
  m <- length(X)
  res <- rep(0, 4*length(X)+4)
  res <- array( data = res, dim = c(length(X)+1, 2, 2) )
  storage.mode(X) <- "double"
  storage.mode(Y) <- "double"
  storage.mode(res) <- "double"
  storage.mode(m) <- "integer"

  ret_var <- .C("fft_1D_c", X = X, Y = Y, m = m, res = res)
  ret_var <- ret_var$res
  df <- data.frame( freq = ret_var[,1,1] , Ampl = ret_var[,2,1] , phase = ret_var[,2,2])
  return(df)
}
