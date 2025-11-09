#' Gaussian in two dimension
#'
#' @description Builds a gaussian mesh out of two **uncorrelated** vectors \eqn{\large \bar{X}} and \eqn{\large \bar{Y}}
#'
#' @usage gauss.2D.nocorr(X, Y)
#' gauss.2D.nocorr(X, Y, mu.x, mu.y, sig.x)
#' gauss.2D.nocorr(X, Y, mu.x, sig.x, sig.y)
#' gauss.2D.nocorr(X, Y, mu.x, mu.y, sig.x, sig.y)
#' gauss.2D.nocorr(X, Y, A.x, A.y, mu.x, mu.y, sig.x, sig.y)
#'
#' @param X,Y The numeric vectors \eqn{\large \bar{X}} and \eqn{\large \bar{Y}}
#'
#'
#' @param mu.x,mu.y The term, \eqn{\mu_x} and \eqn{\mu_y}, denoting the means of the individual gaussian.\cr
#' If not provided, the term defaults to \eqn{\mu_x = 0}, \eqn{\mu_y = 0}
#'
#' @param sig.x,sig.y The terms, \eqn{\sigma_x} and \eqn{\sigma_y}, denoting the satndard-deviations of the individual gaussian.\cr
#' If not provided, the terms default to \eqn{\sigma_x = 1}, \eqn{\sigma_y = 1}
#'
#' @param A.x,A.y The terms, \eqn{A_x}, \eqn{A_y}, denoting the amplitudes of the individual gaussian.\cr
#' If not provided, the terms default to \eqn{A_x=1/\sigma_x \sqrt{2\pi}}, \eqn{A_y=1/\sigma_y \sqrt{2\pi}}
#'
#' @details
#' The function returns a tensor \eqn{\large T_{m,n,p}} where \eqn{\large T_{m,n,1}} is the gaussian mesh or the result of the gaussian function \eqn{\large f_G(x,y)}\cr
#' \deqn{\Large\displaystyle f_G(x,y) = A_x A_y e^{-\{(x-\mu_x)^2 / 2{\sigma_x}^2 + (y-\mu_y)^2 / 2{\sigma_y}^2 \}}} \cr
#' It's important to remember that the gaussians generated here are out of two uncorrelated vectors \eqn{\large \bar{X}} and \eqn{\large \bar{Y}} \cr
#' When \eqn{\large A_{x,y}}, \eqn{\large \mu_{x,y}} and \eqn{\large \sigma_{x,y}} are missing, then the returned mesh \eqn{\large T_{m,n,1}} is the standard normal distribution in two dimensions \cr
#' Whenever \eqn{\large A_x} **and** \eqn{\large A_y} are missing, then the mesh \eqn{\large T_{m,n,1}} is statistically normalized using \eqn{\large A_{x,y}=1/\sigma_{x,y} \sqrt{2\pi}}\cr
#' \eqn{\large T_{m,n,2}} and \eqn{\large T_{m,n,3}} are the meshes from the \eqn{\large \bar{X}} and \eqn{\large \bar{Y}} vectors respectively \cr
#'
#'
#'
#'
#' @return The returned value is always a rank 3 tensor \eqn{\large T_{m,n,p=3}} of the type returned in \link{StatsChitran::img_2_arr} \cr
#'  * \eqn{\large T_{m,n,1}} is the 2D gaussian mesh
#'  * \eqn{\large T_{m,n,2}} is the mesh built out of \eqn{\large \bar{X}}
#'  * \eqn{\large T_{m,n,3}} is the mesh built out of \eqn{\large \bar{Y}}
#'
#'
#'
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' rm(list = ls())
#' #Call the relevant libraries
#' library(ImageMetrologyAnalysis)
#' library(StatsChitran)
#'
#' #Build the X and Y gaussians
#' X <- seq(-8, 10, by=0.01)
#' Y <- seq(-5, 10, by=0.01)
#' tens <- gauss.2D.nocorr(X, Y, mu.x = 1.0, mu.y = 5.0, sig.x = 2.3) #Gaussian centered at (1,5)
#' tens1 <- gauss.2D.nocorr(X, Y, sig.y = 1.5) #Gaussian centered at (0,0) default
#' tens[,,1] <- tens[,,1] + tens1[,,1] #Add the two gaussians
#'
#' #Plot the array
#' plot2D.arr(tens)
#'
#'
#' @export
gauss.2D.nocorr <- function(X, Y, A.x, A.y, mu.x, mu.y, sig.x, sig.y){
  if(missing(mu.x)){
    mu.x <- 0
  }
  if(missing(mu.y)){
    mu.y <- 0
  }
  if(missing(sig.x)){
    sig.x <- 1
  }
  if(missing(sig.y)){
    sig.y <- 1
  }
  if(missing(A.x)){
    A.x <- 1/(sqrt(2*pi)*sig.x)
  }
  if(missing(A.y)){
    A.y <- 1/(sqrt(2*pi)*sig.y)
  }
  #define the lengths
  x_len <- length(X)
  y_len <- length(Y)
  #define the final tensor
  tens <- array(0.0, dim = c(y_len, x_len, 3))
  #define the storage modes
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(A.x) <- 'double'
  storage.mode(A.y) <- 'double'
  storage.mode(mu.x) <- 'double'
  storage.mode(mu.y) <- 'double'
  storage.mode(sig.x) <- 'double'
  storage.mode(sig.y) <- 'double'
  storage.mode(x_len) <- 'integer'
  storage.mode(y_len) <- 'integer'
  storage.mode(tens) <- 'double'
  #gauss_2D_nocorr_c(X, Y, x_len, y_len, Ax, Ay, x0, y0, sig_x, sig_y, tens)
  res <- .C('gauss_2D_nocorr_c', X=X, Y=Y, x_len=x_len, y_len=y_len, Ax=A.x, Ay=A.y, x0=mu.x, y0=mu.y, sig_x=sig.x, sig_y=sig.y, tens=tens)
  return(res$tens)

}
