#' Build a rank-3 tensor pertaining to a periodic rectagular lattice
#'
#' @description The function takes in vectors \eqn{\bar{X}}, \eqn{\bar{Y}} and lattice costants \eqn{\large \bar{R}_1}, \eqn{\large \bar{R}_2} and returns the rank-3 tensor pertaining to the lattice.\cr
#'
#'
#' @usage rect.latt(X, Y, R.latt.x, R.latt.y, A, sig, pl)
#' rect.latt(X, Y, R.latt.x, R.latt.y, A, sig)
#'
#' @param X,Y The \eqn{\large \bar{X}} and \eqn{\large \bar{Y}} vectors pertaining to the image spans\cr
#' See \code{\link{grid_2}} for usage specifics of the X and Y vectors
#'
#' @param R.latt.x,R.latt.y The lattice constants \eqn{\large \bar{R}_1} and \eqn{\large \bar{R}_2}
#'
#' @param A,sig The amplitude and standard deviation of the gaussian to be placed at each lattice site
#'
#' @param pl When set to \code{TRUE}, plots the rank-3 tensor using \code{\link{StatsChitran::plot2D.arr}}\cr
#' Defaults to \code{TRUE}
#'
#' @details
#' Creates a periodic lattice by placing gaussians at each lattice site within the \eqn{\large \bar{X}}, \eqn{\large \bar{Y}} span\cr
#' The lattice periodicity is obviously defined by \eqn{\large \bar{R}_1} and \eqn{\large \bar{R}_2}\cr
#'
#'
#'
#'
#'
#' @return The returned value is always a rank-3 tensor.\cr
#'  * The first slice, \eqn{\large T_{m,n, p = 1}}, are the lattice Z-values
#'  * The second slice, \eqn{\large T_{m,n, p = 2}}, contains the X-values of the X-Y grid
#'  * The third column, \eqn{\large T_{m,n, p = 3}}, contains the Y-values of the X-Y grid
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' rm(list = ls())
#' library(ImageMetrologyAnalysis)
#' library(StatsChitran)
#' X <- seq(-5, 5, by=0.01)
#' Y <- seq(-3, 3, by = 0.01)
#' rec.tens <- rect.latt(X, Y, R.latt.x = 0.5, R.latt.y = 0.75, A = 1, sig = 0.15)
#'
#'
#' @export
rect.latt <- function(X, Y, R.latt.x, R.latt.y, A, sig, pl = T){
  #X, Y, mx, my, R_latt_x, R_latt_y, A, sig, tens ---------> These are the inputs that the fortran subroutine needs
  #Call the libraries needed
  library(StatsChitran)
  #Define the minimum inputs needed for the fortran subroutine
  m.x <- length(X)
  m.y <- length(Y)
  tens <- array(data = 0, dim = c(m.y, m.x, 3))
  #Define the storage modes for passing to fortran
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(R.latt.x) <- 'double'
  storage.mode(R.latt.y) <- 'double'
  storage.mode(A) <- 'double'
  storage.mode(sig) <- 'double'
  storage.mode(tens) <- 'double'
  storage.mode(m.x) <- 'integer'
  storage.mode(m.y) <- 'integer'
  res.list <- .C('rect_latt_sb_c', X = X, Y = Y, mx = m.x, my = m.y, R_latt_x = R.latt.x, R_latt_y = R.latt.y, A = A, sig = sig, tens = tens)
  ret.tens <- res.list$tens
  if(pl){
    plot2D.arr(ret.tens)
  }
  return(ret.tens)

}
