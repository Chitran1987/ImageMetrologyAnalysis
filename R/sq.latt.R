#' Build a rank-3 tensor pertaining to a periodic square lattice
#'
#' @description The function takes in vectors \eqn{\bar{X}}, \eqn{\bar{Y}} and lattice costant \eqn{\large \bar{R}} and returns the rank-3 tensor pertaining to the lattice.\cr
#'
#'
#' @usage sq.latt(X, Y, R.latt, A, sig, pl)
#' sq.latt(X, Y, R.latt, A, sig)
#'
#' @param X,Y The \eqn{\large \bar{X}} and \eqn{\large \bar{Y}} vectors pertaining to the image spans\cr
#' See \code{\link{grid_2}} for usage specifics of the X and Y vectors
#'
#' @param R.latt The lattice constant \eqn{\large \bar{R}} for the square lattice
#'
#' @param A,sig The amplitude and standard deviation of the gaussian to be placed at each lattice site
#'
#' @param pl When set to \code{TRUE}, plots the rank-3 tensor using \code{\link{StatsChitran::plot2D.arr}}\cr
#' Defaults to \code{TRUE}
#'
#' @details
#' Creates a periodic lattice by placing gaussians at each lattice site within the \eqn{\large \bar{X}}, \eqn{\large \bar{Y}} span\cr
#' The lattice periodicity is obviously defined by \eqn{\large \bar{R}}\cr
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
#' X <- seq(-10, 10, by=0.05)
#' Y <- seq(-5, 5, by = 0.05)
#' sq.tens <- sq.latt(X = X, Y = Y, R.latt = 0.75, A = 1, sig = 0.1)
#'
#'
#' @export
sq.latt <- function(X, Y, R.latt, A, sig, pl = T){
  #X, Y, mx, my, R_latt, A, sig, tens ---------> These are the inputs that the fortran subroutine needs
  #Call the libraries needed
  library(StatsChitran)
  #Define the minimum inputs needed for the fortran subroutine
  m.x <- length(X)
  m.y <- length(Y)
  tens <- array(data = 0, dim = c(m.y, m.x, 3))
  #Define the storage modes for passing to fortran
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(R.latt) <- 'double'
  storage.mode(A) <- 'double'
  storage.mode(sig) <- 'double'
  storage.mode(tens) <- 'double'
  storage.mode(m.x) <- 'integer'
  storage.mode(m.y) <- 'integer'
  res.list <- .C('square_latt_sb_c', X = X, Y = Y, mx = m.x, my = m.y, R_latt = R.latt, A = A, sig = sig, tens = tens)
  ret.tens <- res.list$tens
  if(pl){
    plot2D.arr(ret.tens)
  }
  return(ret.tens)

}
