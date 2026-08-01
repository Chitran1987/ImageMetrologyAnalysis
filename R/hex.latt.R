#' Build a rank-3 tensor pertaining to a periodic hexagonal lattice
#'
#' @description
#' The function takes in vectors \eqn{\bar{X}}, \eqn{\bar{Y}} and the
#' hexagonal lattice constant \eqn{\large R_{\mathrm{latt}}}, and returns
#' the rank-3 tensor pertaining to the lattice.\cr
#'
#' @usage
#' hex.latt(X, Y, R.latt, A, sig, pl)
#' hex.latt(X, Y, R.latt, A, sig)
#'
#' @param X,Y The \eqn{\large \bar{X}} and \eqn{\large \bar{Y}} vectors
#' pertaining to the image spans.\cr
#' See \code{\link{grid_2}} for usage specifics of the X and Y vectors.
#'
#' @param R.latt The lattice constant \eqn{\large R_{\mathrm{latt}}}
#' defining the periodicity of the hexagonal lattice.
#'
#' @param A,sig The amplitude and standard deviation of the Gaussian
#' placed at each lattice site.
#'
#' @param pl When set to \code{TRUE}, plots the rank-3 tensor using
#' \code{\link[StatsChitran]{plot2D.arr}}.\cr
#' Defaults to \code{TRUE}.
#'
#' @details
#' Creates a periodic hexagonal lattice by placing Gaussians at each
#' lattice site within the \eqn{\large \bar{X}}, \eqn{\large \bar{Y}} span.\cr
#' The lattice periodicity is defined by the lattice constant
#' \eqn{\large R_{\mathrm{latt}}}.\cr
#'
#' @return
#' The returned value is always a rank-3 tensor.\cr
#'
#' * The first slice, \eqn{\large T_{m,n,p=1}}, contains the lattice
#'   Z-values.
#' * The second slice, \eqn{\large T_{m,n,p=2}}, contains the X-values
#'   of the X-Y grid.
#' * The third slice, \eqn{\large T_{m,n,p=3}}, contains the Y-values
#'   of the X-Y grid.
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' rm(list = ls())
#' library(ImageMetrologyAnalysis)
#' library(StatsChitran)
#'
#' X <- seq(0, 5, by = 0.01)
#' Y <- seq(0, 5, by = 0.01)
#'
#' hex.tens <- hex.latt(
#'   X,
#'   Y,
#'   R.latt = 0.5,
#'   A = 1,
#'   sig = 0.15
#' )
#'
#' @export
hex.latt <- function(X, Y, R.latt, A, sig, pl = TRUE) {

  # X, Y, mx, my, R_latt, A, sig and tens are the inputs required
  # by the Fortran subroutine.

  # Call the required library.
  library(StatsChitran)

  # Define the dimensions and output tensor required by the
  # Fortran subroutine.
  m.x <- length(X)
  m.y <- length(Y)

  tens <- array(
    data = 0,
    dim = c(m.y, m.x, 3)
  )

  # Define the storage modes for passing the variables to Fortran.
  storage.mode(X) <- "double"
  storage.mode(Y) <- "double"
  storage.mode(R.latt) <- "double"
  storage.mode(A) <- "double"
  storage.mode(sig) <- "double"
  storage.mode(tens) <- "double"
  storage.mode(m.x) <- "integer"
  storage.mode(m.y) <- "integer"

  # Call the Fortran subroutine.
  res.list <- .C(
    "hex_latt_sb_c",
    X = X,
    Y = Y,
    R_latt = R.latt,
    A = A,
    sig = sig,
    tens = tens,
    mx = m.x,
    my = m.y
  )

  ret.tens <- res.list$tens

  if (pl) {
    plot2D.arr(ret.tens)
  }

  return(ret.tens)
}

