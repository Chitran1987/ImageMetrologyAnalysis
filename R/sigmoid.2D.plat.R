#' The sigmoid.2D.plat function
#'
#' @description The sigmoid plateau function in two dimensions
#' @usage sigmoid.2D.plat(X, Y, k, x.left, x.right, y.left, y.right, pl)
#' sigmoid.2D.plat(X, Y, k, x.left, x.right, y.left, y.right)
#'
#' @param X,Y The numeric vectors which create the XY grid
#'
#'
#' @param k The sharpness \eqn{k} of the sigmoid function
#'  * higher \eqn{k} implies a sharper cutoff
#'
#' @param x.left,x.right,y.left,y.right The cutoff values along the X and Y directions
#'
#' @param pl Plots the 2D sigmoid plateau when set to \code{TRUE}\cr
#' Defaults to \code{TRUE}
#'
#'
#' @details
#' The function returns a numeric matrix \code{Z(n,m)}\cr
#' \code{n = size(Y)}\cr
#' \code{m = size(X)}\cr
#' The matrix is built as a product of sigmoid plateaus running along the orthogonal X and Y directions\cr
#'
#'
#'
#' @return The returned value is always a numeric matrix
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' #Call the relevant libraries
#' library(ImageMetrologyAnalysis)
#' library(StatsChitran)
#' #Create the sequences
#' X <- seq(-10, 10, by= 0.01)
#' Y <- seq(-7, 7, by=0.01)
#' #Create the plateau matrix and plot it
#' Z <- sigmoid.2D.plat(X, Y, k=3, x.left = -8, x.right = 8, y.left = -3, y.right = 3)
#'
#'
#' @export
sigmoid.2D.plat <- function(X, Y, k, x.left, x.right, y.left, y.right, pl = T){
  library(StatsChitran)
  ###error##########################################
  #
  #
  #
  #
  ###error##########################################

  ###Declare m, n and Z#############################
  m <- length(X)
  n <- length(Y)
  Z <- matrix(data = rep(0, n*m), nrow = n)

  ###Declare storage modes##########################
  ###X, m, Y, n, Z, k, x_left, y_left, x_right, y_right
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(Z) <- 'double'
  storage.mode(k) <- 'double'
  storage.mode(x.left) <- 'double'
  storage.mode(y.left) <- 'double'
  storage.mode(x.right) <- 'double'
  storage.mode(y.right) <- 'double'
  fortran.res <- .C('sigmoid_2D_c',X = X, m = m, Y = Y, n = n, Z = Z, k = k, x_left = x.left, y_left = y.left, x_right = x.right, y_right = y.right)
  if(pl){
    plot2D.mat(X, Y, fortran.res$Z)
    return(fortran.res$Z)
  }
  return(fortran.res$Z)
}
