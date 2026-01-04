#' Return image tensor after a 2D fft mapping over the input image
#'
#' @description The function takes in a rank-3 tensor of the form \eqn{\large T_{m,n,p = 3}} with some periodicity and returns a tensor after mapping over one or multiple fourier spots
#' @usage fft_2D_map(img.tens, DelX, DelY, k1st, k0, pl)
#' fft_2D_map(img.tens, DelX, DelY, k1st, k0)
#'
#' @param img.tens The tensor describing the lattice over which the mapping needs to be done
#'
#' @param DelX The X-width of the box which will be iterated over the image tensor \eqn{\large T_{m,n,p}} or \code{img.tens}
#'  * The function subsets out a box using the \link{sigmoid.2D.plat} function and then uses the \link{fft_2D} function to calculate the fourier transform
#'
#' @param DelY The Y-width of the box which will be iterated over the image tensor \eqn{\large T_{m,n,p}} or \code{img.tens}
#'   * The function subsets out a box using the \link{sigmoid.2D.plat} function and then uses the \link{fft_2D} function to calculate the fourier transform
#'
#' @param k1st The matrix defining the co-ordinates of the boxes/area in reciprocal/fourier space(k-space) around the fourier spots where the area integration will occur
#'  * The no. of rows of \code{k1st} matrix = no. of boxes needed
#'  * The no. of columns of \code{k1st} matrix should be equal to 4
#'  * Each row should be of the form \code{c(x_LL, y_LL, x_UR, y_UR)}
#'  * Here \code{x_LL, y_LL} signify the lower left corner x and y co-ordinate of the rectangular box respectively
#'  * Here \code{x_UR, y_UR} signify the upper right corner x and y co-ordinates of the rectangular box respectively
#'
#' @param k0 A vector of \code{length(k0) = 4} which designates the integration box co-ordinates around the \eqn{(0,0)} spot
#'  * The vector should be of the form \code{c(x_LL, y_LL, x_UR, y_UR)}
#'  * The vector should be like an individual row of input parameter \code{k1st}
#'
#' @param pl Plots \code{img.tens} with the boxes drawn on them if set to \code{TRUE}.\cr
#' Defaults to \code{TRUE}
#'
#'
#' @details
#' The order of operations of the function \code{fft_2D_map()} is described below
#'  * The function selects the first pixel in the image matrix \eqn{\large T_{m,n,p=1}} or \code{img.tens[1,1,1]}
#'  * The function subsets a part of \code{img.tens} around that pixel using the \link{sigmoid.2D.plat} function and the arguments \code{DelX}, \code{DelY}
#'  * The function then performs a fourier transform on the subset image described in the previous step using the \link{fft_2D} function
#'  * The function then integrates over the areas of interest/fourier spots defined by the argument \code{k1st}. Call this value \eqn{\large f_1}
#'  * The function then integrates over the area around \eqn{(0,0)} spot defined by the argument \code{k0}. Call this value \eqn{\large f_0}
#'  * The function then assigns the value \eqn{\large f_1/f_0} to that pixel of interest
#'  * The function then moves on to the next pixel of interest \code{img.tens[2,1,1]} and repeats the operations in the order described
#'
#'
#'
#' @return The returned value is always a rank-3 tensor.\cr
#'  * The X and Y slices of the tensor \eqn{\large T_{m, n, p=2}} and \eqn{\large T_{m,n, p=3}} respectively stay the same
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' #call the relevant libraries
#' library(StatsChitran)
#' library(ImageMetrologyAnalysis)
#'
#' #draw the lattice in question
#' X <- seq(-10, 10, by= 0.05)
#' Y <- X
#' latt_sq <- sq.latt(X, Y, R.latt = 1.0, A=1, sig = 0.2)
#'
#' #build the fourier transform of the lattice in question
#' f.latt_sq <- fft_2D(latt_sq, sampling.del = 0.1, pl='amp')
#' #select the amplitude spectrum
#' f.latt_amp <- f.latt_sq[[1]]
#' #zoom into the area of interest
#' f.latt_amp_interest <- plot2D.zoom(f.latt_amp, center = c(0,0), Del_X = 20, Del_Y = 20)
#'
#'
#' ###Estimate the co-ordinates for the points
#' plot2D.arr(f.latt_amp_interest)
#' df <- lin.prof.h(f.latt_amp_interest, h.val = -0.25)
#' df <- lin.prof.v(f.latt_amp_interest, v.val = 6.5)
#'
#' ###Build the matrix box.mat as b.mat and then check it
#' b.vec <- c(5.0, -1.5, 8.0, 1.5, -1.5, 4.5, 1.5, 7.5 )
#' b.mat <- matrix(data = b.vec, byrow = T, ncol = 4)
#' ###Plot the boxes
#' dat <- plot2D.boxes(f.latt_amp_interest, b.mat, box.thick = 0.025, box_intens = 0.2)
#'
#'
#' @export
fft_2D_map <- function(img.tens, DelX, DelY, k1st, k0, pl=T){
  library(StatsChitran)
  #########error checking#############################
  #
  #
  #
  #
  #
  #
  #########error checking#############################
  #Define the return tensor
  ret.tens <- rep(0, dim(img.tens)[1]*dim(img.tens)[2]*dim(img.tens)[3])
  ret.tens <- array(data = ret.tens, dim = dim(img.tens))
  m <- dim(img.tens)[1]
  n <- dim(img.tens)[2]
  n_spots <- dim(k1st)[1]
  #Define the storage modes
  storage.mode(img.tens) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(ret.tens) <- 'double'
  storage.mode(DelX) <- 'double'
  storage.mode(DelY) <- 'double'
  storage.mode(k1st) <- 'double'
  storage.mode(n_spots) <- 'integer'
  storage.mode(k0) <- 'double'
  #Call the fft_2D_map() function'
  fortran.res.list <- .C('fft_2D_map_c', img_tens = img.tens, m = m, n=n, res_tens = ret.tens , Xspan=DelX, Yspan=DelY, k1st=k1st, n_spots = n_spots, k0=k0)
  ret.val <- fortran.res.list$res_tens
  if(pl){
    plot2D.arr(arr=ret.val)
  }
  return(ret.val)
}
