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
#' \donotrun{
#' #call the relevant libraries
#' library(StatsChitran)
#' library(ImageMetrologyAnalysis)
#'
#' #Load the dataset and plot it
#' latt <- latt_reconstr_1axis_lite
#' plot2D.arr(latt)
#' f_latt <- fft_2D(tens = latt, pl='amp')
#' f_latt <- f_latt[[1]]
#' f_latt_zoom <- plot2D.zoom(f_latt, center = c(0,0), Del_X = 40, Del_Y = 40)
#'
#' #Mask out the (0,0) spot
#' #Check the boxes for the masking
#' box_mat <- c(-1,-1,1,1)
#' box_mat <- matrix(data = box_mat, ncol = 4)
#' dmp <- plot2D.boxes(img.tens = f_latt_zoom, box.mat = box_mat, box.thick = mean(diff(f_latt_zoom[1,,2])), box_intens = 0.25)
#'
#'
#' #Create the mask
#' mask_x <- f_latt_zoom[,,2] >= -1 & f_latt_zoom[,,2] <= 1
#' mask_y <- f_latt_zoom[,,3] >= -1 & f_latt_zoom[,,3] <= 1
#' mask <- mask_x & mask_y
#' mask <- !mask
#' latt_int <- arr.mask(arr = f_latt_zoom, mask = mask)
#' plot2D.arr(latt_int)
#'
#' #Create the k1st matrix before calling the fft_2D() function
#' box_mat <- c(14,-2,17,2,-17,-2,-14,2)
#' box_mat <- matrix(data = box_mat, nrow = 2, byrow = T)
#' dmp <- plot2D.boxes(latt_int, box.mat = box_mat, box.thick = 0.05, box_intens = 0.25)
#'
#' #Call the fft_2D_map() function
#' plot2D.arr(latt)
#' img.map <- fft_2D_map(img.tens = latt, DelX = 2.0, DelY = 2.0, k1st = box_mat, k0 = c(-1,-1,1,1))
#' }
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
