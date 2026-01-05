#' Mask out an area from an image tensor
#'
#' @description Mask out a certain area of a rank-3 image tensor.
#'
#' @usage mask.arr.box(tens, box.vec, pl = T)
#' mask.arr.box(tens, box.vec)
#'
#' @param tens The image tensor of the form \eqn{\large T_{m,n,p=3}} that you want to mask a part of
#'
#' @param box.vec 4-element numeric vector describing the box which will get masked
#'  * The vector has to be of the format \code{box.vec = c(x_LL, y_LL, x_UR, y_UR)}
#'  * Here x_LL and x_UR denote the Lower left and Upper right x-co-ordinates respectively
#'
#' @param pl Plots the returned image tensor when set to \code{TRUE}
#' Defaults to \code{TRUE}description
#'
#'
#' @details
#' The area within the box defined by \code{box.vec} is diminished to the lowest pixel value in the image matrix
#'
#'
#'
#' @return The returned value is always a rank-3 image tensor \eqn{\large T_{m,n,p=3}}\cr
#' It has the exact same dimensions as the \code{tens} argument \cr
#' It has the slice-2 and slice-3 matrixes untouched since they are the X-Y grid of the image
#'
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' #Call the relevant libraries
#' library(StatsChitran)
#' library(ImageMetrologyAnalysis)
#'
#' #Build a square lattice
#' X <- seq(-10, 10, by=0.1)
#' Y <- X
#' sq.tens <- sq.latt(X, Y, R.latt = 1.0, A= 1, sig=0.2)
#'
#' #Build its fourier transform and zoom
#' f.sq.tens <- fft_2D(tens = sq.tens, pl='amp')
#' f.sq.tens <- f.sq.tens[[1]] #The amplitude
#' f.sq.tens <- plot2D.zoom(arr = f.sq.tens, center = c(0,0), Del_X = 30, Del_Y = 30)
#'
#' #Plot boxes to check masking area
#' b.mat <- matrix(data = c(-1,-1,1,1), ncol = 4, byrow = T)
#' dmp <- plot2D.boxes(img.tens = f.sq.tens, box.mat = b.mat, box.thick = 0.1, box_intens = 0.25)
#'
#' #mask out the area
#' masked.img <- mask.arr.box(tens = f.sq.tens, box.vec = c(-1,-1,1,1))
#'
#'
#' @export
mask.arr.box <- function(tens, box.vec, pl = T){
  library(StatsChitran)
  ######### error check ##########################
  ######### error check ##########################

  if(is.tensor.rank3(tens) == F){
    stop('tens needs to be a rank 3 tensor of the (data, X, Y) form')
  }
  if( (length(box.vec) != 4)  || (is.numeric(box.vec) == F) ){
    stop('box.vec needs to be a numeric vector of length 4')
  }
  #Define the result tensor
  res.tens <- tens
  #Define m and n
  m <- dim(tens)[1]
  n <- dim(tens)[2]
  #Define the storage modes
  storage.mode(tens) <- 'double'
  storage.mode(res.tens) <- 'double'
  storage.mode(box.vec) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  #Call the return list
  ret.list <- .C('mask_box_c', tens = tens, res_tens = res.tens, m=m, n=n, box_vec=box.vec)
  Z <- ret.list$res_tens
  if(pl){
    plot2D.arr(Z)
    return(Z)
  }else{
    return(Z)
  }
}
