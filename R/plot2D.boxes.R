#' Plot 2D boxes in 2-dimensions
#'
#' @description The function takes in a rank-3 tensor of the form \eqn{\large T_{m,n,p = 3}} and returns the tensor with boxes drawn around specific co-ordinates
#' @usage plot2D.boxes(img.tens, box.mat, box.thick, box_intens, pl)
#' plot2D.boxes(img.tens, box.mat, box.thick, box_intens)
#'
#' @param img.tens The tensor describing the lattice over which the boxes need to be drawn
#'
#' @param box.mat The matrix defining the co-ordinaes of the boxes that have to be drawn
#'  * The no. of rows = no. of boxes needed
#'  * The no. of columns should be equal to 4
#'  * Each row should be of the form \code{c(x_LL, y_LL, x_UR, y_UR)}
#'  * Here \code{x_LL, y_LL} signify the lower left corner x and y co-ordinate of the rectangular box respectively
#'  * Here \code{x_UR, y_UR} signify the upper right corner x and y co-ordinates of the rectangular box respectively
#'
#' @param box.thick The thickness of the box w.r.t the scale of the x and y co-ordinate respectively
#' @param box_intens The intensity factor which decides the color of the box in grayscale
#'  * An intensity factor of \code{1.0} implies a grayscale value equal to the highest value residing in \code{img.tens}
#' @param pl Plots \code{img.tens} with the boxes drawn on them if set to \code{TRUE}.\cr
#' Defaults to \code{TRUE}
#'
#'
#'
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
plot2D.boxes <- function(img.tens, box.mat, box.thick, box_intens, pl=T){
  #Call the relevant libraries
  library(StatsChitran)
  #Error Handling###################
  #
  #
  #
  #
  #Error Handling###################

  #Define the tensor dimensions
  m = dim(img.tens)[1]
  n = dim(img.tens)[2]
  n.boxes = dim(box.mat)[1]
  res.tens = array(data = 0, dim=dim(img.tens))
  #Define the storage modes
  storage.mode(img.tens) <- 'double'
  storage.mode(box.mat) <- 'double'
  storage.mode(res.tens) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(n.boxes) <- 'integer'
  storage.mode(box.thick) <- 'double'
  storage.mode(box_intens) <- 'double'

  #Call the function
  fortran.res <- .C('plot_boxes_c', img_tens = img.tens, m = m, n = n, box_mat = box.mat, n_boxes = n.boxes, res_tens = res.tens, box_thick = box.thick, box_if = box_intens)
  #Return the result
  if(pl){
    plot2D.arr(fortran.res$res_tens)
    return(fortran.res$res_tens)
  }else{
    return(fortran.res$res_tens)
  }




}
