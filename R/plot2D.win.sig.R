#' Sigmoid based windowing of an image Tensor
#'
#' @description Window a certain area of a rank-3 image tensor using the sigmoid function.
#'
#' @usage plot2D.win.sig(tens, center, k, Xspan, Yspan, pl)
#' plot2D.win.sig(tens, center, k, Xspan, Yspan)
#'
#' @param tens The image tensor of the form \eqn{\large T_{m,n,p=3}} that you want to window a part of
#'
#' @param center The co-ordinates of the center of the window
#'  * Must be a 2-element numeric vector of the form \code{center = c(Xc, Yc)}
#'  * Here \code{Xc} and \code{Yc} are the X and Y co-ordinates of the center
#'
#' @param k smoothness/sharpness parameter of the sigmoid function being used for the windowing
#'  * Higher is sharper and lower is smoother/softer
#'  * Please see \link{sigmoid.2D.plat}, \link{sigmoid.plat}
#'
#' @param Xspan,Yspan The spans around the center of the window. Numeric scalars.
#'
#' @param pl Boolean bit. Permission to plot the resulting window
#' Defaults to \code{TRUE}
#'
#'
#' @details
#' Please see \link{sigmoid}, \link{sigmoid.plat}, \link{sigmoid.2D.plat} for a better understanding\cr
#' Useful for windowing areas of interest in lattices for fourier/spectral analysis
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
#' #Load the lattice
#' data("latt_reconstr_1axis", package = "ImageMetrologyAnalysis")
#' latt <- latt_reconstr_1axis
#' plot2D.arr(latt)
#' latt.zoom <- plot2D.win.sig(tens = latt, center = c(5,0), k = 10, Xspan = 8, Yspan = 2)
#'
#'
#' @export
plot2D.win.sig <- function(tens, center, k, Xspan, Yspan, pl = T){
  library(StatsChitran)
  #error checks
  if(!is.tensor.rank3(tens)){
    stop('argument should be a rank-3 image tensor')
  }
  if( (length(center) != 2 ) || (!is.numeric(center))){
    stop('center should be a numeric vector of length = 2')
  }
  if( (!is.numeric(k)) || (!is.numeric(Xspan)) || (!is.numeric(Yspan))){
    stop('arguments k, Xspan and Yspan should all be of numeric type')
  }
  if( (length(k) != 1) || (length(Xspan) != 1) || (length(Yspan) != 1)){
    stop('Arguments k, Xspan and Yspan all need to be scalars')
  }
  #error checks
  #Define m and n
  m <- dim(tens)[1]
  n <- dim(tens)[2]

  #Define res.tens
  res.tens <- tens

  #Define the storage modes
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(res.tens) <- 'double'
  storage.mode(tens) <- 'double'
  storage.mode(center) <- 'double'
  storage.mode(k) <- 'double'
  storage.mode(Xspan) <- 'double'
  storage.mode(Yspan) <- 'double'

  #Call the function
  for.res <- .C('window_sigmoid_c',tens = tens, res_tens = res.tens, m = m, n = n, cent = center, k = k, Xspan = Xspan, Yspan = Yspan)
  Z <- for.res$res_tens
  if(pl){
    plot2D.arr(Z)
    return(Z)
  }else{
    return(Z)
  }
}
