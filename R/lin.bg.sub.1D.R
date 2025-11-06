#' Linear background subtraction
#'
#' @description Subtracts a linear background from line profiles.\cr
#' Based on the input dataset from which the background is to be subtracted and...\cr
#' the windows over which the background is to be calculated
#'
#'@usage lin.bg.sub.1D(dat, win, min.zero)
#'lin.bg.sub.1D(dat, win)
#'
#' @param dat The dataset from which the background is to be subtracted. \cr
#' The format for the entry is shown below
#' * Must be a two column numeric dataframe
#' * First column must be the X values and the 2nd column the Y values
#'
#' @param win The windows/subsets w.r.t X over which the background is to be calculated for subtraction. \cr
#' The format for the entry is shown below
#' * Must be a two column numeric dataframe
#' * Each row consists of the minimum(1st column) and maximum(2nd column) X-values of a two column dataframe
#' * Each minimum value should be less than each max value in the same row. Else an error is thrown.
#' * Please see \link{Examples}
#'
#' @param min.zero A boolean bit which when set to \code{TRUE} ensures that the minimum value of the returned dataframe's Y values will be clamped at zero.\cr
#'  Defaults to \code{TRUE}
#'
#'
#' @return A Two column dataframe similar in shape, size, topology to the \code{dat} dataframe
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' #Call the relevant libraries
#' library(StatsChitran)
#' library(ImageMetrologyAnalysis)
#'
#' #build the dataset
#' X <- seq(-10, 10, 0.1)
#' Y <- gauss(X, sig = 2, probability = T)
#' Y <- Y + rnorm(length(X), sd=0.005)
#'
#' #add bg
#' Y <- Y + 0.02*X
#' plot(X, Y)
#' #figure out the windows over which to calculate the background
#' ##window 1
#' abline(v=c(-10, -5), col='red')
#' ##window 2
#' abline(v=c(5, 10), col='red')
#' ## build the win dataframe
#' win <- matrix(c(-10, -5, 5, 10), byrow = T, nrow = 2)
#' win <- as.data.frame(win)
#'
#' #Call the lin.bg.sub.1D() function
#' bg.sub.dat <- lin.bg.sub.1D(dat = data.frame(X, Y), win = win, min.zero = T )
#' plot(bg.sub.dat$X, bg.sub.dat$Y)
#'
#'
#' @export
lin.bg.sub.1D <- function(dat, win, min.zero){
  ##error checking
  #
  #
  #
  #
  #ensure that min.zero is a boolean
  #
  #
  #error checking
  dat <- as.matrix(dat)
  win <- as.matrix(win)
  min.zero <- as.integer(min.zero)
  storage.mode(dat) <- "double"
  storage.mode(win) <- "double"
  storage.mode(min.zero) <- 'integer'
  dat.m = dim(dat)[1]
  dat.n = dim(dat)[2]
  win.m = dim(win)[1]
  win.n = dim(win)[2]
  #compute sizes for res_m and res_n
  #don't need to compute they have the exact same size as the input matrix dat
  res <- .C('lin_bg_sub_1D_c', dat=dat, dat_m=dat.m, dat_n=dat.n, win=win, win_m=win.m, win_n=win.n, res=dat, min_zero=min.zero)
  res <- as.data.frame(res$res)
  names(res) <- c('X', 'Y')
  return(res)
}
