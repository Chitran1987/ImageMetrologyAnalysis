#' Fourier Transform in two dimensions
#'
#' @description The function takes in a rank-3 tensor of the form \eqn{\large T_{m,n,p = 3}} and returns the Fourier Lattice of the tensor
#' @usage fft_2D(tens, sampling.del, pl)
#' fft_2D(tens, sampling.del)
#' fft_2D(tens)
#'
#' @param tens The tensor describing the lattice for which the DFT is needed
#'
#' @param sampling.del The allowed relative sampling error difference along the X and Y vectors w.r.t the \code{tens} argument\cr
#'  * For reliably displaying the 2D DFT, the sampling frequencies along X and Y need to be the same \cr
#'  * If the sampling error, \code{sampling.del} \eqn{> (|\Delta X| - |\Delta Y|)/|\Delta X|}, then an error is thrown \cr
#'  * Here, \eqn{\Delta X = } \code{mean(diff(X))} and \eqn{\Delta Y = } \code{mean(diff(Y))}
#'  * \code{sampling.del} defaults to \eqn{0.1}
#'
#' @param pl Plots either the amplitude spectrum, the phase spectrum or nothing.\cr
#' The accepted inputs are \code{pl = 'amp'}, \code{pl = 'phase'} or \code{pl = 'none'}\cr
#'
#'
#' @details
#' Solves for the 2D DFT, \eqn{\large X_{k,l}}, using Fortran's Fast FFTW3 backend <https://www.fftw.org/>. \cr
#' \deqn{\displaystyle \Large X_{k,l} = \sum_{m=0}^{M-1}\sum_{n=0}^{N-1}x_{m,n} e^{-2\pi i (k n/N + l m/M)}}
#' Zero pads the dataset to make \eqn{\large M = N} before DFT computation.\cr
#' Calculates the frequency vector, \eqn{\large \omega_k} using the sampling of the \eqn{\large \bar{X}} or \eqn{\large \bar{Y}} variables from the \code{tens} argument, sice both are forced to be roughly similar.\cr
#' Here, \eqn{ \large \omega_0 \leq \omega_k \leq \omega_s} where \eqn{\large \omega_0} and \eqn{\large \omega_s} are the base and the sampling frequencies.\cr
#' Zero padding with \eqn{\large M = N} forces \eqn{\large \omega_0} to be the same for both fourier frquency axes.\cr
#' While \eqn{\large \Delta X \sim \Delta Y} forces \eqn{\large \omega_s} to be the same for both fourier frequency axes.\cr
#'
#'
#'
#' @return The returned value is always a Two element list.\cr
#'  * The first element contains the amplitude tensor
#'  * The second element contains the phase tensor
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' rm(list = ls())
#' #Call the relevant libraries
#' library(ImageMetrologyAnalysis)
#' library(StatsChitran)
#' #Build X and Y sequences
#' X <- seq(-4, 4, by=0.1)
#' Y <- seq(-5, 5, by = 0.1)
#' #Build the rectangular lattice
#' rec.tens <- rect.latt(X, Y, R.latt.x = 0.5, R.latt.y = 0.8, A = 1, sig = 0.15)
#' #Call the ffts and plot them
#' f.rec.tens <- fft_2D(tens=rec.tens, pl='amp')
#' f.rec.tens.amp = f.rec.tens[[1]]
#' f.rec.tens.amp[,,1] <- log(f.rec.tens.amp[,,1])
#' plot2D.arr(f.rec.tens.amp)
#'
#'
#' @export
fft_2D <- function(tens, sampling.del = 0.1, pl = 'none'){
  ###Call the relevant libraries
  library(StatsChitran)
  ###Error handling
  #Is tens a rank 3 tensor
  if(!is.tensor.rank3(tens)){
    stop('tens has to be a tensor of rank = 3')
  }
  #does pl belong to either none, amp or phase
  if( pl!='none' && pl!='amp' && pl!='phase'){
    stop("The pl argument should either be a string equalling 'amp', 'phase' or 'none'")
  }
  #is sampling.del a numeric scalar
  if(!is.numeric(sampling.del) || length(sampling.del)!=1){
    stop('sampling.del needs to be a numeric scalar')
  }
  #are the sampling rates the same in both directions
  Xsp = tens[,,2]
  Ysp = tens[,,3]
  X = Xsp[1,]
  Y = Ysp[,1]
  delX = mean(diff(X))
  delY = mean(diff(Y))
  if( abs(abs(delX) - abs(delY))/min(c(abs(delY), abs(delX))) > sampling.del){
    stop('sampling frequencies along X and Y need to be the same')
  }

  #Define the return Tensor and the other arguments
  if(dim(tens)[1] >= dim(tens)[2]){
    new.dim <- dim(tens)[1]
  }else{
    new.dim <- dim(tens)[2]
  }
  ret.tens <- rep(0.0, new.dim*new.dim*4)
  ret.tens <- array(data = ret.tens, dim = c(new.dim, new.dim, 4))
  m <- dim(tens)[1]
  n <- dim(tens)[2]
  p <- dim(tens)[3]
  wm <- dim(ret.tens)[1]
  wn <- dim(ret.tens)[2]
  wp <- dim(ret.tens)[3]
  #Define and abstract the storage modes
  storage.mode(tens) <- 'double'
  storage.mode(ret.tens) <- 'double'
  storage.mode(sampling.del) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(p) <- 'integer'
  storage.mode(wm) <- 'integer'
  storage.mode(wn) <- 'integer'
  storage.mode(wp) <- 'integer'
  #Call the fft_2D_c function
  fortran.res.list <- .C('fft_2D_c', Tens = tens, m = m, n = n, p = p, Transform = ret.tens, wm = wm, wn = wn, wp = wp, sampling_del = sampling.del )
  #Define a list for return
  amp.tens <- fortran.res.list$Transform[,,c(1,3,4)]
  phase.tens <- fortran.res.list$Transform[,,c(2,3,4)]
  ret.list <- vector(mode='list', length = 2)
  ret.list[[1]] <- amp.tens
  ret.list[[2]] <- phase.tens

  #Check the plot criterion and return
  if(pl == 'none'){
    return(ret.list)
  }
  if(pl == 'amp'){
    plot2D.arr(ret.list[[1]])
    return(ret.list)
  }
  if(pl == 'phase'){
    plot2D.arr(ret.list[[2]])
    return(ret.list)
  }

}
