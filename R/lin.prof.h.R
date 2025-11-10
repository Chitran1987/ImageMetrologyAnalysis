#' Extract horizontal line profiles from \eqn{T_{m,n,p=3}} type tensors
#'
#' @description Extracts horizontal line profiles from a Tensor of the type \eqn{\large T_{m,n,p=3}}\cr
#' For more information about \eqn{\large T_{m,n,p=3}} type tensors, look up \link{StatsChitran::img_2_arr}
#'
#'@usage lin.prof.h(tens, h.val, F)
#'lin.prof.h(tens, h.val)
#'
#' @param tens The type \eqn{\large T_{m,n,p=3}} tensor from which the line profile is expected \cr
#'
#' @param h.val The value at which the expected line profile is to be extracted
#'
#' @param pl A boolean bit which when set to \code{TRUE} ensures that the tensor is plotted using \link{StatsChitran::plt.2D.arr}\cr
#'  Defaults to \code{TRUE}
#'
#'
#' @return A Two column dataframe whose no. of rows are equal to \eqn{\large n}
#'  * The first column are the distance values
#'  * The second column are the height/magnitude values
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' rm(list=ls())
#' library(StatsChitran)
#' library(ImageMetrologyAnalysis)
#' X <- seq(-10, 10, by = 0.05)
#' Y <- seq(-10, 10, by=0.05)
#' # Create two tensors using gaussians
#' G_tens <- gauss.2D.nocorr(X, Y, mu.x = 3.5, mu.y = 2.5, sig.x = 1.7, sig.y = 2.3) #1st tensors
#' G1_tens <- gauss.2D.nocorr(X,Y) #2nd tensor
#' G_tens[,,1] <- G_tens[,,1]+G1_tens[,,1] #Add the two tensors
#' plot2D.arr(G_tens) #Plot the tensor
#'
#' #Call the lin.prof.h() function
#' df <- lin.prof.h(G_tens, h.val = 1.9)
#'
#' #Plot the extracted line profile
#' plot(df$dist, df$magnitude, type = 'l')
#'
#'
#' @export
lin.prof.h <- function(tens, h.val, pl=T){
  library(StatsChitran)
  if(!is.tensor.rank3(tens)){
    stop('agument tens has to be of type tensor.rank3')
  }
  if( !is.numeric(h.val) || length(h.val) != 1 ){
    stop('h.val should be a numeric scalar')
  }
  if(!is.logical(pl) || length(pl)!= 1){
    stop('pl should be a boolean bit')
  }
  if( h.val<min(tens[,1,3]) || h.val>max(tens[,1,3]) ){
    stop('h.val is not within the range of Y-values of the tens tensor')
  }
  res_mat <- matrix(data = 0.0, nrow = dim(tens)[2], ncol = 2)
  v <- dim(tens)
  storage.mode(tens) <- 'double'
  storage.mode(h.val) <- 'double'
  storage.mode(res_mat) <- 'double'
  storage.mode(v) <- 'integer'
  m <- v[1]
  n <- v[2]
  p <- v[3]
  res <- .C("lin_prof_h_c", Tens = tens, m=m, n=n, p=p, h_val=h.val, res=res_mat)
  res_df <- as.data.frame(res$res)
  names(res_df) <- c('dist', 'magnitude')
  if(pl){
    v_dummy <- tens[,1,3] #The vector to figure out which value h.val is closest to
    v_dummy <- abs(v_dummy - h.val)
    ind <- which.min(v_dummy)
    tens[ind,,1] <- 1.2*max(tens[,,1])
    plot2D.arr(tens)
  }
  return(res_df)

}
