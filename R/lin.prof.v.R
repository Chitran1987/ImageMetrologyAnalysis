#' Extract vertical line profiles from \eqn{T_{m,n,p=3}} type tensors
#'
#' @description Extracts vertical line profiles from a Tensor of the type \eqn{\large T_{m,n,p=3}}\cr
#' For more information about \eqn{\large T_{m,n,p=3}} type tensors, look up \link{StatsChitran::img_2_arr}
#'
#'@usage lin.prof.v(tens, v.val, T)
#'lin.prof.v(tens, v.val)
#'
#' @param tens The type \eqn{\large T_{m,n,p=3}} tensor from which the line profile is expected \cr
#'
#' @param v.val The value at which the expected line profile is to be extracted
#'
#' @param pl A boolean bit which when set to \code{TRUE} ensures that the tensor is plotted using \link{StatsChitran::plt.2D.arr}\cr
#'  Defaults to \code{TRUE}
#'
#'
#' @return A Two column dataframe whose no. of rows are equal to \eqn{\large m}
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
#' # Create two tensors
#' G_tens <- gauss.2D.nocorr(X, Y, mu.x = 3.5, mu.y = 2.5, sig.x = 1.7, sig.y = 2.3) #1st tensors
#' G1_tens <- gauss.2D.nocorr(X,Y) #2nd tensor
#' G_tens[,,1] <- G_tens[,,1]+G1_tens[,,1] #Add the two tensors
#' plot2D.arr(G_tens) #Plot the tensor
#'
#' #Call the lin.prof.v() function
#' df <- lin.prof.v(G_tens, v.val = 1.5)
#'
#' #Plot the extracted line profile
#' plot(df$dist, df$magnitude, type = 'l')
#'
#'
#' @export
lin.prof.v <- function(tens, v.val, pl=T){
  library(StatsChitran)
  if(!is.tensor.rank3(tens)){
    stop('agument tens has to be of type tensor.rank3')
  }
  if( !is.numeric(v.val) || length(v.val) != 1 ){
    stop('v.val should be a numeric scalar')
  }
  if(!is.logical(pl) || length(pl)!= 1){
    stop('pl should be a boolean bit')
  }
  if( v.val<min(tens[1,,2]) || v.val>max(tens[1,,2]) ){
    stop('h.val is not within the range of Y-values of the tens tensor')
  }
  res_mat <- matrix(data = 0.0, nrow = dim(tens)[1], ncol = 2)
  v <- dim(tens)
  storage.mode(tens) <- 'double'
  storage.mode(v.val) <- 'double'
  storage.mode(res_mat) <- 'double'
  storage.mode(v) <- 'integer'
  m <- v[1]
  n <- v[2]
  p <- v[3]
  res <- .C("lin_prof_v_c", Tens = tens, m=m, n=n, p=p, v_val=v.val, res=res_mat)
  res_df <- as.data.frame(res$res)
  names(res_df) <- c('dist', 'magnitude')
  if(pl){
    h_dummy <- tens[1,,2] #The vector to figure out which value v.val is closest to
    h_dummy <- abs(h_dummy - v.val)
    ind <- which.min(h_dummy)
    tens[,ind,1] <- 1.2*max(tens[,,1])
    plot2D.arr(tens)
  }
  return(res_df)
}
