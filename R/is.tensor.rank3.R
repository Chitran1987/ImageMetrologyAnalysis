#' Plottable rank 3 tensors
#'
#' @description checks whether the object is a rank 3 tensor that can be plotted
#'
#'@usage is.tensor.rank3(arr)
#'
#' @param arr Rank 3 numeric array with further checks
#'
#' @details
#' Checks whether the tensor is of type \eqn{T_{m, n, p}} used by \link{StatsChitran::plot2D.arr}
#' The following checks are run by the program
#' * If object is of type array
#' * If \code{dim(arr)} returns a vector of length 3
#' * If the 3rd index of the array \eqn{p} is equal to 3
#' * If the Tensor slices \eqn{T_{m,n,1}, \,\, T_{m, n, 2} \dots} are individually numeric using \code{is.numeric()}
#' * If the X and Y slices \eqn{T_{m, n, 2}, \, \, T_{m, n, 3}} are rank 1 matrixes
#' * It is important to note that these conditions are necessary but not sufficient for it to be a plottable array
#'
#'
#'
#'
#'
#' @return A boolean \code{TRUE} or \code{FALSE} bit
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' library(StatsChitran)
#' dat <- system.file("extdata", "STM.png", package = "StatsChitran")
#' tens <- img_2_arr(source.png = dat, x.lim = c(0, 6.115*10^-9), y.lim = c(0, 6.115*10^-9))
#' is.tensor.rank3(tens)
#'
#'
#' @export
is.tensor.rank3 <-function(arr){
  if(!is.array(arr)){
    return(F)
  }
  if( length(dim(arr)) != 3 ){
    return(F)
  }
  if(!is.numeric(arr[,,1]) || !is.numeric(arr[,,2]) || !is.numeric(arr[,,3])){
    return(F)
  }
  if(dim(arr)[3] != 3){
    return(F)
  }
  for (i in 2:3) {
    if( qr(arr[,,i])$rank != 1){
      return(F)
    }
  }
  #By this point most of the necessary conditions have been met
  #Its important to remember that these conditions are necessary
  #but not sufficient,
  #because arr[,,2] needs to be in the x-direction
  #and arr[,,3] needs to have a variance in the y-direction
  return(T)
}
