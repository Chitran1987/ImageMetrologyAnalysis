#' Normalizes/Recalibrates a matrix between 0 and 1
#'
#' This function takes in a matrix and outputs a linearly recalibrated matrix where the minimum value is recalibrated to 0 and the max value is reclaibrated to 1
#'
#' @param M Numeric matrix that you want to normalize.
#'
#' @return A numeric matrix of the same dimension as \code{M}.
#'
#'#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' M <- matrix(seq(1,10), nrow=2)
#' M
#' N <- normalize(M)
#' N
#' @export
normalize <- function(M){
  if(!is.matrix(M)){
    stop('Input argument is expected to be a matrix')
  }
  if(!is.numeric(M)){
    stop('M is expected to be a numeric matrix')
  }
  storage.mode(M) <- "double"
  res <- .C("lineval", X = M, m = as.integer(dim(M)[1]), n = as.integer(dim(M)[2]), low = as.double(0.0), hi = as.double(1.0))
  return(res$X)
}
