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
