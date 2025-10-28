normalize <- function(M){
  storage.mode(M) <- "double"
  res <- .C("lineval", X = M, m = as.integer(dim(M)[1]), n = as.integer(dim(M)[2]), low = as.double(0.0), hi = as.double(1.0))
  return(res$X)
}
