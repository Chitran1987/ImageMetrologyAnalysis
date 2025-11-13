fft_1D <- function(X, Y){
  if( !is.numeric(X) || !is.numeric(Y)){
    stop('X and Y both have to be numeric types')
  }
  if( length(X) != length(Y)){
    stop('X and Y both need to have equal lengths')
  }
  m <- length(X)
  res <- rep(0, 4*length(X)+4)
  res <- array( data = res, dim = c(length(X)+1, 2, 2) )
  storage.mode(X) <- "double"
  storage.mode(Y) <- "double"
  storage.mode(res) <- "double"
  storage.mode(m) <- "integer"

  ret_var <- .C("fft_1D_c" X = X, Y = Y, m = m, res = res)
  ret_var <- ret_var$res
  df <- data.frame( freq = ret_var[,1,1] , Ampl = [,2,1] , phase = [,2,2])
  return(df)
}
