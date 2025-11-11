integrate.function <- function(X, Y, y0){
  if(!is.numeric(X) || !is.numeric(Y) || is.numeric(y0)){
    stop('X, Y and y0 have all got to be of type numeric')
  }
  if(length(X) != length(Y)){
    stop('lengths of the X and Y vectors have to be the same')
  }
  if(length(y0) != 1){
    stop('y0 needs to be a numeric scalar')
  }
  m <- length(X)
  res <- matrix( data = rep(0.0, 2*length(X)), ncol = 2 )
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(y0) <- 'double'
  storage.mode(res) <- 'double'
  storage.mode(m) <- 'integer'
  res.mat <- .C('integrate_function_c', X = X, Y = Y, m = m, y0 = y0, dat = res)
  df <- data.frame(res.mat$dat)
  names(df) <- c('X', 'Y')
  return(df)
}
