integrate.num <- function(X, Y, xmin, xmax, riemann){
  if(!is.numeric(X) || !is.numeric(X) || !is.numeric(xmin) || !is.numeric(xmax) ){
    stop('X, Y, xmin and xmax need to be numeric')
  }
  if(length(X) != length(Y)){
    stop('Lengths of X and Y vectors are different')
  }
  if(length(xmin) != 1 || length(xmax) != 1){
    stop('xmin and xmax should have length = 1')
  }
  if(!is.logical(riemann)){
    stop('riemann is supposed to be of type logical')
  }
  m <- length(X)
  res <- 0.0
  riemann <- as.integer(riemann)
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(xmin) <- 'double'
  storage.mode(xmax) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(riemann) <- 'integer'
  storage.mode(res) <- 'double'
  res <- .C('integrate_c', X = X, Y = Y, m = m, xmin = xmin, xmax = xmax, Riemann = riemann, int_val = res)
  return(res$int_val)
}
