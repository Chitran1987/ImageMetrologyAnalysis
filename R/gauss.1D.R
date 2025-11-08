gauss.1D <- function(X, A, mu, sig){
  if(missing(mu)){
    mu <- 0
  }
  if(missing(sig)){
    sig <- 1
  }
  if(missing(A)){
    mu <- 1/(sqrt(2*pi)*sig)
  }
  x_len <- length(X)

  #define the storage modes
  storage.mode(X) <- 'double'
  storage.mode(A) <- 'double'
  storage.mode(mu) <- 'double'
  storage.mode(sig) <- 'double'
  storage.mode(x_len) <- 'integer'
  res <- .C("gauss_1D_c", x=X, x_len=x_len, y=X, A=A, x0=mu, sig=sig)
  return(res$y)
}
