gauss.2D.nocorr <- function(X, Y, A.x, A.y, mu.x, mu.y, sig.x, sig.y){
  if(missing(mu.x)){
    mu.x <- 0
  }
  if(missing(mu.y)){
    mu.y <- 0
  }
  if(missing(sig.x)){
    sig.x <- 1
  }
  if(missing(sig.y)){
    sig.y <- 1
  }
  if(missing(A.x)){
    A.x <- 1/(sqrt(2*pi)*sig.x)
  }
  if(missing(A.y)){
    A.y <- 1/(sqrt(2*pi)*sig.y)
  }
  #define the lengths
  x_len <- length(X)
  y_len <- length(Y)
  #define the final tensor
  tens <- array(0.0, dim = c(y_len, x_len, 3))
  #define the storage modes
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(A.x) <- 'double'
  storage.mode(A.y) <- 'double'
  storage.mode(mu.x) <- 'double'
  storage.mode(mu.y) <- 'double'
  storage.mode(sig.x) <- 'double'
  storage.mode(sig.y) <- 'double'
  storage.mode(x_len) <- 'integer'
  storage.mode(y_len) <- 'integer'
  storage.mode(tens) <- 'double'
  #gauss_2D_nocorr_c(X, Y, x_len, y_len, Ax, Ay, x0, y0, sig_x, sig_y, tens)
  res <- .C('gauss_2D_nocorr_c', X=X, Y=Y, x_len=x_len, y_len=y_len, Ax=A.x, Ay=A.y, x0=mu.x, y0=mu.y, sig_x=sig.x, sig_y=sig.y, tens=tens)
  return(res$tens)

}
