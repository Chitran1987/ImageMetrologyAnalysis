###function sigmoid.2D.plat
sigmoid.2D.plat <- function(X, Y, k, x.left, x.right, y.left, y.right, pl = T){
  library(StatsChitran)
  ###error##########################################
  #
  #
  #
  #
  ###error##########################################

  ###Declare m, n and Z#############################
  m <- length(X)
  n <- length(Y)
  Z <- array(data = 0, dim = c(m, n, 3))

  ###Declare storage modes##########################
  ###X, m, Y, n, Z, k, x_left, y_left, x_right, y_right
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(n) <- 'integer'
  storage.mode(Z) <- 'double'
  storage.mode(k) <- 'double'
  storage.mode(x.left) <- 'double'
  storage.mode(y.left) <- 'double'
  storage.mode(x.right) <- 'double'
  storage.mode(y.right) <- 'double'
  fortran.res <- .C('sigmoid_2D_c',X = X, m = m, Y = Y, n = n, Z = Z, k = k, x_left = x.left, y_left = y.left, x_right = x.right, y_right = y.right)
  if(pl){
    plot2D.mat(X, Y, Z)
    return(fortran.res$Z)
  }
  return(fortran.res$Z)
}
