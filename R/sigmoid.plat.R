####Create the sigmoid plateau function
sigmoid.plat <- function(X, k, left.cut, right.cut){
  ###error checking
  #
  #
  #
  #
  ###error checking

  ###Declare the variables Y and m
  m <- length(X)
  Y <- vector(mode = 'numeric', length = m)
  ###Declare the storage modes
  storage.mode(m) <- 'integer'
  storage.mode(X) <- 'double'
  storage.mode(Y) <- 'double'
  storage.mode(k) <- 'double'
  storage.mode(left.cut) <- 'double'
  storage.mode(right.cut) <- 'double'
  ###Call the relevant fortran function
  ret.vec <- .C('sigmoid_plat_c', X = X, m = m, Y = Y, k = k, left_cut = left.cut, right_cut = right.cut)
  return(ret.vec$Y)
}
