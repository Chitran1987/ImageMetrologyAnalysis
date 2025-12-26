sigmoid <- function(X, k, cutoff){
  ######error checking################################
  #
  #
  #
  #
  #
  ######error checking################################

  #####declare m and Y variables######################
  m <- length(X)
  Y <- vector(mode = 'numeric', length = m)

  ###Declare storage modes
  storage.mode(X) <- 'double'
  storage.mode(m) <- 'integer'
  storage.mode(Y) <- 'double'
  storage.mode(k) <- 'double'
  storage.mode(cutoff) <- 'double'

  ###Call the variable
  res <- .C('sigmoid_c', X = X, m = m, Y = Y, k = k, cut = cutoff)
  return(res$Y)
}
