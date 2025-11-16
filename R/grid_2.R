grid_2 <- function(X, Y){
  #Error checking
  if(!is.numeric(X) || !is.numeric(Y)){
    stop("X and Y both should be of type numeric")
  }
  if(!is.vector(X) || !is.vector(Y)){
    stop("X and Y should both be numeric vectors")
  }
  #Introduce the arguments for grid_2_c subroutine
  m.x <- length(X)
  m.y <- length(Y)
  tens <- array(data = 0.0, dim = c(m.y, m.x,2))
  #storage mode for arguments
  storage.mode(X) <- "double"
  storage.mode(Y) <- "double"
  storage.mode(tens) <- "double"
  storage.mode(m.x) <- "integer"
  storage.mode(m.y) <- "integer"
  #Call the function
  res <- .C('grid_2_c', X = X, Y = Y, mx = m.x, my = m.y, tens = tens)
  return(res$tens)
}
